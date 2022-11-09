;;; forgecast.el --- Cast resources to their forges. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/forgecast

;; Forgecast is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Forgecast is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Forgecast. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Forgecast provides a set of helper functions that allow linking
;; files to their corresponding resource at a remote repository.

;;; Code:

(require 'project)
(require 'vc)

(defvar forgecast-forge-plist
  '(("github.com" . #'forgecast--build-github-resource-url)
    ("git.sr.ht" . #'forgecast--build-sourcehut-resource-url)
    ("git.savannah.gnu.org/cgit" . #'forgecast--build-cgit-resource-url)
    ("codeberg.org" . #'forgecast--build-gitea-resource-url))
  "Alist of forges and their corresponding function which is used to
build their resource URLs")

(defun forgecast--forge-function (forge)
  (alist-get forge forgecast-forge-plist nil nil #'string=))

(defun forgecast--git-remote-to-https (remote)
  (string-replace
   "git@"
   "https://"
   (car (split-string remote ":"))))

(defun forgecast--assoc-forge (remote)
  (let ((forge nil))
    (dolist (f forgecast-forge-plist)
      (when (string-prefix-p
	     (concat "https://" (car f))
	     (cond ((string-prefix-p "git@" remote)
		    (forgecast--git-remote-to-https remote))
		   ((string-prefix-p "https://" remote)
		    remote)))
	(setq forge (car f))))
    forge))

(defun forgecast--get-current-branch ()
  (vc-git--symbolic-ref
   (vc-git--rev-parse "@{push}" t)))

(defun forgecast--get-remote ()
  (vc-git-repository-url (buffer-file-name) nil))

(defun forgecast--get-resource-slug ()
  "Determines the slug of the current buffer.

The slug is the path of the resource relative to the value
returned by ’forgecast-get-resource-url'."
  (let* ((buffer (buffer-file-name))
	 (root (vc-find-root buffer ".git")))
    (string-remove-prefix
     (expand-file-name root) buffer)))

(defun forgecast-get-resource-url (type)
  "Construct the standard URL of a given FORGE by specifying
the repository SLUG and the TYPE of information to access.

FORGE is a property from the ’forgecast--forge-plist’ variable.

SLUG is a string and the combination of your username and the
name of your repository, e.g. \"octopus/website\"."
  (let* ((remote (forgecast--get-remote))
	 (forge (forgecast--assoc-forge remote)))
    (funcall (eval (forgecast--forge-function forge)) remote type)))

(defun forgecast--build-cgit-resource-url (remote type)
  "This function returns the URL representing a resource hosted on a
cgit-based repository.

TYPE can be one of ’log’, ’tree’ or ’blob’.
"
  (unless (not (member type '(log tree blob)))
    (format-spec
     "%f/%t/branch/%b/%r"
     `((?f . ,(concat "https://" (forgecast--assoc-forge remote)))
       (?t . ,(cond ((eq type 'log) "log")
		    ((eq type 'tree) "src")
		    ((eq type 'blob) "plain")
		    (t (error "Type is invalid or does not apply to this forge."))))
       (?b . ,(forgecast--get-current-branch))
       (?r . ,(forgecast--get-resource-slug))))))

(defun forgecast--build-gitea-resource-url (remote type)
  "This function returns the URL representing a resource hosted on
Gitea or a Gitea-based repository. TYPE can be one of ’log’,
’tree', ’blob’, or ’blame’.
"
  (unless (not (member type '(log tree blob)))
    (format-spec
     "%d/%s/%t/branch/%b/%r"
     `((?d . ,(concat "https://" (forgecast--assoc-forge remote)))
       (?s . ,(if (string-prefix-p "git@" remote)
		  (string-trim (car (cdr (split-string remote ":"))) nil ".git")
		(string-trim remote
			     (concat (if forge (forgecast--forge-base 'gitea)
				       (forgecast--forge-base forge))
				     "/")
			     ".git")))
       (?t . ,(cond ((eq type 'log) "commits")
		    ((eq type 'tree) "src")
		    ((eq type 'blob) "raw")
		    ((eq type 'blame) "blame")
		    (t (error "Type is invalid or does not apply to this forge."))))
       (?b . ,(forgecast--get-current-branch))
       (?r . ,(forgecast--get-resource-slug))))))

(defun forgecast--build-github-resource-url (remote type)
  "This function returns the URL representing a resource hosted on
GitHub. TYPE can be one of ’log’, ’edit’, ’blob’, ’plain’,
’blame’ or ’tree’."
  (unless (not (member type '(log edit blob blame plain tree)))
    (let* ((forge (if (eq type 'blob)
		      "https://raw.githubusercontent.com"
		    (concat "https://" (forgecast--assoc-forge remote))))
	   (branch (forgecast--get-current-branch))
	   (resource (forgecast--get-resource-slug))
	   (slug (if (string-prefix-p "git@" remote)
		     (string-trim (cadr (split-string remote ":")) nil ".git")
		   (string-trim remote
				(concat forge "/")
				".git")))
	   (plain-query-string (unless (not (eq type 'plain))
				 "?plain=1"))
	   (type (cond ((eq type 'log) "commits")
		       ((eq type 'edit) "edit")
		       ((eq type 'blob) "")
		       ((eq type 'plain) "blob")
		       ((eq type 'blame) "blame")
		       ((or (eq type 'tree) (eq type 'plain)) "blob")
		       (t (error "Type is invalid or does not apply to this forge.")))))
      (mapconcat 'identity (remove "" (list forge slug type branch resource plain-query-string)) "/"))))

(defun forgecast--build-sourcehut-resource-url (remote type)
  "This function returns the URL representing a resource hosted on
SourceHut or a SourceHut-based forge. TYPE can be any one of
’log’, ’tree’, ’blob’ or ’blame’."
  (unless (not (member type '(log tree blob blame)))
    (let* ((forge (concat "https://" (forgecast--assoc-forge remote)))
	   (slug (if (string-prefix-p "git@" remote)
		     (cadr (split-string remote ":"))
		   (string-trim remote (concat forge "/"))))
	   (type (cond ((eq type 'log) "log")
		       ((eq type 'tree) "tree")
		       ((eq type 'blob) "blob")
		       ((eq type 'blame) "blame")))
	   (branch (forgecast--get-current-branch))
	   (suffix (cond ((eq type 'blob) "")
			 (t "item")))
	   (resource (forgecast--get-resource-slug)))
      (mapconcat 'identity (remove "" (list forge slug type branch suffix resource)) "/"))))

(provide 'forgecast)
;; forgecast.el ends here
