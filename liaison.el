;;; liaison.el --- Link your content to its source. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/liaison

;; Liaison is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Liaison is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Liaison. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Liaison generalizes and automates the process of linking an HTML
;; document to its corresponding source document originally written in
;; the Org markup language.

;;; Code:

(require 'vc)

(defvar liaison-forge-alist
  '(("github.com"                   . #'liaison--build-github-resource-url)
    ("gitlab.com"                   . #'liaison--build-gitlab-resource-url)
    ("codeberg.org"                 . #'liaison--build-gitea-resource-url)
    ("git.sr.ht"                    . #'liaison--build-sourcehut-resource-url)
    ("git.savannah.gnu.org/cgit"    . #'liaison--build-cgit-resource-url)
    ("git.savannah.nongnu.org/cgit" . #'liaison--build-cgit-resource-url))
  "Alist of forges and their corresponding function which is used to
build their resource URLs.")

(defun liaison--forge-function (forge)
  (alist-get forge liaison-forge-alist nil nil #'string=))

(defun liaison--git-remote-to-https (remote)
  (string-replace
   "git@"
   "https://"
   (car (split-string remote ":"))))

(defun liaison--assoc-forge (remote)
  (let ((forge nil))
    (dolist (f liaison-forge-alist)
      (when (string-prefix-p
	     (concat "https://" (car f))
	     (cond ((string-prefix-p "git@" remote)
		    (liaison--git-remote-to-https remote))
		   ((string-prefix-p "https://" remote)
		    remote)))
	(setq forge (car f))))
    forge))

(defun liaison--get-branch ()
  (car (vc-git-branches)))

(defun liaison--get-remote ()
  (vc-git-repository-url
   (buffer-file-name) nil))

(defun liaison--get-resource-slug ()
  "Determine the slug of the current buffer."
  (let* ((buffer (buffer-file-name))
	 (root (vc-find-root buffer ".git")))
    (string-remove-prefix
     (expand-file-name root)
     buffer)))

(defun liaison-get-resource-url (type)
  "Return the URL of the current resource given the TYPE."
  (let* ((remote (liaison--get-remote))
	 (forge (liaison--assoc-forge remote)))
    (funcall (eval (liaison--forge-function forge)) remote type)))

(defun liaison--build-cgit-resource-url (remote type)
  "Return the URL representing a resource hosted on a cgit
instance. TYPE can be one of ’log’, ’tree’ or ’blob’."
  (unless (not (member type '(log tree blob)))
    (format-spec
     "%f/%t/branch/%b/%r"
     `((?f . ,(concat "https://" (liaison--assoc-forge remote)))
       (?t . ,(cond ((eq type 'log) "log")
		    ((eq type 'tree) "src")
		    ((eq type 'blob) "plain")))
       (?b . ,(liaison--get-branch))
       (?r . ,(liaison--get-resource-slug))))))

(defun liaison--build-gitea-resource-url (remote type)
  "Return the URL representing a resource hosted on Gitea or a
custom instance. TYPE can be one of ’log’, ’tree', ’blob’, or
’blame’."
  (unless (not (member type '(log tree blob blame)))
    (format-spec
     "%d/%s/%t/branch/%b/%r"
     `((?d . ,(concat "https://" (liaison--assoc-forge remote)))
       (?s . ,(if (string-prefix-p "git@" remote)
		  (string-trim (car (cdr (split-string remote ":"))) nil ".git")
		(string-trim remote
			     (concat "https://" (liaison--assoc-forge remote) "/")
			     ".git")))
       (?t . ,(cond ((eq type 'log) "commits")
		    ((eq type 'tree) "src")
		    ((eq type 'blob) "raw")
		    ((eq type 'blame) "blame")))
       (?b . ,(liaison--get-branch))
       (?r . ,(liaison--get-resource-slug))))))

(defun liaison--build-github-resource-url (remote type)
  "Return the URL representing a resource hosted on
GitHub. TYPE can be one of ’log’, ’edit’, ’blob’, ’plain’,
’blame’ or ’tree’."
  (unless (not (member type '(log edit blob blame plain tree)))
    (let* ((forge (if (eq type 'blob)
		      "https://raw.githubusercontent.com"
		    (concat "https://" (liaison--assoc-forge remote))))
	   (branch (liaison--get-branch))
	   (resource (liaison--get-resource-slug))
	   (slug (if (string-prefix-p "git@" remote)
		     (string-trim (cadr (split-string remote ":")) nil ".git")
		   (string-trim remote
				(concat "https://" (liaison--assoc-forge remote) "/")
				".git")))
	   (plain-query-string (unless (not (eq type 'plain))
				 "?plain=1"))
	   (type (cond ((eq type 'log) "commits")
		       ((eq type 'edit) "edit")
		       ((eq type 'blob) "")
		       ((eq type 'plain) "blob")
		       ((eq type 'blame) "blame")
		       ((or (eq type 'tree) (eq type 'plain)) "blob"))))
      (concat 
       (mapconcat 'identity (remove "" (list forge slug type branch resource)) "/")
       plain-query-string))))

(defun liaison--build-gitlab-resource-url (remote type)
    "Return the URL representing a resource hosted on GitLab or a
custom instance. TYPE can be any one of ’log’, ’tree’, ’blob’ or
’blame’ or ’plain’."
  (unless (not (member type '(log tree blob blame plain)))
    (let* ((?d (concat "https://" (liaison--assoc-forge remote)))
	   (?s (if (string-prefix-p "git@" remote)
		   (string-trim (car (cdr (split-string remote ":"))) nil ".git")
		 (string-trim remote
			      (concat "https://" (liaison--assoc-forge remote) "/")
			      ".git")))
	   (?t (cond ((eq type 'log) "commits")
		     ((eq type 'tree) "blob")
		     ((eq type 'blob) "raw")
		     ((eq type 'blame) "blame")
		     ((eq type 'plain) "blob")))
	   (plain-query-string (unless (not (eq type 'plain))
				 "?plain=1"))
	   (?b (liaison--get-branch))
	   (?r (liaison--get-resource-slug)))
      (concat
       (mapconcat 'identity (remove "" (list forge slug "-" type branch resource)) "/")
       plain-query-string))))

(defun liaison--build-sourcehut-resource-url (remote type)
  "Return the URL representing a resource hosted on SourceHut or a
custom instance. TYPE can be any one of ’log’, ’tree’, ’blob’ or
’blame’."
  (unless (not (member type '(log tree blob blame)))
    (let* ((forge (concat "https://" (liaison--assoc-forge remote)))
	   (slug (if (string-prefix-p "git@" remote)
		     (cadr (split-string remote ":"))
		   (string-trim remote (concat "https://" (liaison--assoc-forge remote) "/"))))
	   (type (downcase (symbol-name type)))
	   (branch (liaison--get-branch))
	   (suffix (if (eq type 'blob) "" "item"))
	   (resource (liaison--get-resource-slug)))
      (mapconcat 'identity (remove "" (list forge slug type branch suffix resource)) "/"))))

(provide 'liaison)
;; liaison.el ends here
