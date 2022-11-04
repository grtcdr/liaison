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
  '((github    . (:domain "github.com"
		  :via #'forgecast--build-github-resource-url))
    (sourcehut . (:domain "git.sr.ht"
		  :via #'forgecast--build-sourcehut-resource-url))
    (codeberg  . (:domain "codeberg.org"
		  :via #'forgecast--build-gitea-resource-url))
    (gitea     . (:domain "gitea.com"
		  :via #'forgecast--build-gitea-resource-url)))
  "Association list of forges and their corresponding plist of domains and constructors.")

(defun forgecast--forge-domain (forge)
  (plist-get (alist-get forge forgecast-forge-plist) :domain))

(defun forgecast--forge-url-builder (forge)
  (plist-get (alist-get forge forgecast-forge-plist) :via))

(defun forgecast--get-current-branch ()
  (vc-git-symbolic-commit
   (vc-git--rev-parse "@{push}")))

(defun forgecast--get-resource-slug ()
  "Determines the slug of the current buffer.

The slug is the path of the resource relative to the value
returned by ’forgecast-get-resource-url'."
  (let* ((buffer (buffer-file-name))
	 (root (vc-find-root buffer ".git")))
    (string-remove-prefix
     (expand-file-name root) buffer)))

(defun forgecast--build-github-resource-url (slug type &optional forge)
  (let* ((forge (if (eq type 'blob)
		    "raw.githubusercontent.com"
		  (forgecast--forge-domain 'github)))
	 (branch (forgecast--get-current-branch))
	 (plain-query-string (unless (not (eq type 'plain)) "?plain=1"))
	 (type (cond ((eq type 'log) "commits")
		     ((eq type 'edit) "edit")
		     ((eq type 'blob) "")
		     ((eq type 'plain) "blob")
		     ((eq type 'blame) "blame")
		     ((or (eq type 'tree) (eq type 'plain)) "blob")
		     (t (error "Type is invalid or does not apply to this backend."))))
	 (resource (forgecast--get-resource-slug)))
    (concat "https://"
	    (mapconcat 'identity (remove "" (list forge slug type branch resource)) "/")
	    plain-query-string)))

(defun forgecast--build-sourcehut-resource-url (slug type &optional forge)
  (format-spec
   "https://%d/%s/%t/%b/%x/%r"
   `((?d . ,(forgecast--forge-domain forge))
     (?s . ,(concat "~" slug))
     (?t . ,(cond ((eq type 'log) "log")
		  ((eq type 'tree) "tree")
		  ((eq type 'blob) "blob")
		  ((eq type 'blame) "blame")
		  (t (error "Type is invalid or does not apply to this backend."))))
     (?b . ,(forgecast--get-current-branch))
     (?x . ,(cond ((eq type 'blob) "")
		  (t "item")))
     (?r . ,(forgecast--get-resource-slug)))))



(defun forgecast--build-gitea-resource-url (slug type &optional forge)
  (format-spec
   "https://%d/%s/%t/branch/%b/%r"
   `((?d . ,(forgecast--forge-domain forge))
     (?s . ,(concat "~" slug))
     (?t . ,(cond ((eq type 'log) "commits")
		  ((eq type 'tree) "src")
		  ((eq type 'blob) "raw")
		  ((eq type 'blame) "blame")
		  (t (error "Type is invalid or does not apply to this backend."))))
     (?b . ,(forgecast--get-current-branch))
     (?r . ,(forgecast--get-resource-slug)))))

(defun forgecast-get-resource-url (slug type forge)
  "Construct the standard URL of a given FORGE by specifying
the repository SLUG and the TYPE of information to access.

FORGE is a property from the ’forgecast--forge-plist’ variable.

- If FORGE is set to ’github’ then TYPE can be one of ’log’, ’tree’, ’blob’, ’blame’ or ’plain’.
- If FORGE is set to ’gitea’ then TYPE can be one of ’log’, ’tree’, ’blob’ or ’plain’.
- If FORGE is set to ’sourcehut’ then TYPE can take a value ’log’, ’tree’, ’blob’ or ’blame’.

SLUG is a string and the combination of your username and the
name of your repository, e.g. \"octopus/website\"."
  (funcall (eval (forgecast--forge-url-builder forge)) slug type)
  (cond ((equal forge 'github)
  	 (forgecast--forge-constructor)
  	 (forgecast--build-github-resource-url slug type))
  	((equal forge 'sourcehut)
  	 (forgecast--build-sourcehut-resource-url slug type))
  	(t (error "Could not find forge from known list of forges."))))

(provide 'forgecast)
;; forgecast.el ends here
