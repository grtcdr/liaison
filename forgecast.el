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
;; along with Stack. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Forgecast provides a set of helper functions that allow the user to
;; link various resources to the forge that hosts them.

;;; Code:

(require 'project)
(require 'vc)

(defvar forgecast-forge-plist
  '(:github "github.com"
    :rawgithub "raw.githubusercontent.com"
    :sourcehut "git.sr.ht")
  "A property list mapping forges to their respective domains.")

(defun forgecast-get-resource-slug ()
  "Determines the slug of the current buffer.

The slug is the path of the resource relative to the value
returned by ’forgecast-get-resource-url'."
  (let* ((buffer (buffer-file-name))
	 (root (vc-find-root buffer ".git")))
    (string-remove-prefix
     (expand-file-name root) buffer)))

(defun forgecast--build-github-resource-url (slug branch type)
  (let ((forge (cond ((eq type 'blob) (plist-get forgecast-forge-plist :rawgithub))
		     (t (plist-get forgecast-forge-plist :github))))
	(type (cond ((eq type 'log) "commits")
		    ((eq type 'tree) "blob")
		    ((eq type 'blob) "")
		    (t (error "Type is invalid or does not apply to this backend."))))
	(resource (forgecast-get-resource-slug)))
    (mapconcat 'identity (remove "" (list "https://" forge slug type branch resource)) "/")))

(defun forgecast--build-sourcehut-resource-url (slug branch type)
  (format-spec
   "https://%d/%s/%t/%b/%x/%r"
   `((?d . ,(plist-get forgecast-forge-plist :sourcehut))
     (?s . ,(concat "~" slug))
     (?t . ,(cond ((eq type 'log) "log")
		  ((eq type 'tree) "tree")
		  ((eq type 'blob) "blob")
		  (t (error "Type is invalid or does not apply to this backend."))))
     (?b . ,branch)
     (?x . ,(cond ((eq type 'blob) "")
		  (t "item")))
     (?r . ,(forgecast-get-resource-slug)))))

(defun forgecast-get-resource-url (forge slug type)
  "Construct the standard URL of a given FORGE by specifying
the repository SLUG and the TYPE of information to access.

FORGE is a property from the ’forges’ variable.

SLUG is a string and the combination of your username and the
name of your repository, e.g. \"octopus/website\".

TYPE can take a value of ’log’, ’tree’ or ’blob’.

RESOURCE is a filename relative to the root of the project."
  (let ((branch "main"))
    (cond ((equal forge :github)
	   (forgecast--build-github-resource-url slug branch type))
	  ((equal forge :sourcehut)
	   (forgecast--build-sourcehut-resource-url slug branch type))
	  (t (error "Could not find forge from known list of forges.")))))

(provide 'forgecast)
