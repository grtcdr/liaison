;;; op-publish.el  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/liaison

;; This file is not part of GNU Emacs.

;; op-publish.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; op-publish.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with op-publish.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; op-publish.el is the publishing specification of http://grtcdr.tn/liaison.

;;; Code:

(add-to-list 'load-path (concat default-directory "lisp"))

(require 'ox-publish)
(require 'op-template)
(require 'op-package)
(require 'project)

;; Temporarily change the default directory because this website is
;; nested within the library - it's either this or a symlink.
(let ((default-directory (project-root (project-current))))
  ;; Import the library
  (add-to-list 'load-path default-directory)
  (require 'liaison))

;; Redefinition of a built-in function
(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble."
  `((?e . ,(liaison-get-resource-url 'edit))
    (?m . ,(liaison-get-resource-url 'blame))
    (?b . ,(liaison-get-resource-url 'blob))
    (?t . ,(liaison-get-resource-url 'tree))
    (?l . ,(liaison-get-resource-url 'log))
    (?p . ,(liaison-get-resource-url 'plain))))

(defun op-publish-manual-publisher (plist filename pub-dir)
  "Publish the manual to multiple formats."
  (org-html-publish-to-html plist filename pub-dir)
  (if (string= (getenv "CI") "true")
      (org-latex-publish-to-latex plist filename pub-dir)
    (org-latex-publish-to-pdf plist filename pub-dir)))

;; Metadata which appears in the manual
(setq user-full-name "Aziz Ben Ali")

;; This is where the project cache is stored
(setq org-publish-timestamp-directory ".cache/")

;; Define how source code is exported
(setq org-src-fontify-natively t
      org-src-preserve-indentation t)

;; Global settings for LaTeX exports
(setq org-latex-src-block-backend 'engraved)

;; Global settings for HTML exports
(setq org-html-doctype "html5"
      org-html-preamble nil
      org-html-postamble nil
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-htmlize-output-type 'css
      org-html-htmlize-output-type 'css)

;; Project specification
(setq org-publish-project-alist
      (list
       (list "root" ;; Specify how files at the root of the site are published
	     :base-extension "org"
	     :base-directory "src"
	     :publishing-directory "public"
	     :publishing-function 'org-html-publish-to-html
	     :with-toc nil
	     :section-numbers nil
	     :html-head (op-template-metadata)
	     :html-preamble 'op-template-main-navbar)
       (list "examples" ;; Specify how articles are published
	     :base-extension "org"
	     :base-directory "src/examples"
	     :publishing-directory "public/examples"
	     :publishing-function 'org-html-publish-to-html
	     :html-head
	     (concat (op-template-metadata)
		     (op-template-stylesheet "/liaison/css/article.css"))
	     :html-preamble 'op-template-main-navbar
	     :html-postamble 'op-template-meta-links)
       (list "manual" ;; Specify how the manual is published
	     :base-extension "org"
	     :base-directory "src"
	     :publishing-directory "public"
	     :publishing-function 'op-publish-manual-publisher
	     :exclude ".*"
	     :include '("manual.org")
	     :html-toplevel-hlevel 2
	     :html-head (op-template-metadata)
	     :html-preamble 'op-template-main-navbar
	     :filename "manual"
	     :with-email t
	     :with-author t)
       (list "stylesheets" ;; Specify how stylesheets are published
	     :base-extension "css"
	     :base-directory "src/css"
	     :publishing-directory "public/css"
	     :publishing-function 'org-publish-attachment)
       (list "all" ;; Combine the publishing projects
	     :components '("root" "manual" "examples" "stylesheets"))))
