;;; publish.el --- A minimal publishing script.

;; Copyright (C) 2022 Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/liaison

;; publish.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; publish.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with publish.el. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ox-publish)
(require 'project)
(require 'shr)

;; Import the library
(let ((default-directory (project-root (project-current))))
  (add-to-list 'load-path default-directory)
  (require 'liaison))

(defun site/link (rel href)
  "Format as a ’link’ tag, a resource located at HREF with a
relationship of REL."
  (shr-dom-to-xml
   `(link ((rel . ,rel)
	   (href . ,href)))))

(defvar site/html-head
  (concat
   (shr-dom-to-xml '(base ((href . "/liaison/"))))
   (site/link "stylesheet" "https://grtcdr.tn/css/common.css")
   (site/link "stylesheet" "https://grtcdr.tn/css/heading.css")
   (site/link "stylesheet" "https://grtcdr.tn/css/source.css")
   (site/link "stylesheet" "https://grtcdr.tn/css/table.css")
   (site/link "stylesheet" "https://grtcdr.tn/css/nav.css")
   (site/link "stylesheet" "https://grtcdr.tn/css/org.css")
   (site/link "icon" "https://grtcdr.tn/assets/favicon.ico"))
  "HTML headers shared across projects.")

;; Redefinition of built-in function
(defun org-html-format-spec (info)
  "Return a list of format strings representing the format specification."
  `((?b . ,(liaison-get-resource-url 'blob))
    (?m . ,(liaison-get-resource-url 'blame))
    (?t . ,(liaison-get-resource-url 'tree))
    (?l . ,(liaison-get-resource-url 'log))
    (?p . ,(liaison-get-resource-url 'plain))
    (?e . ,(liaison-get-resource-url 'edit))))

(defvar main-preamble
      (shr-dom-to-xml
       '(nav nil
	     (ul nil
		 (li nil
		     (a ((href . "https://grtcdr.tn"))
			"grtcdr.tn"))
		 (li nil
		     (a ((href . "index.html")) "liaison"))
		 (li nil
		     (a ((href . "manual/liaison.html"))
			"manual"))
		 (li nil
		     (a ((href . "CHANGELOG.html"))
			"changelog"))
		 (li nil
		     (a ((href . "TODO.html"))
			"to-dos"))
		 (li nil
		     (a ((href . "https://github.com/grtcdr/liaison"))
			"github")))))
      "Define an HTML snippet/template used as a preamble across all
projects.")

(defvar article-postamble
  (shr-dom-to-xml
   '(div ((class . "meta"))
	 (ul nil
	     (li nil
		 (a ((href . "%e"))
		    "edit"))
	     (li nil
		 (a ((href . "%m"))
		    "blame"))
	     (li nil
		 (a ((href . "%b"))
		    "blob"))
	     (li nil
		 (a ((href . "%t"))
		    "tree"))
	     (li nil
		 (a ((href . "%l"))
		    "log"))
	     (li nil
		 (a ((href . "%p"))
		    "plain")))))
  "Define an HTML snippet/template used as a postamble by the
articles project.")

(defvar site/alternate-divs
  '((preamble "div" "_preamble")
    (content "div" "_content")
    (postamble "div" "_postamble"))
  "Defines an alternate div format which avoids duplicate identifiers.")

;; You don't necessarily have to set these variables
(setq org-publish-timestamp-directory ".cache/"
      org-src-fontify-natively nil
      org-html-preamble nil
      org-html-postamble nil
      org-html-doctype "html5"
      org-html-htmlize-output-type nil
      org-html-head-include-default-style nil)

;; Project specification
(setq org-publish-project-alist
      (list
       (list "main" ;; This specifies how files at the root of the site get published
	     :base-extension "org"
	     :base-directory "src/"
	     :publishing-directory "public/"
	     :publishing-function 'org-html-publish-to-html
	     :html-head (concat site/html-head (site/link "stylesheet" "css/meta.css"))
	     :html-preamble main-preamble)
       (list "articles" ;; This specifies how articles get published
	     :base-extension "org"
	     :base-directory "src/articles/"
	     :publishing-directory "public/articles/"
	     :publishing-function 'org-html-publish-to-html
	     :html-divs site/alternate-divs
	     :html-postamble article-postamble)
       (list "manual" ;; This specifies how the manual gets published
	     :base-extension "org"
	     :base-directory "../manual/"
	     :publishing-directory "public/manual/"
	     :publishing-function 'org-html-publish-to-html
	     :html-head site/html-head
	     :html-preamble main-preamble)
       (list "doc" ;; This specifies how general documentation gets published
	     :base-extension "org"
	     :base-directory "../"
	     :publishing-directory "public/"
	     :publishing-function 'org-html-publish-to-html
	     :with-toc nil
	     :section-numbers nil
	     :html-head site/html-head
	     :html-preamble main-preamble)
       (list "css" ;; This specifies how stylesheets get published
	     :base-extension "css"
	     :base-directory "src/css"
	     :publishing-directory "public/css/"
	     :publishing-function 'org-publish-attachment)
       (list "all" ;; This combines all the previous projects
	     :components '("articles" "doc" "manual" "main" "css"))))
