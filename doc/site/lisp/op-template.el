;;; op-template.el  -*- lexical-binding:t -*-

;; Copyright (C) 2023  Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/liaison

;; This file is not part of GNU Emacs.

;; op-template.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; op-template.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with op-template.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; op-template.el provides HTML templates for https://grtcdr.tn/darkman.el.

;;; Code:

(require 'shr)

(defalias 'sexp->xml #'shr-dom-to-xml)

(defun op-template-stylesheet (href)
  "Format a stylesheet with attribute HREF."
  (sexp->xml
   `(link ((rel . "stylesheet")
	   (href . ,href)))))

(defun op-template-main-navbar (_)
  "HTML template shared among publishing projects."
  (sexp->xml
   '(nav nil
	 (ul nil
	     (li nil
		 (a ((href . "/liaison/index.html"))
		    "Home"))
	     (li nil
		 (a ((href . "/liaison/CHANGELOG.html"))
		    "Changelog"))
	     (li nil
		 (a ((href . "/liaison/TODO.html"))
		    "To-dos"))
	     (li nil
		 (a ((href . "https://github.com/grtcdr/liaison"))
		    "Development"))))))

(defun op-template-meta-links (_)
  "HTML template used by the examples publishing project."
  (sexp->xml
   '(div ((class . "meta"))
	 (ul nil
	     (li nil
		 (a ((href . "%e"))
		    "Edit"))
	     (li nil
		 (a ((href . "%m"))
		    "Blame"))
	     (li nil
		 (a ((href . "%b"))
		    "Blob"))
	     (li nil
		 (a ((href . "%t"))
		    "Tree"))
	     (li nil
		 (a ((href . "%l"))
		    "Log"))
	     (li nil
		 (a ((href . "%p"))
		    "Plain"))))))

(defun op-template-metadata ()
  "Return the metadata shared among publishing projects."
  (concat
   (op-template-stylesheet "https://grtcdr.tn/css/common.css")
   (sexp->xml
    '(link ((rel . "icon")
	    (type . "image/x-icon")
	    (href . "https://grtcdr.tn/assets/favicon.ico"))))))

(provide 'op-template)
;; op-template.el ends here
