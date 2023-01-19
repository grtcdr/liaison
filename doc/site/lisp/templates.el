;;; templates.el

;; Copyright (C) 2023  Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/liaison

;; This file is not part of GNU Emacs.

;; templates.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; templates.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with templates.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; templates.el provides the XML templates of http://grtcdr.tn/darkman.el.

;;; Code:

(require 'shr)

(defun site/main-preamble (_)
  "Return HTML template shared among publishing projects."
  (shr-dom-to-xml
   '(nav nil
	 (ul nil
	     (li nil
		 (a ((href . "/liaison/index.html")) "Liaison"))
	     (li nil
		 (a ((href . "/liaison/CHANGELOG.html"))
		    "Changelog"))
	     (li nil
		 (a ((href . "/liaison/TODO.html"))
		    "To-dos"))
	     (li nil
		 (a ((href . "https://github.com/grtcdr/liaison"))
		    "Development"))))))

(defun site/article-postamble (_)
    "Return HTML template used as a postamble by the articles publishing project."
  (shr-dom-to-xml
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

(provide 'templates)
;; templates.el ends here
