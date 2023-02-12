;;; op-package.el  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/liaison

;; This file is not part of GNU Emacs.

;; op-package.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; op-package.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with op-package.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; op-package.el provides helper functions to handle the dependencies of
;; https://grtcdr.tn.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun op-package--initialize ()
  "Initialize the package manager and its archives."
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(defun op-package-install (packages)
  "Install the list of PACKAGES."
  (op-package--initialize)
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(op-package-install '(htmlize))

(provide 'op-package)
;; op-package.el ends here
