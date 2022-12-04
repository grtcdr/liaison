;;; publish.el --- A minimal publishing script.

(require 'ox-publish)
(require 'project)

;; Import the library
(let ((default-directory (project-root (project-current))))
  (add-to-list 'load-path default-directory)
  (require 'forgecast))

;; You don't necessarily have to set these variables
(setq org-publish-timestamp-directory ".cache/"
      org-src-fontify-natively nil
      org-html-htmlize-output-type nil)

(defun site/read-template (filename)
  "Read contents of FILENAME from the templates directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "src" "templates" filename))
    (buffer-string)))

(defvar site/html-head
  "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\">"
  "HTML header shared across projects.")

;; Redefinition of built-in function
(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble."
  `((?b . ,(forgecast-get-resource-url 'blob))
    (?m . ,(forgecast-get-resource-url 'blame))
    (?t . ,(forgecast-get-resource-url 'tree))
    (?l . ,(forgecast-get-resource-url 'log))
    (?p . ,(forgecast-get-resource-url 'plain))
    (?e . ,(forgecast-get-resource-url 'edit))))

;; Project specification
(setq org-publish-project-alist
      (let ((main-preamble (site/read-template "main-preamble.html"))
	    (article-postamble (site/read-template "article-postamble.html"))
	    (manual-preamble (site/read-template "manual-preamble.html")))
	(list
	 (list "main" ;; This specifies how files at the root of the site get published
	       :base-extension "org"
	       :base-directory "src/"
	       :publishing-directory "public/"
	       :publishing-function 'org-html-publish-to-html
	       :html-head (concat site/html-head "\n"
				  "<link rel=\"stylesheet\" href=\"css/main.css\">")
	       :html-preamble main-preamble
	       :html-postamble nil)
	 (list "articles" ;; This specifies how articles get published
	       :base-extension "org"
	       :base-directory "src/articles/"
	       :publishing-directory "public/articles/"
	       :publishing-function 'org-html-publish-to-html
	       :html-head site/html-head
	       :html-preamble nil
	       :html-postamble article-postamble)
	 (list "manual" ;; This specifies how the manual gets published
	       :base-extension "org"
	       :base-directory "../manual/"
	       :publishing-directory "public/manual/"
	       :publishing-function 'org-html-publish-to-html
	       :html-head (concat site/html-head "\n"
				  "<link rel=\"stylesheet\" href=\"../css/main.css\">")
	       :html-preamble manual-preamble
	       :html-postamble nil)
	 (list "css" ;; This specifies how stylesheets get published
	       :base-extension "css"
	       :base-directory "src/css"
	       :publishing-directory "public/css/"
	       :publishing-function 'org-publish-attachment)
	 (list "all" ;; This combines all the previous projects
	       :components '("articles" "manual" "main" "css")))))
