;;; publish.el --- A minimal publishing script.

(require 'ox-publish)

;; Add subdirectories of the current working directory to the load-path
(normal-top-level-add-subdirs-to-load-path)

;; Require the library
(require 'forgecast)

;; Configure the location of the build cache
(setq org-publish-timestamp-directory ".cache/")

(defun my/read-template (filename)
  "Read contents of FILENAME from the templates directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "src" "templates" filename))
    (buffer-string)))

(defun my/html-head ()
  "Return HTML headers used throughout the website."
  (string-join
   '("<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\">"
     "<link rel=\"stylesheet\" href=\"https://grtcdr.tn/forgecast/css/main.css\">")
   "\n"))

;;; Redefinition of built-in org-html-format-spec:
(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  `((?b . ,(forgecast-get-resource-url 'blob))
    (?m . ,(forgecast-get-resource-url 'blame))
    (?t . ,(forgecast-get-resource-url 'tree))
    (?l . ,(forgecast-get-resource-url 'log))
    (?p . ,(forgecast-get-resource-url 'plain))
    (?e . ,(forgecast-get-resource-url 'edit))))

;;; Project specification:
(setq org-publish-project-alist
      (let ((postamble (my/read-template "postamble.html"))
	    (preamble (my/read-template "preamble.html"))
	    (html-head (my/html-head)))
	(list
	 (list "main"
	       :base-extension "org"
	       :base-directory "src/"
	       :publishing-directory "public/"
	       :publishing-function 'org-html-publish-to-html
	       :html-head html-head
	       :html-preamble preamble
	       :html-postamble nil)
	 (list "articles"
	       :base-extension "org"
	       :base-directory "src/articles/"
	       :publishing-directory "public/articles/"
	       :publishing-function 'org-html-publish-to-html
	       :html-head html-head
	       :html-preamble nil
	       :html-postamble postamble)
	 (list "documentation"
	       :base-extension "org"
	       :base-directory "lisp/forgecast/doc/"
	       :publishing-directory "public/doc/"
	       :publishing-function 'org-html-publish-to-html
	       :html-head html-head
	       :html-preamble preamble
	       :html-postamble nil)
	 (list "css"
	       :base-extension "css"
	       :base-directory "src/css"
	       :publishing-directory "public/css"
	       :publishing-function 'org-publish-attachment)
	 (list "all"
	       :components '("articles" "documentation" "main" "css")))))