all: build

build:
	@emacs --quick --batch --load lisp/publish.el --funcall org-publish-all t t

clean:
	@rm -rvf "public/"
	@rm -rvf ".cache/"
