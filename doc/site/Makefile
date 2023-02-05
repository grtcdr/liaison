all: build

build:
	@rm -rvf .cache
	@emacs --quick --batch --load lisp/op-publish.el --funcall org-publish-all t t

serve: build
	@miniserve public --route-prefix=liaison --index=index.html

clean:
	@rm -rvf .cache
	@rm -rvf public
