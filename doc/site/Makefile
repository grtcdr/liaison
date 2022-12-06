all: build

build:
	@emacs --quick --batch --load publish.el --funcall org-publish-all t t

serve: build
	@miniserve public --route-prefix=liaison --index=index.html

clean:
	@rm -rvf "public/"
	@rm -rvf ".cache/"
