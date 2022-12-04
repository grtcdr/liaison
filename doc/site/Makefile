all: build

build:
	@emacs --quick --batch --load publish.el --funcall org-publish-all t t

serve: build
	@miniserve public --route-prefix=forgecast

clean:
	@rm -rvf "public/"
	@rm -rvf ".cache/"
