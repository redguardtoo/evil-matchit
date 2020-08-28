# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

clean:
	@rm -f *~
	@rm -f \#*\#
	@rm -f *.elc

.PHONY: deps
deps:
	@mkdir -p deps;
	@if [ ! -f deps/evil-1.14.0/evil.el ]; then curl -L https://stable.melpa.org/packages/evil-1.14.0.tar | tar x -C deps/; fi;
	@if [ ! -f deps/lua-mode.el ]; then curl -L https://raw.githubusercontent.com/immerrr/lua-mode/master/lua-mode.el > deps/lua-mode.el; fi;
	@if [ ! -f deps/markdown-mode.el ]; then curl -L https://raw.githubusercontent.com/jrblevin/markdown-mode/master/markdown-mode.el > deps/markdown-mode.el; fi;
	@if [ ! -f deps/tuareg-2.2.0/tuareg.el ]; then curl -L https://stable.melpa.org/packages/tuareg-2.2.0.tar | tar x -C deps/; fi


.PHONY: test
test: deps clean
	@$(EMACS) -batch -Q -L . -L deps/evil-1.14.0 -l deps/lua-mode.el -l deps/markdown-mode.el -L deps/tuareg-2.2.0 -l evil-matchit.el -l tests/evil-matchit-tests.el
