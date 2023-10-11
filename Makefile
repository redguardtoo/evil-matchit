# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

.PHONY: deps clean test compile

EMACS_BATCH_OPTS=--batch -Q \
-L . \
-L deps/evil-1.14.2 \
-l deps/yaml-mode.el \
-l deps/lua-mode.el \
-l deps/markdown-mode.el \
-L deps/tuareg-3.0.1 \
-l evil-matchit.el

RM = @rm -rf

clean:
	$(RM) *~
	$(RM) \#*\#
	$(RM) *.elc
	$(RM) deps/*

deps:
	@mkdir -p deps;
	@if [ ! -f deps/evil-1.14.2/evil.el ]; then curl -L https://stable.melpa.org/packages/evil-1.14.2.tar | tar x -C deps/; fi;
	@if [ ! -f deps/lua-mode.el ]; then curl -L https://raw.githubusercontent.com/immerrr/lua-mode/master/lua-mode.el > deps/lua-mode.el; fi;
	@if [ ! -f deps/markdown-mode.el ]; then curl -L https://raw.githubusercontent.com/jrblevin/markdown-mode/master/markdown-mode.el > deps/markdown-mode.el; fi;
	@if [ ! -f deps/tuareg-3.0.1/tuareg.el ]; then curl -L https://stable.melpa.org/packages/tuareg-3.0.1.tar | tar x -C deps/; fi;
	@if [ ! -f deps/yaml-mode.el ]; then curl -L https://raw.githubusercontent.com/yoshiki/yaml-mode/master/yaml-mode.el > deps/yaml-mode.el; fi;

compile: deps
	$(RM) *.elc
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/my-byte-compile.el 2>&1 | grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

test: compile deps
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/evil-matchit-tests.el
