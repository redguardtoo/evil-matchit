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
-L deps/tuareg-2.2.0 \
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
	@if [ ! -f deps/lua-mode.el ]; then curl -L https://stable.melpa.org/packages/lua-mode-20210802.el > deps/lua-mode.el; fi;
	@if [ ! -f deps/markdown-mode.el ]; then curl -L https://stable.melpa.org/packages/markdown-mode-2.5.el > deps/markdown-mode.el; fi;
	@if [ ! -f deps/tuareg-2.2.0/tuareg.el ]; then curl -L https://stable.melpa.org/packages/tuareg-2.2.0.tar | tar x -C deps/; fi;
	@if [ ! -f deps/yaml-mode.el ]; then curl -L https://stable.melpa.org/packages/yaml-mode-0.0.15.el > deps/yaml-mode.el; fi;

lint: deps
	@$(EMACS) ${EMACS_BATCH_OPTS} -l tests/my-elint.el 2>&1 | grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

compile: deps
	$(RM) *.elc
	@$(EMACS) ${EMACS_BATCH_OPTS} -l tests/my-byte-compile.el 2>&1 | grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

test: compile deps
	@$(EMACS) ${EMACS_BATCH_OPTS} -l evil-matchit.el -l tests/evil-matchit-tests.el
