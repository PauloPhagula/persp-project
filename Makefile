# vim: noet:ts=2:sw=2
EMACS ?= emacs
SELECTOR ?=
LOAD_PATH=-L .
# A space-separated list of required package names
DEPS = perspective package-lint

INIT_PACKAGES="(progn \
              (require 'package) \
              (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
              (package-initialize) \
              (dolist (pkg '(${DEPS})) \
              (unless (package-installed-p pkg) \
              (unless (assoc pkg package-archive-contents) \
              (package-refresh-contents)) \
              (package-install pkg))) \
              (unless package-archive-contents (package-refresh-contents)) \
              )"

all: compile package-lint test clean-elc

compile: clean-elc
	${EMACS} -Q -batch -eval $(subst PACKAGES,${DEPS},${INIT_PACKAGES}) -L . -f batch-byte-compile *.el

clean-elc:
	find . -iname '*.elc' -exec rm {} \;

package-lint:
	${EMACS} -Q -batch -eval $(subst PACKAGES,package-lint,${INIT_PACKAGES}) -f package-lint-batch-and-exit persp-project.el

test:
	${EMACS} -Q -batch -eval $(subst PACKAGES,${DEPS},${INIT_PACKAGES}) $(LOAD_PATH) -l persp-project -l persp-project-test . -eval '(setq ert-batch-backtrace-right-margin 200)' -f ert-run-tests-batch-and-exit

.PHONY: all compile clean-elc package-lint test

# Local Variables:
# indent-tabs-mode: t
# End:
