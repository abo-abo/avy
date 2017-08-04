emacs ?= emacs
# EMACS = emacs-24.3

LOAD = -l avy.el -l avy-test.el

.PHONY: all test clean checkdoc

all: compile test checkdoc

test:
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

compile:
	$(emacs) -batch -l targets/avy-init.el

run:
	$(emacs) -Q -l targets/avy-init.el

clean:
	rm -f *.elc
