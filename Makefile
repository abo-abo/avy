emacs ?= emacs
# EMACS = emacs-24.3

LOAD = -l avy.el -l avy-test.el

.PHONY: all test clean

all: test

test:
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -batch -l avy-init.el

run:
	$(emacs) -Q -l avy-init.el

clean:
	rm -f *.elc
