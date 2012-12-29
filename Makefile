EMACS = emacs
EMACSFLAGS =

OBJECTS = flycheck.elc

.PHONY: build
build : $(OBJECTS)

.PHONY: test
test :
	@$(EMACS) --no-site-file --no-site-lisp --batch $(EMACSFLAGS) \
		-l ert -l deps/dash.el/dash.el -l deps/s.el/s.el \
		-l flycheck.el -l tests.el -f ert-run-tests-batch-and-exit

.PHONY: clean
clean :
	rm -f $(OBJECTS)

%.elc : %.el
	$(EMACS) --no-site-file --no-site-lisp --batch $(EMACSFLAGS) \
		-l deps/dash.el/dash.el -l deps/s.el/s.el \
		-f batch-byte-compile $<
