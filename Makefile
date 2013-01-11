EMACS = emacs
EMACSFLAGS =
VAGRANT = vagrant

OBJECTS = flycheck.elc

.PHONY: build
build : $(OBJECTS)

.PHONY: test
test :
	@$(EMACS) --no-site-file --no-site-lisp --batch $(EMACSFLAGS) \
		-l dependencies.el -l tests/testsuite.el \
		-f ert-run-tests-batch-and-exit

.PHONY: virtual-test
virtual-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /vagrant EMACS=$(EMACS) clean test"

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf elpa # Clean packages installed for development

%.elc : %.el
	$(EMACS) --no-site-file --no-site-lisp --batch $(EMACSFLAGS) \
		-l dependencies.el -f batch-byte-compile $<
