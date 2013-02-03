EMACS = emacs
EMACSFLAGS =
CARTON = carton
VAGRANT = vagrant

OBJECTS = flycheck.elc

.PHONY: deps
deps :
	$(CARTON) install
	$(CARTON) update

.PHONY: build
build : deps $(OBJECTS)

.PHONY: test
test : build
	$(CARTON) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-l tests/testsuite.el -f ert-run-tests-batch-and-exit

.PHONY: virtual-test
virtual-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /vagrant CARTON=/opt/carton/bin/carton EMACS=$(EMACS) clean test"

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf elpa # Clean packages installed for development

%.elc : %.el
	$(CARTON) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
