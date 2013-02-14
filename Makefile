EMACS = emacs
EMACSFLAGS =
CARTON = carton
VAGRANT = vagrant
INSTALL-INFO = install-info

OBJECTS = flycheck.elc

VERSION = $(shell $(CARTON) version)

PACKAGE_SRCS = flycheck.el \
	flycheck-pkg.el \
	doc/flycheck.info doc/dir flycheck.el
PACKAGE = flycheck-$(VERSION).tar

.PHONY: build
build : deps $(OBJECTS)

.PHONY: deps
deps :
	$(CARTON) install
	$(CARTON) update

.PHONY: doc
doc : doc/dir

.PHONY: clean-doc
clean-doc :
	rm -f doc/flycheck.info doc/dir

.PHONY: test
test : build
	$(CARTON) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-l tests/testsuite.el -f ert-run-tests-batch-and-exit

.PHONY: virtual-test
virtual-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /vagrant EMACS=$(EMACS) clean test"

.PHONY: package
package : $(PACKAGE)

$(PACKAGE) : $(PACKAGE_SRCS)
	rm -rf flycheck-$(VERSION)
	mkdir -p flycheck-$(VERSION)
	cp -f $(PACKAGE_SRCS) flycheck-$(VERSION)
	tar cf $(PACKAGE) flycheck-$(VERSION)
	rm -rf flycheck-$(VERSION)

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf elpa # Clean packages installed for development
	rm -rf $(PACKAGE) flycheck-pkg.el

%.elc : %.el
	$(CARTON) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<

flycheck-pkg.el : Carton
	$(CARTON) package

doc/dir : doc/flycheck.info
	$(INSTALL-INFO) doc/flycheck.info doc/dir
