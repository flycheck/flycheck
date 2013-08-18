EMACS ?= emacs
EMACSFLAGS =
CASK = cask
VAGRANT = vagrant
INSTALL-INFO = install-info
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = flycheck.el
OBJECTS = $(SRCS:.el=.elc)
DOC_SRCS = doc/flycheck.texi
HTML_SRCS = $(DOC_SRCS) doc/htmlxref.cnf
HTML_TARGETS = doc/html/index.html \
	doc/html/screenshot.png \
	doc/html/flycheck.css
PACKAGE_SRCS = $(SRCS) \
	flycheck-pkg.el \
	doc/flycheck.info doc/dir
PACKAGE = flycheck-$(VERSION).tar

.PHONY: compile
compile : $(OBJECTS)

.PHONY: package
package : $(PACKAGE)

$(PACKAGE) : $(PACKAGE_SRCS)
	rm -rf flycheck-$(VERSION)
	mkdir -p flycheck-$(VERSION)
	cp -f $(PACKAGE_SRCS) flycheck-$(VERSION)
	tar cf $(PACKAGE) flycheck-$(VERSION)
	rm -rf flycheck-$(VERSION)

.PHONY: clean-all
clean-all : clean clean-pkgdir clean-doc

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf flycheck-*.tar flycheck-pkg.el

.PHONY: packages
packages : $(PKGDIR)

.PHONY: clean-pkgdir
clean-pkgdir :
	rm -rf $(PKGDIR)

.PHONY: test
test : compile
	$(CASK) exec ert-runner

.PHONY: vagrant-test
vagrant-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /flycheck EMACS=$(EMACS) clean test"

.PHONY: doc
doc : info html

.PHONY: clean-doc
clean-doc : clean-info clean-html

.PHONY: info
info: doc/dir

.PHONY: clean-info
clean-info :
	rm -f doc/flycheck.info doc/dir

.PHONY: html
html : $(HTML_TARGETS)

.PHONY: clean-html
clean-html:
	rm -rf doc/html

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

flycheck-pkg.el : Cask
	$(CASK) package

doc/dir : doc/flycheck.info
	$(INSTALL-INFO) doc/flycheck.info doc/dir

doc/flycheck.info : $(DOC_SRCS)

doc/html/screenshot.png : doc/screenshot.png
	cp -f $< $@

doc/html/flycheck.css : doc/flycheck.css
	cp -f $< $@

doc/html/index.html: $(DOC_SRCS)
	$(MAKEINFO) --html --split=chapter --css-ref=flycheck.css -o doc/html doc/flycheck.texi
