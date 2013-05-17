EMACS = emacs
EMACSFLAGS =
CARTON = carton
VAGRANT = vagrant
INSTALL-INFO = install-info
VERSION = $(shell EMACS=$(EMACS) $(CARTON) version)

SRCS = flycheck.el
OBJECTS = $(SRCS:.el=.elc)

DOC_SRCS = doc/api.texi \
	doc/changes.texi \
	doc/checkers.texi \
	doc/contribution.texi \
	doc/credits.texi \
	doc/extending.texi \
	doc/fdl-1.3.texi \
	doc/flycheck.texi \
	doc/github-ribbon.texi \
	doc/html-frontmatter.texi \
	doc/introduction.texi \
	doc/usage.texi
HTML_SRCS = $(DOC_SRCS) doc/htmlxref.cnf
HTML_TARGETS = doc/html/index.html \
	doc/html/screenshot.png \
	doc/html/flycheck.css


PACKAGE_SRCS = $(SRCS) \
	flycheck-pkg.el \
	doc/flycheck.info doc/dir flycheck.el
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

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf $(PACKAGE) flycheck-pkg.el

.PHONY: deps
deps :
	EMACS=$(EMACS) $(CARTON) install
	EMACS=$(EMACS) $(CARTON) update

.PHONY: clean-deps
clean-deps :
	rm -rf elpa

.PHONY: test
test : compile
	EMACS=$(EMACS) $(CARTON) exec $(EMACS) -Q $(EMACSFLAGS) --script tests/flycheck-testrunner.el

.PHONY: vagrant-test
vagrant-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /vagrant EMACS=$(EMACS) clean test"

.PHONY: doc
doc : info html

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

%.elc : %.el
	EMACS=$(EMACS) $(CARTON) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<

flycheck-pkg.el : Carton
	EMACS=$(EMACS) $(CARTON) package

doc/dir : doc/flycheck.info
	$(INSTALL-INFO) doc/flycheck.info doc/dir

doc/flycheck.info : $(DOC_SRCS)

doc/html/screenshot.png : doc/screenshot.png
	cp -f $< $@

doc/html/flycheck.css : doc/flycheck.css
	cp -f $< $@

doc/html/index.html: $(DOC_SRCS)
	$(MAKEINFO) --html --split=chapter --css-ref=flycheck.css -o doc/html doc/flycheck.texi
