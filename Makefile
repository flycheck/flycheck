EMACS = emacs
EMACSFLAGS =
CASK = cask
ERTSELECTOR = t
CONVERT = convert
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = flycheck.el flycheck-ert.el
OBJECTS = $(SRCS:.el=.elc)

DISTDIR = dist
BUILDDIR = build

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: deps compile check dist texinfo images \
	clean clean-elc clean-doc \
	clobber clobber-dist clobber-deps \
	test

# Build targets
compile : $(OBJECTS)

check :
	$(EMACSBATCH) --eval '(setq checkdoc-arguments-in-order-flag nil)' \
		-l test/flycheck-checkdoc.el \
		-f flycheck-checkdoc-batch-and-exit -- $(SRCS)

dist :
	$(CASK) package

texinfo: doc/flycheck.info

images: doc/images/logo.png

# Test targets
test : $(OBJECTS)
	$(EMACSBATCH) --script test/run.el '$(ERTSELECTOR)'

# Support targets
deps : $(PKGDIR)

# Cleanup targets
clean : clean-elc clean-doc
clobber: clobber-dist clobber-deps

clean-elc :
	rm -rf $(OBJECTS)

clean-doc:
	rm -rf doc/flycheck.info doc/dir

clobber-dist :
	rm -rf $(DISTDIR)

clobber-deps :
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

doc/flycheck.info: doc/flycheck.texi doc/languages.texi doc/fdl-1.3.texi

flycheck-ert.elc: flycheck.elc

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<

doc/images/logo.png: flycheck.svg
	$(CONVERT) $< -trim -background white \
		-bordercolor white -border 5 $@
