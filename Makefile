EMACS = emacs
EMACSFLAGS =
CASK = cask
ERTSELECTOR = t
SPHINX-BUILD = sphinx-build
SPHINXFLAGS =
CONVERT = convert
INSTALL-INFO = install-info
INSTALL = install
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = flycheck.el
OBJECTS = $(SRCS:.el=.elc)

DISTDIR = dist
BUILDDIR = build
DOCBUILDDIR = $(BUILDDIR)/doc
DOCTREEDIR = $(DOCBUILDDIR)/doctrees

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: compile dist doc html texinfo images \
	clean clean-elc clean-dist clean-doc clean-deps \
	test \
	deps linkcheck

# Build targets
compile : $(OBJECTS)

dist :
	$(CASK) package

doc : html texinfo

html :
	$(SPHINX-BUILD) -b html -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/html

texinfo : doc/flycheck.texi

images: doc/images/logo.png doc/images/favicon.ico # To update the image files

# Test targets
test : $(OBJECTS)
	$(EMACSBATCH) --script test/run-tests.el '$(ERTSELECTOR)'

# Support targets
deps : $(PKGDIR)

linkcheck :
	$(SPHINX-BUILD) -b linkcheck -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/linkcheck

pseudoxml :
	$(SPHINX-BUILD) -b pseudoxml -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/pseudoxml

# Cleanup targets
clean : clean-elc clean-dist clean-deps clean-doc

clean-elc :
	rm -rf $(OBJECTS)

clean-dist :
	rm -rf $(DISTDIR)

clean-doc:
	rm -rf $(DOCBUILDDIR)

clean-deps :
	rm -rf $(PKGDIR)

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

doc/flycheck.texi : $(DOCBUILDDIR)/info/flycheck.texi
	cp -f $< $@

# Sphinx has its own sophisticated dependency tracking, so we mark this rule as
# phony to always let Sphinx attempt to rebuild it.  If its up to date that's a
# noop anyway.
.PHONY : $(DOCBUILDDIR)/info/flycheck.texi
$(DOCBUILDDIR)/info/flycheck.texi :
	$(SPHINX-BUILD) -b texinfo -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/info

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<

doc/images/favicon.ico: flycheck.svg
	$(CONVERT) $< -background white \
		\( -clone 0 -resize 16x16 \) \
		\( -clone 0 -resize 32x32 \) \
		\( -clone 0 -resize 48x48 \) \
		\( -clone 0 -resize 64x64 \) \
		\( -clone 0 -resize 96x96 \) \
		\( -clone 0 -resize 196x196 \) \
		-delete 0 -alpha off -colors 256 $@

doc/images/logo.png: flycheck.svg
	$(CONVERT) $< -trim -background white \
		-bordercolor white -border 5 $@
