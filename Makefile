EMACS = emacs
EMACSFLAGS =
CASK = cask
ERTSELECTOR = t
SPHINX-BUILD = sphinx-build
SPHINXFLAGS =
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

.PHONY: compile dist texinfo images \
	clean clean-elc clean-dist clean-doc clean-deps \
	test \
	deps linkcheck

# Build targets
compile : $(OBJECTS)

dist :
	$(CASK) package

texinfo: doc/flycheck.info

images: doc/images/logo.png doc/images/favicon.ico # To update the image files

# Test targets
test : $(OBJECTS)
	$(EMACSBATCH) --script test/run.el '$(ERTSELECTOR)'

# Support targets
deps : $(PKGDIR)

# Cleanup targets
clean : clean-elc clean-dist clean-deps clean-doc

clean-elc :
	rm -rf $(OBJECTS)

clean-dist :
	rm -rf $(DISTDIR)

clean-doc:
	rm -rf doc/flycheck.info doc/dir

clean-deps :
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

doc/flycheck.info: doc/flycheck.texi doc/languages.texi doc/fdl-1.3.texi

flycheck-ert.elc: flycheck.elc

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<

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
