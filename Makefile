EMACS = emacs
EMACSFLAGS =
CASK = cask
SPHINX-BUILD = sphinx-build
SPHINXFLAGS =
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

.PHONY: compile dist doc html texinfo \
	clean clean-elc clean-dist clean-doc clean-deps \
	test \
	deps linkcheck

# Build targets
compile : $(OBJECTS)

dist :
	$(CASK) package

doc : html texinfo

html :
	$(SPHINX-BUILD) -b html -n -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/html

texinfo : doc/flycheck.texi

# Test targets
test : compile
	$(CASK) exec ert-runner

# Support targets
deps : $(PKGDIR)

linkcheck :
	$(SPHINX-BUILD) -b linkcheck -n -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/linkcheck

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
	$(SPHINX-BUILD) -b texinfo -n -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/info

# Pattern rules
$(HOOKS): .git/hooks/%: scripts/hooks/% .git/hooks
	ln -sf ../../$< $@

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<
