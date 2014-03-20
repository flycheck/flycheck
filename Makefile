EMACS = emacs
EMACSFLAGS =
CASK = cask
VAGRANT = vagrant
SPHINX-BUILD = sphinx-build
INSTALL-INFO = install-info
INSTALL = install
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = flycheck.el
OBJECTS = $(SRCS:.el=.elc)
HOOKS = .git/hooks/pre-push

.PHONY: compile dist doc html texinfo \
	clean clean-elc clean-dist clean-doc clean-deps \
	test vagrant-test \
	deps linkcheck hooks

# Build targets
compile : $(OBJECTS)

dist :
	$(CASK) package

doc : html texinfo

html :
	$(SPHINX-BUILD) -b html -n -d doc/_build/doctrees doc doc/_build/html

texinfo : doc/flycheck.texi

# Test targets
test : compile
	$(CASK) exec ert-runner

vagrant-test :
	$(VAGRANT) up --provision
	$(VAGRANT) ssh -c "make -C /flycheck EMACS=$(EMACS) clean test"

# Support targets
deps : $(PKGDIR)

linkcheck :
	$(SPHINX-BUILD) -b linkcheck -n -d doc/_build/doctrees doc doc/_build/linkcheck

hooks: $(HOOKS)

# Cleanup targets
clean : clean-elc clean-pkgdir clean-doc

clean-elc :
	rm -rf $(OBJECTS)

clean-dist :
	rm -rf dist/

clean-doc:
	rm -rf doc/_build/

clean-deps :
	rm -rf $(PKGDIR)

# File targets
.git/hooks:
	install -d $@

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

doc/flycheck.texi : doc/_build/info/flycheck.texi
	cp -f $< $@

# Sphinx has its own sophisticated dependency tracking, so we mark this rule as
# phony to always let Sphinx attempt to rebuild it.  If its up to date that's a
# noop anyway.
.PHONY : doc/_build/info/flycheck.texi
doc/_build/info/flycheck.texi :
	$(SPHINX-BUILD) -b texinfo -n -d doc/_build/doctrees doc doc/_build/info

# Pattern rules
$(HOOKS): .git/hooks/%: scripts/hooks/% .git/hooks
	ln -sf ../../$< $@

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<
