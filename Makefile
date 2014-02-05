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
PACKAGE_SRCS = $(SRCS) \
	flycheck-pkg.el \
	doc/flycheck.info doc/dir
PACKAGE = flycheck-$(VERSION).tar
HOOKS = .git/hooks/pre-push

.PHONY: packages \
	compile package \
	clean clean-all clean-doc clean-pkgdir \
	test vagrant-test \
	doc html linkcheck info doc/_build/info/flycheck.texi \
	hooks

compile : $(OBJECTS)

package : $(PACKAGE)

$(PACKAGE) : $(PACKAGE_SRCS)
	rm -rf flycheck-$(VERSION)
	mkdir -p flycheck-$(VERSION)
	cp -f $(PACKAGE_SRCS) flycheck-$(VERSION)
	tar cf $(PACKAGE) flycheck-$(VERSION)
	rm -rf flycheck-$(VERSION)

clean-all : clean clean-pkgdir clean-doc

clean :
	rm -f $(OBJECTS)
	rm -rf flycheck-*.tar flycheck-pkg.el

packages : $(PKGDIR)

clean-pkgdir :
	rm -rf $(PKGDIR)

test : compile
	$(CASK) exec ert-runner

vagrant-test :
	$(VAGRANT) up --provision
	$(VAGRANT) ssh -c "make -C /flycheck EMACS=$(EMACS) clean test"

doc : info html

html :
	$(SPHINX-BUILD) -b html -n -d doc/_build/doctrees doc doc/_build/html

linkcheck :
	$(SPHINX-BUILD) -b linkcheck -n -d doc/_build/doctrees doc doc/_build/linkcheck

info : doc/dir

hooks: $(HOOKS)

clean-doc:
	rm -rf doc/_build/

$(HOOKS): .git/hooks/%: scripts/hooks/% .git/hooks
	ln -sf ../../$< $@

.git/hooks:
	install -d $@

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) \
		--eval '(setq package-user-dir "$(PKGDIR)")' -f package-initialize \
		-f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

flycheck-pkg.el : Cask
	$(CASK) package

doc/dir : doc/flycheck.info
	$(INSTALL-INFO) doc/flycheck.info doc/dir

doc/flycheck.texi : doc/_build/info/flycheck.texi
	cp -f $< $@

doc/_build/info/flycheck.texi :
	$(SPHINX-BUILD) -b texinfo -n -d doc/_build/doctrees doc doc/_build/info
