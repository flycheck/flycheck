# Makefile for Travis CI

ifeq ($(origin TRAVIS_BUILD), undefined)
$(error "TRAVIS_BUILD not set")
endif

ifeq ($(findstring $(TRAVIS_BUILD), unit integration manual),)
$(error "Unsupported TRAVIS_BUILD=$(TRAVIS_BUILD), must be either unit or manual")
endif

ERTSELECTOR=(not (tag external-tool))

ifeq ($(EMACS),emacs-snapshot)
EMACSFLAGS_unit=--eval '(setq byte-compile-error-on-warn t)'
endif
EMACSFLAGS=$(EMACSFLAGS_$(TRAVIS_BUILD))

ifneq ($(TRAVIS_TAG),)
MANUAL_VERSION=--version $(TRAVIS_TAG)
endif

ifeq ($(origin EMACS), undefined)
$(error "No $$EMACS in environment!")
endif
export EMACS

.PHONY: deps compile check test texinfo deploy_manual \
	before_install before_install_unit before_install_emacs \
	install install_unit install_manual \
	script script_unit script_manual

# SUPPORT TARGETS
install_emacs:
	sudo add-apt-repository -yy 'ppa:cassou/emacs'
	sudo add-apt-repository -yy 'ppa:ubuntu-elisp/ppa'
	sudo apt-get update
	sudo apt-get install -y $(EMACS)-nox

install_cask: install_emacs
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

install_texinfo:
	curl -o '/tmp/texinfo-5.2.tar.gz' 'http://ftp.gnu.org/gnu/texinfo/texinfo-5.2.tar.gz'
	tar xzf '/tmp/texinfo-5.2.tar.gz' -C /tmp
	cd '/tmp/texinfo-5.2' && ./configure
	sudo make -C '/tmp/texinfo-5.2' install

deps:
	make EMACS=$(EMACS) deps

compile check:
	make EMACS=$(EMACS) EMACSFLAGS="$(EMACSFLAGS)" "$@"

test: compile
	make EMACS=$(EMACS) ERTSELECTOR="$(ERTSELECTOR)" test

texinfo:
	makeinfo --version
	make texinfo

deploy_manual: texinfo
	bash doc/deploy-travis.bash

# TARGETS FOR TRAVIS PHASES
before_install_unit: install_cask

before_install_manual: install_texinfo

before_install: before_install_$(TRAVIS_BUILD)

install_unit: deps

install_manual:

install: install_$(TRAVIS_BUILD)

script_unit: check test

script_manual: deploy_manual

script: script_$(TRAVIS_BUILD)
