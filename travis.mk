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

# Build with as few features as possible to cut build times, but keep XML2 for
# our XML-parser tests
EMACSBUILDFLAGS = --quiet --enable-silent-rules \
	--with-x-toolkit=no --without-x --without-all --with-xml2
EMACSBUILDVARS = CFLAGS='' CXXFLAGS=''

ifeq ($(origin EMACS_VERSION), undefined)
$(error "No $$EMACS_VERSION in environment!")
endif
EMACS="emacs-$(EMACS_VERSION)"
export EMACS

.PHONY: download_emacs24 checkout_emacs_trunk install_emacs \
	install_cask install_texinfo \
	deps compile check test texinfo deploy_manual \
	before_install before_install_unit before_install_manual \
	install install_unit install_manual \
	script script_unit script_manual

ifeq ($(EMACS_VERSION),trunk)
GETEMACS = checkout_emacs_trunk
else
GETEMACS = download_emacs24
endif

# SUPPORT TARGETS
download_emacs24:
	curl -o "/tmp/emacs-$(EMACS_VERSION).tar.gz" "https://ftp.gnu.org/gnu/emacs/emacs-$(EMACS_VERSION).tar.gz"
	tar xzf "/tmp/emacs-$(EMACS_VERSION).tar.gz" -C /tmp
	mv /tmp/emacs-$(EMACS_VERSION) /tmp/emacs

checkout_emacs_trunk:
	git clone --depth=1 'http://git.sv.gnu.org/r/emacs.git' /tmp/emacs
	cd /tmp/emacs && ./autogen.sh

# Build a small Emacs executable without anything for tests
install_emacs: $(GETEMACS)
	cd '/tmp/emacs' && ./configure $(EMACSBUILDFLAGS) --prefix="$(HOME)" $(EMACSBUILDVARS)
	make -j2 -C '/tmp/emacs' V=0 install

install_cask: install_emacs
	git clone https://github.com/cask/cask.git "$(HOME)/.cask"

install_texinfo:
	curl -o '/tmp/texinfo-6.0.tar.gz' 'http://ftp.gnu.org/gnu/texinfo/texinfo-6.0.tar.gz'
	tar xzf '/tmp/texinfo-6.0.tar.gz' -C /tmp
	cd '/tmp/texinfo-6.0' && ./configure --prefix="$(HOME)"
	make -C '/tmp/texinfo-6.0' install

deps:
	make deps

compile check:
	make EMACSFLAGS="$(EMACSFLAGS)" "$@"

test: compile
	make ERTSELECTOR="$(ERTSELECTOR)" test

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
