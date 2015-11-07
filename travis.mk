# Makefile for Travis CI

ifeq ($(origin TRAVIS_BUILD), undefined)
$(error "TRAVIS_BUILD not set")
endif

ifeq ($(findstring $(TRAVIS_BUILD), unit integration manual),)
$(error "Unsupported TRAVIS_BUILD=$(TRAVIS_BUILD), must be either unit or manual")
endif

ERTSELECTOR=(not (tag external-tool))

ifeq ($(EMACS_VERSION),snapshot)
EMACSFLAGS_unit=--eval '(setq byte-compile-error-on-warn t)'
endif
EMACSFLAGS=$(EMACSFLAGS_$(TRAVIS_BUILD))

ifneq ($(TRAVIS_TAG),)
MANUAL_VERSION=--version $(TRAVIS_TAG)
endif

.PHONY: deps compile check test texinfo deploy_manual \
	install install_unit install_manual \
	script script_unit script_manual

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
install_unit: deps

install_manual:

install: install_$(TRAVIS_BUILD)

script_unit: check test

script_manual: deploy_manual

script: script_$(TRAVIS_BUILD)
