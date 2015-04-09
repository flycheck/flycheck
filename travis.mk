# Makefile for Travis CI

ifeq ($(origin TRAVIS_BUILD), undefined)
$(error "TRAVIS_BUILD not set")
endif

ifeq ($(findstring $(TRAVIS_BUILD), unit integration manual),)
$(error "Unsupported TRAVIS_BUILD=$(TRAVIS_BUILD), must be either unit, integration or manual")
endif

INVENTORY = playbooks/travis_inventory
PLAYBOOK = playbooks/site.yml

# Skip Cabal and CRAN packages on Travis CI, because they take ages to build
ANSIBLE_SKIP_TAGS=cabal,cran

# Tags for different Travis builds
ANSIBLE_TAGS_unit=$(EMACS)
ANSIBLE_TAGS_integration=$(EMACS),languages
ANSIBLE_TAGS_manual=language-texinfo
ANSIBLE_TAGS=$(ANSIBLE_TAGS_$(TRAVIS_BUILD))

ERTSELECTOR_unit=(not (tag external-tool))
ERTSELECTOR_integration=(tag external-tool)
ERTSELECTOR=$(ERTSELECTOR_$(TRAVIS_BUILD))

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

.PHONY: install_ansible provision deps compile test texinfo deploy_manual \
	before_install \
	install install_unit install_integration install_manual \
	script script_unit script_integration script_manual \
	after_success after_success_unit after_success_integration after_success_manual

# SUPPORT TARGETS
install_ansible:
	sudo add-apt-repository -y ppa:ansible/ansible
	sudo apt-get update -y -q -m
	sudo apt-get install -y ansible

provision: install_ansible
	ansible-playbook -i $(INVENTORY) --tags=$(ANSIBLE_TAGS) --skip-tags=$(ANSIBLE_SKIP_TAGS) -v $(PLAYBOOK)

deps:
	make EMACS=$(EMACS) deps

compile:
	make EMACS=$(EMACS) EMACSFLAGS="$(EMACSFLAGS)" compile

test: compile
	make EMACS=$(EMACS) ERTSELECTOR="$(ERTSELECTOR)" test

texinfo:
	makeinfo --version
	make texinfo

deploy_manual:
	bash doc/deploy-travis.bash

# TARGETS FOR TRAVIS PHASES
before_install: provision

install_unit install_integration: deps

install_manual:

install: install_$(TRAVIS_BUILD)

script_unit script_integration: test

script_manual: texinfo

script: script_$(TRAVIS_BUILD)

after_success_unit after_success_integration:

after_success_manual: deploy_manual

after_success: after_success_$(TRAVIS_BUILD)
