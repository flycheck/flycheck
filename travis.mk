# Makefile for Travis CI

ifeq ($(origin TRAVIS_BUILD), undefined)
$(error "TRAVIS_BUILD not set")
endif

ifeq ($(findstring $(TRAVIS_BUILD), unit integration),)
$(error "Unsupported TRAVIS_BUILD=$(TRAVIS_BUILD), must be either unit or integration")
endif

INVENTORY = playbooks/travis_inventory
PLAYBOOK = playbooks/site.yml

# Skip Cabal and CRAN packages on Travis CI, because they take ages to build
ANSIBLE_SKIP_TAGS=cabal,cran

# Tags for different Travis builds
ANSIBLE_TAGS_unit=$(EMACS)
ANSIBLE_TAGS_integration=$(EMACS),languages
ANSIBLE_TAGS=$(ANSIBLE_TAGS_$(TRAVIS_BUILD))

ERTSELECTOR_unit=(not (tag external-tool))
ERTSELECTOR_integration=(tag external-tool)
ERTSELECTOR=$(ERTSELECTOR_$(TRAVIS_BUILD))

ifeq ($(EMACS),emacs-snapshot)
EMACSFLAGS_unit=--eval '(setq byte-compile-error-on-warn t)'
endif
EMACSFLAGS=$(EMACSFLAGS_$(TRAVIS_BUILD))

ifeq ($(origin EMACS), undefined)
$(error "No $$EMACS in environment!")
endif
export EMACS

.PHONY: install_ansible provision deps compile test\
	before_install install script

# SUPPORT TARGETS
install_ansible:
	sudo add-apt-repository -y ppa:rquillo/ansible
	sudo apt-get update -y -q
	sudo apt-get install -y ansible

provision: install_ansible
	ansible-playbook -i $(INVENTORY) --tags=$(ANSIBLE_TAGS) --skip-tags=$(ANSIBLE_SKIP_TAGS) -v $(PLAYBOOK)

deps:
	make EMACS=$(EMACS) deps

compile:
	make EMACS=$(EMACS) EMACSFLAGS="$(EMACSFLAGS)" compile

test: compile
	make EMACS=$(EMACS) ERTSELECTOR="$(ERTSELECTOR)" test

# TARGETS FOR TRAVIS PHASES
before_install: provision

install: deps

script: test
