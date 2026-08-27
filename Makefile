# Copyright (c) 2018 Flycheck contributors
# Copyright (c) 2012-2016 Sebastian Wiesner and Flycheck contributors

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

# Programs
EASK = eask
EMACS = emacs
GIT = git
INKSCAPE = inkscape
CONVERT = convert
OPTIPNG = optipng

# Program options
EMACSOPTS =
PATTERN = .*
# Internal variables
EMACSBATCH = $(EMACS) -Q --batch -L . -L test/specs $(EMACSOPTS)
RUNEMACS =

# Program availability
ifdef EASK
RUNEMACS = $(EASK) exec $(EMACSBATCH)
HAVE_EASK := $(shell sh -c "command -v $(EASK)")
ifndef HAVE_EASK
$(warning "$(EASK) is not available.  Please run make help")
endif
else
RUNEMACS = $(EMACSBATCH)
endif
HAVE_INKSCAPE := $(shell sh -c "command -v $(INKSCAPE)")
HAVE_CONVERT := $(shell sh -c "command -v $(CONVERT)")
HAVE_OPTIPNG := $(shell sh -c "command -v $(OPTIPNG)")

# Export Emacs to goals, mainly for EASK
EASK_EMACS = $(EMACS)
export EMACS
export EASK_EMACS

# Run make help by default
.DEFAULT_GOAL = help

# File lists
SRCS = flycheck.el
IMGS = doc/_static/logo.png

# File rules
flycheck-buttercup.elc: flycheck.elc

doc/_static/logo.png: flycheck.svg
ifndef HAVE_CONVERT
	$(error "$(CONVERT) not available.  Please run make help.")
endif
ifndef HAVE_INKSCAPE
	$(error "$(INKSCAPE) not available.  Please run make help.")
endif
ifndef HAVE_OPTIPNG
	$(error "$(OPTIPNG) not available.  Please run make help.")
endif
	$(CONVERT) $< -trim -background white -bordercolor white \
		-border 5 $@
	$(OPTIPNG) $@

# Public targets
.PHONY: init
init:
	$(EASK) install-deps --dev
# Opportunistic: install-deps has already satisfied everything Eask asks
# for, and an upgrade that cannot be satisfied must not fail the build.
# A dependency raising its Emacs floor otherwise breaks every job on an
# older Emacs, without a commit landing here.
	-$(EASK) upgrade

.PHONY: clean
clean:
	$(EASK) clean elc
	$(MAKE) -C doc clean

.PHONY: purge
purge:
	$(GIT) clean -xfd

.PHONY: checkdoc
checkdoc:
	$(EASK) lint checkdoc

.PHONY: check
check: checkdoc

.PHONY: compile
compile:
	$(EASK) compile --strict

.PHONY: specs
specs: compile
	$(EASK) exec buttercup -p '$(PATTERN)' -L . -L test -L test/specs test/specs

# Times the hot paths; nothing asserts, the numbers are for comparing a
# branch against master on the same machine.
.PHONY: bench
bench:
	$(EASK) exec emacs --batch -L . -l test/flycheck-benchmark.el \
		-f flycheck-benchmark-batch

DOCKER ?= docker
CHECKER_IMAGE ?= flycheck-checkers

# Mounting the checkout means anything written lands in it, so run as the
# user who owns it rather than leaving root-owned files behind.  That user
# does not own /root, so point HOME somewhere writable: without it `go
# vet' cannot create its build cache and reports that instead of the code.
CHECKER_RUN = $(DOCKER) run --rm -v "$(CURDIR)":/flycheck \
	-u "$$(id -u):$$(id -g)" -e HOME=/tmp $(CHECKER_IMAGE)
# `load-prefer-newer' because a checkout compiled on the host carries .elc
# files this Emacs may not be able to read
CHECKER_EMACS = emacs -Q --batch --eval "(setq load-prefer-newer t)" \
	-L . -l test/record-fixture.el

.PHONY: checker-image
checker-image:
	$(DOCKER) build -t $(CHECKER_IMAGE) test/docker

.PHONY: record-fixtures
record-fixtures: checker-image
	$(CHECKER_RUN)

.PHONY: checker-shell
checker-shell: checker-image
	$(DOCKER) run --rm -it -v "$(CURDIR)":/flycheck -u "$$(id -u):$$(id -g)" \
		$(CHECKER_IMAGE) bash

.PHONY: verify-fixtures
verify-fixtures:
	$(EASK) exec emacs --batch -L . -l test/record-fixture.el \
		-f flycheck-verify-fixtures-batch

# The same check against the image, which has far more of the tools than
# any one machine does, so it reaches recordings `verify-fixtures' skips.
.PHONY: verify-fixtures-image
verify-fixtures-image: checker-image verify-fixtures-in-image

# Split out so a build that made the image some other way can run the
# check without a second `docker build' throwing that work away.
.PHONY: verify-fixtures-in-image
verify-fixtures-in-image:
	$(CHECKER_RUN) $(CHECKER_EMACS) -f flycheck-verify-fixtures-batch

.PHONY: images
images: $(IMGS)

.PHONY: help
help:
	@echo 'Run `make init` first to install and update all local dependencies.'
	@echo ''
	@echo 'Available targets:'
	@echo '  init:    Initialise the project.  RUN FIRST!'
	@echo '  check:   Check all Emacs Lisp sources'
	@echo '  compile: Byte-compile Emacs Lisp sources'
	@echo '  specs:   Run all buttercup specs for Flycheck'
	@echo '  verify-fixtures: Check recorded checker output against the tools'
	@echo '  record-fixtures: Record checker output in a container of checkers'
	@echo '  checker-shell:   A shell in that container'
	@echo '  images:  Generate PNG images from SVG sources'
	@echo '  clean:   Clean compiled files'
	@echo '  purge:   Clean everything'
	@echo ''
	@echo 'Available make variables:'
	@echo '  PATTERN:  A regular expression matching spec names to run with `specs`'
	@echo '  EMACSOPTS: Additional options to pass to `emacs`'
	@echo '  EMACS:    The path or name of the Emacs to use for tests and compilation'
	@echo ''
	@echo 'Available programs:'
	@echo '  $(EASK): $(if $(HAVE_EASK),yes,no)'
	@echo ''
	@echo 'You need $(EASK) to develop Flycheck.'
	@echo 'See https://emacs-eask.github.io/ for more information.'
