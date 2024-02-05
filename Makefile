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
LANGUAGE =
ifdef LANGUAGE
SELECTOR = (language $(LANGUAGE))
endif

# Internal variables
EMACSBATCH = $(EMACS) -Q --batch -L . $(EMACSOPTS)
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

RUNTEST = $(RUNEMACS) --load test/flycheck-test --load test/run.el \
	-f flycheck-run-tests-main

# Export Emacs to goals, mainly for EASK
EASK_EMACS = $(EMACS)
export EMACS
export EASK_EMACS

# Run make help by default
.DEFAULT_GOAL = help

# File lists
SRCS = flycheck.el flycheck-ert.el
IMGS = doc/_static/logo.png
TEST_SRCS = flycheck.el flycheck-ert.el test/flycheck-test.el

# File rules
flycheck-ert.elc: flycheck.elc

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
	$(EASK) upgrade

.PHONY: clean
clean:
	$(EASK) clean elc
	$(MAKE) -C doc clean

.PHONY: purge
purge:
	$(GIT) clean -xfd

.PHONY: format
format:
	$(RUNEMACS) -l maint/flycheck-format.el -f flycheck/batch-format

.PHONY: check-format
check-format:
	$(RUNEMACS) -l maint/flycheck-format.el -f flycheck/batch-check-format

.PHONY: checkdoc
checkdoc:
	$(EASK) lint checkdoc

.PHONY: check
check: check-format checkdoc

.PHONY: compile
compile:
	$(EASK) compile

.PHONY: specs
specs: compile
	$(EASK) exec buttercup -L . test/specs

.PHONY: unit
unit: compile
	$(RUNTEST) '(and (not (tag external-tool)) $(SELECTOR))'

.PHONY: integ
integ: compile
	$(RUNTEST) '(and (tag external-tool) $(SELECTOR))'

.PHONY: images
images: $(IMGS)

.PHONY: help
help:
	@echo 'Run `make init` first to install and update all local dependencies.'
	@echo ''
	@echo 'Available targets:'
	@echo '  init:    Initialise the project.  RUN FIRST!'
	@echo '  check:   Check all Emacs Lisp sources (needs Emacs 25)'
	@echo '  compile: Byte-compile Emacs Lisp sources'
	@echo '  format:  Format all Emacs Lisp sources'
	@echo '  specs:   Run all buttercup specs for Flycheck'
	@echo '  unit:    Run all ERT unit tests for Flycheck (legacy)'
	@echo '  integ:   Run all integration tests for Flycheck'
	@echo '  images:  Generate PNG images from SVG sources'
	@echo '  clean:   Clean compiled files'
	@echo '  purge:   Clean everything'
	@echo ''
	@echo 'Available make variables:'
	@echo '  PATTERN:  A regular expression matching spec names to run with `specs`'
	@echo '  SELECTOR: An ERT selector expression for `unit` and `integ`'
	@echo '  LANGUAGE: The name of a language for `integ`.  Overrides `SELECTOR`'
	@echo '  EMACSOPTS: Additional options to pass to `emacs`'
	@echo '  EMACS:    The path or name of the Emacs to use for tests and compilation'
	@echo ''
	@echo 'Available programs:'
	@echo '  $(EASK): $(if $(HAVE_EASK),yes,no)'
	@echo ''
	@echo 'You need $(EASK) to develop Flycheck.'
	@echo 'See https://emacs-eask.github.io/ for more information.'
