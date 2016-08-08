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
CASK = cask
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
EMACSBATCH = $(CASK) exec $(EMACS) -Q --batch -L . $(EMACSOPTS)

# Program availability
HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
endif
HAVE_INKSCAPE := $(shell sh -c "command -v $(INKSCAPE)")
HAVE_CONVERT := $(shell sh -c "command -v $(CONVERT)")
HAVE_OPTIPNG := $(shell sh -c "command -v $(OPTIPNG)")

# Export Emacs to goals, mainly for CASK
CASK_EMACS = $(EMACS)
export EMACS
export CASK_EMACS

# Run make help by default
.DEFAULT_GOAL = help

# File lists
SRCS = flycheck.el flycheck-buttercup.el flycheck-ert.el
OBJS = $(SRCS:.el=.elc)
IMGS = doc/_static/logo.png

# File rules
flycheck-ert.elc: flycheck.elc

flycheck-buttercup.elc: flycheck.elc

$(OBJS): %.elc: %.el
	$(EMACSBATCH) -l maint/flycheck-compile.el -f flycheck/batch-byte-compile $<

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
	$(CASK) install
	$(CASK) update

.PHONY: clean
clean:
	rm -rf $(OBJS)
	$(MAKE) -C doc clean

.PHONY: purge
	$(GIT) clean -xfd

.PHONY: compile
compile: $(OBJS)

.PHONY: specs
specs: compile
	$(CASK) exec buttercup -L . --pattern '$(PATTERN)' test/specs

.PHONY: unit
unit: compile
	$(EMACSBATCH) --load test/run.el -f flycheck-run-tests-main \
		'(and (not (tag external-tool)) $(SELECTOR))'

.PHONY: integ
integ: compile
	$(EMACSBATCH) --load test/run.el -f flycheck-run-tests-main \
		'(and (tag external-tool) $(SELECTOR))'

.PHONY: images
images: $(IMGS)

.PHONY: help
help:
	@echo 'Run `make init` first to install and update all local dependencies.'
	@echo ''
	@echo 'Available targets:'
	@echo '  init:    Initialise the project.  RUN FIRST!'
	@echo '  compile: Byte-compile Emacs Lisp sources'
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
	@echo '  EMCSOPTS: Additional options to pass to `emacs`'
	@echo '  EMACS:    The path or name of the Emacs to use for tests and compilation'
	@echo ''
	@echo 'Available programs:'
	@echo '  $(CASK): $(if $(HAVE_CASK),yes,no)'
	@echo ''
	@echo 'You need $(CASK) to develop Flycheck.'
	@echo 'See http://cask.readthedocs.io/ for more information.'
