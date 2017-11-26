# Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors
# This file is not part of GNU Emacs.

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

PIP = pip3
SPHINXOPTS = -j4
SPHINXBUILD = sphinx-build
SPHINXAUTOBUILD = sphinx-autobuild
OPTIPNG = optipng
BUILDDIR = _build

# Whether to build offline HTML that loads no external resources, for use in 3rd
# party packages, see https://github.com/flycheck/flycheck/issues/999
OFFLINE =

ALLSPHINXOPTS = -d $(BUILDDIR)/doctrees $(SPHINXOPTS) .
ifdef OFFLINE
ALLSPHINXOPTS += -Dflycheck_offline_html=1
endif

IMAGES = images/*.png

.DEFAULT_GOAL := help

HAVE_SPHINXBUILD := $(shell sh -c "command -v $(SPHINXBUILD)")
ifndef HAVE_SPHINXBUILD
$(warning "$(SPHINXBUILD) is not available.  Please run make help.")
endif
HAVE_SPHINXAUTOBUILD := $(shell sh -c "command -v $(SPHINXAUTOBUILD)")
HAVE_PIP := $(shell sh -c "command -v $(PIP)")

.PHONY: help
help:
	@echo 'Available targets:'
	@echo '  html:            Build HTML documentation to $(BUILDDIR)/html'
	@echo '  html-auto:       Like html, but automatically rebuild on changes'
	@echo '  linkcheck:       Check all links and references'
	@echo '  clean:           Remove generated documentation'
	@echo '  optimise-images: Optimise all images in the documentation'
	@echo ''
	@echo 'To build the documentation you need $(SPHINXBUILD).'
	@echo 'For *-auto targets you also need $(SPHINXAUTOBUILD).'
	@echo ''
	@echo 'Available make variables:'
	@echo '  OFFLINE:  If set build HTML that loads no external resources'
	@echo ''
	@echo 'Available programs:'
	@echo '  $(SPHINXBUILD): $(if $(HAVE_SPHINXBUILD),yes,no)'
	@echo '  $(SPHINXAUTOBUILD): $(if $(HAVE_SPHINXAUTOBUILD),yes,no)'
	@echo ''
	@echo 'You need Python 3.4 or newer to install Sphinx for Flycheck.'
	@echo ''
	@echo 'Run make init to install all missing tools.  It is recommended'
	@echo 'that you use virtualenv (https://virtualenv.pypa.io/en/latest/)'
	@echo 'to avoid a global installation of Python packages.  make init'
	@echo 'will warn you if you do not.'

.PHONY: init
init:
ifndef HAVE_PIP
	$(error "$(PIP) not available.  Please run make help.")
endif
ifndef VIRTUAL_ENV
	$(warning "No virtualenv active.  Installing Sphinx globally is not recommended.")
ifndef FORCE
	$(error "Aborted.  Run make FORCE=1 init to override or make help.")
endif
endif
	pip install -r requirements.txt

.PHONY: clean
clean:
	rm -rf $(BUILDDIR)/*

# HTML and related formats
.PHONY: html
html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."

.PHONY: html-auto
html-auto: SPHINXOPTS = -B
html-auto:
ifndef HAVE_SPHINXAUTOBUILD
	$(error "sphinx-autobuild not available.  Run make help.")
endif
	$(SPHINXAUTOBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

# House-keeping targets
.PHONY: linkcheck
linkcheck:
	$(SPHINXBUILD) -b linkcheck $(ALLSPHINXOPTS) $(BUILDDIR)/linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in $(BUILDDIR)/linkcheck/output.txt."

.PHONY: optimise-images
optimise-images:
	$(OPTIPNG) $(filter %.png,$(IMAGES))
