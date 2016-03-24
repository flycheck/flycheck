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

SPHINXOPTS =
SPHINXBUILD = sphinx-build
SPHINXAUTOBUILD = sphinx-autobuild
BUILDDIR = _build

ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(SPHINXOPTS) .

.DEFAULT_GOAL := help

HAVE_SPHINXBUILD := $(shell command -v $(SPHINXBUILD))
ifndef HAVE_SPHINXBUILD
$(warning "$(SPHINXBUILD) is not available.  Run make help.")
endif
HAVE_SPHINXAUTOBUILD := $(shell command -v $(SPHINXAUTOBUILD))

.PHONY: help
help:
	@echo "Available targets:"
	@echo "  html: Build HTML documentation to $(BUILDDIR)/html"
	@echo "  html-auto: Like html, but automatically rebuild on changes"
	@echo "  linkcheck: Check all links and references"
	@echo "  clean: Remove generated documentation"
	@echo ""
	@echo "To build the documentation you need $(SPHINXBUILD)."
	@echo "For *-auto targets you also need $(SPHINXAUTOBUILD)."
	@echo ""
	@echo "Available tools:"
	@echo "  $(SPHINXBUILD): $(if $(HAVE_SPHINXBUILD),yes,no)"
	@echo "  $(SPHINXAUTOBUILD): $(if $(HAVE_SPHINXAUTOBUILD),yes,no)"

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
