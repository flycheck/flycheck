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

PYTHON = python3
FLAKE8 = flake8
PIP = pip3
HAVE_PIP := $(shell sh -c "command -v $(PIP)")

.DEFAULT_GOAL := help

.PHONY: init
init:
ifndef HAVE_PIP
	$(error "$(PIP) not available.  Please run make help.")
endif
ifndef VIRTUAL_ENV
	$(warning "No virtualenv active.  Installing globally is not recommended.")
ifndef FORCE
	$(error "Aborted.  Run make FORCE=1 init to override or make help.")
endif
endif
	pip install -r requirements.txt

.PHONY: help
help:
	@echo 'Available targets:'
	@echo '  init:    Install dependencies of maintenance scripts'
	@echo '  check:   Check maintenance scripts'
	@echo '  release: Make a Flycheck release'
	@echo ''
	@echo 'You need Python 3.5 for all maintenance scripts'
	@echo ''
	@echo 'Run make init to install required libraries.  It is recommended'
	@echo 'that you use virtualenv (https://virtualenv.pypa.io/en/latest/)'
	@echo 'to avoid a global installation of Python packages.  make init'
	@echo 'will warn you if you do not.'

.PHONY: release
release:
	@./release.py

.PHONY: lint
lint:
	$(FLAKE8) ..

.PHONY: check
check: lint
