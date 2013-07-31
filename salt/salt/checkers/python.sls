# -*- mode: yaml; -*-

# Syntax checking tools in Python, available through Pip

# Install Python and PIP
python:
  pkg.installed:
    - names:
        - python
        - python-pip

python-flake8:
  pip.installed:
    - name: flake8
    - require:
        - pkg: python

# Flake8 plugins
python-flake8-pep-naming:
  pip.installed:
    - name: pep8-naming
    - require:
        - pkg: python

python-pylint:
  pip.installed:
    - name: pylint
    - require:
        - pkg: python

rst:
  pip.installed:
    - name: docutils
    - require:
        - pkg: python
