# -*- mode: yaml; -*-

# Syntax checking tools in Python, available through Pip

# Install pip itself
python-pip:
  pkg.installed

python-flake8:
  pip.installed:
    - name: flake8
    - require:
        - pkg: python-pip

# Flake8 plugins
python-flake8-pep-naming:
  pip.installed:
    - name: pep8-naming
    - require:
        - pkg: python-pip

python-pylint:
  pip.installed:
    - name: pylint
    - require:
        - pkg: python-pip

rst:
  pip.installed:
    - name: docutils
    - require:
        - pkg: python-pip
