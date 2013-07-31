# -*- mode: yaml; -*-

include:
  - base.tools

# Emacs
emacs:
  pkgrepo.managed:
    - ppa: cassou/emacs
    - require:
        - pkg: ppa
    - require_in:
        - pkg: emacs
  pkg.installed:
    - names:
        - emacs24-nox
        - emacs-snapshot-nox