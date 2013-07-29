# -*- mode: yaml; -*-

# Tools for shell script checkers:

bash:
  pkg.installed

sh-dash:
  pkg.installed:
    - name: dash

sh-bash:
  pkg.installed:
    - name: bash

zsh:
  pkg.installed