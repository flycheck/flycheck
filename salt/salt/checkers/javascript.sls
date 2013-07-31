# -*- mode: yaml; -*-

# Syntax checkers implemented in Javascript and available through Node.js
include:
  - base.tools

# Node.js itself, along with NPM
nodejs:
  pkgrepo.managed:
    - ppa: chris-lea/node.js
    - require:
        - pkg: ppa
    - require_in:
        - pkg: nodejs
  pkg.installed: []

# Checkers
coffee-coffeelint:
  npm.installed:
    - name: coffeelint
    - require:
        - pkg: nodejs

css-csslint:
  npm.installed:
    - name: csslint
    - require:
        - pkg: nodejs

javascript-jshint:
  npm.installed:
    - name: jshint
    - require:
        - pkg: nodejs

json-jsonlint:
  npm.installed:
    - name: jsonlint
    - require:
        - pkg: nodejs

less:
  npm.installed:
    - require:
        - pkg: nodejs
