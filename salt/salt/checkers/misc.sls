# -*- mode: yaml; -*-

# Checker tools which don't fit into any other category
include:
  - base.tools

c/c++-clang:
  pkg.installed:
    - name: clang

c/c++-cppcheck:
  pkg.installed:
    - name: cppcheck

html-tidy:
  pkg.installed:
    - name: tidy

lua:
  pkg.installed:
    - name: lua5.2

perl:
  pkg.installed

puppet-parser:
  pkg.installed:
    - name: puppet

rust:
  pkgrepo.managed:
    - ppa: kevincantu/rust
    - require:
        - pkg: ppa
    - require_in:
        - pkg: rust
  pkg.installed: []

scala:
  pkg.installed

tex-chktex:
  pkg.installed:
    - name: chktex

tex-lacheck:
  pkg.installed:
    - name: lacheck

xml-xmlstarlet:
  pkg.installed:
    - name: xmlstarlet
