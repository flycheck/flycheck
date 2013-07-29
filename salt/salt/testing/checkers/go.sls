# -*- mode: yaml; -*-

# Go
go-repo:
  pkgrepo.managed:
    - ppa: gophers/go
    - require:
        - pkg: python-software-properties
    - require_in:
        - pkg: go-gofmt
        - pkg: go-build
        - pkg: go-test

go-gofmt:
  pkg.installed:
    - name: golang-stable

go-build:
  pkg.installed:
    - name: golang-stable

go-test:
  pkg.installed:
    - name: golang-stable
