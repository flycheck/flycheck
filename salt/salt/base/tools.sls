# -*- mode: yaml; -*-

# Adding of PPAs
ppa:
  pkg.installed:
    - name: python-software-properties

# Basic build and archive tools
archivetools:
  pkg.installed:
    - names:
        - tar
        - unzip

buildtools:
  pkg.installed:
    - names:
        - make
