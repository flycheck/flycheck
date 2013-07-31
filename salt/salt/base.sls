# -*- mode: yaml; -*-

# Base packages

# Basic utilities
tar:
  pkg.installed

unzip:
  pkg.installed

python-software-properties: # To add PPAs
  pkg.installed

# Basic build tools
make:
  pkg.installed

# Texinfo documentation system
texinfo:
  pkg.installed

install-info:
  pkg.installed

# Emacs packages
emacs:
  pkgrepo.managed:
    - ppa: cassou/emacs
    - require:
        - pkg: python-software-properties
    - require_in:
        - pkg: emacs24-nox
        - pkg: emacs-snapshot-nox

emacs24-nox:
  pkg.installed

emacs-snapshot-nox:
  pkg.installed

# Carton for Emacs dependency management
{% set carton_version = salt['pillar.get']('carton:version') %}
{% set carton_hash = salt['pillar.get']('carton:hash') %}
{% set carton_archive = '/usr/src/carton-{0}.tar.gz'.format(carton_version) %}
{% set carton_directory = '/opt/carton-{0}'.format(carton_version) %}

{{carton_archive}}:
  file.managed:
    - source: https://github.com/rejeep/carton/archive/v{{carton_version}}.tar.gz
    - source_hash: md5={{carton_hash}}

{{carton_directory}}:
  cmd.run:
    - name: tar xzf {{carton_archive}} -C /opt/
    - unless: test -d {{carton_directory}}
    - require:
        - pkg: tar
        - file: {{carton_archive}}

carton:
  file.symlink:
    - name: /usr/local/bin/carton
    - target: {{carton_directory}}/bin/carton
    - require:
        - cmd: {{carton_directory}}
