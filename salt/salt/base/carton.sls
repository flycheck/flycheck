# -*- mode: yaml; -*-

# Carton

include:
  - base.tools

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
