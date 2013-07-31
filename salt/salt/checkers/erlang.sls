# -*- mode: yaml; -*-

include:
  - base.tools

# Erlang and Elixir
{% set elixir_version = salt['pillar.get']('elixir:version') %}
{% set elixir_hash = salt['pillar.get']('elixir:hash') %}
{% set elixir_archive = '/usr/src/elixir-{0}.zip'.format(elixir_version) %}
{% set elixir_directory = '/opt/elixir-{0}'.format(elixir_version) %}

{{elixir_archive}}:
  file.managed:
    - source: https://github.com/elixir-lang/elixir/releases/download/v{{elixir_version}}/v{{elixir_version}}.zip
    - source_hash: md5={{elixir_hash}}

{{elixir_directory}}:
  cmd.run:
    - name: unzip -o {{elixir_archive}} -d /opt/elixir-{{elixir_version}}
    - unless: test -d {{elixir_directory}}
    - require:
        - file: {{elixir_archive}}
        - pkg: unzip

elixir:
  file.symlink:
    - name: /usr/local/bin/elixirc
    - target: {{elixir_directory}}/bin/elixirc
    - require:
        - cmd: {{elixir_directory}}

erlang:
  pkgrepo.managed:
    - name: deb http://binaries.erlang-solutions.com/debian precise contrib
    - key_url: http://binaries.erlang-solutions.com/debian/erlang_solutions.asc
    - require_in:
        pkg: erlang
  pkg.installed:
    - name: esl-erlang
