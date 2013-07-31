# -*- mode: yaml; -*-

# Erlang and Elixir
{% set elixir_archive = '/usr/src/elixir-{0}.zip'.format(pillar['elixir']['version']) %}
{% set elixir_directory = '/opt/elixir-{0}'.format(pillar['elixir']['version']) %}

{{elixir_archive}}:
  file.managed:
    - source: http://dl.dropbox.com/u/4934685/elixir/v{{pillar['elixir']['version']}}.zip
    - source_hash: md5={{pillar['elixir']['hash']}}

{{elixir_directory}}:
  cmd.run:
    - name: unzip -o {{elixir_archive}} -d /opt/elixir-{{pillar['elixir']['version']}}
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
