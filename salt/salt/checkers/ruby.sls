# -*- mode: yaml; -*-

# Syntax checkers tools for Ruby, available as Gems

{% set gem_runas = salt['pillar.get']('gem:runas', 'null') %}

haml:
  gem.installed:
    - ruby: default
    - runas: {{gem_runas}}
    - require:
        - pkg: ruby

puppet-lint:
  gem.installed:
    - ruby: default
    - runas: {{gem_runas}}
    - require:
        - pkg: ruby

ruby-rubocop:
  gem.installed:
    - ruby: default
    - runas: {{gem_runas}}
    - name: rubocop
    - require:
        - pkg: ruby

ruby:
  pkg.installed:
    - name: ruby1.9.1
  cmd.wait:
    - name: update-alternatives --set ruby /usr/bin/ruby1.9.1
    - watch:
        - pkg: ruby

ruby-jruby:
  pkg.installed:
    - name: jruby

sass:
  gem.installed:
    - ruby: default
    - runas: {{gem_runas}}
    - require:
        - pkg: ruby

scss: # SCSS is simply another syntax provided by Sass
  gem.installed:
    - ruby: default
    - runas: {{gem_runas}}
    - name: sass
    - require:
        - pkg: ruby
