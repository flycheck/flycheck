# -*- mode: yaml; -*-

# Pillars specific to Travis CI

gem:
  runas: {{salt['grains.get']('travis_user')}}
