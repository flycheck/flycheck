# -*- mode: yaml; -*-

# Huskily and Haskell syntax checking tools

# Install the Cabal package manager
cabal-install:
  pkg.installed:
    - require:
        - pkg: ghc
  cmd.wait:
    - name: cabal update
    - watch:
        - pkg: cabal-install

# Syntax checkers:
haskell-hdevtools:
  cmd.run:
    - name: cabal install --global hdevtools
    - unless: hdevtools --version
    - require:
        - cmd: cabal-install

haskell-ghc:
  pkg.installed:
    - name: ghc

haskell-hlint:
  pkg.installed:
    - name: hlint
