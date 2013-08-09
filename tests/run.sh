#!/bin/sh

if [ -z "$EMACS" ]; then
  export EMACS=emacs
fi

cask exec "${EMACS}" -Q --no-site-lisp --script \
  "$(dirname $0)/flycheck-testrunner.el" "$@"
