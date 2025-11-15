#!/usr/bin/env zsh
# shellcheck shell=bash
var="foo bar"
if [[ $var =~ "foo.*bar" ]]; then
    echo "Match!"
fi
