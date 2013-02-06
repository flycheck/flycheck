#!/bin/sh
# Copyright (C) 2013  Sebastian Wiesner <lunaryorn@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Provision a VM with necessary packages to test all Flycheck checkers

ppa () {
    sudo apt-add-repository -y "$1"
}

apt_update () {
    sudo apt-get update -qq
}

apt () {
    sudo apt-get install -yy "$@"
}

npm () {
    sudo npm install -g "$@"
}

pear () {
    sudo pear -q install "$@"
}

pip () {
    sudo pip install "$@"
}

gem () {
    # Do not force-install Gems as root, because this breaks on Travis CI
    command gem install "$@"
}

# Silence debconf
export DEBIAN_FRONTEND='noninteractive'

# Update repositories to prevent errors caused by missing packages
apt_update

# Install basic packages
apt make python-software-properties

# Bring in the necessary PPAs
ppa ppa:cassou/emacs
ppa ppa:ondrej/php5
ppa ppa:richarvey/nodejs
apt_update

# Install Emacs 24.2 and Emacs snapshot
apt emacs24 emacs24-el emacs24-common-non-dfsg \
    emacs-snapshot emacs-snapshot-el

# Install the necessary interpreters and tools
apt nodejs npm \
    python python-pip \
    php5-cli php-pear \
    ruby1.9.1

# Install all checker tools
# HACK: Install Lua mode from repositories, because the MELPA package causes
# tests to hang on Emacs snapshot
apt bash \
    tidy \
    lua5.2 lua-mode \
    chktex lacheck \
    xmlstarlet \
    zsh

pip flake8 pylint pyflakes

pear PHP_CodeSniffer

npm coffee-script coffeelint \
    csslint \
    jshint \
    jsonlint

gem haml \
    sass

# Install carton for Emacs dependency management
CARTON_DIR=/opt/carton-0.1.0
if ! [ -d "$CARTON_DIR" -a -x "/$CARTON_DIR/bin/carton" ]; then
  sudo rm -rf "$CARTON_DIR"
  wget -O - https://github.com/rejeep/carton/archive/v0.1.0.tar.gz | \
    sudo tar xz -C /opt
  # Bring carton into $PATH
  sudo ln -fs "$CARTON_DIR/bin/carton" /usr/local/bin
fi
