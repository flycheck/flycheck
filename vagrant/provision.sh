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

wget () {
    command wget -q -O - "$1"
}

ppa () {
    for ppa in "$@"; do
        sudo apt-add-repository -y "$ppa"
    done
}

deb_src () {
    local name="$1"
    local content="$2"
    echo "$content" | sudo tee "/etc/apt/sources.list.d/${name}.list"
}

deb_src_key () {
    for key in "$@"; do
         wget "$key" | sudo apt-key add -
    done
}

apt_update () {
    sudo apt-get update -qq
}

apt () {
    sudo apt-get install -yy --fix-missing "$@"
}

npm () {
    sudo npm install -g "$@"
}

pear () {
    sudo pear update-channels
    sudo pear -q install "$@"
}

pip () {
    # Install from Crate index because link crawling on PyPI is damn slow.
    # See https://github.com/pypa/pip/issues/864 for more information.
    sudo pip install -i https://restricted.crate.io/ -U "$@"
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
apt unzip make python-software-properties

# Bring in the necessary PPAs and 3rd party repositories
ppa ppa:cassou/emacs \
    ppa:ondrej/php5 \
    ppa:richarvey/nodejs \
    ppa:gophers/go \
    ppa:kevincantu/rust
# We cannot add the Erlang repository as PPA, because PPAs get a corresponding
# `deb-src` entry in sources.list.  However, this repository doesn't provide
# source packages, and thus would break apt-get update if added with a `deb-src`
# entry
deb_src "esl-erlang" "deb http://binaries.erlang-solutions.com/debian precise contrib"
deb_src_key "http://binaries.erlang-solutions.com/debian/erlang_solutions.asc"
apt_update

# Install texinfo to build documentation
apt texinfo

if [ -z "$EMACS" ]; then
  # Install Emacs 24.2 and Emacs snapshot, if no Emacs is chosen
  apt emacs24-nox
  apt emacs-snapshot-nox
else
  apt "$EMACS"-nox || exit 1
fi

# Install the necessary interpreters and tools
apt nodejs npm \
    python python-pip \
    php5-cli php-pear \
    ruby1.9.1 \
    esl-erlang

# Install all checker tools
apt bash \
    tidy \
    lua5.2 \
    chktex lacheck \
    xmlstarlet \
    zsh \
    golang-stable \
    rust

pip flake8 pep8-naming pylint docutils

pear PHP_CodeSniffer

npm coffee-script coffeelint \
    csslint \
    jshint \
    jsonlint

gem haml \
    sass \
    rubocop

# Install Elixir compiler.
ELIXIR_VERSION=0.8.1
ELIXIR_DIR="/opt/elixir-${ELIXIR_VERSION}"
ELIXIR_SYMDIR="/usr/local/bin"
if ! [ -d "$ELIXIR_DIR" -a -x "$ELIXIR_DIR/elixirc" ]; then
    sudo rm -rf "$ELIXIR_DIR"
    # Download to a temporary file, because silly unzip can't cope with stdin
    ELIXIR_DOWNLOAD="$(tempfile)"
    wget "http://dl.dropbox.com/u/4934685/elixir/v0.8.1.zip" > "$ELIXIR_DOWNLOAD"
    sudo unzip -qq -d "$ELIXIR_DIR" "$ELIXIR_DOWNLOAD"
    rm "$ELIXIR_DOWNLOAD"
    sudo ln -fs "$ELIXIR_DIR/bin/elixirc" "$ELIXIR_SYMDIR"
fi

# Install carton for Emacs dependency management
CARTON_VERSION=0.3.0
CARTON_DIR="/opt/carton-${CARTON_VERSION}"
if ! [ -d "$CARTON_DIR" -a -x "/$CARTON_DIR/bin/carton" ]; then
  sudo rm -rf "$CARTON_DIR"
  wget "https://github.com/rejeep/carton/archive/v${CARTON_VERSION}.tar.gz" | \
    sudo tar xz -C /opt
  # Bring carton into $PATH
  sudo ln -fs "$CARTON_DIR/bin/carton" /usr/local/bin
fi
