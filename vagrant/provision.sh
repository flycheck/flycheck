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
    for ppa in "$@"; do
      sudo apt-add-repository -y "$ppa"
    done
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
    sudo pear update-channels
    sudo pear -q install "$@"
}

pip () {
    sudo pip install -U --use-mirrors "$@"
}

gem () {
    # Do not force-install Gems as root, because this breaks on Travis CI
    command gem install "$@"
}

install_from_github () {
    # $1: github account
    # $2: repository name
    # $3: branch
    cd /tmp
    git clone -b $3 "git://github.com/$1/$2.git"
    cd $2
    make -j $(nproc)
    sudo make install
}

# Silence debconf
export DEBIAN_FRONTEND='noninteractive'

# Update repositories to prevent errors caused by missing packages
apt_update

# Install basic packages
apt make python-software-properties

# Bring in the necessary PPAs
ppa ppa:cassou/emacs ppa:ondrej/php5 ppa:richarvey/nodejs ppa:gophers/go ppa:kevincantu/rust
apt_update

# Install texinfo to build documentation
apt texinfo

if [ -z "$EMACS" ]; then
  # Install Emacs 24.2 and Emacs snapshot, if n
  apt emacs24-nox
  apt emacs-snapshot-nox
else
  apt "$EMACS"-nox || exit 1
fi

# Install the necessary interpreters and tools
apt nodejs npm \
    python python-pip \
    php5-cli php-pear \
    ruby1.9.1

# Install all checker tools
apt bash \
    tidy \
    lua5.2 \
    chktex lacheck \
    xmlstarlet \
    zsh \
    golang-stable \
    rust

pip flake8 pep8-naming pylint pyflakes docutils

pear PHP_CodeSniffer

npm coffee-script coffeelint \
    csslint \
    jshint \
    jsonlint

gem haml \
    sass

# Install a stable version of Elixir from source
# wget "https://elearning.erlang-solutions.com/couchdb//rbingen_adapter//package_R16B_precise64_1361901944/esl-erlang_16.b-1~ubuntu~precise_amd64.deb"
# sudo dpkg -i esl-erlang_16.b-1~ubuntu~precise_amd64.deb
ppa "deb http://binaries.erlang-solutions.com/debian precise contrib"
wget -O - "http://binaries.erlang-solutions.com/debian/erlang_solutions.asc" | \
  sudo apt-key add -
apt_update
sudo apt-get install -y --fix-missing  esl-erlang
install_from_github elixir-lang elixir stable

# Install carton for Emacs dependency management
CARTON_VERSION=0.2.0
CARTON_DIR="/opt/carton-${CARTON_VERSION}"
if ! [ -d "$CARTON_DIR" -a -x "/$CARTON_DIR/bin/carton" ]; then
  sudo rm -rf "$CARTON_DIR"
  wget -O - "https://github.com/rejeep/carton/archive/v${CARTON_VERSION}.tar.gz" | \
    sudo tar xz -C /opt
  # Bring carton into $PATH
  sudo ln -fs "$CARTON_DIR/bin/carton" /usr/local/bin
fi
