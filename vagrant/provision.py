#!/usr/bin/env python2
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

"""
Provision a VM with necessary packages to test all of Flycheck.
"""

from __future__ import unicode_literals, print_function

import os
from subprocess import check_call
from collections import namedtuple


def sudo_check_call(cmd, *args, **kwargs):
    if os.geteuid() != 0:
        cmd = ['sudo'] + cmd
    return check_call(cmd, *args, **kwargs)


def update_repositories():
    sudo_check_call(['apt-get', 'update', '-qq'])


class Package(namedtuple('_Package', 'name')):
    def install(self):
        raise NotImplementedError()


class Ppa(Package):
    def install(self):
        sudo_check_call(['apt-add-repository', '-y', self.name])


class Apt(Package):
    def install(self):
        sudo_check_call(['apt-get', 'install', '-yy', self.name])


class Npm(Package):
    def install(self):
        sudo_check_call(['npm', 'install', '-g', self.name])


class Pip(Package):
    def install(self):
        sudo_check_call(['pip', 'install', self.name])


class Gem(Package):
    def install(self):
        check_call(['gem', 'install', self.name])


def install(packages):
    for pkg in packages:
        pkg.install()


BASE_PACKAGES = [
    Apt('make'), Apt('python-software-properties'),
    Ppa('ppa:cassou/emacs'), Ppa('ppa:ondrej/php5'),
    Ppa('ppa:richarvey/nodejs'),
]

EMACS = [
    Apt('emacs24'), Apt('emacs24-el'), Apt('emacs24-common-non-dfsg'),
    Apt('emacs-snapshot-el'), Apt('emacs-snapshot')  # Emacs Snapshot
]

INTERPRETERS = [
    Apt('nodejs'), Apt('npm'),
    Apt('python'), Apt('python-pip'),
    Apt('ruby1.9.1'),
]

PACKAGES = [
    # Bash
    Apt('bash'),
    # CoffeeScript
    Npm('coffee-script'), Npm('coffeelint'),
    # CSS
    Npm('csslint'),
    # Haml
    Gem('haml'),
    # HTML
    Apt('tidy'),
    # JavaScript,
    Npm('jshint'),
    # JSON
    Npm('jsonlint'),
    # Lua
    # HACK: Install Lua mode from repositories, because the MELPA packages
    # causes tests to hang with Emacs Snapshot
    Apt('lua5.2'), Apt('lua-mode'),
    # PHP
    Apt('php5-cli'),
    # Python
    Pip('flake8'), Pip('pylint'), Pip('pyflakes'),
    # Sass
    Gem('sass'),
    # LaTeX
    Apt('chktex'), Apt('lacheck'),
    # XML
    Apt('xmlstarlet'),
    # Zsh
    Apt('zsh')
]


# Disable debconf queries
os.environ['DEBIAN_FRONTEND'] = 'noninteractive'

update_repositories()
install(BASE_PACKAGES)
update_repositories()
install(EMACS + INTERPRETERS)
install(PACKAGES)
