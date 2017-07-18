#!/usr/bin/env python3
# Copyright (C) 2017 Flycheck contributors
# Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

# This file is not part of GNU Emacs.

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

import re
import sys
import subprocess
from datetime import date
from collections import namedtuple
from pathlib import Path

import requests
from git import Repo


SOURCE_DIR = Path(__file__).resolve().parent.parent
FLYCHECK_EL = SOURCE_DIR.joinpath('flycheck.el')
CHANGELOG = SOURCE_DIR.joinpath('CHANGES.rst')

TRAVIS_ENDPOINT = 'https://api.travis-ci.org/repos/flycheck/flycheck'

VERSION_HEADER_RE = re.compile(
    r'^(?P<label>;;\s*Version:\s*)(?P<value>\S+)\s*$',
    re.MULTILINE)


class CannotReleaseError(Exception):
    pass


class Version(namedtuple('Version', 'version is_snapshot')):

    RE = re.compile(r'^(?P<version>\d+)(?:(?P<snapshot>-cvs))?$')

    @classmethod
    def fromstring(cls, s):
        match = cls.RE.match(s)
        if not match:
            raise ValueError('Not a version: {}'.format(s))
        return cls(version=int(match.group('version')),
                   is_snapshot=match.group('snapshot') is not None)

    def __str__(self):
        if self.is_snapshot:
            return '{}-cvs'.format(self.version)
        else:
            return str(self.version)

    @property
    def is_released(self):
        return not self.is_snapshot

    def bump(self):
        if self.is_snapshot:
            # If snapshot, then bump to release version by dropping the
            # snapshot indicator
            return self._replace(is_snapshot=False)
        else:
            # If release bump to the next snapshot version
            return self._replace(version=self.version + 1, is_snapshot=True)


class BuildState(namedtuple('BuildState', 'commit state')):

    @classmethod
    def get_from_travis_ci(cls):
        response = requests.get(
            TRAVIS_ENDPOINT + '/branches/master',
            headers={'Accept': 'application/vnd.travis-ci.2+json'}).json()
        return cls(commit=response['commit']['sha'],
                   state=response['branch']['state'])


def read_version_from_library_header(path):
    contents = path.read_text()
    match = VERSION_HEADER_RE.search(contents)
    if match:
        return Version.fromstring(match.group('value'))
    else:
        raise ValueError('Could not find version header in {}'.format(path))


def set_version_in_library_header(path, version):
    contents = path.read_text()
    path.write_text(VERSION_HEADER_RE.sub(
        r'\g<label>{}'.format(version), contents))


def finalise_relase_in_changelog(path, version, date):
    lines = path.read_text().splitlines()
    if not lines[0].endswith(' (in development)'):
        raise ValueError('Failed to find snapshot header in {}'.format(path))
    new_header = '{} ({})'.format(version, date.strftime('%b %d, %Y'))
    header_underline = '=' * len(new_header)
    path.write_text(
        '\n'.join([new_header, header_underline] + lines[2:]) + '\n')


def add_snapshot_to_changelog(path, version):
    header = '{} (in development)'.format(version)
    contents = path.read_text()
    underline = '=' * len(header)
    path.write_text('{}\n{}\n\n{}'.format(header, underline, contents))


def commit_and_push_release(repo, version):
    repo.index.add(str(p) for p in [FLYCHECK_EL, CHANGELOG])
    repo.index.commit('Release version {}'.format(version))
    repo.create_tag(str(version), message='Flycheck {}'.format(version),
                    sign=True)
    repo.remotes.origin.push('master', follow_tags=True)


def commit_and_push_snapshot(repo):
    repo.index.add(str(p) for p in [FLYCHECK_EL, CHANGELOG])
    repo.index.commit('Bump version in master')
    repo.remotes.origin.push('master')


def build_dist():
    subprocess.run(['cask', 'package'], cwd=str(SOURCE_DIR), check=True)


def ask_yes_or_no(prompt):
    return input(prompt).lower() == 'y'


def ensure_can_make_release(repo):
    if repo.head.ref != repo.refs.master:
        raise CannotReleaseError(
            'Cannot make release from branch {}.'
            ' Switch to master!'.format(repo.head.ref))
    if repo.is_dirty(untracked_files=True):
        raise CannotReleaseError(
            'Cannot release from dirty working directory.'
            ' Please commit or stash all changes!')
    state = BuildState.get_from_travis_ci()
    if state.commit != repo.head.ref.object.hexsha:
        raise CannotReleaseError(
            'HEAD not tested on Travis CI.\n'
            'Please push your changes and wait for the build to complete.')
    if state.state != 'passed':
        raise CannotReleaseError(
            'Build not passed (state: {})\n'
            'Wait for the build to finish or fix the error!'.format(
                state.state))


def main():
    try:
        repo = Repo(str(SOURCE_DIR))
        ensure_can_make_release(repo)

        current_version = read_version_from_library_header(FLYCHECK_EL)
        next_version = current_version.bump()

        if not ask_yes_or_no('Releasing Flycheck {}, '
                             'are you sure? [yn] '.format(next_version)):
            raise CannotReleaseError('Aborted')

        set_version_in_library_header(FLYCHECK_EL, next_version)
        finalise_relase_in_changelog(CHANGELOG, next_version, date.today())
        commit_and_push_release(repo, next_version)
        build_dist()

        # Now bump to next snapshot version
        next_snapshot = next_version.bump()
        set_version_in_library_header(FLYCHECK_EL, next_snapshot)
        add_snapshot_to_changelog(CHANGELOG, next_snapshot)
        commit_and_push_snapshot(repo)

        print('Flycheck {} out now, new snapshot {}! Please'.format(
            next_version, next_snapshot))
        print("""
* add information about the release to https://github.com/flycheck/flycheck/releases/edit/{0}
* upload `dist/flycheck-{0}.tar,
* enable version {0} on https://readthedocs.org/dashboard/flycheck/versions/, and
* announce the release in the flycheck/flycheck Gitter channel.
""".format(next_version))       # noqa: E501

    except CannotReleaseError as error:
        sys.exit(str(error))


if __name__ == '__main__':
    main()
