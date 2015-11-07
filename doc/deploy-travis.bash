#!/bin/bash
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

set -e

check_environment() {
    local actual="$1"
    local expected="$2"
    local message="$3"

    if [[ $actual != "$expected" ]]; then
      echo "DEPLOYMENT SKIPPED (${message})"
      exit 0
    fi
}

eval_ssh_agent() {
  # shellcheck disable=SC2046
  eval $(ssh-agent "${@}")
}

check_environment "$TRAVIS_REPO_SLUG" "flycheck/flycheck" "not our repo"
check_environment "$TRAVIS_PULL_REQUEST" "false" "pull request"
check_environment "$TRAVIS_SECURE_ENV_VARS" "true" "secure variables missing"

if [[ -n $TRAVIS_TAG ]]; then
  MANUAL_VERSION="${TRAVIS_TAG}"
else
  MANUAL_VERSION="latest"
  check_environment "$TRAVIS_BRANCH" "master" "not the master branch"
fi

echo "Publishing manual..."

# Decrypt and load the deployment key
# shellcheck disable=SC2154
openssl aes-256-cbc -K "${encrypted_923a5f7c915e_key}" -iv "${encrypted_923a5f7c915e_iv}" -in doc/deploy.enc -out doc/deploy -d
chmod 600 doc/deploy
eval_ssh_agent -s
ssh-add doc/deploy
rm doc/deploy

# Git setup
export GIT_COMMITTER_EMAIL='travis@flycheck.org'
export GIT_COMMITTER_NAME='Flycheck Travis CI'
export GIT_AUTHOR_EMAIL='travis@flycheck.org'
export GIT_AUTHOR_NAME='Flycheck Travis CI'

git clone --quiet --branch=master "git@github.com:flycheck/flycheck.github.io.git" doc/_deploy

cd doc/_deploy
rake "build:manual[../..,${MANUAL_VERSION}]" 'build:documents[../..]'
git add --force --all .
git status
if ! git diff --quiet HEAD; then
  git commit -m "Update manual from flycheck/flycheck@$(git rev-parse --short "${TRAVIS_COMMIT}")"
  git push --force --quiet origin master
else
  echo "No changes"
fi
cd ../..

# shellcheck disable=SC2046
eval_ssh_agent -k
echo "Published manual!"
