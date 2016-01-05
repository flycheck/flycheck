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
check_environment "$TRAVIS_BRANCH" "master" "not the master branch"

echo "Publishing manual..."

mkdir "build/"

# Decrypt and load the deployment key
# shellcheck disable=SC2154
openssl aes-256-cbc -K "${encrypted_923a5f7c915e_key}" -iv "${encrypted_923a5f7c915e_iv}" -in admin/deploy.enc -out build/deploy -d
chmod 600 build/deploy
eval_ssh_agent -s
ssh-add build/deploy
rm build/deploy

# Git setup
export GIT_COMMITTER_EMAIL='travis@flycheck.org'
export GIT_COMMITTER_NAME='Flycheck Travis CI'
export GIT_AUTHOR_EMAIL='travis@flycheck.org'
export GIT_AUTHOR_NAME='Flycheck Travis CI'

git clone --quiet --branch=master "git@github.com:flycheck/flycheck.github.io.git" build/_deploy

cd build/_deploy
rake "build:manual[../..,latest]" 'build:documents[../..]'
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
