# Copyright (c) 2012-2016 Sebastian Wiesner and Flycheck contributors

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

require 'bundler'
require 'rake'
require 'pathname'
require 'git'
require 'subprocess'

require_relative 'util'

module Flycheck
  # Provides utilities for Travis CI
  module Travis
    REPO_PATH = 'flycheck/flycheck.github.io'.freeze

    def self.travis_ci?
      ENV['CI'] == 'true' && ENV['TRAVIS'] == 'true'
    end

    def self.skip_unless_environment(variable, value, message)
      Util.check_environment variable, value, "DEPLOYMENT SKIPPED (#{message})"
    end

    def self.check_environment
      skip_unless_environment 'TRAVIS_REPO_SLUG', 'flycheck/flycheck',
                              'not our repo'
      skip_unless_environment 'TRAVIS_PULL_REQUEST', 'false', 'pull request'
      skip_unless_environment 'TRAVIS_SECURE_ENV_VARS', 'true',
                              'secure variables missing'
      skip_unless_environment 'TRAVIS_BRANCH', 'master',
                              'not the master branch'
    end

    def self.clone_and_configure_repo(target_directory)
      url = "https://github.com/#{REPO_PATH}.git"
      dest = Pathname.new(target_directory) / 'flycheck.github.io'
      repo = Git.clone(url, dest.to_s)
      repo.config('user.name', 'Flycheck Travis CI')
      repo.config('user.email', 'travis@flycheck.org')
      repo
    end

    def self.build_manual(repo)
      source_dir = Pathname.new(Dir.pwd).expand_path
      bundle_path = source_dir / 'vendor' / 'bundle'
      repo.chdir do
        Bundler.with_clean_env do
          # Install required gems for the website repo
          Subprocess.check_call(['bundle', 'install', '--jobs=3', '--retry=3',
                                 '--path', bundle_path.to_s])
          Subprocess.check_call(['bundle', 'exec',
                                 'rake', "build:manual[#{source_dir},latest]",
                                 "build:documents[#{source_dir}]"])
        end
      end
    end

    def self.configure_ssh_for_github(key)
      ssh_directory = Pathname.new(Dir.home) / '.ssh'
      ssh_directory.mkpath
      config_file = ssh_directory / 'config'
      config_file.write <<EOF
Host github.com
  Compression yes
  User git
  IdentityFile #{key.expand_path}
EOF
    end

    def self.try_commit_changes(repo)
      repo.add('.', all: true)
      revision = ENV['TRAVIS_COMMIT'][0..7]
      message = "Update from flycheck/flycheck@#{revision}"
      begin
        repo.commit(message)
        true
      rescue Git::GitExecuteError => e
        raise unless /^nothing to commit, working directory clean$/ =~ e.message
        false
      end
    end

    def self.push_changes(repo)
      repo.add_remote('deploy', "github.com:#{REPO_PATH}.git")
      repo.push('deploy', 'master:master')
    end

    def self.decrypt_deployment_key(source, target)
      Util.with_safe_umask do
        # Decrypt the deployment key
        key = ENV['encrypted_923a5f7c915e_key']
        iv = ENV['encrypted_923a5f7c915e_iv']
        openssl = ['openssl', 'aes-256-cbc',
                   '-K', key, '-iv', iv,
                   '-in', source.to_s, '-out', target.to_s, '-d']
        Subprocess.check_call(openssl)
        # Just to be on the safe side, explicitly restrict the permissions of
        # the decrypted key
        File.chmod(0700, target)
      end
    end

    def self.do_deploy_manual(directory)
      puts 'Clone website repository'
      repo = clone_and_configure_repo(directory)
      puts 'Build manual'
      build_manual(repo)

      puts 'Try to commit changes'
      if try_commit_changes(repo)
        key = Pathname.new(directory) / 'deploy'
        puts 'Decrypt deployment key'
        decrypt_deployment_key('admin/deploy.enc', key)
        puts 'Setup Github SSH authentication'
        configure_ssh_for_github(key)
        puts 'Push changes'
        push_changes(repo)
      else
        puts 'DEPLOYMENT SKIPPED (no changes)'
      end
    end

    def self.deploy_manual
      check_environment

      Dir.mktmpdir do |dir|
        do_deploy_manual(dir)
      end
    rescue Flycheck::UnexpectedEnvironmentError => e
      puts e.message
    end
  end
end
