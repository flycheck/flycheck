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

begin
  require 'bundler'
  require 'bundler/setup'
rescue LoadError
  puts '\e[31mFailed to load bundler\e[0m'
  puts '\e[33mPlease run `gem install bundler` and `bundle install`.\e[0m'
  exit 1
end

require 'rake'
require 'rake/clean'

require 'rubocop/rake_task'
require 'html/proofer'

def emacs_batch(*args)
  [ENV['EMACS'] || 'emacs', '-Q', '--batch'] + args
end

SOURCES = FileList['flycheck.el', 'flycheck-ert.el']
OBJECTS = SOURCES.ext('.elc')

DOC_SOURCES = FileList['doc/flycheck.texi']
DOC_SOURCES.add('doc/*.texi')

MARKDOWN_SOURCES = FileList['*.md']

RUBY_SOURCES = FileList['Rakefile']

# File tasks and rules
file 'doc/images/logo.png' => ['flycheck.svg'] do |t|
  sh 'convert', t.prerequisites.first,
     '-trim', '-background', 'white',
     '-bordercolor', 'white', '-border', '5',
     t.name
  sh 'optipng', t.name
end

file 'doc/flycheck.info' => DOC_SOURCES do |t|
  sh 'texi2any', '-o', t.name, t.prerequisites.first
end

file 'doc/flycheck.html' => DOC_SOURCES do |t|
  customizations = {
    'TOP_NODE_UP_URL' => 'http://www.flycheck.org',
    # Suggest that we use HTML 5.  We don't do actually, but we want the browse
    # to think that we do
    'DOCTYPE' => '<!DOCTYPE html>'
  }
  cmd = ['texi2any', '--html', '--no-split']
  cmd += customizations.map do |var, value|
    ['--set-customization-variable', "#{var}=#{value}"]
  end.flatten

  cmd << '-o' << t.name << t.prerequisites.first
  sh(*cmd)
end

rule '.elc', [:build_flags] => ['.el'] do |t, args|
  batch_args = ['-L', '.']
  if args.to_a.include? 'error-on-warn'
    batch_args << '--eval'
    batch_args << '(setq byte-compile-error-on-warn t)'
  end

  batch_args << '-f'
  batch_args << 'batch-byte-compile'
  batch_args += t.prerequisites

  sh 'cask', 'exec', *emacs_batch(*batch_args)
end

# Tasks
namespace :init do
  CLOBBER << '.cask/'

  desc 'Install all dependencies'
  task :deps do
    sh 'cask', 'install'
    sh 'cask', 'update'
  end

  desc 'Initialise the project'
  task all: [:deps]
end

namespace :verify do
  desc 'Verify Travis configuration'
  task :travis do
    sh('bundle', 'exec', 'travis', 'lint', '--exit-code', '--no-interactive')
  end

  desc 'Verify Markdown documents'
  task :markdown do
    sh('bundle', 'exec', 'mdl',
       '--style', 'admin/markdown_style',
       *MARKDOWN_SOURCES)
  end

  desc 'Verify Ruby sources'
  RuboCop::RakeTask.new(:ruby) do |task|
    task.patterns = RUBY_SOURCES
  end

  desc 'Verify Emacs Lisp sources'
  task :elisp do
    sh(*emacs_batch('--eval', '(setq checkdoc-arguments-in-order-flag nil)',
                    '-l', 'test/flycheck-checkdoc.el',
                    '-f', 'flycheck-checkdoc-batch-and-exit',
                    *SOURCES))
  end

  task 'Verify all source files'
  task all: [:travis, :markdown, :ruby, :elisp]
end

namespace :generate do
  desc 'Generate the log'
  task logo: 'doc/images/logo.png'

  desc 'Generate all sources'
  task generate: [:logo]
end

namespace :compile do
  CLEAN.add(OBJECTS)

  desc 'Compile byte code'
  task :elc, [:build_flags] => OBJECTS

  desc 'Compile everything'
  task :all, [:build_flags] => [:elc]
end

namespace :test do
  desc 'Run unit test suite'
  task :unit, [:selector] => OBJECTS do |_, args|
    test_args = args.selector ? [args.selector] : []
    sh(*emacs_batch('--script', 'test/run.el', *test_args))
  end

  desc 'Test HTML manual for broken links'
  task html: ['doc/flycheck.html'] do |t|
    HTML::Proofer
      .new(t.prerequisites.first,
           disable_external: true,
           checks_to_ignore: ['ScriptCheck'])
      .run
  end

  desc 'Run all tests'
  task all: [:unit, :html]
end

namespace :doc do
  CLEAN << 'doc/flycheck.info'
  CLEAN << 'doc/flycheck.html'
  CLEAN << 'doc/dir'

  desc 'Build Info manual'
  task info: ['doc/flycheck.info']

  desc 'Build HTML manual'
  task html: ['doc/flycheck.html']

  desc 'Build all documentation'
  task all: [:info, :html]
end

namespace :dist do
  CLEAN << 'dist/'

  desc 'Build an Emacs package'
  task :package do
    sh 'cask', 'package'
  end

  desc 'Build all distributions'
  task all: [:package]
end
