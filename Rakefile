# Copyright (c) 2012-2015 Sebastian Wiesner and Flycheck contributors

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

require 'rake'
require 'rake/clean'

def emacs_batch(*args)
  [ENV['EMACS'] || 'emacs', '-Q', '--batch'] + args
end

SOURCES = FileList['flycheck.el', 'flycheck-ert.el']
OBJECTS = SOURCES.ext('.elc')

DOC_SOURCES = FileList['doc/flycheck.texi']
DOC_SOURCES.add('doc/*.texi')

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
  sh 'texi2any', '--html', '--no-split', '-o', t.name, t.prerequisites.first
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
  desc 'Run checkdoc on all sources'
  task :checkdoc do
    sh(*emacs_batch('--eval', '(setq checkdoc-arguments-in-order-flag nil)',
                    '-l', 'test/flycheck-checkdoc.el',
                    '-f', 'flycheck-checkdoc-batch-and-exit',
                    *SOURCES))
  end

  task 'Verify all source files'
  task all: [:checkdoc]
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

  desc 'Run all tests'
  task all: [:unit]
end

namespace :doc do
  CLEAN << 'doc/flycheck.info'
  CLEAN << 'doc/dir'

  desc 'Build Texinfo manual'
  task texinfo: ['doc/flycheck.info']

  desc 'Build HTML manual'
  task html: ['doc/flycheck.html']

  desc 'Build all documentation'
  task all: [:texinfo, :html]
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
