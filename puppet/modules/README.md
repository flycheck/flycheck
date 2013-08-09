Puppet manifests
================

This directory contains Puppet modules and manifests to provision our virtual
machine for testing.  These are split into the following directories:

- `manifests/`: The root manifests.  `travis.pp` is a
  special manifest solely for Travis CI, which is used to install Ruby Gems as
  normal user.
- `lib/`: External Puppet modules.  See `Puppetfile` for the required modules,
  and use `librarian-puppet install` to install these modules.  Use `gem install
  librarian-puppet` to install the tool first.
- `modules/`: Our own modules to provision the VM.
