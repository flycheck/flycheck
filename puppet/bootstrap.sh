#!/bin/sh

cd /tmp
if ! dpkg -l | grep -q puppetlabs-release; then
  wget http://apt.puppetlabs.com/puppetlabs-release-precise.deb
  sudo dpkg -i puppetlabs-release-precise.deb
fi

sudo apt-get update -yy
sudo apt-get install -y puppet
