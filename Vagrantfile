# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"

  config.vm.provision :shell, :path => "vagrant/provision.sh"

  config.vm.provider :virtualbox do |v|
    v.name = "Flycheck testing"
  end
end
