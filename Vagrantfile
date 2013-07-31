# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure('2') do |config|
  config.vm.box = 'precise64'
  config.vm.box_url = 'http://files.vagrantup.com/precise64.box'

  config.vm.synced_folder '.', '/flycheck'

  config.vm.provision :salt do |salt|
    salt.minion_config = './salt/vagrant.minion'
    salt.run_highstate = true
  end

end
