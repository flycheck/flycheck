# -*- mode: ruby -*-

Vagrant.configure('2') do |config|

  config.vm.box = 'hashicorp/precise64'
  config.vm.host_name = 'flycheck-test'
  config.vm.synced_folder '.', '/flycheck'

  config.vm.provider "virtualbox" do |vb|
    vb.name = 'flycheck'
    vb.memory = 1024
  end

  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "playbooks/site.yml"

    tags = ENV['ANSIBLE_TAGS']
    ansible.tags = tags.split(',') if tags
  end
end
