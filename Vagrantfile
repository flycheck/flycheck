# -*- mode: ruby -*-

Vagrant.configure('2') do |config|
  config.vm.define "flycheck", primary: true do |flycheck|
    flycheck.vm.box = 'hashicorp/precise64'
    flycheck.vm.host_name = 'flycheck-test'
    flycheck.vm.synced_folder '.', '/flycheck'

    flycheck.vm.provider "virtualbox" do |vb|
      vb.name = 'flycheck'
      vb.memory = 1024
    end

    flycheck.vm.provision "ansible" do |ansible|
      ansible.playbook = "playbooks/vm.yml"
    end
  end
end
