# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure('2') do |config|
  config.vm.box = 'precise64'
  config.vm.box_url = 'http://files.vagrantup.com/precise64.box'

  config.vm.synced_folder '.', '/flycheck'

  config.vm.provision :shell, :inline => 'puppet apply /flycheck/puppet/manifests/bootstrap.pp'

  config.vm.provision :puppet do |puppet|
    puppet.manifests_path = 'puppet/manifests'
    puppet.manifest_file = 'site.pp'
    # We used to use the standard .module_path here, but since 1.4.1 Vagrant
    # does not longer add the default module path, which breaks our setup of
    # installing modules into /etc/puppet/modules in the guest VM.  Hence, we
    # now use this arguably questionable work around.  See
    # https://github.com/mitchellh/vagrant/pull/2677
    puppet.options = '--modulepath /etc/puppet/modules:/flycheck/puppet/modules'
  end

end
