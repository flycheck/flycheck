# Bootstrap Puppet 3 and Puppet modules
#
# This manifest must be kept compatible with Puppet, and must not use any
# external modules.

# Bootstrap Puppet 3
$release_package = 'puppetlabs-release-precise.deb'

exec { 'release-package':
  cwd     => '/tmp',
  command => "wget http://apt.puppetlabs.com/${release_package}",
  creates => "/tmp/${release_package}.deb",
  unless  => 'dpkg -l | grep -q puppetlabs-release',
  path    => ['/usr/bin', '/bin'],
}

package { 'puppetlabs-release':
  ensure   => installed,
  provider => dpkg,
  source   => '/tmp/puppetlabs-release-precise.deb',
  require  => Exec['release-package'],
  notify   => Exec['apt-get update'],
}

exec { 'apt-get update':
  command     => 'apt-get update -yy',
  path        => ['/usr/bin', '/bin'],
  refreshonly => true,
}

package { 'puppet':
  ensure  => latest,
  require => Exec['apt-get update']
}

# Bootstrap the required Puppet modules
define puppet::module($module = $title, $installed_name = undef, $version = undef) {

  $version_argument = $version ? {
    undef   => '',
    default => "--version ${version}",
  }

  $directory_name = $installed_name ? {
    undef   => regsubst($module, '/', '-'),
    default => $installed_name
  }

  exec { "puppet::module::install::${module}":
    command => "puppet module install ${module} ${version_argument}",
    path    => ['/usr/bin', '/bin'],
    unless  => "puppet module list | grep -q ${directory_name}",
    require => Package['puppet'],
  }
}

puppet::module { ['puppetlabs/stdlib',
                  'puppetlabs/apt',
                  'puppetlabs/nodejs',
                  'gini/archive',
                  'adrien/alternatives']:
}

puppet::module { 'nodes/php':
  installed_name => 'puppet-php',
}
