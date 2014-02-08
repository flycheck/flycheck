# Class: flycheck::go
#
# Install Go
class flycheck::go {

  $root = '/usr/local/go/'      # Go distribution root
  $path = '/usr/local/lib/go'   # Go global package directory

  $version = '1.2'
  $url = "https://go.googlecode.com/files/go${version}.linux-amd64.tar.gz"

  archive { "go-${version}":
    ensure        => present,
    url           => $url,
    extension     => 'tar.gz',
    digest_type   => 'sha1',
    digest_string => '664e5025eae91412a96a10f4ed1a8af6f0f32b7d',
    target        => '/usr/local/',
  }

  # Place Go tools in $PATH
  file { '/etc/profile.d/go.sh':
    content => "export PATH=\$PATH:${path}/bin:${root}/bin/\n",
    mode    => '0644',
    owner   => 'root',
    group   => 'root',
    require => Archive["go-${version}"],
  }

  file { $path:
    ensure  => directory,
    require => Archive["go-${version}"]
  }

}
