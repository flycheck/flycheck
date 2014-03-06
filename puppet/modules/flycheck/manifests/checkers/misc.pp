# Class: flycheck::checkers::misc
#
# Installs syntax checkers from miscellaneous sources
class flycheck::checkers::misc {

  require flycheck::erlang

  $elixir_version = '0.12.4'

  archive { "elixir-${elixir_version}":
    ensure        => present,
    url           => "https://github.com/elixir-lang/elixir/releases/download/v${elixir_version}/Precompiled.zip",
    extension     => 'zip',
    digest_string => 'c6a41e761ee26687ec0939d666e0bc07',
    target        => "/opt/elixir-${elixir_version}",
    root_dir      => '.',
  }

  file { '/usr/local/bin/elixirc':
    ensure  => link,
    target  => "/opt/elixir-${elixir_version}/bin/elixirc",
    require => Archive["elixir-${elixir_version}"],
  }
}
