# Class: flycheck::checkers::misc
#
# Installs syntax checkers from miscellaneous sources
class flycheck::checkers::misc {

  include flycheck::erlang

  $elixir_version = '0.12.0'

  archive { "elixir-${elixir_version}":
    ensure        => present,
    url           => "https://github.com/elixir-lang/elixir/releases/download/v${elixir_version}/v${elixir_version}.zip",
    extension     => 'zip',
    digest_string => '62fc9173158ba919b2d0f792b827eca7',
    target        => "/opt/elixir-${elixir_version}",
    root_dir      => '.',
    require       => Package['esl-erlang'],
  }

  file { '/usr/local/bin/elixirc':
    ensure  => link,
    target  => "/opt/elixir-${elixir_version}/bin/elixirc",
    require => Archive["elixir-${elixir_version}"],
  }
}
