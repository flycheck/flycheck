# Install Erlang and related checkers

class flycheck::checkers::erlang {
  apt::source { 'erlang-solutions':
    location    => 'http://binaries.erlang-solutions.com/debian',
    repos       => 'contrib',
    key         => 'A14F4FCA',
    key_source  => 'http://binaries.erlang-solutions.com/debian/erlang_solutions.asc',
    include_src => false,
  }

  package { 'esl-erlang':
    ensure  => latest,
    require => Apt::Source['erlang-solutions']
  }

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
