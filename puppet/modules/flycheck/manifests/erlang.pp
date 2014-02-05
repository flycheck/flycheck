# Class: flycheck::erlang
#
# Provides an Erlang environment
class flycheck::erlang {

  apt::source { 'erlang-solutions':
    location    => 'http://binaries.erlang-solutions.com/debian',
    repos       => 'contrib',
    key         => 'A14F4FCA',
    key_source  => 'http://binaries.erlang-solutions.com/debian/erlang_solutions.asc',
    include_src => false,
  }

  package { 'esl-erlang':
    ensure  => latest,
    require => Apt::Source['erlang-solutions'],
  }
}
