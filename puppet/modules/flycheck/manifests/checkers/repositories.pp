# Class: flycheck::checkers::repositories
#
# Provide all required repositories
class flycheck::checkers::repositories {

  # Clang 3.2 for Ubuntu 12.04
  apt::ppa { 'ppa:kxstudio-team/builds': }

  # Cfengine
  apt::source { 'cfengine-community':
    location    => 'http://cfengine.com/pub/apt',
    repos       => 'main',
    key         => '89107B44',
    key_source  => 'http://cfengine.com/pub/gpg.key',
    include_src => false,
  }

  # Go
  apt::ppa { 'ppa:juju/golang': }

  # Racket
  apt::ppa { 'ppa:plt/racket': }

  # Rust
  apt::ppa { 'ppa:hansjorg/rust': }

}
