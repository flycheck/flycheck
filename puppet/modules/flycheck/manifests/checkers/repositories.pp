# Class: flycheck::checkers::repositories
#
# Provide all required repositories
class flycheck::checkers::repositories {

  # Clang packages (see http://llvm.org/apt/)
  apt::ppa { 'ppa:ubuntu-toolchain-r/test': } # GCC 4.8, common files

  apt::source { 'llvm-toolchain': # Clang packages
    location    => 'http://llvm.org/apt/precise/',
    repos       => 'main',
    release     => 'llvm-toolchain-precise-3.4',
    key         => 'AF4F7421',
    key_source  => 'http://llvm.org/apt/llvm-snapshot.gpg.key',
    include_src => false,
  }

  # Cfengine
  apt::source { 'cfengine-community':
    location    => 'http://cfengine.com/pub/apt/packages',
    repos       => 'main',
    release     => 'stable',
    key         => '89107B44',
    key_source  => 'http://cfengine.com/pub/gpg.key',
    include_src => false,
  }

  # Racket
  apt::ppa { 'ppa:plt/racket': }

  # Rust
  apt::ppa { 'ppa:hansjorg/rust': }

}
