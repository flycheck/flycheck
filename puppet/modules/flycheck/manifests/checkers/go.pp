# Go syntax checkers

class flycheck::checkers::go {
  apt::ppa { 'ppa:gophers/go': }

  package { 'golang-stable':
    ensure  => latest,
    require => Apt::Ppa['ppa:gophers/go'],
  }
}
