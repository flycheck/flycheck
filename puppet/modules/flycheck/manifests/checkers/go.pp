# Class: flycheck::checkers::go
#
# Install checkers with the Go package system.
class flycheck::checkers::go {

  include flycheck::git         # For Github

  flycheck::go::get { 'github.com/golang/lint/golint': # go-golint
    require => Class['flycheck::git']
  }
}
