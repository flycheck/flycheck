# Class: flycheck::checkers::go
#
# Install checkers with the Go package system.
class flycheck::checkers::go {

  include flycheck::git         # For Github
  include flycheck::mercurial   # For Mercurial on Google Code

  flycheck::go::get { 'github.com/golang/lint/golint': # go-golint
    require => Class['flycheck::git']
  }

  flycheck::go::get { 'code.google.com/p/go.tools/cmd/vet': # go-vet
    require => Class['flycheck::mercurial']
  }
}
