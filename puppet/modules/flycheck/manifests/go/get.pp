# Definition: flycheck::go::get
#
# This class installs Go packages
#
# Parameters:
# - The $package
define flycheck::go::get($package = $title) {

  include flycheck::go

  exec { $package:
    command     => "go get -u ${package}",
    path        => ["${flycheck::go::root}/bin", '/usr/bin', '/bin'],
    environment => ["GOROOT=${flycheck::go::root}",
                    "GOPATH=${flycheck::go::path}"],
    require     => Class['flycheck::go']}
}
