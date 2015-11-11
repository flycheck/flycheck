# Class: foo::bar
class foo::bar {
  case $::operatingsystem {
    debian: {
      $version = '1.2.3'
    }
    redhat: {
      $version = '6.6.6'
    }
  }
}
