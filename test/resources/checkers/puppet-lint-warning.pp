# "case statement without a default case"
case $::operatingsystem {
  debian: {
    $version = '1.2.3'
  }
  redhat: {
    $version = '6.6.6'
  }
}
