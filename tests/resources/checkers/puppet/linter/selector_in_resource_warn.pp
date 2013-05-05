# "selector inside resource block"
file {'/tmp/somefile4321':
  mode => $::operatingsystem ? {
    debian => '0640',
    redhat => '0640',
  }
}
