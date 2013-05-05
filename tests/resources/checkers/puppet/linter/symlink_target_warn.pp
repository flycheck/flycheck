# "symlink target specified in ensure attr"
file {'/tmp/somefile4321':
  ensure => '/tmp/realfile4321',
}
