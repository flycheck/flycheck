# "duplicate parameter found in resource"
file {'/tmp/somefile4321':
  mode => '0640',
  mode => '0640'
}
