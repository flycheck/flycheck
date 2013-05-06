# "ensure found on line but it’s not the first attribute"
service {'nginx':
  arg1   => true,
  ensure => enabled
}
