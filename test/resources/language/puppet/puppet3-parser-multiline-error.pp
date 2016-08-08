# Test that the multiline error output of puppet parser is parsed
file { 'test':
  ensure   => file,
  contents => something'
}
