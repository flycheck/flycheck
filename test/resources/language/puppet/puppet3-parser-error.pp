# Test that the missing comma error is parsed
class {'parser_error':
  hello      => 'test'
  helloagain => 'test'
}
