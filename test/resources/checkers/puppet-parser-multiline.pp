# Test multi-line error messages.
#
#   Error: Could not parse for environment production: Unclosed quote after '' in 'something
#   }
#   ' at parser_err3.pp:2
#
class {'parser_err3':
  hello => 'something
}
