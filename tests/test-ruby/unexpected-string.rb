# A syntax error caused by operator precedence.
# Taken from http://stackoverflow.com/q/8155219/355252

is_contain_customer = s=='helloCustomer' || s.include? 'Customer' ? 'YES' : 'NO'
