# Trigger a McCabe complexity warning

import sys


def foo(spam=None):
    if spam:
        if sys.python_version[0] > 3:
            print('yes')
        else:
            print('no')
    else:
        return 'foo'
