# -*- coding: utf-8; -*-

import sys

from . import antigravit  # Typo intended!

class Spam(object):

    def withEggs(self, number_of_eggs):
        return map(str, range(number_of_eggs))

    def with_ham(self, blubb = None):
        if blubb:
            if sys.python_version[0] > 3:
                print('yes')
            else:
                print('no')
        else:
            return 'foo'


antigravity()
