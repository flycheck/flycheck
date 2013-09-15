# Use the builtin "map()"

class Spam(object):

    def with_eggs(self, n):
        return map(str, range(n))
