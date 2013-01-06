# Redefine an unused function
#
# Checkers: Flake8


if 5 < 10:
    def foo():
        pass
else:
    def foo():
        pass
