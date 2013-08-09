# Install all Ruby Gems.  Intended for Travis CI, where we need to install Gems
# as normal user to not mess up with RVM.

include flycheck::checkers::gems
