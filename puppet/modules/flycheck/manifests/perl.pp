# Class: flycheck::perl
#
# Install Perl
class flycheck::perl {

  # For core tools
  require flycheck::base

  include ::perl
}
