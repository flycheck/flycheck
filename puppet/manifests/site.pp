# The main manifest for test provisioning

include flycheck::base
include flycheck::emacs

# Do not install Gems on Travis
if $::travis {
  notice('Skipping syntax checker tools on Travis CI')
}
else {
  include flycheck::checkers
}
