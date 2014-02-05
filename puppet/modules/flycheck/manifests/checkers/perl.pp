# Class: flycheck::checkers::perl
#
# Install syntax checkers from CPAN
class flycheck::checkers::perl {
  include ::perl

  $perl_packages = ['Perl::Critic' # perl-perlcritic
                    ]

  perl::module { $perl_packages: }
}
