# Class: flycheck::checkers::perl
#
# Install syntax checkers from CPAN
class flycheck::checkers::perl {
  require flycheck::perl

  $perl_packages = ['Perl::Critic' # perl-perlcritic
                    ]

  perl::module { $perl_packages: }
}
