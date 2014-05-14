#!/bin/bash

# Print commands (-v) and fail on errors (-e)
set -v

get() {
    curl -fsSL "$@"
}

apt_repos=(
    'ppa:cassou/emacs'          # Stable Emacs 24.3
    'ppa:ubuntu-elisp/ppa'      # Nightly Emacs trunk builds
    'ppa:hansjorg/rust'         # Nightly Rust builds
    'ppa:ondrej/php5'           # Recent PHP versions
    'ppa:plt/racket'            # Racket
    'ppa:hvr/ghc'               # Latest GHC and Cabal
)

for repo in "${apt_repos[@]}"; do
    sudo add-apt-repository -y "${repo}"
done

# Erlang
echo 'deb http://packages.erlang-solutions.com/debian precise contrib' |
    sudo tee -a /etc/apt/sources.list.d/erlang-solutions.list
get 'http://packages.erlang-solutions.com/debian/erlang_solutions.asc' |
    sudo apt-key add -

# For CFEngine
echo 'deb http://cfengine.com/pub/apt/packages stable main' |
    sudo tee -a /etc/apt/sources.list.d/cfengine-community.list
get 'http://cfengine.com/pub/gpg.key' | sudo apt-key add -

sudo apt-get update

apt_pkgs=(
    "${EMACS}-nox"              # The right Emacs for our testing env
    # Languages and package managers
    cpanminus                   # For Perl packages
    php-pear                    # For PHP packages
    python2.7 python-pip        # For Python packages
    erlang                      # For Erlang
    # Various dependencies
    git-core mercurial          # For go get
    libxslt1-dev libxml2-dev    # For food-critic
    gcc-multilib xdg-utils      # For dmd
    # Syntax checkers
    asciidoc                    # asciidoc
    cfengine-community          # cfengine
    chktex                      # tex-chktex
    cppcheck                    # c/c++-cppcheck
    dash                        # sh-posix-dash
    ghc-"${GHC_VERSION}"        # haskell-ghc
    hlint                       # haskell-hlint
    lacheck                     # tex-lacheck
    libperl-critic-perl         # perl-perlcritic
    libxml2-utils               # xml-xmllint
    lua5.2                      # lua
    perl                        # perl
    php5-cli                    # php
    pmake                       # make
    racket                      # racket
    rust-nightly                # rust
    texinfo                     # texinfo
    tidy                        # html-tidy
    verilator                   # verilog-verilator
    xmlstarlet                  # xml-xmlstarlet
    zsh                         # sh-zsh
)

sudo apt-get install --no-install-recommends -yy "${apt_pkgs[@]}"

dmd_url='http://downloads.dlang.org/releases/2014/dmd_2.065.0-0_amd64.deb'
get -o "/tmp/dmd.deb" "${dmd_url}"
sudo dpkg -i "/tmp/dmd.deb"

# PHP packages
php_channels=(
    'pear.phpmd.org' 'pear.pdepend.org' # For PHP_PMD
)

for channel in "${php_channels[@]}"; do
    sudo pear channel-discover "${channel}"
done

php_pkgs=(
    'PHP_CodeSniffer'           # php-phpcs
    'phpmd/PHP_PMD'             # php-phpmd
)

for pkg in "${php_pkgs[@]}"; do
    sudo pear install "${pkg}"
done

pip_pkgs=(
    docutils                    # rst
    flake8 pep8-naming          # python-flake8
    pylint                      # python-pylint
    sphinx                      # rst-sphinx
)

pip install --user "${pip_pkgs[@]}"
# For javascript-gjslint
pip install --user 'http://closure-linter.googlecode.com/files/closure_linter-latest.tar.gz'

# Perl packages
sudo apt-get remove libperl-critic-perl # Remove outdated Perl::Critic
sudo cpanm --notest 'Perl::Critic' # Re-install the latest version of Perl::Critic

# Javascript packages
node_pkgs=(
  coffee-script                 # coffee
  coffeelint                    # coffee-coffeelint
  csslint                       # css-csslint
  eslint                        # javascript-eslint
  handlebars                    # handlebars
  js-yaml                       # yaml-jsyaml
  jshint                        # javascript-jshint
  jsonlint                      # json-jsonlint
  less                          # less
)

npm install --global "${node_pkgs[@]}"

# Ruby packages
gems=(
    erubis                      # eruby-erubis
    foodcritic                  # chef-foodcritic
    haml                        # haml
    puppet                      # puppet-parser
    puppet-lint                 # puppet-lint
    rubocop                     # ruby-rubocop
    ruby-lint                   # ruby-rubylint
    sass                        # sass
    slim                        # slim
)
gem install --no-ri --no-rdoc "${gems[@]}"

# Go packages
go_pkgs=(
    'code.google.com/p/go.tools/cmd/vet' # go-vet
    'github.com/golang/lint/golint' # go-golint
)
go get "${go_pkgs[@]}"

# Elixir
elixir_url='https://github.com/elixir-lang/elixir/releases/download/v0.12.4/Precompiled.zip'
get -o '/tmp/elixir.zip' "${elixir_url}"
sudo unzip '/tmp/elixir.zip' -d '/opt/elixir'
sudo ln -sf '/opt/elixir/bin/elixirc' '/usr/local/bin/elixirc'

# Cask
cask_version='0.6.0'
cask_url="https://github.com/cask/cask/archive/v${cask_version}.tar.gz"
get -o "/tmp/cask.tar.gz" "${cask_url}"
sudo tar -xzf "/tmp/cask.tar.gz" -C "/opt"
sudo ln -sf "/opt/cask-${cask_version}/bin/cask" "/usr/local/bin/cask"
