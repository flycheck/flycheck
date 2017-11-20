# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure("2") do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.
  config.vm.box = "terrywang/archlinux"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  config.vm.provider "virtualbox" do |vb|
    # Need memory to compile Coq!
    vb.memory = 2048
    vb.cpus = 2
  end
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push strategies
  # such as FTP and Heroku are also available. See the documentation at
  # https://docs.vagrantup.com/v2/push/atlas.html for more information.
  # config.push.define "atlas" do |push|
  #   push.app = "YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.

  # Make sure keys are up to date before upgrading packages, otherwise PGP
  # signatures will fail to check
  config.vm.provision "upgrade",
                      type: "shell",
                      privileged: false,
                      run: "always",
                      inline: <<-SHELL
    sudo pacman --noconfirm -Sy
    sudo pacman --noconfirm --needed -S archlinux-keyring
    sudo pacman --noconfirm -Su
  SHELL

  # Get the latest flycheck master
  config.vm.provision "flycheck",
                      type: "shell",
                      privileged: false,
                      run: "always",
                      inline: <<-SHELL
    if [ ! -d flycheck ]; then
      git clone --depth=1 https://github.com/flycheck/flycheck.git
    else
      pushd flycheck
      git pull
    fi
  SHELL

  # The additional packages needed to run the Flycheck integration tests
  config.vm.provision "flycheck-deps",
                      type: "shell",
                      privileged: false,
                      inline: <<-SHELL
    echo "Installing packages with pacman..."
    sudo pacman --noconfirm --needed -S npm \
                                        jdk8-openjdk \
                                        gcc-ada \
                                        asciidoc \
                                        asciidoctor \
                                        clang \
                                        cppcheck \
                                        coffee-script \
                                        coq \
                                        dmd \
                                        erlang \
                                        gcc-fortran \
                                        go \
                                        groovy \
                                        ghc \
                                        haskell-hlint \
                                        stack \
                                        tidy \
                                        perl \
                                        cpanminus \
                                        php \
                                        processing \
                                        protobuf \
                                        puppet \
                                        flake8 \
                                        python-pip \
                                        python-pylint \
                                        racket-minimal \
                                        python-docutils \
                                        python-sphinx \
                                        r \
                                        jruby \
                                        rustup \
                                        scala \
                                        chicken \
                                        shellcheck \
                                        xmlstarlet
    echo "Installing packages from AUR..."
    yaourt --noconfirm --needed -S cask \
                                   cfengine \
                                   luacheck \
                                   nix \
                                   phpmd \
                                   php-pear \
                                   scalastyle
                                   # verilator

    echo "Installing packages with gem..."
    gem install --no-document foodcritic \
                              haml \
                              mdl \
                              puppet-lint \
                              reek \
                              rubocop \
                              ruby-lint \
                              sass \
                              scss_lint \
                              scss_lint_reporter_checkstyle \
                              slim \
                              sqlint

    echo "Installing packages from npm..."
    npm install --global --prefix ~/.node_modules coffeelint \
                                                  csslint \
                                                  handlebars \
                                                  jshint \
                                                  eslint \
                                                  jscs \
                                                  standard \
                                                  semistandard \
                                                  jsonlint \
                                                  less \
                                                  pug-cli \
                                                  tslint \
                                                  typescript \
                                                  js-yaml

    echo "Installing packages with pip..."
    pip install --user schema-salad

    echo "Installing packages with go get..."
    go get -u github.com/kisielk/errcheck \
              github.com/mdempsky/unconvert \
              github.com/golang/lint/golint \
              honnef.co/go/tools/cmd/megacheck

    echo "Installing packages with raco pkg..."
    raco pkg install --auto --skip-installed compiler-lib

    echo "Installing rust with rustup..."
    rustup install stable
    rustup default stable

    echo "Installing GHC for stack..."
    stack setup

    echo "Installing packages from CPAN..."
    sudo cpanm Perl::Critic
    sudo chmod -R a+rX /usr/share/perl5 /usr/lib/perl5

    echo "Installing packages from PEAR..."
    sudo pear install PHP_CodeSniffer
    echo 'include_path=".:/usr/share/pear/"' | sudo tee /etc/php/conf.d/pear.ini
    echo 'extension=iconv.so' | sudo tee /etc/php/conf.d/iconv.ini
    sudo chmod -R a+rX /usr/share/pear/PHP /etc/php/conf.d

    echo "Installing packages from CRAN..."
    export R_LIBS_USER=~/R
    cat <<-EOF > install-packages.R
      dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
      install.packages("lintr", Sys.getenv("R_LIBS_USER"), repos="http://cran.case.edu")
EOF
    R CMD BATCH install-packages.R

    echo "Installing hadolint..."
    if [ ! -d hadolint ]; then
      git clone --depth=1 https://github.com/lukasmartinelli/hadolint
    else
      pushd hadolint
      git pull
      popd
    fi
    pushd hadolint
    stack setup
    stack install
    popd
  SHELL

  # Add binaries from ad-hoc package managers to PATH
  config.vm.provision "path-setup",
                      type: "shell",
                      privileged: false,
                      inline: <<-SHELL
    echo export PATH=$(ruby -e 'print Gem.user_dir')/bin:'$PATH' >> ~/.profile
    echo export PATH=$HOME/.node_modules/bin:'$PATH' >> ~/.profile
    echo export PATH=$HOME/go/bin:'$PATH' >> ~/.profile
    echo export PATH=$HOME/.local/bin:'$PATH' >> ~/.profile
    echo export R_LIBS_USER=$HOME/R >> ~/.profile
  SHELL

end
