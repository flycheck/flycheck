# An image with as many of Flycheck's checkers installed as is practical,
# for recording what they print.  See test/record-fixture.el.
#
# Ubuntu rather than anything smaller, and the same release the CI runners
# use, so what is recorded here is what the weekly job meets when it asks
# whether the checkers can still read their tools.

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    PATH=/usr/local/go/bin:/root/go/bin:/root/.cargo/bin:$PATH

# Emacs runs the recorder; the rest is what the checkers need to exist at
# all.  Kept in one layer per ecosystem so a change to one does not
# rebuild the others.
RUN apt-get update && apt-get install --yes --no-install-recommends \
      ca-certificates curl git make unzip xz-utils \
      emacs-nox \
      build-essential clang cppcheck gcc gfortran gnat \
      cfengine3 chicken-bin ghdl groovy \
      llvm luarocks php-cli protobuf-compiler puppet \
      python3 python3-pip python3-venv \
      r-base-core racket ruby ruby-dev \
      nodejs npm \
      rpmlint shellcheck systemd tcl tidy \
      texinfo texlive-extra-utils chktex \
      verilator \
      gawk jq lua5.4 xmlstarlet libxml2-utils \
      erlang-base erlang-dev rebar3 elixir \
      ocaml ocaml-findlib ocaml-dune opam \
      lacheck \
      dash zsh \
    && rm -rf /var/lib/apt/lists/*

# The Node checkers.  --no-fund and --no-audit keep the log readable.
RUN npm install --global --no-fund --no-audit \
      coffeescript \
      ember-template-lint \
      eslint \
      handlebars \
      js-yaml \
      less \
      markdownlint-cli markdownlint-cli2 \
      oxlint \
      pug-cli \
      pyright \
      standard \
      stylelint stylelint-config-standard \
      textlint \
    && npm cache clean --force

# The Python checkers.  A virtualenv rather than --break-system-packages,
# and on PATH, so `python3 -m flake8' finds them the way the checkers ask.
RUN python3 -m venv /opt/venv \
    && /opt/venv/bin/pip install --no-cache-dir --upgrade pip \
    && /opt/venv/bin/pip install --no-cache-dir \
         docutils \
         flake8 \
         mypy \
         proselint \
         pylint \
         pymarkdownlnt \
         ruff \
         salt-lint \
         schema-salad \
         sphinx \
         yamllint
ENV PATH=/opt/venv/bin:$PATH

# The Ruby checkers.
RUN gem install --no-document \
      asciidoctor \
      cookstyle \
      haml haml_lint \
      mdl \
      reek \
      rubocop \
      slim_lint \
      sqlint \
      standard \
      puppet-lint

# Ubuntu's Go is a release behind what some of these need to build, so
# take it from upstream.  The version is whatever is current: this image
# is for recording, not for shipping, and a checker's output does not
# depend on the Go that built its linter.
RUN arch="$(dpkg --print-architecture)" \
    && version="$(curl -fsSL 'https://go.dev/VERSION?m=text' | head -1)" \
    && curl -fsSL "https://go.dev/dl/${version}.linux-${arch}.tar.gz" \
      -o /tmp/go.tar.gz \
    && tar -C /usr/local -xzf /tmp/go.tar.gz && rm /tmp/go.tar.gz \
    && go version

# The Go checkers, which are all `go install'.
RUN go install github.com/kisielk/errcheck@latest \
    && go install honnef.co/go/tools/cmd/staticcheck@latest \
    && go install github.com/mdempsky/unconvert@latest \
    && go install github.com/bazelbuild/buildtools/buildifier@latest \
    && go install github.com/google/go-jsonnet/cmd/jsonnet@latest \
    && go install github.com/rhysd/actionlint/cmd/actionlint@latest \
    && rm -rf /root/.cache/go-build

# hadolint ships as a single binary, named for the architecture the way
# hadolint spells it rather than the way uname does.
RUN case "$(dpkg --print-architecture)" in \
      amd64) arch=x86_64 ;; \
      arm64) arch=arm64 ;; \
      *) echo "no hadolint for $(dpkg --print-architecture)" >&2; exit 1 ;; \
    esac \
    && curl -fsSL "https://github.com/hadolint/hadolint/releases/latest/download/hadolint-Linux-${arch}" \
      -o /usr/local/bin/hadolint \
    && chmod +x /usr/local/bin/hadolint

# The Perl checkers.
RUN cpan -T Perl::Critic App::perlimports 2>&1 | tail -5 \
    && rm -rf /root/.cpan

# luacheck comes from luarocks rather than apt.
RUN luarocks install luacheck && rm -rf /root/.cache/luarocks

# Rust, for the cargo-based checkers.
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
      | sh -s -- -y --profile minimal --component clippy

# Terraform and tflint, which ship as release archives per architecture.
RUN arch="$(dpkg --print-architecture)" \
    && curl -fsSL "https://releases.hashicorp.com/terraform/1.9.8/terraform_1.9.8_linux_${arch}.zip" \
      -o /tmp/tf.zip \
    && unzip -q /tmp/tf.zip -d /usr/local/bin && rm /tmp/tf.zip \
    && curl -fsSL https://raw.githubusercontent.com/terraform-linters/tflint/master/install_linux.sh \
      | bash

# Deliberately not here:
#
#   cuda-nvcc, processing  need toolchains far larger than the rest of
#                          this image put together
#   d-dmd, hlint, stack    each pull in a whole language toolchain for
#                          one checker; add them if the recordings are
#                          worth the gigabytes
#   nix, statix            nix wants a daemon or a single-user install
#                          that fights with the image
#   nagelfar               not packaged anywhere; a Tcl script to vendor
#   scala, scalastyle      the JVM plus sbt, for two checkers
#   phpcs, phpmd           composer, which wants a project to install into

WORKDIR /flycheck

# Recording writes into the mounted repository, so run it as the user who
# owns the checkout rather than leaving root-owned files behind:
#
#   docker run --rm -v "$PWD":/flycheck -u "$(id -u):$(id -g)" flycheck-checkers
# `load-prefer-newer' because a checkout that was compiled on the host
# carries .elc files this Emacs may not be able to read
CMD ["emacs", "-Q", "--batch", "--eval", "(setq load-prefer-newer t)", \
     "-L", ".", "-l", "test/record-fixture.el", \
     "-f", "flycheck-record-fixture-all-batch"]
