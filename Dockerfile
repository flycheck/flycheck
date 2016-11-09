FROM larsbrinkhoff/emacsen:latest

ENV HOME /root

RUN mkdir -p $HOME/bin \
  && echo "#!/bin/bash\n\"/usr/local/emacs-\$EMACS_VERSION/bin/emacs\" \"\$@\"" > $HOME/bin/emacs \
  && chmod a+x $HOME/bin/emacs

RUN apt-get update \
  && apt-get install --yes git make python3 \
  && ln -s /usr/bin/python3 /usr/bin/python

RUN git clone --depth=1 https://github.com/cask/cask.git "$HOME/.cask" \
  && ln -s "$HOME/.cask/bin/cask" "$HOME/bin/cask"

ENV PATH $HOME/bin:$PATH
