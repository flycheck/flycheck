FROM larsbrinkhoff/emacsen

RUN mkdir -p $HOME/bin \
  && export PATH="$HOME/bin:$PATH" \
  && echo "exec \"/usr/local/emacs-\$EMACS_VERSION/bin/emacs\" \"\$@\"" > $HOME/bin/emacs

RUN apt-get update \
  && apt-get install --yes git

RUN git clone --depth=1 https://github.com/cask/cask.git "$HOME/.cask" \
  && ln -s "$HOME/.cask/bin/cask" "$HOME/bin/cask"

RUN chmod a+x $HOME/bin/emacs

ENV HOME /root
ENV PATH $HOME/bin:$PATH
RUN echo "#!/bin/bash\n\"/usr/local/emacs-\$EMACS_VERSION/bin/emacs\" \"\$@\"" > $HOME/bin/emacs

RUN apt-get install --yes python3 \
  && ln -s /usr/bin/python3 /usr/bin/python

RUN apt-get install --yes make
