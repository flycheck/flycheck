;;; test-dockerfile.el --- Flycheck Specs: Dockerfile -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Dockerfile"
  (flycheck-buttercup-def-checker-test dockerfile-hadolint dockerfile error
    (flycheck-buttercup-should-syntax-check
     "language/dockerfile/Dockerfile.error" 'dockerfile-mode
     '(2 1 error "unexpected 'I' expecting '#', '\\', ADD, ARG, CMD, COPY, ENTRYPOINT, ENV, EXPOSE, FROM, HEALTHCHECK, LABEL, MAINTAINER, ONBUILD, RUN, SHELL, STOPSIGNAL, USER, VOLUME, WORKDIR, at least one space, or end of input"
         :checker dockerfile-hadolint)))

  (flycheck-buttercup-def-checker-test dockerfile-hadolint dockerfile warnings
    (flycheck-buttercup-should-syntax-check
     "language/dockerfile/Dockerfile.warning" 'dockerfile-mode
     '(1 nil warning "Always tag the version of an image explicitly"
         :id "DL3006" :checker dockerfile-hadolint)
     '(2 nil error "Do not use apt-get upgrade or dist-upgrade"
         :id "DL3005" :checker dockerfile-hadolint)
     '(2 nil info "Delete the apt-get lists after installing something"
         :id "DL3009" :checker dockerfile-hadolint)
     '(3 nil error "Use absolute WORKDIR"
         :id "DL3000" :checker dockerfile-hadolint))))

;;; test-dockerfile.el ends here
