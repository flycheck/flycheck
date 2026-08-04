;;; test-dockerfile.el --- Flycheck Specs: Dockerfile -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Dockerfile"
  (flycheck-buttercup-def-checker-test dockerfile-hadolint dockerfile error
    (flycheck-buttercup-should-syntax-check
     "language/dockerfile/Dockerfile.error" 'dockerfile-mode
     '(2 nil error "unexpected 'I'
expecting '#', '\\', ADD, ARG, CMD, COPY, ENTRYPOINT, ENV, EXPOSE, FROM, HEALTHCHECK, LABEL, MAINTAINER, ONBUILD, RUN, SHELL, STOPSIGNAL, USER, VOLUME, WORKDIR, a pragma, at least one space, or end of input"
         :id "DL1000" :checker dockerfile-hadolint)))

  (flycheck-buttercup-def-checker-test dockerfile-hadolint dockerfile warnings
    (flycheck-buttercup-should-syntax-check
     "language/dockerfile/Dockerfile.warning" 'dockerfile-mode
     '(1 nil warning "Always tag the version of an image explicitly"
         :id "DL3006" :checker dockerfile-hadolint)
     '(2 nil info "Delete the apt lists (/var/lib/apt/lists) after installing something"
         :id "DL3009" :checker dockerfile-hadolint)
     '(3 nil error "Use absolute WORKDIR"
         :id "DL3000" :checker dockerfile-hadolint))))

  (describe "the dockerfile-hadolint checker command"
    (it "adds no config or extra arguments by default"
      (let ((flycheck-dockerfile-hadolint-config nil)
            (flycheck-dockerfile-hadolint-args nil))
        (expect (flycheck-checker-substituted-arguments 'dockerfile-hadolint)
                :to-equal '("--format" "sarif" "-"))))

    (it "appends flycheck-dockerfile-hadolint-args before the stdin marker"
      (let ((flycheck-dockerfile-hadolint-config nil)
            (flycheck-dockerfile-hadolint-args '("--no-fail")))
        (expect (flycheck-checker-substituted-arguments 'dockerfile-hadolint)
                :to-equal '("--format" "sarif" "--no-fail" "-"))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test dockerfile-hadolint "language/dockerfile/Dockerfile.error"
      '(2 nil error "unexpected 'I'\nexpecting '#', '\\', ADD, ARG, CMD, COPY, ENTRYPOINT, ENV, EXPOSE, FROM, HEALTHCHECK, LABEL, MAINTAINER, ONBUILD, RUN, SHELL, STOPSIGNAL, USER, VOLUME, WORKDIR, a pragma, at least one space, or end of input" :id "DL1000"))))

;;; test-dockerfile.el ends here
