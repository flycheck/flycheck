(package "flycheck"
         "33-cvs"
         "On-the-fly syntax checking")

(website-url "http://www.flycheck.org")
(keywords "convenience" "languages" "tools")

(package-file "flycheck.el")

(files "flycheck-ert.el" "flycheck-buttercup.el")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "26.1")
(depends-on "dash")

(development
 (depends-on "f")                       ; For some maintenance tools
 (depends-on "buttercup")               ; BDD test framework for Emacs
 (depends-on "shut-up")                 ; Silence Emacs

 ;; Various modes for use in the unit tests
 (depends-on "adoc-mode")
 (depends-on "bazel")
 (depends-on "coffee-mode")
 (depends-on "cperl-mode")
 (depends-on "cwl-mode")
 (depends-on "d-mode")
 (depends-on "dockerfile-mode")
 (depends-on "elixir-mode")
 (depends-on "erlang")
 (depends-on "ess")
 (depends-on "geiser")
 (depends-on "go-mode")
 (depends-on "groovy-mode")
 (depends-on "haml-mode")
 (depends-on "handlebars-mode")
 (depends-on "haskell-mode")
 (depends-on "js2-mode")
 (depends-on "js3-mode")
 (depends-on "rjsx-mode")
 (depends-on "json-mode")
 (depends-on "julia-mode")
 (depends-on "less-css-mode")
 (depends-on "lua-mode")
 (depends-on "markdown-mode")
 (depends-on "mmm-mode")
 (depends-on "nix-mode")
 (depends-on "php-mode")
 (depends-on "processing-mode")
 (depends-on "protobuf-mode")
 (depends-on "pug-mode")
 (depends-on "puppet-mode")
 ;; (depends-on "racket-mode") ;; https://github.com/greghendershott/racket-mode/issues/461
 (depends-on "rhtml-mode")
 (depends-on "rpm-spec-mode")
 (depends-on "rust-mode")
 (depends-on "sass-mode")
 (depends-on "scala-mode")
 (depends-on "scss-mode")
 (depends-on "slim-mode")
 (depends-on "systemd")
 (depends-on "terraform-mode")
 (depends-on "tuareg")
 (depends-on "typescript-mode")
 (depends-on "web-mode")
 (depends-on "yaml-mode"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432