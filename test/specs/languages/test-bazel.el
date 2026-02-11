;;; test-bazel.el --- Flycheck Specs: Bazel -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Bazel"
  (flycheck-buttercup-def-checker-test bazel-build-buildifier bazel error
    (flycheck-buttercup-should-syntax-check
     "language/bazel/BUILD.bazel-error" 'bazel-build-mode
     '(1 11 error "syntax error near !" :checker bazel-build-buildifier)))

  (flycheck-buttercup-def-checker-test bazel-build-buildifier bazel nil
    (flycheck-buttercup-should-syntax-check
     "language/bazel/BUILD.bazel-warning" 'bazel-build-mode
     '(1 nil warning "Variable \"foo\" is unused. Please remove it. (https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md#unused-variable)" :id "unused-variable" :checker bazel-build-buildifier)))

  (flycheck-buttercup-def-checker-test bazel-module-buildifier bazel error
    (flycheck-buttercup-should-syntax-check
     "language/bazel/MODULE.bazel-error" 'bazel-module-mode
     '(1 11 error "syntax error near !" :checker bazel-module-buildifier)))

  (flycheck-buttercup-def-checker-test bazel-module-buildifier bazel nil
    (flycheck-buttercup-should-syntax-check
     "language/bazel/MODULE.bazel-warning" 'bazel-module-mode
     '(1 nil warning "The file has no module docstring." :id "module-docstring" :checker bazel-module-buildifier)))

  (flycheck-buttercup-def-checker-test bazel-starlark-buildifier bazel error
    (flycheck-buttercup-should-syntax-check
     "language/bazel/rules.bzl-error" 'bazel-starlark-mode
     '(1 11 error "syntax error near !" :checker bazel-starlark-buildifier)))

  (flycheck-buttercup-def-checker-test bazel-starlark-buildifier bazel nil
    (flycheck-buttercup-should-syntax-check
     "language/bazel/rules.bzl-warning" 'bazel-starlark-mode
     '(1 nil warning "The file has no module docstring." :id "module-docstring" :checker bazel-starlark-buildifier)))

  (flycheck-buttercup-def-checker-test bazel-workspace-buildifier bazel error
    (flycheck-buttercup-should-syntax-check
     "language/bazel/WORKSPACE.bazel-error" 'bazel-workspace-mode
     '(1 11 error "syntax error near !" :checker bazel-workspace-buildifier)))

  (flycheck-buttercup-def-checker-test bazel-workspace-buildifier bazel nil
    (flycheck-buttercup-should-syntax-check
     "language/bazel/WORKSPACE.bazel-warning" 'bazel-workspace-mode
     '(1 nil warning "Variable \"foo\" is unused. Please remove it. (https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md#unused-variable)" :id "unused-variable" :checker bazel-workspace-buildifier))))

;;; test-bazel.el ends here
