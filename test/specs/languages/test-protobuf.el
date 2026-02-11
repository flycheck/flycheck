;;; test-protobuf.el --- Flycheck Specs: Protobuf -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Protobuf"
  (flycheck-buttercup-def-checker-test protobuf-protoc protobuf syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/protobuf/protobuf/syntax-error.proto" 'protobuf-mode
     '(2 23 error "Missing field number."
         :checker protobuf-protoc)))

  (flycheck-buttercup-def-checker-test protobuf-prototool protobuf warnings
    (flycheck-buttercup-should-syntax-check
     "language/protobuf/prototool/missing_syntax.proto" 'protobuf-mode
     '(1 1 warning "No syntax specified. Please use 'syntax = \"proto2\";' or 'syntax = \"proto3\";' to specify a syntax version."
         :checker protobuf-prototool)
     '(4 3 warning "Expected \"required\", \"optional\", or \"repeated\"."
         :checker protobuf-prototool))))

;;; test-protobuf.el ends here
