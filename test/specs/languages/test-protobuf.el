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

)

;;; test-protobuf.el ends here
