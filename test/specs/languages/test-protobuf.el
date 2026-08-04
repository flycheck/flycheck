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

  (describe "the protobuf-protoc checker command"
    (it "appends flycheck-protoc-args before the source file"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (make-temp-file "flycheck-proto" nil ".proto"))
        (let ((flycheck-protoc-import-path nil)
              (flycheck-protoc-args '("--fatal_warnings")))
          (expect (flycheck-checker-substituted-arguments 'protobuf-protoc)
                  :to-contain "--fatal_warnings")))))



  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test protobuf-protoc "language/protobuf/protobuf/syntax-error.proto"
      '(2 23 error "Missing field number."))))

;;; test-protobuf.el ends here
