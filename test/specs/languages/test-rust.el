;;; test-rust.el --- Flycheck Specs: Rust      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Specs for Rust support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Rust"
  (flycheck-buttercup-def-checker-test rust-cargo rust warning
    (let ((flycheck-disabled-checkers '(rust))
          (flycheck-rust-crate-type "bin")
          (flycheck-rust-binary-name "flycheck-test"))
      (flycheck-buttercup-cargo-clean "language/rust/flycheck-test/Cargo.toml")
      (flycheck-buttercup-should-syntax-check
       "language/rust/flycheck-test/src/warnings.rs" 'rust-mode
       '(3 4 warning "function is never used: `main`"
           :checker rust-cargo :id "dead_code" :group 1
           :end-line 3 :end-column 8)
       '(3 4 info "`#[warn(dead_code)]` on by default"
           :checker rust-cargo :id "dead_code" :group 1
           :end-line 3 :end-column 8)
       '(4 9 warning "unused variable: `x`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 10)
       '(4 9 info "if this is intentional, prefix it with an underscore: `_x`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 10))))

  (flycheck-buttercup-def-checker-test rust-cargo rust default-target
    (let ((flycheck-disabled-checkers '(rust))
          (flycheck-rust-crate-type nil)
          (flycheck-rust-binary-name nil))
      (flycheck-buttercup-cargo-clean "language/rust/flycheck-test/Cargo.toml")
      (flycheck-buttercup-should-syntax-check
       "language/rust/flycheck-test/src/warnings.rs" 'rust-mode
       '(3 4 warning "function is never used: `main`"
           :checker rust-cargo :id "dead_code" :group 1
           :end-line 3 :end-column 8)
       '(3 4 info "`#[warn(dead_code)]` on by default"
           :checker rust-cargo :id "dead_code" :group 1
           :end-line 3 :end-column 8)
       '(4 9 warning "unused variable: `x`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 10)
       '(4 9 info "if this is intentional, prefix it with an underscore: `_x`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 4 :end-column 10))))

  (flycheck-buttercup-def-checker-test rust-cargo rust lib-main
    (let ((flycheck-disabled-checkers '(rust))
          (flycheck-rust-crate-type "bin")
          (flycheck-rust-binary-name "lib-main"))
      (flycheck-buttercup-should-syntax-check
       "language/rust/lib-main/src/main.rs" 'rust-mode
       `(3 12 error "cannot find value `zorglub` in this scope (not found in this scope)"
           :checker rust-cargo :id "E0425"
           :filename ,(flycheck-buttercup-resource-filename "language/rust/lib-main/src/lib.rs")
           :group 1
           :end-line 3 :end-column 19))))

  (flycheck-buttercup-def-checker-test rust-cargo rust conventional-layout
    (let ((flycheck-disabled-checkers '(rust)))
      (let ((flycheck-rust-crate-type "lib"))
        (flycheck-buttercup-cargo-clean "language/rust/cargo-targets/Cargo.toml")
        (flycheck-buttercup-should-syntax-check
         "language/rust/cargo-targets/src/lib.rs" 'rust-mode
         '(3 4 warning "function is never used: `foo_lib`"
             :checker rust-cargo :id "dead_code" :group 1
             :end-line 3 :end-column 11)
         '(6 17 warning "unused variable: `foo_lib_test`"
             :checker rust-cargo  :id "unused_variables" :group 2
             :end-line 6 :end-column 29)
         '(6 17 info "`#[warn(unused_variables)]` on by default"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 6 :end-column 29)
         '(6 17 info "if this is intentional, prefix it with an underscore: `_foo_lib_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 6 :end-column 29)))

      (let ((flycheck-rust-crate-type "lib"))
        (flycheck-buttercup-cargo-clean "language/rust/cargo-targets/Cargo.toml")
        (flycheck-buttercup-should-syntax-check
         "language/rust/cargo-targets/src/a.rs" 'rust-mode
         '(1 4 warning "function is never used: `foo_a`"
             :checker rust-cargo :id "dead_code" :group 1
             :end-line 1 :end-column 9)
         '(1 4 info "`#[warn(dead_code)]` on by default"
             :checker rust-cargo :id "dead_code" :group 1
             :end-line 1 :end-column 9)
         '(4 17 warning "unused variable: `foo_a_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 27)
         '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_a_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 27)))

      (let ((flycheck-rust-crate-type "bin")
            (flycheck-rust-binary-name "cargo-targets"))
        (flycheck-buttercup-cargo-clean "language/rust/cargo-targets/Cargo.toml")
        (flycheck-buttercup-should-syntax-check
         "language/rust/cargo-targets/src/main.rs" 'rust-mode
         '(1 17 warning "unused variable: `foo_main`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 25)
         '(1 17 info "`#[warn(unused_variables)]` on by default"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 25)
         '(1 17 info "if this is intentional, prefix it with an underscore: `_foo_main`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 25)
         '(4 17 warning "unused variable: `foo_main_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 30)
         '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_main_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 30)))

      (let ((flycheck-rust-crate-type "bin")
            (flycheck-rust-binary-name "a"))
        (flycheck-buttercup-cargo-clean "language/rust/cargo-targets/Cargo.toml")
        (flycheck-buttercup-should-syntax-check
         "language/rust/cargo-targets/src/bin/a.rs" 'rust-mode
         '(1 17 warning "unused variable: `foo_bin_a`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 26)
         '(1 17 info "`#[warn(unused_variables)]` on by default"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 26)
         '(1 17 info "if this is intentional, prefix it with an underscore: `_foo_bin_a`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 26)
         '(4 17 warning "unused variable: `foo_bin_a_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 31)
         '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_bin_a_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 31)))

      (let ((flycheck-rust-crate-type "bench")
            (flycheck-rust-binary-name "a"))
        (flycheck-buttercup-cargo-clean "language/rust/cargo-targets/Cargo.toml")
        (flycheck-buttercup-should-syntax-check
         "language/rust/cargo-targets/benches/a.rs" 'rust-mode
         '(1 17 warning "unused variable: `foo_bench_a`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 28)
         '(1 17 info "`#[warn(unused_variables)]` on by default"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 28)
         '(1 17 info "if this is intentional, prefix it with an underscore: `_foo_bench_a`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 28)
         '(4 17 warning "unused variable: `foo_bench_a_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 33)
         '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_bench_a_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 33)))

      (let ((flycheck-rust-crate-type "test")
            (flycheck-rust-binary-name "a"))
        (flycheck-buttercup-cargo-clean "language/rust/cargo-targets/Cargo.toml")
        (flycheck-buttercup-should-syntax-check
         "language/rust/cargo-targets/tests/a.rs" 'rust-mode
         '(2 16 warning "unused variable: `foo_test_a_test`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 2 :end-column 31)
         '(2 16 info "`#[warn(unused_variables)]` on by default"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 2 :end-column 31)
         '(2 16 info "if this is intentional, prefix it with an underscore: `_foo_test_a_test`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 2 :end-column 31)
         '(4 4 warning "function is never used: `foo_test_a`"
             :checker rust-cargo :id "dead_code" :group 2
             :end-line 4 :end-column 14)
         '(4 4 info "`#[warn(dead_code)]` on by default"
             :checker rust-cargo :id "dead_code" :group 2
             :end-line 4 :end-column 14)))

      (let ((flycheck-rust-crate-type "example")
            (flycheck-rust-binary-name "a"))
        (flycheck-buttercup-cargo-clean "language/rust/cargo-targets/Cargo.toml")
        (flycheck-buttercup-should-syntax-check
         "language/rust/cargo-targets/examples/a.rs" 'rust-mode
         '(1 17 warning "unused variable: `foo_ex_a`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 25)
         '(1 17 info "`#[warn(unused_variables)]` on by default"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 25)
         '(1 17 info "if this is intentional, prefix it with an underscore: `_foo_ex_a`"
             :checker rust-cargo :id "unused_variables" :group 1
             :end-line 1 :end-column 25)
         '(4 17 warning "unused variable: `foo_ex_a_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 30)
         '(4 17 info "if this is intentional, prefix it with an underscore: `_foo_ex_a_test`"
             :checker rust-cargo :id "unused_variables" :group 2
             :end-line 4 :end-column 30)))))

  (flycheck-buttercup-def-checker-test rust-cargo rust workspace-subcrate
    (let ((flycheck-disabled-checkers '(rust))
          (flycheck-rust-crate-type "lib")
          (flycheck-rust-check-tests t))
      (flycheck-buttercup-cargo-clean "language/rust/workspace/crate1/Cargo.toml")
      (flycheck-buttercup-should-syntax-check
       "language/rust/workspace/crate1/src/lib.rs" 'rust-mode
       '(2 7 warning "unused variable: `a`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 2 :end-column 8)
       '(2 7 info "`#[warn(unused_variables)]` on by default"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 2 :end-column 8)
       '(2 7 info "if this is intentional, prefix it with an underscore: `_a`"
           :checker rust-cargo :id "unused_variables" :group 1
           :end-line 2 :end-column 8))))

  (flycheck-buttercup-def-checker-test rust-cargo rust dev-dependencies
    (let ((flycheck-disabled-checkers '(rust))
          (flycheck-rust-crate-type "lib")
          (flycheck-rust-check-tests t))
      (flycheck-buttercup-cargo-clean "language/rust/dev-deps/Cargo.toml")
      (flycheck-buttercup-should-syntax-check
       "language/rust/dev-deps/src/lib.rs" 'rust-mode
       '(2 1 warning "unused `#[macro_use]` import"
           :checker rust-cargo :id "unused_imports" :group 1
           :end-line 2 :end-column 13)
       '(2 1 info "`#[warn(unused_imports)]` on by default"
           :checker rust-cargo :id "unused_imports" :group 1
           :end-line 2 :end-column 13)
       '(8 9 warning "unused variable: `foo`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 8 :end-column 12)
       '(8 9 info "`#[warn(unused_variables)]` on by default"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 8 :end-column 12)
       '(8 9 info "if this is intentional, prefix it with an underscore: `_foo`"
           :checker rust-cargo :id "unused_variables" :group 2
           :end-line 8 :end-column 12))))

  (flycheck-buttercup-def-checker-test rust rust syntax-error
    (let ((flycheck-disabled-checkers '(rust-cargo)))
      (flycheck-buttercup-should-syntax-check
       "language/rust/flycheck-test/src/syntax-error.rs" 'rust-mode
       '(4 5 error "cannot find value `bla` in this scope (not found in this scope)"
           :checker rust :id "E0425" :group 1 :end-line 4 :end-column 8))))

  (flycheck-buttercup-def-checker-test rust rust type-error
    (let ((flycheck-disabled-checkers '(rust-cargo)))
      (flycheck-buttercup-should-syntax-check
       "language/rust/flycheck-test/src/multiline-error.rs" 'rust-mode
       '(7 9 error "mismatched types (expected `u8`, found `i8`)"
           :checker rust :id "E0308" :group 1
           :end-line 7 :end-column 10)
       '(7 9 info "you can convert an `i8` to a `u8` and panic if the converted value doesn't fit: `i.try_into().unwrap()`"
           :checker rust :id "E0308" :group 1
           :end-line 7 :end-column 10))))

  (flycheck-buttercup-def-checker-test rust rust warning
    (let ((flycheck-disabled-checkers '(rust-cargo)))
      (flycheck-buttercup-should-syntax-check
       "language/rust/flycheck-test/src/warnings.rs" 'rust-mode
       '(4 9 warning "unused variable: `x`"
           :checker rust :id "unused_variables" :group 1
           :end-line 4 :end-column 10)
       '(4 9 info "`#[warn(unused_variables)]` on by default"
           :checker rust :id "unused_variables" :group 1
           :end-line 4 :end-column 10)
       '(4 9 info "if this is intentional, prefix it with an underscore: `_x`"
           :checker rust :id "unused_variables" :group 1
           :end-line 4 :end-column 10))))

  (flycheck-buttercup-def-checker-test rust rust note-and-help
    (let ((flycheck-disabled-checkers '(rust-cargo)))
      (flycheck-buttercup-should-syntax-check
       "language/rust/flycheck-test/src/note-and-help.rs" 'rust-mode
       '(10 9 info "move occurs because `_x` has type `NonPOD`, which does not implement the `Copy` trait"
            :checker rust :id "E0382" :group 1
            :end-line 10 :end-column 11)
       '(11 14 info "value moved here"
            :checker rust :id "E0382" :group 1
            :end-line 11 :end-column 16)
       '(12 14 error "use of moved value: `_x` (value used here after move)"
            :checker rust :id "E0382" :group 1
            :end-line 12 :end-column 16))))

  (flycheck-buttercup-def-checker-test rust rust crate-root-not-set
    (let ((flycheck-disabled-checkers '(rust-cargo)))
      (flycheck-buttercup-should-syntax-check
       "language/rust/flycheck-test/src/importing.rs" 'rust-mode
       '(1 5 error "failed to resolve: there are too many leading `super` keywords (there are too many leading `super` keywords)"
           :checker rust :id "E0433" :group 2
           :end-line 1 :end-column 10)
       '(4 24 error "failed to resolve: use of undeclared crate or module `imported` (use of undeclared crate or module `imported`)"
           :checker rust :id "E0433" :group 3
           :end-line 4 :end-column 32))))

  (flycheck-buttercup-def-checker-test rust rust macro-error
    (let ((flycheck-disabled-checkers '(rust-cargo)))
      (flycheck-buttercup-should-syntax-check
       "language/rust/flycheck-test/src/macro-error.rs" 'rust-mode
       '(2 13 error "1 positional argument in format string, but no arguments were given"
           :checker rust :group 1
           :end-line 2 :end-column 15)))))

;;; test-rust.el ends here
