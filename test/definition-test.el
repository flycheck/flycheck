;;; definition-test.el --- Tests for syntax checker definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

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

;; Tests for definitions.

;;; Code:

(require 'test-helper)

(ert-deftest flycheck-command-argument-p/with-symbols ()
  (--each '(source source-inplace source-original
                   temporary-directory temporary-file-name)
    (should (flycheck-command-argument-p it))))

(ert-deftest flycheck-command-argument-p/config-file-with-variable-symbol ()
  (should (flycheck-command-argument-p '(config-file "foo" bar))))

(ert-deftest flycheck-command-argument-p/config-file-with-quoted-variable-symbol ()
  (should-not (flycheck-command-argument-p '(config-file "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/config-file-without-variable-symbol ()
  (should-not (flycheck-command-argument-p '(config-file "foo"))))

(ert-deftest flycheck-command-argument-p/option-without-filter ()
  (should (flycheck-command-argument-p '(option "foo" bar))))

(ert-deftest flycheck-command-argument-p/option-with-filter ()
  (should (flycheck-command-argument-p '(option "foo" bar filter))))

(ert-deftest flycheck-command-argument-p/option-with-quoted-variable-symbol ()
  (should-not (flycheck-command-argument-p '(option "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/option-with-quoted-filter-symbol ()
  (should-not (flycheck-command-argument-p '(option "foo" bar 'filter))))

(ert-deftest flycheck-command-argument-p/option-without-variable ()
  (should-not (flycheck-command-argument-p '(option "foo"))))

(ert-deftest flycheck-command-argument-p/option-list-without-filter-and-prepender ()
  (should (flycheck-command-argument-p '(option-list "foo" bar))))

(ert-deftest flycheck-command-argument-p/option-list-with-prepender ()
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn))))

(ert-deftest flycheck-command-argument-p/option-list-with-prepender-and-filter ()
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn filter))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-variable-symbol ()
  (should-not (flycheck-command-argument-p '(option-list "foo" 'bar))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-prepender-symbol ()
  (should-not (flycheck-command-argument-p '(option-list "foo" bar 'prepend-fn))))

(ert-deftest flycheck-command-argument-p/option-list-with-quoted-filter-symbol ()
  (should-not (flycheck-command-argument-p '(option-list "foo" bar prepend-fn 'filter))))

(ert-deftest flycheck-command-argument-p/option-list-without-variable-symbol ()
  (should-not (flycheck-command-argument-p '(option-list "foo"))))

(ert-deftest flycheck-command-argument-p/eval-with-variable ()
  (should (flycheck-command-argument-p '(eval bar))))

(ert-deftest flycheck-command-argument-p/eval-with-function-call ()
  (should (flycheck-command-argument-p '(eval (spam "with eggs")))))

(ert-deftest flycheck-command-argument-p/eval-with-no-form ()
  (should-not (flycheck-command-argument-p '(eval))))

(ert-deftest flycheck-command-argument-p/eval-with-multiple-forms ()
  (should-not (flycheck-command-argument-p '(eval foo bar))))

(ert-deftest flycheck-command-argument-p/integer-literal ()
  (should-not (flycheck-command-argument-p 100)))

(ert-deftest flycheck-command-argument-p/unknown-argument-symbol ()
  (should-not (flycheck-command-argument-p 'foo)))

(ert-deftest flycheck-command-argument-p/unknown-argument-cell ()
  (should-not (flycheck-command-argument-p '(foo bar))))

;;; definition-test.el ends here
