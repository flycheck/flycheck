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

(ert-deftest flycheck-error-pattern-p ()
  (should (flycheck-error-pattern-p '("foo" . warning)))
  (should-not (flycheck-error-pattern-p '("bar" . foo)))
  (should-not (flycheck-error-pattern-p '("bar" warning)))
  (should-not (flycheck-error-pattern-p "foo"))
  (should-not (flycheck-error-pattern-p 'warning)))

(ert-deftest flycheck-command-argument-p-symbols ()
  (--each '(source source-inplace source-original temporary-directory)
    (should (flycheck-command-argument-p it))))

(ert-deftest flycheck-command-argument-p-config-file ()
  (should (flycheck-command-argument-p '(config-file "foo" bar)))
  (should-not (flycheck-command-argument-p '(config-file "foo" 'bar)))
  (should-not (flycheck-command-argument-p '(config-file "foo"))))

(ert-deftest flycheck-command-argument-p-option ()
  (should (flycheck-command-argument-p '(option "foo" bar)))
  (should (flycheck-command-argument-p '(option "foo" bar filter)))
  (should-not (flycheck-command-argument-p '(option "foo" 'bar)))
  (should-not (flycheck-command-argument-p '(option "foo" bar 'filter)))
  (should-not (flycheck-command-argument-p '(option "foo"))))

(ert-deftest flycheck-command-argument-p-option-list ()
  (should (flycheck-command-argument-p '(option-list "foo" bar)))
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn)))
  (should (flycheck-command-argument-p '(option-list "foo" bar prepend-fn filter)))
  (should-not (flycheck-command-argument-p '(option-list "foo" 'bar)))
  (should-not (flycheck-command-argument-p '(option-list "foo" bar 'prepend-fn)))
  (should-not (flycheck-command-argument-p '(option-list "foo"))))

(ert-deftest flycheck-command-argument-p-eval ()
  (should (flycheck-command-argument-p '(eval bar)))
  (should (flycheck-command-argument-p '(eval (bar))))
  (should-not (flycheck-command-argument-p '(eval)))
  (should-not (flycheck-command-argument-p '(eval foo bar))))

(ert-deftest flycheck-command-argument-p-invalid-argument ()
  (should-not (flycheck-command-argument-p 100))
  (should-not (flycheck-command-argument-p 'foo))
  (should-not (flycheck-command-argument-p '(foo bar))))

;;; definition-test.el ends here
