;;; test-error-filters.el --- Flycheck Specs: Error Filters  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Specs for error filters.

;;; Code:

(require 'flycheck-buttercup)

(describe "Error filters"

  (describe "flycheck-sanitize-errors"

    (it "removes trailing whitespace"
      (let ((err (flycheck-error-new-at 1 1 'error " foo ")))
        (expect (flycheck-sanitize-errors (list err))
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 1 1 'error "foo")))))

    (it "removes empty error IDs"
      (let ((err (flycheck-error-new-at 1 1 'error "foo" :id "")))
        (expect (flycheck-sanitize-errors (list err))
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 1 1 'error "foo")))))

    (it "removes zero columns"
      (let ((err (flycheck-error-new-at 1 0 'error "foo")))
        (expect (flycheck-sanitize-errors (list err))
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 1 nil 'error "foo"))))))

  (describe "flycheck-remove-error-file-names"

    (it "removes the given filename from errors"
      (let ((errors
             (list
              (flycheck-error-new-at 1 1 'error "foo" :filename "hello")
              (flycheck-error-new-at 2 2 'warning "bar" :filename "world")
              (flycheck-error-new-at 3 3 'info "spam"))))
        (expect (flycheck-remove-error-file-names "world" errors)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 1 1 'error "foo" :filename "hello")
                 (flycheck-error-new-at 2 2 'warning "bar")
                 (flycheck-error-new-at 3 3 'info "spam"))))))

  (describe "flycheck-increment-error-columns"
    (it "ignores nil columns"
      (let ((errors (list (flycheck-error-new-at 4 nil))))
        (expect (flycheck-increment-error-columns errors)
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 4 nil)))))

    (it "increments with the default offset"
      (let ((errors (list (flycheck-error-new-at 4 6)
                          (flycheck-error-new-at 7 9))))
        (expect (flycheck-increment-error-columns errors)
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 4 7)
                      (flycheck-error-new-at 7 10)))))

    (it "increments with a custom offset"
      (let ((errors (list (flycheck-error-new-at 4 6)
                          (flycheck-error-new-at 7 9))))
        (expect (flycheck-increment-error-columns errors 10)
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 4 16)
                      (flycheck-error-new-at 7 19))))))

  (describe "flycheck-fold-include-levels"
    (it "skips over intermittent errors"
      ;; See https://github.com/flycheck/flycheck/pull/783
      (let ((errors
             (list (flycheck-error-new-at 1 0 'error "In file included from"
                                          :filename "foo.cpp")
                   (flycheck-error-new-at 6 11 'error "b is not a member of hi"
                                          :filename "foo.cpp")
                   (flycheck-error-new-at 5 20 'note
                                          "in definition of macro CHECK"
                                          :filename "foo.h")
                   (flycheck-error-new-at 8 5 'error
                                          "xx was not declared in this scope"
                                          :filename "foo.cpp"))))
        (expect
         (flycheck-fold-include-levels errors "In file included from")
         :to-be-equal-flycheck-errors
         (list (flycheck-error-new-at 1 0 'error "In include foo.h"
                                      :filename "foo.cpp")
               (flycheck-error-new-at 6 11 'error "b is not a member of hi"
                                      :filename "foo.cpp")
               (flycheck-error-new-at 5 20 'note "in definition of macro CHECK"
                                      :filename "foo.h")
               (flycheck-error-new-at 8 5 'error
                                      "xx was not declared in this scope"
                                      :filename "foo.cpp"))))))

  (describe "flycheck-collapse-error-message-whitespace"
    (it "collapses all whitespace in error messages"
      (let ((err (flycheck-error-new-at 1 1 'error
                                        "spam  \nwith\t   eggs")))
        (expect (flycheck-collapse-error-message-whitespace (list err))
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 1 1 'error
                                             "spam with eggs"))))))

  (describe "flycheck-dequalify-error-ids"
    (it "removes all nested qualifiers"
      (let ((errors
             (list (flycheck-error-new-at 1 2 nil nil :id "foo.bar")
                   (flycheck-error-new-at 1 2 nil nil :id "Spam.With.Eggs"))))
        (expect (flycheck-dequalify-error-ids errors)
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 1 2 nil nil :id "bar")
                      (flycheck-error-new-at 1 2 nil nil :id "Eggs")))))

    (it "leaves unqualified IDs alone"
      (let ((errors (list (flycheck-error-new-at 1 2 nil nil :id "foobar"))))
        (expect (flycheck-dequalify-error-ids errors)
                :to-be-equal-flycheck-errors errors)))

    (it "ignores errors without IDs"
      (let ((errors (list (flycheck-error-new-at 3 4))))
        (expect (flycheck-dequalify-error-ids errors)
                :to-be-equal-flycheck-errors errors))))

  (describe "flycheck-remove-error-ids"
    (it "ignores errors without IDs"
      (let ((errors (list (flycheck-error-new-at 1 2))))
        (expect (flycheck-remove-error-ids errors)
                :to-be-equal-flycheck-errors errors)))

    (it "removes error IDs"
      (let ((errors (list (flycheck-error-new-at 1 2 nil nil :id "Foo.Bar"))))
        (expect (flycheck-remove-error-ids errors)
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 1 2))))))

  (describe "flycheck-fill-empty-line-numbers"
    (it "ignores errors with line numbers"
      (let ((errors (list (flycheck-error-new-at 1 2))))
        (expect (flycheck-fill-empty-line-numbers errors)
                :to-be-equal-flycheck-errors errors)))

    (it "sets errors missing line numbers to line 0"
      (let ((errors (list (flycheck-error-new-at nil nil))))
        (expect (flycheck-fill-empty-line-numbers errors)
                :to-be-equal-flycheck-errors
                (list (flycheck-error-new-at 0 nil)))))))

;;; test-error-filters.el ends here
