;;; test-error-api.el --- Flycheck Specs: Error API  -*- lexical-binding: t; -*-

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

;; Specs for the error API (regions, positioning, formatting, comparison).

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Error API"

  (describe "flycheck-error-line-region"
    (it "computes the region for a line"
      (flycheck-buttercup-with-temp-buffer
        (insert "Hello\n    World\n")
        (expect (flycheck--line-region (flycheck-line-column-to-position 1 1))
                :to-equal '(1 . 6))
        (expect (flycheck--line-region (flycheck-line-column-to-position 2 4))
                :to-equal '(11 . 16))
        ;; An error column beyond the end of the line is simply ignored just
        ;; like all other error columns
        (expect (flycheck--line-region (flycheck-line-column-to-position 2 10))
                :to-equal '(11 . 16))
        ;; An error line beyond the end of file should highlight the last line
        (expect (flycheck--line-region (flycheck-line-column-to-position 4 3))
                :to-equal '(16 . 17)))))

  (describe "flycheck-error-column-region"
    (it "computes the region for a column"
      (flycheck-buttercup-with-temp-buffer
        (insert "Hello\n    World\n")
        (expect (flycheck--column-region (flycheck-line-column-to-position 1 4))
                :to-equal '(4 . 5))
        (expect (flycheck--column-region (flycheck-line-column-to-position 2 6))
                :to-equal '(12 . 13))
        ;; A column beyond the end of a line
        (expect (flycheck--column-region (flycheck-line-column-to-position 1 7))
                :to-equal '(6 . 7))
        ;; A column right at the end of the last empty line of a file (an
        ;; important special case, because the Emacs Lisp checker reports
        ;; undefined functions at this place!)
        (expect (flycheck--column-region (flycheck-line-column-to-position 3 1))
                :to-equal '(16 . 17))
        ;; A column beyond the end of file
        (expect (flycheck--column-region (flycheck-line-column-to-position 4 2))
                :to-equal '(16 . 17)))))

  (describe "flycheck-error-thing-region"
    (it "computes bounds of things at point"
      (flycheck-buttercup-with-temp-buffer
        (insert "    (message)\n    (message")
        (emacs-lisp-mode)
        (expect (flycheck-bounds-of-thing-at-point
                 'sexp (flycheck-line-column-to-position 1 2))
                :not :to-be-truthy)
        (expect (flycheck-bounds-of-thing-at-point
                 'sexp (flycheck-line-column-to-position 1 5))
                :to-equal '(5 . 14))
        (expect (flycheck-bounds-of-thing-at-point
                 'symbol (flycheck-line-column-to-position 1 5))
                :not :to-be-truthy)
        (expect (flycheck-bounds-of-thing-at-point
                 'sexp (flycheck-line-column-to-position 1 8))
                :to-equal '(6 . 13))
        (expect (flycheck-bounds-of-thing-at-point
                 'symbol (flycheck-line-column-to-position 1 8))
                :to-equal '(6 . 13))
        ;; An incomplete expression
        (expect (flycheck-bounds-of-thing-at-point
                 'sexp (flycheck-line-column-to-position 2 5))
                :not :to-be-truthy)))

    (it "computes the error region for each mode"
      (flycheck-buttercup-with-temp-buffer
        (insert "    (message) ;; Hello world\n    (message")
        (emacs-lisp-mode)
        ;; Test an expression at the error column for all modes
        (let ((err (flycheck-error-new-at 1 7)))
          (expect (flycheck-error-region-for-mode err 'lines)
                  :to-equal '(5 . 29))
          (expect (flycheck-error-region-for-mode err 'columns)
                  :to-equal '(7 . 8))
          (expect (flycheck-error-region-for-mode err 'symbols)
                  :to-equal '(6 . 13))
          (expect (flycheck-error-region-for-mode err 'sexps)
                  :to-equal '(6 . 13)))
        ;; Test an error column which does not point to an expression
        (let ((err (flycheck-error-new-at 2 5)))
          (expect (flycheck-error-region-for-mode err 'lines)
                  :to-equal '(34 . 42))
          (dolist (mode '(columns symbols sexps))
            (expect (flycheck-error-region-for-mode err mode)
                    :to-equal '(34 . 35))))
        ;; Test an error without column for all modes
        (let ((err (flycheck-error-new-at 1 nil)))
          (dolist (mode '(lines columns symbols sexps))
            (expect (flycheck-error-region-for-mode err mode)
                    :to-equal '(5 . 29))))
        ;; With end-line and end-col set, MODE shouldn't make a difference
        (expect (flycheck-error-region-for-mode
                 (flycheck-error-new-at
                  1 18 nil nil :end-line 1 :end-column 29)
                 nil)
                :to-equal '(18 . 29))
        (expect (flycheck-error-region-for-mode
                 (flycheck-error-new-at
                  1 14 nil nil :end-line 1 :end-column 18)
                 nil)
                :to-equal '(14 . 18))
        (expect (flycheck-error-region-for-mode
                 (flycheck-error-new-at
                  1 5 nil nil :end-line 2 :end-column 13)
                 nil)
                :to-equal '(5 . 42))
        (expect (flycheck-error-region-for-mode
                 (flycheck-error-new-at
                  1 nil nil nil :end-line 2)
                 'lines)
                :to-equal '(5 . 29))
        (expect (flycheck-error-region-for-mode
                 (flycheck-error-new-at
                  2 5 nil nil :end-column 13)
                 nil)
                :to-equal '(34 . 42))
        (expect (flycheck-error-region-for-mode
                 (flycheck-error-new-at
                  10 1 nil nil :end-line 10 :end-column 3)
                 nil)
                :to-equal '(41 . 42))
        (expect (flycheck-error-region-for-mode
                 (flycheck-error-new-at
                  1 1 nil nil :end-line 10 :end-column 1)
                 nil)
                :to-equal '(1 . 42)))))

  (describe "flycheck-error-pos"
    (it "computes the error position"
      (flycheck-buttercup-with-temp-buffer
        (insert "    Hello\n   World\n")
        (expect (flycheck-error-pos (flycheck-error-new-at 1 1))
                :to-equal 1)
        (expect (flycheck-error-pos (flycheck-error-new-at 1 4))
                :to-equal 4)
        (expect (flycheck-error-pos (flycheck-error-new-at 1 nil))
                :to-equal 5)
        (expect (flycheck-error-pos (flycheck-error-new-at 2 nil))
                :to-equal 14)
        (expect (flycheck-error-pos (flycheck-error-new-at 3 1))
                :to-equal 19)
        (expect (flycheck-error-pos (flycheck-error-new-at 4 1))
                :to-equal 19)
        (expect (flycheck-error-pos (flycheck-error-new-at 4 nil))
                :to-equal 19))))

  (describe "flycheck-error-format-message-and-id"
    (it "formats without id"
      (expect (flycheck-error-format-message-and-id
               (flycheck-error-new-at 3 5 'warning "Hello world"))
              :to-equal "Hello world"))

    (it "formats with id"
      (expect (flycheck-error-format-message-and-id
               (flycheck-error-new-at 3 5 'warning "Hello world"
                                      :id "Foo"))
              :to-equal "Hello world [Foo]")))

  (describe "flycheck-error-format"
    (it "formats a warning"
      (expect (flycheck-error-format
               (flycheck-error-new-at 3 5 'warning "Hello world"
                                      :checker 'emacs-lisp))
              :to-equal "3:5:warning: Hello world (emacs-lisp)"))

    (it "formats an error"
      (expect (flycheck-error-format
               (flycheck-error-new-at 20 7 'error "Spam with eggs"
                                      :checker 'ruby))
              :to-equal "20:7:error: Spam with eggs (ruby)"))

    (it "handles line breaks"
      ;; Specific test for https://github.com/magnars/s.el/issues/34
      (expect (flycheck-error-format
               (flycheck-error-new-at 14 15 'error "dash\\nbroken"
                                      :checker 'foo))
              :to-equal "14:15:error: dash\\nbroken (foo)"))

    (it "formats with id"
      (expect (flycheck-error-format
               (flycheck-error-new-at 14 15 'error "A message"
                                      :id "E001" :checker 'foo))
              :to-equal "14:15:error: A message [E001] (foo)")))

  (describe "flycheck-error-format-position"
    (it "formats various position combinations"
      (cl-flet ((fmt
                 (l c el ec)
                 (flycheck-error-format-position
                  (flycheck-error-new-at
                   l c 'error "err" :end-line el :end-column ec
                   :checker 'emacs-lisp))))
        (expect (fmt 14 nil nil nil) :to-equal "14")
        (expect (fmt 14 nil nil 1)   :to-equal "14")
        (expect (fmt 14 nil 14  nil) :to-equal "14")
        (expect (fmt 14 nil 15  nil) :to-equal "14-15")
        (expect (fmt 14 nil 15  1)   :to-equal "14-15")
        (expect (fmt 14 1   nil nil) :to-equal "14:1")
        (expect (fmt 14 1   nil 2)   :to-equal "14:1")
        (expect (fmt 14 1   nil 1)   :to-equal "14:1-1")
        (expect (fmt 14 1   14  nil) :to-equal "14:1")
        (expect (fmt 14 1   14  2)   :to-equal "14:1")
        (expect (fmt 14 1   14  1)   :to-equal "14:1-1")
        (expect (fmt 14 1   14  5)   :to-equal "14:1-5")
        (expect (fmt 14 1   15  5)   :to-equal "(14:1)-(15:5)"))))

  (describe "flycheck-error-<"
    (it "compares by line without column"
      (expect (flycheck-error-< (flycheck-error-new-at 10 nil)
                                (flycheck-error-new-at 11 nil))
              :to-be-truthy)
      (expect (flycheck-error-< (flycheck-error-new-at 10 nil)
                                (flycheck-error-new-at 10 nil))
              :not :to-be-truthy)
      (expect (flycheck-error-< (flycheck-error-new-at 10 nil)
                                (flycheck-error-new-at 9 nil))
              :not :to-be-truthy))

    (it "compares by line with column"
      (expect (flycheck-error-< (flycheck-error-new-at 10 2)
                                (flycheck-error-new-at 11 1))
              :to-be-truthy)
      (expect (flycheck-error-< (flycheck-error-new-at 10 1)
                                (flycheck-error-new-at 9 2))
              :not :to-be-truthy))

    (it "compares by column"
      (expect (flycheck-error-< (flycheck-error-new-at 10 10)
                                (flycheck-error-new-at 10 11))
              :to-be-truthy)
      (expect (flycheck-error-< (flycheck-error-new-at 10 10)
                                (flycheck-error-new-at 10 10))
              :not :to-be-truthy)
      (expect (flycheck-error-< (flycheck-error-new-at 10 11)
                                (flycheck-error-new-at 10 10))
              :not :to-be-truthy)))

  (describe "flycheck-error-level-<"
    (it "compares by level severity"
      (expect (flycheck-error-level-< (flycheck-error-new-at 10 nil 'info)
                                      (flycheck-error-new-at 8 nil 'warning))
              :to-be-truthy)
      (expect (flycheck-error-level-< (flycheck-error-new-at 8 nil 'warning)
                                      (flycheck-error-new-at 8 nil 'warning))
              :not :to-be-truthy)
      (expect (flycheck-error-level-< (flycheck-error-new-at 7 nil 'error)
                                      (flycheck-error-new-at 8 nil 'warning))
              :not :to-be-truthy))

    (it "compares by level name"
      (expect (flycheck-error-level-< (flycheck-error-new-at 10 nil 'a)
                                      (flycheck-error-new-at 8 nil 'b))
              :to-be-truthy)
      (expect (flycheck-error-level-< (flycheck-error-new-at 7 nil 'c)
                                      (flycheck-error-new-at 8 nil 'b))
              :not :to-be-truthy))

    (it "compares by location"
      (expect (flycheck-error-level-< (flycheck-error-new-at 8 nil 'info)
                                      (flycheck-error-new-at 10 nil 'info))
              :to-be-truthy)
      (expect (flycheck-error-level-< (flycheck-error-new-at 8 nil 'info)
                                      (flycheck-error-new-at 8 nil 'info))
              :not :to-be-truthy)
      (expect (flycheck-error-level-< (flycheck-error-new-at 8 nil 'info)
                                      (flycheck-error-new-at 7 nil 'info))
              :not :to-be-truthy)))

  (describe "flycheck-assert-error-list-p"
    (it "accepts a list of flycheck errors"
      (let ((errors (list (flycheck-error-new-at 8 10 nil 'info)
                          (flycheck-error-new-at 9 11 nil 'info))))
        (expect (flycheck-assert-error-list-p errors) :to-be errors)))

    (it "rejects a non-list"
      (let ((data (should-error (flycheck-assert-error-list-p 'foo)
                                :type 'wrong-type-argument)))
        (expect (error-message-string data)
                :to-equal "Wrong type argument: listp, foo")))

    (it "rejects nil in list"
      (let* ((errors (list (flycheck-error-new-at 8 10 nil 'info)
                           nil))
             (data (should-error (flycheck-assert-error-list-p errors)
                                 :type 'wrong-type-argument)))
        (expect (error-message-string data)
                :to-equal "Wrong type argument: flycheck-error-p, nil")))

    (it "rejects wrong type in list"
      (let* ((errors (list (flycheck-error-new-at 8 10 nil 'info)
                           "foo"))
             (data (should-error (flycheck-assert-error-list-p errors)
                                 :type 'wrong-type-argument)))
        (expect (error-message-string data)
                :to-equal "Wrong type argument: flycheck-error-p, \"foo\""))))

  (describe "flycheck-related-errors"
    (it "finds related errors by group and checker"
      (let ((flycheck-current-errors
             (list (flycheck-error-new-at 5 7 'error "foo" :checker 'a :group 1)
                   (flycheck-error-new-at 8 9 'error "bar" :checker 'a :group 2)
                   (flycheck-error-new-at 1 4 'error "gul" :checker 'a :group 1)
                   (flycheck-error-new-at 4 5 'error "lag" :checker 'b :group 1))))
        (expect (flycheck-related-errors (nth 0 flycheck-current-errors))
                :to-equal (list (nth 0 flycheck-current-errors)
                                (nth 2 flycheck-current-errors)))
        (expect (flycheck-related-errors (nth 1 flycheck-current-errors))
                :to-equal (list (nth 1 flycheck-current-errors)))))))

;;; test-error-api.el ends here
