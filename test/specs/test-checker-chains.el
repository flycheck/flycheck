;;; test-checker-chains.el --- Flycheck Specs: Syntax checker chains  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sebastian Wiesner

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

;; Specs for syntax checker chains.

;;; Code:

(require 'flycheck-buttercup)

(describe "Syntax checker chains"

  ;; Declare a couple of fake checkers
  (setf (flycheck-checker-get 'chain-syntax-1 'stage) 'syntax)
  (setf (flycheck-checker-get 'chain-syntax-2 'stage) 'syntax)
  (setf (flycheck-checker-get 'chain-lint-1 'stage) 'lint)
  (setf (flycheck-checker-get 'chain-style-1 'stage) 'style)
  (setf (flycheck-checker-get 'chain-style-2 'stage) 'style)

  (before-each
    ;; Assume that we can use all syntax checkers
    (spy-on 'flycheck-may-use-checker :and-return-value t))

  (it "sorts stably by stage"
    (let ((flycheck-checkers '(chain-lint-1
                               chain-syntax-1
                               chain-style-2
                               chain-syntax-2
                               chain-style-1)))
      (expect (flycheck-get-syntax-checker-chain-for-buffer)
              :to-equal
              '(chain-syntax-1
                chain-syntax-2
                chain-lint-1
                chain-style-2
                chain-style-1))))

  (it "evicts conflicting checkers"
    (cl-letf (
              ;; A forward conflict
              ((flycheck-checker-get 'chain-syntax-1 'conflicts-with)
               '(chain-syntax-2 chain-style-1))
              ;; A backward conflict
              ((flycheck-checker-get 'chain-style-2 'conflicts-with)
               '(chain-syntax-1))
              ;; This conflict should be ignore because `chain-style-1' is
              ;; already evicted
              ((flycheck-checker-get 'chain-style-1 'conflicts-with)
               '(chain-lint-1)))
      (expect (flycheck-evict-checkers  '(chain-syntax-1
                                          chain-syntax-2
                                          chain-lint-1
                                          chain-style-1
                                          chain-style-2))
              :to-equal '(chain-syntax-1 chain-lint-1))))

  (it "skips unusable syntax checkers"
    (spy-on 'flycheck-may-use-checker
            :and-call-fake (lambda (c) (eq c 'chain-syntax-1)))
    (let ((flycheck-checkers '(chain-syntax-1 chain-lint-1 chain-style-1)))
      (expect (flycheck-get-syntax-checker-chain-for-buffer)
              :to-equal '(chain-syntax-1))))

  (it "ignores conflicts from unusable syntax checkers"
    (spy-on 'flycheck-may-use-checker
            :and-call-fake (lambda (c) (not (eq c 'chain-syntax-1))))
    (cl-letf (((flycheck-checker-get 'chain-syntax-1 'conflicts-with)
               ;; Even though `chain-syntax-1' conflicts with `chain-lint-1' the
               ;; latter must be included in the chain because the former is
               ;; ruled out due to `flycheck-may-use-checker'
               '(chain-lint-1))
              (flycheck-checkers '(chain-syntax-1 chain-lint-1 chain-style-1)))
      (expect (flycheck-get-syntax-checker-chain-for-buffer)
              :to-equal '(chain-lint-1 chain-style-1)))))

;;; test-checker-chains.el ends here
