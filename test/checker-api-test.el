;;; checker-api-test.el --- Tests for syntax checker API  -*- lexical-binding: t; -*-

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

;; Tests for the syntax checker API.

;;; Code:

(require 'test-helper)

(defvar flycheck-test-config-var)
(defvar flycheck-test-option-var)

(ert-deftest flycheck-substitute-argument-source ()
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (unwind-protect
        (progn
          (should (equal (flycheck-substitute-argument 'source-original 'emacs-lisp)
                         (buffer-file-name)))

          (let ((filename (flycheck-substitute-argument 'source-inplace 'emacs-lisp)))
            (should (equal filename (flycheck-testsuite-resource-filename
                                     "flycheck-substitute-dummy")))
            (should (f-exists? filename)))

          (let ((filename (flycheck-substitute-argument 'source 'emacs-lisp)))
            (should (s-starts-with? temporary-file-directory filename))
            (should (f-exists? filename)))))
    (flycheck-safe-delete flycheck-temporaries)))

(ert-deftest flycheck-substitute-argument-temporary-directory ()
  (with-temp-buffer
    (unwind-protect
        (progn
          (let ((dirname (flycheck-substitute-argument 'temporary-directory
                                                       'emacs-lisp)))
            (should (f-directory? dirname))
            (should (s-starts-with? temporary-file-directory dirname))))
      (flycheck-safe-delete flycheck-temporaries))))

(ert-deftest flycheck-substitute-argument-config-file ()
  (let ((flycheck-test-config-var "substitute-dummy")
        (config-file (flycheck-testsuite-resource-filename "substitute-dummy")))
    (mocker-let
        ((locate-config-file-nil
          (filename checker)
          ((:input '("substitute-dummy" emacs-lisp) :output nil
                   :min-occur 2 :max-occur 2)))
         (locate-config-file-real
          (filename checker)
          ((:input '("substitute-dummy" emacs-lisp) :output config-file
                   :min-occur 2 :max-occur 2))))
      (let ((flycheck-locate-config-file-functions
             '(locate-config-file-nil locate-config-file-real)))
        (should (equal (flycheck-substitute-argument
                        '(config-file "--foo" flycheck-test-config-var)
                        'emacs-lisp)
                       (list "--foo" config-file)))
        (should (equal (flycheck-substitute-argument
                        '(config-file "--foo=" flycheck-test-config-var)
                        'emacs-lisp)
                       (list (concat "--foo=" config-file))))))))

(ert-deftest flycheck-substitute-argument-option ()
  (let ((flycheck-test-option-var "bar"))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo" flycheck-test-option-var) 'emacs-lisp)
                   '("--foo" "bar")))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo=" flycheck-test-option-var) 'emacs-lisp)
                   '("--foo=bar"))))
  (let ((flycheck-test-option-var 200))
    (should-error (flycheck-substitute-argument
                   '(option "--foo" flycheck-test-option-var) 'emacs-lisp))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo" flycheck-test-option-var number-to-string) 'emacs-lisp)
                   '("--foo" "200")))
    (should (equal (flycheck-substitute-argument
                    '(option "--foo=" flycheck-test-option-var number-to-string) 'emacs-lisp)
                   '("--foo=200"))))
  (let (flycheck-test-option-var)
    (should-not (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var) 'emacs-lisp))
    ;; Catch an error, because `number-to-string' is called with nil
    (should-error (flycheck-substitute-argument
                   '(option "--foo" flycheck-test-option-var number-to-string) 'emacs-lisp)
                  :type 'wrong-type-argument)
    (should-error (flycheck-substitute-argument
                   '(option "--foo=" flycheck-test-option-var number-to-string) 'emacs-lisp)
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-argument-option-list ()
  (let ((flycheck-test-option-var "spam"))
    (should-error (flycheck-substitute-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var '("spam" "eggs")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                   '("-I" "spam" "-I" "eggs")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var s-prepend) 'emacs-lisp)
                   '("-Ispam" "-Ieggs"))))
  (let ((flycheck-test-option-var '(10 20)))
    (should-error (flycheck-substitute-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                   '("-I" "10" "-I" "20")))
    (should (equal (flycheck-substitute-argument
                    '(option-list "-I" flycheck-test-option-var
                                  s-prepend number-to-string) 'emacs-lisp)
                   '("-I10" "-I20"))))
  (let (flycheck-test-option-var)
    (should-not (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var '(nil)))
    ;; Catch an error, because `number-to-string' is called with nil
    (should-error (flycheck-substitute-argument
                   '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-argument-eval ()
  (let ((flycheck-test-option-var '("Hello " "World")))
    (should (equal (flycheck-substitute-argument '(eval flycheck-test-option-var) 'emacs-lisp)
                   '("Hello " "World"))))
  (should (equal (flycheck-substitute-argument '(eval (concat "Hello" "World")) 'emacs-lisp)
                 "HelloWorld"))
  (should-not (flycheck-substitute-argument
               '(eval (when (string= "foo" "bar") "yes")) 'emacs-lisp))
  (should-error (flycheck-substitute-argument '(eval 200) 'emacs-lisp))
  (should-error (flycheck-substitute-argument '(eval '("foo" 200)) 'emacs-lisp)))

(ert-deftest flycheck-substitute-argument-unknown ()
  (--each '(flycheck-substitute-argument flycheck-substitute-shell-argument)
    (should-error (funcall it '(foo "bar") 'emacs-lisp))
    (should-error (funcall it 200 'emacs-lisp))))

(ert-deftest flycheck-substitute-shell-argument-source ()
  (flycheck-testsuite-with-resource-buffer "substitute-dummy"
    (--each '(source source-inplace source-original)
      (should (equal (flycheck-substitute-shell-argument it 'emacs-lisp)
                     (buffer-file-name))))))

(ert-deftest flycheck-substitute-shell-argument-temporary-directory ()
  (mocker-let
      ((flycheck-substitute-argument
        (arg checker)
        ((:input '(temporary-directory emacs-lisp) :output "spam with eggs"))))
    (should (equal (flycheck-substitute-shell-argument 'temporary-directory
                                                       'emacs-lisp)
                   "spam\\ with\\ eggs"))))

(ert-deftest flycheck-substitute-shell-argument-config-file ()
  (let ((filename "spam with eggs"))
    (mocker-let
        ((flycheck-substitute-argument
          (arg checker)
          ((:input '((config-file "--foo" flycheck-test-config-var) emacs-lisp)
                   :output (list "--foo" filename))
           (:input '((config-file "--foo=" flycheck-test-config-var) emacs-lisp)
                   :output (list (concat "--foo=" filename))))))
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo" flycheck-test-config-var)
                      'emacs-lisp)
                     (concat "--foo " (shell-quote-argument filename))))
      (should (equal (flycheck-substitute-shell-argument
                      '(config-file "--foo=" flycheck-test-config-var)
                      'emacs-lisp)
                     (shell-quote-argument (concat "--foo=" filename)))))))

(ert-deftest flycheck-substitute-shell-argument-option ()
  (mocker-let
      ((flycheck-substitute-argument
        (arg checker)
        ((:input '((option "--foo" flycheck-test-option-var) emacs-lisp)
                 :output '("--foo" "spam with eggs"))
         (:input '((option "--foo=" flycheck-test-option-var) emacs-lisp)
                 :output '("--foo=spam with eggs"))
         (:input '((option "--foo" flycheck-test-option-var number-to-string)
                   emacs-lisp)
                 :output '("--foo" "spam with eggs"))
         (:input '((option "--foo=" flycheck-test-option-var number-to-string)
                   emacs-lisp)
                 :output '("--foo=spam with eggs")))))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo" flycheck-test-option-var)
                    'emacs-lisp)
                   "--foo spam\\ with\\ eggs"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo=" flycheck-test-option-var)
                    'emacs-lisp)
                   "--foo\\=spam\\ with\\ eggs"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo" flycheck-test-option-var number-to-string)
                    'emacs-lisp)
                   "--foo spam\\ with\\ eggs"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option "--foo=" flycheck-test-option-var number-to-string)
                    'emacs-lisp)
                   "--foo\\=spam\\ with\\ eggs"))))

(ert-deftest flycheck-substitute-shell-argument-option-list ()
  (let ((flycheck-test-option-var "spam"))
    (should-error (flycheck-substitute-shell-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)))
  (let ((flycheck-test-option-var '("spam" "with eggs")))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                   "-I spam -I with\\ eggs"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var s-prepend) 'emacs-lisp)
                   "-Ispam -Iwith\\ eggs")))
  (let ((flycheck-test-option-var '(10 20)))
    (should-error (flycheck-substitute-shell-argument
                   '(option-list "-I" flycheck-test-option-var) 'emacs-lisp))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                   "-I 10 -I 20"))
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var s-prepend number-to-string) 'emacs-lisp)
                   "-I10 -I20")))
  (let (flycheck-test-option-var)
    (should (equal (flycheck-substitute-shell-argument
                    '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                   "")))
  (let ((flycheck-test-option-var '(nil)))
    ;; Catch an error, because `number-to-string' is called with nil
    (should-error (flycheck-substitute-shell-argument
                   '(option-list "-I" flycheck-test-option-var nil number-to-string) 'emacs-lisp)
                  :type 'wrong-type-argument)))

(ert-deftest flycheck-substitute-shell-argument-eval ()
  (mocker-let
      ((flycheck-substitute-argument
        (arg checker)
        ((:input '((eval foo) emacs-lisp) :output '("foo bar" "spam eggs")))))
    (should (equal (flycheck-substitute-shell-argument '(eval foo) 'emacs-lisp)
                   "foo\\ bar spam\\ eggs"))))

(ert-deftest flycheck-checker-modes ()
  (dolist (checker flycheck-checkers)
    (should (listp (flycheck-checker-modes checker)))
    (should (-all? #'symbolp (flycheck-checker-modes checker)))))

(ert-deftest flycheck-checker-executable ()
  (dolist (checker flycheck-checkers)
    (should (stringp (flycheck-checker-executable checker)))))

(ert-deftest flycheck-check-executable ()
  (dolist (checker flycheck-checkers)
    (if (executable-find (flycheck-checker-executable checker))
        (should (flycheck-check-executable checker))
      (should-not (flycheck-check-executable checker)))))

;;; checker-api-test.el ends here
