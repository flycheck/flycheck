;;; test-checker-api.el --- Flycheck Specs: Checker API  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2025 Sebastian Wiesner and Flycheck contributors

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

;; Specs for the Flycheck checker API.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Checker API"

  (describe "flycheck-valid-checker-p"
    (it "not-a-symbol"
      (expect (flycheck-valid-checker-p "foo") :not :to-be-truthy))

    (it "no-checker-version"
      (expect (get 'foo 'flycheck-generic-checker-version) :not :to-be-truthy)
      (expect (flycheck-valid-checker-p 'foo) :not :to-be-truthy))

    (it "checker-version-too-low"
      (cl-letf* ((version (- flycheck-generic-checker-version 1))
                 ((get 'foo 'flycheck-generic-checker-version) version))
        (expect (get 'foo 'flycheck-generic-checker-version) :to-equal version)
        (expect (flycheck-valid-checker-p 'foo) :not :to-be-truthy))
      (expect (get 'foo 'flycheck-generic-checker-version) :not :to-be-truthy))

    (it "checker-version-too-high"
      (cl-letf* ((version (+ flycheck-generic-checker-version 1))
                 ((get 'foo 'flycheck-generic-checker-version) version))
        (expect (get 'foo 'flycheck-generic-checker-version) :to-equal version)
        (expect (flycheck-valid-checker-p 'foo) :not :to-be-truthy))
      (expect (get 'foo 'flycheck-generic-checker-version) :not :to-be-truthy))

    (it "checker-version-ok"
      (cl-letf* ((version flycheck-generic-checker-version)
                 ((get 'foo 'flycheck-generic-checker-version) version))
        (expect (get 'foo 'flycheck-generic-checker-version) :to-equal version)
        (expect (flycheck-valid-checker-p 'foo) :to-be-truthy))
      (expect (get 'foo 'flycheck-generic-checker-version) :not :to-be-truthy)))

  (describe "flycheck-disabled-checker-p"
    (it "enabled-checker"
      (let ((flycheck-checkers '(emacs-lisp)))
        (expect (flycheck-disabled-checker-p 'emacs-lisp) :not :to-be-truthy)))

    (it "disabled-checker"
      (let ((flycheck-disabled-checkers '(emacs-lisp)))
        (expect (flycheck-disabled-checker-p 'emacs-lisp) :to-be-truthy))))

  (describe "flycheck-defined-checkers"
    (it "are-valid"
      (dolist (checker (flycheck-defined-checkers))
        (expect (flycheck-valid-checker-p checker) :to-be-truthy)))

    (it "are-registered"
      (dolist (checker (flycheck-defined-checkers))
        (expect (flycheck-registered-checker-p checker) :to-be-truthy))))

  (describe "flycheck-checker-executable"
    (it "is-string"
      (dolist (checker flycheck-checkers)
        (when (flycheck-checker-get checker 'command)
          (expect (stringp (flycheck-checker-executable checker)) :to-be-truthy))))

    (it "override-the-executable"
      (dolist (checker flycheck-checkers)
        (when (flycheck-checker-get checker 'command)
          (let ((variable (flycheck-checker-executable-variable checker)))
            (expect (eval `(let ((,variable "some-nice-executable"))
                             (flycheck-checker-executable ',checker)))
                    :to-equal "some-nice-executable"))))))

  (describe "flycheck-checker-get"
    (it "modes"
      (dolist (checker flycheck-checkers)
        (expect (listp (flycheck-checker-get checker 'modes)) :to-be-truthy)
        (expect (seq-every-p #'symbolp (flycheck-checker-get checker 'modes))
                :to-be-truthy))))

  (describe "flycheck-substitute-argument"
    (it "source"
      (flycheck-buttercup-with-resource-buffer "substitute-dummy"
        (unwind-protect
            (progn
              (expect (flycheck-substitute-argument 'source-original 'emacs-lisp)
                      :to-equal (list (buffer-file-name)))

              (let ((filename (flycheck-substitute-argument 'source-inplace 'emacs-lisp)))
                (expect filename :to-equal (list (flycheck-buttercup-resource-filename
                                                  (concat flycheck-temp-prefix
                                                          "_substitute-dummy"))))
                (expect (file-exists-p (car filename)) :to-be-truthy))

              (let ((filename (flycheck-substitute-argument 'source 'emacs-lisp)))
                (expect (string-prefix-p temporary-file-directory (car filename))
                        :to-be-truthy)
                (expect (file-exists-p (car filename)) :to-be-truthy)))
          (mapc #'flycheck-safe-delete flycheck-temporaries))))

    (it "temporary-directory"
      (unwind-protect
          (let ((dirname (car (flycheck-substitute-argument 'temporary-directory
                                                            'emacs-lisp))))
            (expect (file-directory-p dirname) :to-be-truthy)
            (expect (string-prefix-p temporary-file-directory dirname)
                    :to-be-truthy))
        (mapc #'flycheck-safe-delete flycheck-temporaries)))

    (it "temporary-filename"
      (unwind-protect
          (let ((filename (car (flycheck-substitute-argument 'temporary-file-name
                                                             'emacs-lisp))))
            ;; The filename should not exist, but its parent directory should
            (expect (file-exists-p filename) :not :to-be-truthy)
            (expect (file-directory-p (file-name-directory filename)) :to-be-truthy)
            (expect (string-prefix-p temporary-file-directory filename)
                    :to-be-truthy)
            (expect (member (directory-file-name (file-name-directory filename))
                            flycheck-temporaries)
                    :to-be-truthy))
        (mapc #'flycheck-safe-delete flycheck-temporaries)))

    (it "null-device"
      (expect (flycheck-substitute-argument 'null-device 'emacs-lisp)
              :to-equal (list null-device)))

    (it "config-file"
      (let* ((flycheck-test-config-var "substitute-dummy")
             (config-file (flycheck-buttercup-resource-filename "substitute-dummy"))
             first-args second-args
             (locate-nil (lambda (&rest args) (setq first-args args) nil))
             (locate-real (lambda (&rest args) (setq second-args args)
                            config-file))
             (flycheck-locate-config-file-functions (list locate-nil locate-real)))
        (expect (flycheck-substitute-argument
                 '(config-file "--foo" flycheck-test-config-var)
                 'emacs-lisp)
                :to-equal (list "--foo" config-file))
        (expect first-args :to-equal (list "substitute-dummy" 'emacs-lisp))
        (expect second-args :to-equal (list "substitute-dummy" 'emacs-lisp))
        (setq first-args nil
              second-args nil)
        (expect (flycheck-substitute-argument
                 '(config-file "--foo=" flycheck-test-config-var concat)
                 'emacs-lisp)
                :to-equal (list (concat "--foo=" config-file)))))

    (it "option"
      (let ((flycheck-test-option-var "bar"))
        (expect (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var) 'emacs-lisp)
                :to-equal '("--foo" "bar"))
        (expect (flycheck-substitute-argument
                 '(option "--foo=" flycheck-test-option-var concat)
                 'emacs-lisp)
                :to-equal '("--foo=bar")))
      (let ((flycheck-test-option-var 200))
        (expect (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var) 'emacs-lisp)
                :to-throw)
        (expect (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var nil number-to-string)
                 'emacs-lisp)
                :to-equal '("--foo" "200"))
        (expect (flycheck-substitute-argument
                 '(option "--foo=" flycheck-test-option-var concat number-to-string)
                 'emacs-lisp)
                :to-equal '("--foo=200")))
      (let (flycheck-test-option-var)
        (expect (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var) 'emacs-lisp)
                :not :to-be-truthy)
        ;; Catch an error, because `number-to-string' is called with nil
        (expect (flycheck-substitute-argument
                 '(option "--foo" flycheck-test-option-var nil number-to-string)
                 'emacs-lisp)
                :to-throw 'wrong-type-argument)
        (expect (flycheck-substitute-argument
                 '(option "--foo=" flycheck-test-option-var concat number-to-string)
                 'emacs-lisp)
                :to-throw 'wrong-type-argument)))

    (it "option-list"
      (let ((flycheck-test-option-var "spam"))
        (expect (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                :to-throw))
      (let ((flycheck-test-option-var '("spam" "eggs")))
        (expect (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                :to-equal '("-I" "spam" "-I" "eggs"))
        (expect (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var concat) 'emacs-lisp)
                :to-equal '("-Ispam" "-Ieggs")))
      (let ((flycheck-test-option-var '(10 20)))
        (expect (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                :to-throw)
        (expect (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var nil number-to-string)
                 'emacs-lisp)
                :to-equal '("-I" "10" "-I" "20"))
        (expect (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var
                               concat number-to-string) 'emacs-lisp)
                :to-equal '("-I10" "-I20")))
      (let (flycheck-test-option-var)
        (expect (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var) 'emacs-lisp)
                :not :to-be-truthy))
      (let ((flycheck-test-option-var '(nil)))
        ;; Catch an error, because `number-to-string' is called with nil
        (expect (flycheck-substitute-argument
                 '(option-list "-I" flycheck-test-option-var nil number-to-string)
                 'emacs-lisp)
                :to-throw 'wrong-type-argument)))

    (it "option-flag"
      (let ((flycheck-test-option-var nil))
        (expect (flycheck-substitute-argument
                 '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                :not :to-be-truthy))
      (let ((flycheck-test-option-var t))
        (expect (flycheck-substitute-argument
                 '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                :to-equal (list "--foo")))
      (let ((flycheck-test-option-var (list "bar")))
        (expect (flycheck-substitute-argument
                 '(option-flag "--foo" flycheck-test-option-var) 'emacs-lisp)
                :to-equal (list "--foo"))))

    (it "eval"
      (let ((flycheck-test-option-var '("Hello " "World")))
        (expect (flycheck-substitute-argument '(eval flycheck-test-option-var) 'emacs-lisp)
                :to-equal '("Hello " "World")))
      (expect (flycheck-substitute-argument '(eval (concat "Hello" "World")) 'emacs-lisp)
              :to-equal '("HelloWorld"))
      (expect (flycheck-substitute-argument
               '(eval (when (string= "foo" "bar") "yes")) 'emacs-lisp)
              :not :to-be-truthy)
      (expect (flycheck-substitute-argument '(eval 200) 'emacs-lisp)
              :to-throw)
      (expect (flycheck-substitute-argument '(eval '("foo" 200)) 'emacs-lisp)
              :to-throw))

    (it "unknown"
      (expect (flycheck-substitute-argument '(foo "bar") 'emacs-lisp)
              :to-throw)
      (expect (flycheck-substitute-argument 200 'emacs-lisp)
              :to-throw)))

  (describe "flycheck-may-use-checker"
    (it "invalid-checker"
      (expect (flycheck-valid-checker-p 'foo) :not :to-be-truthy)
      (expect (flycheck-may-use-checker 'foo) :not :to-be-truthy))

    (it "disabled-checker"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (flycheck-may-use-checker 'emacs-lisp)
        (let ((flycheck-disabled-checkers '(emacs-lisp)))
          (expect (flycheck-may-use-checker 'emacs-lisp) :not :to-be-truthy))))

    (it "checks-executable"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (let* ((was-called nil)
               (flycheck-executable-find (lambda (_) (setq was-called t))))
          (expect (flycheck-may-use-checker 'emacs-lisp) :to-be-truthy)
          (expect was-called :to-be-truthy)))))

  (describe "flycheck-may-enable-checker"
    (it "emacs-lisp"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (expect (flycheck-may-enable-checker 'emacs-lisp) :to-be-truthy)
        (expect (flycheck-may-enable-checker 'gibberish) :not :to-be-truthy)
        (expect flycheck--automatically-enabled-checkers :to-equal '(emacs-lisp))))

    (it "respects-cache"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (cl-letf* ((counter 0)
                   (enabled t)
                   ((flycheck-checker-get 'emacs-lisp 'enabled)
                    (lambda (&rest _ignore) (cl-incf counter) enabled)))
          ;; :enabled isn't called when a positive cached result is available
          (let ((flycheck--automatically-enabled-checkers '(emacs-lisp)))
            (expect (flycheck-may-enable-checker 'emacs-lisp) :to-be-truthy)
            (expect counter :to-equal 0))
          ;; :enabled isn't called when a negative cached result is available
          (let ((flycheck--automatically-disabled-checkers '(emacs-lisp)))
            (expect (flycheck-may-enable-checker 'emacs-lisp) :not :to-be-truthy)
            (expect counter :to-equal 0))
          ;; Returning a cached result doesn't change caches
          (expect flycheck--automatically-disabled-checkers :to-be nil)
          (expect flycheck--automatically-enabled-checkers :to-be nil)
          ;; :enabled should only be called once with positive results
          (setq enabled t)
          (dotimes (_ 10) (flycheck-may-enable-checker 'emacs-lisp))
          (expect flycheck--automatically-enabled-checkers :to-equal '(emacs-lisp))
          (setq flycheck--automatically-enabled-checkers nil)
          (expect counter :to-equal 1)
          ;; :enabled should only be called once with negative results
          (setq enabled nil counter 0)
          (dotimes (_ 10) (flycheck-may-enable-checker 'emacs-lisp))
          (expect flycheck--automatically-disabled-checkers :to-equal '(emacs-lisp))
          (expect counter :to-equal 1))))))

;;; test-checker-api.el ends here
