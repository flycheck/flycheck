;;; test-config-file-functions.el --- Tests for config file functions -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; Test functions that locate config files

;;; Code:

(require 'ert)
(require 'flycheck)
(require 'projectile)

(ert-deftest flycheck-locate-config-file-functions ()
  (should (equal flycheck-locate-config-file-functions
                 '(flycheck-locate-config-file-absolute-path
                   flycheck-locate-config-file-projectile
                   flycheck-locate-config-file-ancestor-directories
                   flycheck-locate-config-file-home))))

(ert-deftest flycheck-locate-config-file-absolute-path ()
  (with-temp-buffer
    (cd flycheck-testsuite-dir)
    (should-not (flycheck-locate-config-file-absolute-path "flycheck-testsuite.el"
                                                           'emacs-lisp))
    (should (equal (flycheck-locate-config-file-absolute-path "../Makefile"
                                                              'emacs-lisp)
                   (expand-file-name "../Makefile" flycheck-testsuite-dir)))))

(ert-deftest flycheck-locate-config-file-projectile ()
  (with-temp-buffer
    (set-visited-file-name (expand-file-name "foo" flycheck-testsuite-dir)
                           :no-query)
    (should (projectile-project-p))
    (should (equal
             (flycheck-locate-config-file-projectile "Makefile" 'emacs-lisp)
             (expand-file-name "../Makefile" flycheck-testsuite-dir)))
    (should-not (flycheck-locate-config-file-projectile "Foo" 'emacs-lisp)))
  (with-temp-buffer
    (set-visited-file-name (expand-file-name "foo" temporary-file-directory)
                           :no-query)
    (should-not (projectile-project-p))
    (should-not (flycheck-locate-config-file-projectile "Foo" 'emacs-dir))))

(ert-deftest flycheck-locate-config-file-ancestor-directories ()
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "flycheck-testsuite.el"
                                             flycheck-testsuite-dir))
    (should-not (flycheck-locate-config-file-ancestor-directories "foo" 'emacs-lisp))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "flycheck-testrunner.el" 'emacs-lisp)
                   (expand-file-name "flycheck-testrunner.el" flycheck-testsuite-dir)))
    (should (equal (flycheck-locate-config-file-ancestor-directories
                    "Makefile" 'emacs-lisp)
                   (expand-file-name "../Makefile" flycheck-testsuite-dir)))))

(ert-deftest flycheck-locate-config-file-home ()
  (let ((old-home (getenv "HOME")))
    (unwind-protect
        (progn
          (setenv "HOME" flycheck-testsuite-dir)
          (should-not (flycheck-locate-config-file-home "foo" 'emacs-lisp))
          (should-not (flycheck-locate-config-file-home "Makefile" 'emacs-lisp))
          (should (equal (flycheck-locate-config-file-home
                          "flycheck-testsuite.el" 'emacs-lisp)
                         (expand-file-name "flycheck-testsuite.el"
                                           flycheck-testsuite-dir))))
      (setenv "HOME" old-home))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-config-file-functions.el ends here
