;;; version-test.el --- Tests for version information  -*- lexical-binding: t; -*-

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

;; Unit tests for version information.

;;; Code:

(require 'test-helper)

(ert-deftest flycheck-library-version ()
  :expected-result (if (executable-find "cask") :passed :failed)
  ;;  The default directory must end with a slash
  (let* ((default-directory (-> flycheck-testsuite-dir
                              f-parent
                              file-name-as-directory))
         (version (car (process-lines "cask" "version"))))
    (should (string= version (flycheck-library-version)))))

(ert-deftest flycheck-package-version ()
  (require 'package)
  (let* ((entry
          (if (fboundp 'package-desc-create)
              (list 'flycheck (package-desc-create :name 'flycheck
                                                   :version '(2 12)
                                                   :summary "Foo"))
            (cons 'flycheck (vector '(2 12) nil "Foo"))))
         (package-alist (list entry)))
    (should (string= "2.12" (flycheck-package-version)))))

(ert-deftest flycheck-version ()
  (let* ((default-directory (-> flycheck-testsuite-dir
                              f-parent
                              file-name-as-directory))
         (version (car (process-lines "cask" "version"))))
    ;; Just the library version
    (should (string= version (flycheck-version)))
    ;; Library and package version
    (let* ((entry
            (if (fboundp 'package-desc-create)
                (list 'flycheck (package-desc-create :name 'flycheck
                                                     :version '(0 5)
                                                     :summary "Foo"))
              (cons 'flycheck (vector '(0 5) nil "Foo"))))
           (package-alist (list entry)))
      (should (string= (format "%s (package: 0.5)" version)
                       (flycheck-version))))))

;;; version-test.el ends here
