;;; test-version.el --- Tests for version functions -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ert)
(require 'flycheck)


(ert-deftest flycheck-library-version ()
  :expected-result (if (executable-find "carton") :passed :failed)
  (let* ((source-dir (expand-file-name ".." flycheck-testsuite-dir))
         (default-directory (file-name-as-directory source-dir))
         (version (car (process-lines "carton" "version"))))
    (should (string= version (flycheck-library-version)))))

(ert-deftest flycheck-package-version ()
  (require 'package)
  (let ((package-alist '((flycheck . [(2 12) nil "Foo"]))))
    (should (string= "2.12" (flycheck-package-version)))))

(ert-deftest flycheck-version ()
  (let* ((source-dir (expand-file-name ".." flycheck-testsuite-dir))
         (default-directory (file-name-as-directory source-dir))
         (version (car (process-lines "carton" "version"))))
    ;; Just the library version
    (should (string= version (flycheck-version)))
    ;; Library and package version
    (let ((package-alist '((flycheck . [(0 5) nil "Foo"]))))
      (should (string= (format "%s (package: 0.5)" version)
                       (flycheck-version))))))


;; Local Variables:
;; coding: utf-8
;; End:

;;; test-version.el ends here
