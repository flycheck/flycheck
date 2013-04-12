;;; test-global-mode.el --- Tests for the global mode -*- lexical-binding: t; -*-

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

;; Test the global mode setup.

;;; Code:

(require 'ert)
(require 'flycheck)

(ert-deftest flycheck-may-enable-mode-no-undo-buffers ()
  (with-temp-buffer
    (should-not (flycheck-may-enable-mode)))
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (rename-buffer " foo")
    (should (string= (buffer-name) " foo"))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode-tramp ()
  :expected-result :failed
  (error "Implement me: Find a dummy tramp backend to use for this test!"))

(ert-deftest flycheck-may-enable-mode-no-checker-found ()
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (text-mode)
    (should-not (s-starts-with? " " (buffer-name)))
    (should-not (flycheck-get-checker-for-buffer))
    (should-not (flycheck-may-enable-mode))))

(ert-deftest flycheck-may-enable-mode-checker-found ()
  (with-temp-buffer
    (setq buffer-file-name "foo")
    (rename-buffer "foo")
    (emacs-lisp-mode)
    (should (flycheck-get-checker-for-buffer))
    (should (flycheck-may-enable-mode))))

(defmacro flycheck-with-global-mode (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn
         (global-flycheck-mode 1)
         ,@body)
     (global-flycheck-mode -1)))

(ert-deftest flycheck-global-mode-no-undo-buffers ()
  (flycheck-with-global-mode
    (with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer " foo")
      (emacs-lisp-mode)
      (should-not flycheck-mode))))

(ert-deftest flycheck-global-mode-no-checker-found ()
  (flycheck-with-global-mode
    (with-temp-buffer
      (rename-buffer "foo")
      (text-mode)
      (should-not flycheck-mode))))

(ert-deftest flycheck-global-mode-checker-found ()
  (flycheck-with-global-mode
    (with-temp-buffer
      (setq buffer-file-name "foo")
      (rename-buffer "foo")
      (emacs-lisp-mode)
      (should flycheck-mode))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-global-mode.el ends here
