;;; test-help.el --- Tests for checker help -*- lexical-binding: t; -*-

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

(ert-deftest flycheck-describe-checker-navigate-to-source ()
  "Test that checkers are properly described."
  (unwind-protect
      (progn
        (flycheck-describe-checker 'bash)
        (let ((help-buffer (get-buffer (help-buffer))))
          (should help-buffer)
          (with-current-buffer help-buffer
            (goto-char (point-min))
            (re-search-forward "`.*?'")
            (should (string= (match-string 0) "`flycheck.el'"))
            (push-button (+ 2 (match-beginning 0)))
            (unwind-protect
                (progn
                  (should (string= (buffer-name) "flycheck.el"))
                  (should (string=
                           (buffer-substring-no-properties
                            (point) (line-end-position))
                           "(flycheck-declare-checker bash")))
              (kill-buffer)))))
    (let ((help-buffer (get-buffer (help-buffer))))
      (when (buffer-live-p help-buffer)
        (kill-buffer help-buffer)))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-help.el ends here
