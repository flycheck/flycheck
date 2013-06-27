;;; test-configurable-display-function.el --- Test custom display functions -*- lexical-binding: t; -*-

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

(ert-deftest flycheck-uses-custom-display-function ()
  (mocker-let ((display-function (messages)
                                 ((:input '("test") :min-occur 1))))
    (let ((flycheck-display-error-messages-function 'display-function))
      (flycheck-display-error-messages "test"))))

(ert-deftest flycheck-uses-default-display-function ()
  (mocker-let ((flycheck-display-message-or-buffer (messages)
                                 ((:input '("test") :min-occur 1))))
    (flycheck-display-error-messages "test")))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-configurable-display-function.el ends here
