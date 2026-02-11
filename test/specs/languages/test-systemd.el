;;; test-systemd.el --- Flycheck Specs: systemd      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for systemd support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language systemd"
  (flycheck-buttercup-def-checker-test systemd-analyze systemd nil
    (flycheck-buttercup-should-syntax-check
     "language/systemd-analyze-test.service" 'systemd-mode
     '(0 nil error "Service has no ExecStart=, ExecStop=, or SuccessAction=. Refusing."
         :checker systemd-analyze)
     '(3 nil error "Invalid URL, ignoring: foo://bar"
         :checker systemd-analyze)
     '(5 nil error "ListenStream= references a path below legacy directory /var/run/, updating /var/run/dbus/system_bus_socket \u2192 /run/dbus/system_bus_socket; please update the unit file accordingly."
         :filename "/lib/systemd/system/dbus.socket"
         :checker systemd-analyze)
     '(6 nil error "Unknown key name 'ExecSmart' in section 'Service', ignoring."
         :checker systemd-analyze)
     '(8 nil error "Unknown section 'Dog'. Ignoring."
         :checker systemd-analyze))))

;;; test-systemd.el ends here
