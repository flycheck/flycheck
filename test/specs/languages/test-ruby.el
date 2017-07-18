;;; test-typescript.el --- Flycheck Specs: Ruby      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

;; Author: Saša Jovanić <sasa@simplify.ba>

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

;; Specs for Ruby support.

;;; Code:

(require 'flycheck-buttercup)

(describe "Language Ruby"
  (describe "The Reek error parser"
    (let ((json "[{\"context\":\"ApplicationController#validate_type\",
                   \"lines\":[15,15,16],
                   \"message\":\"calls 'params['data']' 3 times\",
                   \"smell_type\":\"DuplicateMethodCall\",
                   \"source\":\"app/controllers/application_controller.rb\",
                   \"name\":\"params['data']\",
                   \"count\":3,
                   \"wiki_link\":\"https://github.com/troessner/reek/blob/master/docs/Duplicate-Method-Call.md\"},
                  {\"context\":\"ApplicationController#pagination_meta\",
                   \"lines\":[52],
                   \"message\":\"doesn't depend on instance state (maybe move it to another class?)\",
                   \"smell_type\":\"UtilityFunction\",
                   \"source\":\"app/controllers/application_controller.rb\",
                   \"wiki_link\":\"https://github.com/troessner/reek/blob/master/docs/Utility-Function.md\"}]"))
      (it "parses Reek JSON output"
        (expect (flycheck-parse-reek json 'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 15 nil 'warning
                                        "ApplicationController#validate_type calls 'params['data']' 3 times"
                                        :id "DuplicateMethodCall"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "app/controllers/application_controller.rb")
                 (flycheck-error-new-at 16 nil 'warning
                                        "ApplicationController#validate_type calls 'params['data']' 3 times"
                                        :id "DuplicateMethodCall"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "app/controllers/application_controller.rb")
                 (flycheck-error-new-at 52 nil 'warning
                                        "ApplicationController#pagination_meta doesn't depend on instance state (maybe move it to another class?)"
                                        :id "UtilityFunction"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "app/controllers/application_controller.rb")))))))

;;; test-ruby.el ends here
