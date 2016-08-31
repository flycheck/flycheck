;;; test-elixir.el --- Flycheck Specs: Elixir -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

;; Author: Kári Tristan Helgason <kari@kthelgason.org>

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

;; Specs for Elixir support.

;;; Code:

(require 'flycheck-buttercup)

(describe "Language Elixir"
  (describe "The Dogma error parser"
    (let ((json "{
                \"metadata\": {
                    \"dogma_version\": \"0.3.0\",
                    \"elixir_version\": \"1.0.5\",
                    \"erlang_version\": \"Erlang/OTP 10 [erts-7.0.3] [64-bit]\",
                    \"system_architecture\": \"x86_64-apple-darwin14.5.0\"
                },
                \"files\": [{
                    \"path\": \"lib/bar.ex\",
                    \"errors\": [{
                        \"line\": 1,
                        \"rule\": \"ModuleDoc\",
                        \"message\": \"Module without @moduledoc detected\"
                    }, {
                        \"line\": 14,
                        \"rule\": \"ComparisonToBoolean\",
                        \"message\": \"Comparison to a boolean is useless\"
                    }
                    ]
                }],
                \"summary\": {
                    \"error_count\": 2,
                    \"inspected_file_count\": 1
                }}"))
      (it "parses Dogma JSON output"
        (expect (flycheck-elixir--parse-dogma-json json 'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 1 1 'error
                                        "Module without @moduledoc detected"
                                        :id "ModuleDoc"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "lib/bar.ex")
                 (flycheck-error-new-at 14 1 'error
                                        "Comparison to a boolean is useless"
                                        :id "ComparisonToBoolean"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "lib/bar.ex")))))))

;;; test-elixir.el ends here
