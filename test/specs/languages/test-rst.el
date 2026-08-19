;;; test-rst.el --- Flycheck Specs: reStructuredText -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(defun test-rst/sphinx-warning-types-p ()
  "Whether the installed sphinx-build tags diagnostics with their type.
Sphinx 8 turned `show_warning_types' on by default, appending
e.g. [ref.envvar], which the checker reads as the error's id."
  (when-let* ((sphinx (flycheck-find-checker-executable 'rst-sphinx))
              (version (with-temp-buffer
                         (and (eq 0 (call-process sphinx nil t nil
                                                  "--version"))
                              (goto-char (point-min))
                              (re-search-forward
                               "sphinx-build \\([0-9.]+\\)" nil t)
                              (match-string 1)))))
    (version<= "8" version)))

(describe "Language reStructuredText"
  (flycheck-buttercup-def-checker-test rst rst nil
    (flycheck-buttercup-should-syntax-check
     "language/rst/errors.rst" 'rst-mode
     '(8 nil warning "Title underline too short." :checker rst)
     '(14 nil error "Unexpected section title." :checker rst)
     '(16 nil error "Unknown target name: \"restructuredtext\"." :checker rst)
     '(19 nil warning "Title underline too short." :checker rst)
     '(21 nil error "Unknown target name: \"cool\"." :checker rst)
     '(26 nil error "Unexpected section title." :checker rst)))

  (flycheck-buttercup-def-checker-test rst-sphinx rst nil
    ;; Sphinx 8+ tags each diagnostic with its type, which becomes the id;
    ;; the messages read the same on either side of that.
    (let ((tagged (test-rst/sphinx-warning-types-p)))
      (flycheck-buttercup-should-syntax-check
       "language/rst/sphinx/index.rst" 'rst-mode
       '(2 nil warning "Title underline too short." :checker rst-sphinx)
       `(9 nil error "Unknown target name: \"cool\"."
           :id ,(and tagged "docutils") :checker rst-sphinx)
       `(9 nil warning "'envvar' reference target not found: FOO"
           :id ,(and tagged "ref.envvar") :checker rst-sphinx))))

  (flycheck-buttercup-def-checker-test rst-sphinx rst not-outside-of-a-sphinx-project
    (flycheck-buttercup-with-resource-buffer "language/rst/errors.rst"
      (rst-mode)
      (expect (flycheck-may-use-checker 'rst-sphinx) :not :to-be-truthy)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test rst "language/rst/errors.rst"
      '(8 nil warning "Title underline too short.")
      '(14 nil error "Unexpected section title.")
      '(19 nil warning "Title underline too short.")
      '(26 nil error "Unexpected section title.")
      '(16 nil error "Unknown target name: \"restructuredtext\".")
      '(21 nil error "Unknown target name: \"cool\"."))
    (flycheck-buttercup-def-parse-test rst-sphinx "language/rst/sphinx/index.rst"
      '(2 nil warning "Title underline too short.")
      '(9 nil error "Unknown target name: \"cool\"." :id "docutils")
      '(9 nil warning "'envvar' reference target not found: FOO"
          :id "ref.envvar"))))

;;; test-rst.el ends here
