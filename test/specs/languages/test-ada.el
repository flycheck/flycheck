;;; test-ada.el --- Flycheck Specs: Ada -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Ada"
  (flycheck-buttercup-def-checker-test ada-gnat ada syntax-error
    (require 'speedbar)
    (flycheck-buttercup-should-syntax-check
     "language/ada/syntaxerror.adb" 'ada-mode
     '(7 32 error "missing \";\"" :checker ada-gnat)
     '(8 5 error "misspelling of \"SYNTAXERROR\"" :checker ada-gnat)))

  (flycheck-buttercup-def-checker-test ada-gnat ada warnings
    (require 'speedbar)
    (flycheck-buttercup-should-syntax-check
     "language/ada/hello.adb" 'ada-mode
     '(6 4 warning "variable \"Name\" is not referenced"
         :id "-gnatwu" :checker ada-gnat)
     '(8 11 warning "unrecognized pragma \"Foo\""
         :id "-gnatwg" :checker ada-gnat)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test ada-gnat "language/ada/syntaxerror.adb"
      '(7 32 error "missing \";\"")
      '(8 5 error "misspelling of \"SYNTAXERROR\""))

    (flycheck-buttercup-def-parse-test ada-gnat "language/ada/hello.adb"
      '(6 4 warning "variable \"Name\" is not referenced" :id "-gnatwu")
      '(8 11 warning "unrecognized pragma \"Foo\"" :id "-gnatwg"))

    (it "reads each level GNAT gives, with the switch as the id"
      (with-temp-buffer
        (expect
         (flycheck-buttercup-parse
          'ada-gnat
          (concat
           "elab.adb:3:04: warning: constant \"X\" is not referenced "
           "[-gnatwu]\n"
           "elab.adb:3:31: info: call to \"F\" during elaboration\n"
           "elab.adb:4:15: (style) space required [-gnatyt]\n"
           "elab.adb:7:32: error: missing \";\"\n"
           ;; GCC's back end, which also has its own switches
           "elab.adb:9:14: note: called from here\n"
           "elab.adb:1:1: warning: stack usage is 400016 bytes "
           "[-Wstack-usage=]\n"
           ;; Tags that name no switch
           "elab.adb:8:10: warning: violation of restriction \"No_Recursion\" "
           "at line 2 [restriction warning]\n"
           "elab.adb:4:04: error: variable \"A\" is assigned but never read "
           "[-gnatwm] [warning-as-error]\n"))
         :to-be-equal-flycheck-errors
         (list (flycheck-error-new-at
                3 4 'warning "constant \"X\" is not referenced"
                :id "-gnatwu" :checker 'ada-gnat
                :buffer (current-buffer) :filename "elab.adb")
               (flycheck-error-new-at
                3 31 'info "call to \"F\" during elaboration"
                :checker 'ada-gnat
                :buffer (current-buffer) :filename "elab.adb")
               (flycheck-error-new-at
                4 15 'warning "space required"
                :id "-gnatyt" :checker 'ada-gnat
                :buffer (current-buffer) :filename "elab.adb")
               (flycheck-error-new-at
                7 32 'error "missing \";\""
                :checker 'ada-gnat
                :buffer (current-buffer) :filename "elab.adb")
               (flycheck-error-new-at
                9 14 'info "called from here"
                :checker 'ada-gnat
                :buffer (current-buffer) :filename "elab.adb")
               (flycheck-error-new-at
                1 1 'warning "stack usage is 400016 bytes"
                :id "-Wstack-usage=" :checker 'ada-gnat
                :buffer (current-buffer) :filename "elab.adb")
               (flycheck-error-new-at
                8 10 'warning
                "violation of restriction \"No_Recursion\" at line 2"
                :checker 'ada-gnat
                :buffer (current-buffer) :filename "elab.adb")
               (flycheck-error-new-at
                4 4 'error "variable \"A\" is assigned but never read"
                :id "-gnatwm" :checker 'ada-gnat
                :buffer (current-buffer) :filename "elab.adb")))))))

;;; test-ada.el ends here
