;; Tests for Flycheck

(eval-when-compile
  (require 'cl))
(require 'ert)
(require 's)

(require 'flycheck)

;; Test the utility functions
(ert-deftest temp-file-system-no-filename ()
  "Test `flycheck-temp-file-system' without a filename."
  (let ((filename (flycheck-temp-file-system nil "flycheck-test")))
    (unwind-protect
        (progn
          (should-not (file-name-extension filename))
          (should (s-starts-with? "flycheck-test"
                                  (file-name-nondirectory filename)))
          (should (file-exists-p filename)))
      (ignore-errors (delete-file filename)))))

(ert-deftest temp-file-system-filename-no-extension ()
  "Test `flycheck-temp-file-system' with an extension."
  (let ((filename (flycheck-temp-file-system "spam/with/eggs" "flycheck-test")))
    (unwind-protect
        (progn
          (should-not (file-name-extension filename))
          (should (s-starts-with? "flycheck-test"
                                  (file-name-nondirectory filename)))
          (should (file-exists-p filename)))
      (ignore-errors (delete-file filename)))))

(ert-deftest temp-file-system-filename-extension ()
  "Test `flycheck-temp-file-system' works with a complete
  filename."
  (let ((filename (flycheck-temp-file-system "spam/with/eggs.el"
                                             "flycheck-test")))
    (unwind-protect
        (progn
          (should (string= (file-name-extension filename) "el"))
          (should (s-starts-with? "flycheck-test"
                                  (file-name-nondirectory filename)))
          (should (file-exists-p filename)))
      (ignore-errors (delete-file filename)))))

(ert-deftest temp-file-inplace-basename ()
  "Test `flycheck-temp-file-inplace' with a base name."
  (let ((filename (flycheck-temp-file-inplace "eggs.el" "flycheck-test")))
    (should (string= filename (expand-file-name "flycheck-test-eggs.el" nil)))
    (should-not (file-exists-p filename))))

(ert-deftest temp-file-inplace-path ()
  "Test `flycheck-temp-file-inplace' with complete path."
  (let ((filename (flycheck-temp-file-inplace "spam/with/eggs.el"
                                              "flycheck-test")))
    (should (string= filename (expand-file-name "flycheck-test-eggs.el"
                                                "spam/with")))
    (should-not (file-exists-p filename))))

(ert-deftest temp-file-inplace-no-filename ()
  "Test `flycheck-temp-file-inplace' without a path."
  (let ((filename (flycheck-temp-file-inplace nil "flycheck-test")))
    (unwind-protect
        (progn
          (should-not (file-name-extension filename))
          (should (s-starts-with? "flycheck-test"
                                  (file-name-nondirectory filename)))
          (should (file-exists-p filename)))
      (ignore-errors (delete-file filename)))))

(ert-deftest same-files-p ()
  "Test `flycheck-same-files-p'."
  (should (flycheck-same-files-p "../flycheck/flycheck.el"
                                 "../flycheck/flycheck.el"))
  (should (flycheck-same-files-p "../flycheck/flycheck.el" "flycheck.el"))
  (should-not (flycheck-same-files-p "../flycheck/flycheck.el" "tests.el")))

(ert-deftest save-buffer-to-file ()
  "Test `flycheck-save-buffer-to-file'."
  (let ((filename (expand-file-name "tests-temp")))
    (unwind-protect
        (with-temp-buffer
          (should-not (file-exists-p filename))
          (insert "Hello world")
          (flycheck-save-buffer-to-file filename)
          (should (file-exists-p filename))
          (with-temp-buffer
            (insert-file-contents-literally filename)
            (should (string= (buffer-string) "Hello world"))))
      (ignore-errors
        (delete-file filename)))))

(ert-deftest temp-buffer-copy-system-no-filename ()
  "Test `flycheck-temp-buffer-copy' with system tempfile and no
buffer filename."
  (with-temp-buffer
    (insert "Hello world")
    ;; No file name
    (let ((tempfile (flycheck-temp-buffer-copy
                     'flycheck-temp-file-system)))
      (unwind-protect
          (with-temp-buffer
            (should (file-exists-p tempfile))
            (should-not (file-name-extension tempfile))
            (should (s-starts-with? "flycheck"
                                    (file-name-nondirectory tempfile)))
            (insert-file-contents-literally tempfile)
            (should (string= (buffer-string) "Hello world")))
        (ignore-errors (delete-file tempfile))))))

(ert-deftest temp-buffer-copy-system-filename ()
  "Test `flycheck-temp-buffer-copy' with system tempfile and
buffer file name."
  (with-temp-buffer
    (setq buffer-file-name "testfile.txt")
    (insert "Hello world")
    ;; No file name
    (let ((tempfile (flycheck-temp-buffer-copy
                     'flycheck-temp-file-system)))
      (unwind-protect
          (with-temp-buffer
            (should (file-exists-p tempfile))
            (should (string= (file-name-extension tempfile) "txt"))
            (should (s-starts-with? "flycheck"
                                    (file-name-nondirectory tempfile)))
            (insert-file-contents-literally tempfile)
            (should (string= (buffer-string) "Hello world")))
        (ignore-errors (delete-file tempfile))))))

(ert-deftest temp-buffer-copy-inplace-no-filename ()
  "Test `flycheck-temp-buffer-copy' with inplace copy and no file
  name."
  (with-temp-buffer
    (insert "Hello world")
    (let ((tempfile (flycheck-temp-buffer-copy
                     'flycheck-temp-file-inplace)))
      (unwind-protect
          (with-temp-buffer
            (should (file-exists-p tempfile))
            (should-not (file-name-extension tempfile))
            (should (s-starts-with? "flycheck"
                                    (file-name-nondirectory tempfile)))
            (insert-file-contents-literally tempfile)
            (should (string= (buffer-string) "Hello world")))
        (ignore-errors (delete-file tempfile))))))

(ert-deftest temp-buffer-copy-inplace-filename ()
  "Test `flycheck-temp-buffer-copy' with inplace copy and file
  name."
  (with-temp-buffer
    (setq buffer-file-name "testfile.txt")
    (insert "Hello world")
    (let ((tempfile (flycheck-temp-buffer-copy
                     'flycheck-temp-file-inplace)))
      (unwind-protect
          (with-temp-buffer
            (should (file-exists-p tempfile))
            (should (string= tempfile
                             (expand-file-name "flycheck-testfile.txt")))
            (insert-file-contents-literally tempfile)
            (should (string= (buffer-string) "Hello world")))
        (ignore-errors (delete-file tempfile))))))


;; Test sanity of built-in registered checkers
(defmacro* ert-deftest-checkers (test () docstring &body body)
  "Define BODY as test over all `flycheck-checkers'.

Within BODY the current checker is bound to checker."
  (declare (doc-string 3)
           (indent 2))

  `(ert-deftest ,test ()
     ,docstring
     (dolist (checker flycheck-checkers)
       ,@body)))

(ert-deftest-checkers all-checkers-registered ()
  "Test that all `flycheck-checkers' are considered registered."
  (should (flycheck-registered-checker-p checker)))

(ert-deftest-checkers all-checkers-valid ()
  "Test that all `flycheck-checkers' are valid."
  (should (flycheck-valid-checker-p checker)))
