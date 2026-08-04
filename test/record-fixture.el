;;; record-fixture.el --- Record checker output fixtures -*- lexical-binding: t; -*-

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

;; Record what a syntax checker's tool prints, so a spec can test the
;; parsing of that output without the tool being installed.
;;
;; The command comes from the checker itself, via the same substitution
;; Flycheck uses to run it, so a fixture cannot drift from the command
;; that produced it without the recording failing.
;;
;; Usage:
;;
;;     emacs -Q --batch -l test/record-fixture.el \
;;       -f flycheck-record-fixture-batch CHECKER RESOURCE [OUTPUT-FILE]
;;
;; RESOURCE is a path under test/resources.  With no OUTPUT-FILE the
;; fixture goes to test/fixtures/CHECKER/<resource-basename>.txt.

;;; Code:

(require 'flycheck)
(require 'seq)
(require 'subr-x)

(defconst flycheck-record-fixture--test-directory
  ;; Captured now, while this file is the one being loaded
  (file-name-directory (or load-file-name buffer-file-name))
  "The test directory, as of loading this file.")

(defvar flycheck-record-fixture-directory
  (expand-file-name "fixtures" flycheck-record-fixture--test-directory)
  "Directory holding recorded checker output.")

(defun flycheck-record-fixture-file (checker resource)
  "The file holding CHECKER's recorded output over RESOURCE.

Checker names can contain slashes, as `c/c++-gcc' does, so they are
flattened rather than read as directories."
  (expand-file-name
   (format "%s/%s.txt"
           (replace-regexp-in-string "/" "_" (symbol-name checker))
           (file-name-nondirectory resource))
   flycheck-record-fixture-directory))

(defun flycheck-record-fixture--resource (resource)
  "Absolute name of RESOURCE under the test resources directory."
  (expand-file-name
   resource
   (expand-file-name "resources" flycheck-record-fixture--test-directory)))

(defun flycheck-record-fixture-settings (checker)
  "Variables to bind while running CHECKER, as an alist.

Some checkers need pointing at a configuration file before they say
anything, the way the specs point them.  Recording and checking a
recording both go through here, so they cannot disagree about how
the tool was run."
  (cond
   ((memq checker '(css-stylelint scss-stylelint sass-stylelint less-stylelint))
    `((flycheck-stylelint-config
       . ,(flycheck-record-fixture--resource "language/css/.stylelintrc.json"))))))

(defun flycheck-record-fixture--run (checker file)
  "Run CHECKER's command over FILE and return (COMMAND EXIT-STATUS OUTPUT).

The command is the one Flycheck would run, so a fixture recorded
here matches what the checker actually invokes.  A checker that
reads standard input gets the file's contents there, like it would
during a real check."
  (with-current-buffer (find-file-noselect file)
    (let* ((settings (flycheck-record-fixture-settings checker))
           (_ (dolist (setting settings)
                (set (make-local-variable (car setting)) (cdr setting))))
           (args (flycheck-checker-substituted-arguments checker))
           (program (flycheck-find-checker-executable checker))
           (stdin (flycheck-checker-get checker 'standard-input))
           (source (buffer-string))
           (default-directory
            (if-let* ((wd (flycheck-checker-get checker 'working-directory)))
                (or (funcall wd checker) default-directory)
              default-directory)))
      (unless program
        (error "%s is not installed, so there is nothing to record" checker))
      (unwind-protect
          (with-temp-buffer
            (let ((status
                   (if stdin
                       (let ((input (current-buffer)))
                         (insert source)
                         (apply #'call-process-region (point-min) (point-max)
                                program 'delete (list input t) nil args))
                     (apply #'call-process program nil t nil args))))
              (list (cons program args) status (buffer-string))))
        ;; Substituting the arguments can leave a copy of the file beside
        ;; the original, which a real check would have cleaned up and
        ;; which the next tool to look at that directory would see
        (flycheck-safe-delete-temporaries)))))

(defconst flycheck-record-fixture--volatile-paths
  (list
   ;; The copy Flycheck checks, and the caches some checkers ask for,
   ;; live in directories named afresh each run
   "\\(?:[A-Za-z]:\\)?[^ \t\n\"'()]*[/\\\\]flycheck[A-Za-z0-9---]*[/\\\\]"
   ;; And a checker given the file itself sees wherever the repository is
   "\\(?:[A-Za-z]:\\)?[^ \t\n\"'()]*[/\\\\]test[/\\\\]resources[/\\\\]")
  "Patterns for the parts of a tool's output that differ between runs.")

(defun flycheck-record-fixture--stabilize (output)
  "Drop the directories in OUTPUT that differ between runs and machines.

A recording has to be reproducible for it to be worth comparing
against later, and it should not carry the absolute path of whoever
recorded it into the repository.  File names are kept; only the
directories leading to them go."
  (let ((result output))
    (dolist (pattern flycheck-record-fixture--volatile-paths result)
      (setq result (replace-regexp-in-string pattern "" result 'fixedcase)))))

(defun flycheck-record-fixture (checker resource &optional output-file)
  "Record CHECKER's output over RESOURCE into OUTPUT-FILE.

Return the file the fixture was written to."
  (let* ((file (flycheck-record-fixture--resource resource))
         (recorded (flycheck-record-fixture--run checker file))
         (command (nth 0 recorded))
         (status (nth 1 recorded))
         (output (flycheck-record-fixture--stabilize (nth 2 recorded)))
         (target (or output-file
                     (flycheck-record-fixture-file checker file))))
    (make-directory (file-name-directory target) 'parents)
    (with-temp-file target (insert output))
    (message "%s\n  command : %s\n  exit    : %s\n  bytes   : %d\n  written : %s"
             checker (mapconcat #'identity command " ") status
             (length output) target)
    target))

(defun flycheck-record-fixture-batch ()
  "Record a fixture from the remaining command-line arguments."
  (let ((checker (intern (pop command-line-args-left)))
        (resource (pop command-line-args-left))
        (target (pop command-line-args-left)))
    (flycheck-record-fixture checker resource target)))


;;; Checking recorded output against the tools

;; A recording says what a tool printed on the day it was recorded.  The
;; specs that read it keep passing whatever the tool does afterwards,
;; which is the point on a machine without the tool and a blind spot
;; everywhere else.  Running the tools again closes it, by asking whether
;; the checker can still read them rather than whether they print the
;; same bytes; see `flycheck-verify-fixtures'.

(defconst flycheck-record-fixture-unstable
  '((rust-clippy . "cargo reports whether the crate was already built, \
so its output depends on the state of the target directory"))
  "Checkers whose output cannot be compared between runs, and why.

Their recordings are still worth having, since the specs that read
them only care what Flycheck makes of the output.  It is the
comparison against a later run that cannot mean anything.")

(defconst flycheck-record-fixture-modules
  '((python-flake8 . "flake8")
    (python-pylint . "pylint"))
  "Checkers whose executable is an interpreter, and the module they run.

Finding the interpreter says nothing about whether the linter is
installed for it, so these are asked the fuller question.")

(defun flycheck-record-fixture-tool-available-p (checker)
  "Whether CHECKER's tool can actually run here."
  (when-let* ((program (flycheck-find-checker-executable checker)))
    (if-let* ((module (cdr (assq checker flycheck-record-fixture-modules))))
        (eq 0 (call-process program nil nil nil "-c" (format "import %s" module)))
      t)))

(defun flycheck-fixture--checker-of (directory)
  "The checker whose recordings live in DIRECTORY, or nil."
  (let ((name (intern (replace-regexp-in-string
                       "_" "/" (file-name-nondirectory
                                (directory-file-name directory))))))
    (and (flycheck-valid-checker-p name) name)))

(defun flycheck-fixture--resource-for (checker recorded)
  "The resource RECORDED came from, for CHECKER, or nil.

Recordings are named after the resource, so the resource is found
by matching that name under the resources directory."
  (let* ((base (file-name-base recorded))
         (root (expand-file-name
                "resources" flycheck-record-fixture--test-directory))
         (matches (directory-files-recursively
                   root (concat "\\`" (regexp-quote base) "\\'"))))
    (ignore checker)
    (when matches
      (file-relative-name (car matches) root))))

(defun flycheck-fixture--reads (checker output)
  "How many errors CHECKER reads out of OUTPUT."
  (length (flycheck-filter-errors
           (flycheck-parse-output output checker (current-buffer))
           checker)))

(defun flycheck-verify-fixtures ()
  "Check that every checker can still read what its tool prints.

The question is not whether the tool prints the same bytes as when
its output was recorded.  It will not: a recording is made on
somebody's machine, and the same checker faces a different build of
the tool elsewhere, so Clang answers for GCC and every version
differs in wording.  Comparing the text would report all of that,
every week, until nobody read the report.

What matters is whether Flycheck can still make sense of the
output.  Every bug this was built for looked the same from here:
jq grew a prefix, rebar3 replaced its format, the byte compiler
moved a warning onto one line, and each time the checker went from
reading errors to reading none.

So the tool is run again and its output parsed.  A checker that
read errors when the output was recorded and reads none now has
stopped understanding its tool.

Returns a list of (CHECKER . REASON) for those, skipping the ones
whose tool is not installed."
  (let (drifted (checked 0) (skipped 0) (unstable 0))
    (dolist (dir (directory-files flycheck-record-fixture-directory
                                  'full "\\`[^.]"))
      (when (file-directory-p dir)
        (if-let* ((checker (flycheck-fixture--checker-of dir)))
            (dolist (recorded (directory-files dir 'full "\\.txt\\'"))
              (let ((resource (flycheck-fixture--resource-for checker recorded)))
                (cond
                 ((not resource)
                  (push (cons checker "the resource it was recorded from is gone")
                        drifted))
                 ((assq checker flycheck-record-fixture-unstable)
                  (setq unstable (1+ unstable))
                  (message "  not compared: %s, because %s" checker
                           (cdr (assq checker
                                      flycheck-record-fixture-unstable))))
                 ((not (flycheck-record-fixture-tool-available-p checker))
                  (setq skipped (1+ skipped)))
                 (t
                  (setq checked (1+ checked))
                  (let* ((was (with-temp-buffer
                                (insert-file-contents recorded)
                                (buffer-string)))
                         (now (condition-case err
                                  (flycheck-record-fixture--stabilize
                                   (nth 2 (flycheck-record-fixture--run
                                           checker
                                           (flycheck-record-fixture--resource
                                            resource))))
                                (error (format "<could not run: %S>" err))))
                         (read-before (flycheck-fixture--reads checker was))
                         (read-now (flycheck-fixture--reads checker now)))
                    (cond
                     ((and (> read-before 0) (= read-now 0))
                      (push (cons checker
                                  (format "reads nothing out of %s now, \
where the recording gives it %d"
                                          (file-name-nondirectory
                                           (flycheck-checker-default-executable
                                            checker))
                                          read-before))
                            drifted))
                     ((not (equal was now))
                      ;; Worth saying, not worth failing over: a different
                      ;; build of the tool words things differently
                      (message "  %s prints something else than the \
recording, and still reads as %d error(s)"
                               checker read-now))))))))
          (push (cons (file-name-nondirectory (directory-file-name dir))
                      "is not a checker")
                drifted))))
    (message "checked %d recording(s); %d skipped for a missing tool, \
%d not comparable"
             checked skipped unstable)
    (nreverse drifted)))

(defun flycheck-verify-fixtures-batch ()
  "Report checkers that no longer read their tool, and exit non-zero."
  (let ((drifted (flycheck-verify-fixtures)))
    (if (null drifted)
        (message "Every checker still reads its tool.")
      (message "\n%d checker(s) no longer read their tool:" (length drifted))
      (dolist (d drifted)
        (message "  %s: %s" (car d) (cdr d)))
      (message "\nRun the tool by hand and compare with the recording under \
test/fixtures: the format has changed and the checker needs to follow it.")
      (kill-emacs 1))))

(provide 'record-fixture)

;;; record-fixture.el ends here
