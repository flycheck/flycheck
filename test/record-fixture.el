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

RESOURCE keeps its path under the checker's directory, so the resource
a recording came from can be read back off the recording rather than
searched for: several of them share a name, and `main.rs' alone
matches four.

Checker names can contain slashes, as `c/c++-gcc' does, so those are
flattened rather than read as directories."
  (expand-file-name
   (concat (replace-regexp-in-string "/" "_" (symbol-name checker))
           "/" resource ".txt")
   flycheck-record-fixture-directory))

(defun flycheck-record-fixture-resource-of (recording)
  "The resource RECORDING was made from.

The inverse of `flycheck-record-fixture-file': the path under the
checker's directory is the resource, without the `.txt'."
  (let* ((root (file-name-as-directory flycheck-record-fixture-directory))
         (relative (file-relative-name recording root)))
    (unless (string-prefix-p ".." relative)
      (let ((under-checker (mapconcat #'identity
                                      (cdr (split-string relative "/")) "/")))
        (when (string-suffix-p ".txt" under-checker)
          (substring under-checker 0 (- (length ".txt"))))))))

(defun flycheck-record-fixture--resource (resource)
  "Absolute name of RESOURCE under the test resources directory."
  (expand-file-name
   resource
   (expand-file-name "resources" flycheck-record-fixture--test-directory)))

(defconst flycheck-record-fixture-quirks
  '((css-stylelint  :config "language/css/.stylelintrc.json")
    (scss-stylelint :config "language/css/.stylelintrc.json")
    (sass-stylelint :config "language/css/.stylelintrc.json")
    (less-stylelint :config "language/css/.stylelintrc.json")
    (python-flake8  :module "flake8")
    (python-pylint  :module "pylint")
    (rust-clippy    :uncomparable "cargo reports whether the crate was \
already built, so its output depends on the state of the target directory"))
  "What is unusual about a checker, when recording or checking a recording.

Each entry is a checker followed by a plist:

`:config'        a configuration file the tool needs pointing at before
                it says anything, as the specs point it
`:module'        the module a checker runs through an interpreter, whose
                presence says nothing about the linter being installed
`:uncomparable'  why a later run of this tool cannot be compared with a
                recording of it, and what to say instead of failing")

(defun flycheck-record-fixture-quirk (checker property)
  "PROPERTY of CHECKER from `flycheck-record-fixture-quirks', or nil."
  (plist-get (cdr (assq checker flycheck-record-fixture-quirks)) property))

(defun flycheck-record-fixture-settings (checker)
  "Variables to bind while running CHECKER, as an alist.

Some checkers need pointing at a configuration file before they say
anything, the way the specs point them.  Recording and checking a
recording both go through here, so they cannot disagree about how
the tool was run."
  (when-let* ((config (flycheck-record-fixture-quirk checker :config)))
    `((flycheck-stylelint-config . ,(flycheck-record-fixture--resource config)))))

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
                     ;; RESOURCE, not FILE: the recording keeps the
                     ;; resource's path, not the absolute one it was
                     ;; read from
                     (flycheck-record-fixture-file checker resource))))
    (make-directory (file-name-directory target) 'parents)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file target (insert output)))
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

(defun flycheck-record-fixture-tool-available-p (checker)
  "Whether CHECKER's tool can actually run here."
  (when-let* ((program (flycheck-find-checker-executable checker)))
    (if-let* ((module (flycheck-record-fixture-quirk checker :module)))
        (eq 0 (call-process program nil nil nil "-c" (format "import %s" module)))
      t)))

(defun flycheck-fixture--checker-of (directory)
  "The checker whose recordings live in DIRECTORY, or nil."
  (let ((name (intern (replace-regexp-in-string
                       "_" "/" (file-name-nondirectory
                                (directory-file-name directory))))))
    (and (flycheck-valid-checker-p name) name)))

(defun flycheck-fixture--reads (checker output)
  "How many errors CHECKER reads out of OUTPUT.

None, if reading it raises: a parser that chokes has understood the
output no better than one that finds nothing in it.  Cargo printing
that it has no library target rather than the JSON the checker
expects gets here."
  (condition-case nil
      (length (flycheck-filter-errors
               (flycheck-parse-output output checker (current-buffer))
               checker))
    (error 0)))

(defvar flycheck-verify-fixtures--checked 0
  "How many recordings the last check actually compared.")

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
            ;; Recordings keep the resource's path under the checker's
            ;; directory, so they are nested as deep as the resource was
            (dolist (recorded (directory-files-recursively dir "\\.txt\\'"))
              (let ((resource (flycheck-record-fixture-resource-of recorded)))
                (cond
                 ((not resource)
                  (push (cons checker "the resource it was recorded from is gone")
                        drifted))
                 ((flycheck-record-fixture-quirk checker :uncomparable)
                  (setq unstable (1+ unstable))
                  (message "  not compared: %s, because %s" checker
                           (flycheck-record-fixture-quirk
                            checker :uncomparable)))
                 ((not (flycheck-record-fixture-tool-available-p checker))
                  (setq skipped (1+ skipped)))
                 (t
                  (setq checked (1+ checked))
                  (let* ((was (with-temp-buffer
                                (let ((coding-system-for-read 'utf-8))
                                  (insert-file-contents recorded))
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
                     ((string-blank-p now)
                      ;; Nothing to read is not the same as unreadable:
                      ;; this build of the tool has no complaint about
                      ;; the file, the way Bash 5 allows what Bash 3
                      ;; refuses
                      (message "  %s: %s had nothing to say about %s here"
                               checker
                               (file-name-nondirectory
                                (flycheck-checker-default-executable checker))
                               (file-name-nondirectory resource)))
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
    (setq flycheck-verify-fixtures--checked checked)
    (message "checked %d recording(s); %d skipped for a missing tool, \
%d not comparable"
             checked skipped unstable)
    (nreverse drifted)))

(defun flycheck-verify-fixtures-batch ()
  "Report checkers that no longer read their tool, and exit non-zero."
  (let ((drifted (flycheck-verify-fixtures)))
    (when (zerop flycheck-verify-fixtures--checked)
      ;; Nothing compared reads as success and is not: either no tool
      ;; here is one we have a recording for, or the recordings stopped
      ;; being found where they are looked for
      (message "Nothing was compared.  If any of these tools are \
installed, the recordings are not being found.")
      (kill-emacs 1))
    (if (null drifted)
        (message "Every checker still reads its tool.")
      (message "\n%d checker(s) no longer read their tool:" (length drifted))
      (dolist (d drifted)
        (message "  %s: %s" (car d) (cdr d)))
      (message "\nRun the tool by hand and compare with the recording under \
test/fixtures: the format has changed and the checker needs to follow it.")
      (kill-emacs 1))))

;;; Recording everything that can be recorded

;; Which file to run a checker over is already written down, in the
;; resource its own spec checks, so the specs are read for it rather than
;; keeping a second list that could disagree with them.

(defun flycheck-record-fixture--walk (form fn)
  "Call FN on FORM and every sub-form of it.

Walks car and cdr rather than iterating, because specs contain
dotted pairs such as (disable . \"reason\")."
  (funcall fn form)
  (when (consp form)
    (flycheck-record-fixture--walk (car form) fn)
    (flycheck-record-fixture--walk (cdr form) fn)))

(defun flycheck-record-fixture--resource-in (body)
  "The first resource file BODY checks, if any."
  (let (found)
    (flycheck-record-fixture--walk
     body
     (lambda (f)
       (when (and (not found) (consp f)
                  (eq (car-safe f) 'flycheck-buttercup-should-syntax-check)
                  (stringp (nth 1 f)))
         (setq found (nth 1 f)))))
    found))

(defun flycheck-record-fixture-spec-resources ()
  "Map every checker a spec names to the resource that spec checks."
  (let ((table (make-hash-table :test #'eq))
        (dir (expand-file-name
              "specs/languages" flycheck-record-fixture--test-directory)))
    (dolist (file (directory-files dir t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (flycheck-record-fixture--walk
               (read (current-buffer))
               (lambda (f)
                 (when (and (consp f)
                            (eq (car-safe f)
                                'flycheck-buttercup-def-checker-test))
                   (let* ((raw (nth 1 f))
                          (checkers (if (listp raw) raw (list raw)))
                          (resource (flycheck-record-fixture--resource-in
                                     (nthcdr 4 f))))
                     (dolist (c checkers)
                       (when (and resource (not (gethash c table)))
                         (puthash c resource table))))))))
          (end-of-file nil))))
    table))

(defun flycheck-record-fixture-all ()
  "Record every checker whose tool is installed and whose spec names a file.

Returns a list of (CHECKER . REASON) for the ones that could not be
recorded."
  (let ((table (flycheck-record-fixture-spec-resources))
        (recorded 0) failed pairs)
    (maphash (lambda (c r) (push (cons c r) pairs)) table)
    (dolist (pair (sort pairs (lambda (a b)
                                (string< (symbol-name (car a))
                                         (symbol-name (car b))))))
      (let ((checker (car pair)) (resource (cdr pair)))
        (when (and (flycheck-checker-get checker 'command)
                   (flycheck-record-fixture-tool-available-p checker))
          (condition-case err
              (let* ((target (flycheck-record-fixture-file checker resource))
                     (previous (when (file-exists-p target)
                                 (with-temp-buffer
                                   (let ((coding-system-for-read 'utf-8))
                                     (insert-file-contents target))
                                   (buffer-string))))
                     (written (flycheck-record-fixture checker resource)))
                ;; A tool that will not start still prints something, and
                ;; writing that down gives a spec asserting how the
                ;; failure looks.  What the checker reads is the test of
                ;; whether the tool actually ran: credo without a mix.exs
                ;; and the Go checkers without a module land here.
                (if (> (flycheck-fixture--reads
                        checker (with-temp-buffer
                                  (let ((coding-system-for-read 'utf-8))
                                    (insert-file-contents written))
                                  (buffer-string)))
                       0)
                    (setq recorded (1+ recorded))
                  (if previous
                      ;; Somebody else's machine got something out of this
                      ;; tool.  Whatever is wrong with it here, throwing
                      ;; their recording away is not the answer.
                      (progn
                        (let ((coding-system-for-write 'utf-8))
                          (with-temp-file written (insert previous)))
                        (push (cons checker
                                    "reads nothing out of this tool here; \
kept the recording that was already there")
                              failed))
                    (delete-file written)
                    ;; And the directory it was the only thing in, so a
                    ;; checker with nothing recorded looks like one with
                    ;; nothing recorded
                    (ignore-errors
                      (delete-directory (file-name-directory written)))
                    (push (cons checker
                                "ran, but the checker reads nothing out of \
what it printed; record it by hand if that is really what it says")
                          failed))))
            (error (push (cons checker (error-message-string err)) failed))))))
    (message "\nrecorded %d, failed %d" recorded (length failed))
    (nreverse failed)))

(defun flycheck-record-fixture-all-batch ()
  "Record everything recordable, and say what could not be."
  (let ((failed (flycheck-record-fixture-all)))
    (when failed
      (message "\nnot recorded:")
      (dolist (f failed)
        (message "  %s: %s" (car f) (cdr f))))
    ;; Not an error: a tool that will not run here is the normal case,
    ;; and the recordings that did work are still worth keeping
    (message "\nReview what was written before committing it.  A recording \
of a tool failing to start is worse than no recording at all.")))

(provide 'record-fixture)

;;; record-fixture.el ends here
