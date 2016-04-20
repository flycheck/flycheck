;;; release.el --- Flycheck: Create and publish a release  -*- lexical-binding: t; -*-

;; Copyright (c) 2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Keywords: abbrev

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

;; Create and tag a new Flycheck release.
;;
;; - TODO: Package archive and publish on Github

;;; Code:

(require 'json)
(require 'lisp-mnt)
(require 'rst)
(eval-when-compile (require 'let-alist))

(defvar url-http-end-of-headers)
(defvar url-request-extra-headers)

(setq debug-on-error t)

(defconst flycheck/this-file
  (if load-in-progress load-file-name (buffer-file-name))
  "Path to this file.")

(defconst flycheck/source-dir (locate-dominating-file flycheck/this-file "Cask")
  "Source code directory of Flycheck.")

(defconst flycheck/lib
  (expand-file-name "flycheck.el" flycheck/source-dir)
  "Path to flycheck.")

(defconst flycheck/changelog
  (expand-file-name "CHANGES.rst" flycheck/source-dir)
  "Path to Flycheck's changelog.")

(defconst flycheck/travis-endpoint
  "https://api.travis-ci.org/repos/flycheck/flycheck"
  "Travis CI endpoint.")

(defun flycheck/current-version ()
  "Get the current Flycheck version."
  (lm-version flycheck/lib))

(defun flycheck/current-branch ()
  "Get the name of the current banch."
  (car (process-lines "git" "rev-parse" "--abbrev-ref" "HEAD")))

(defun flycheck/snapshot-version-p (version)
  "Whether VERSION is a snapshot version."
  (let ((snapshot-part (cadr (version-to-list version))))
    (and snapshot-part (< snapshot-part 0))))

(defun flycheck/git-or-fail (&rest args)
  "Call git with ARGS.

Fail if git returns a non-zero exit code."
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil t nil args)))
      (unless (eq status 0)
        (error "Git exited with status %s (args: %S):
%s" status args (buffer-string))))))

(defun flycheck/working-tree-changed-p ()
  "Whether the working tree has changed.

Return non-nil if the working tree or the index have changed, nil
otherwise."
  (let* ((default-directory flycheck/source-dir)
         (staged (call-process "git" nil nil nil "diff-index"
                               "--quiet" "--cached" "HEAD"))
         (unstaged (call-process "git" nil nil nil "diff-files" "--quiet")))
    (> (+ staged unstaged) 0)))

(defun flycheck/untracked-files-p ()
  "Whether the working tree has untracked files."
  (let* ((default-directory flycheck/source-dir)
         (files (process-lines "git" "ls-files"
                              "--exclude-standard" "--others")))
    (> (length files) 0)))

(defun flycheck/travis-build-state ()
  "Get the latest build state from Travis CI.

Return `(COMMIT-SHA . PASSED)' where COMMIT-SHA is the SHA1 of
the tested commit and PASSED is non-nil when the build passed."
  (with-current-buffer
    (let ((url-request-extra-headers '(("Accept" . "application/vnd.travis-ci.2+json"))))
      (url-retrieve-synchronously (concat flycheck/travis-endpoint
                                          "/branches/master") 'silent))
  (goto-char url-http-end-of-headers)
  (let-alist (json-read)
    (cons .commit.sha (equal .branch.state "passed")))))

(defun flycheck/next-version (current-version)
  "Get the next version after CURRENT-VERSION."
  (pcase (version-to-list current-version)
    ;; If the version is a snapshot just drop the snapshot part
    (`(,num ,snapshot)
     (unless (< snapshot 0)
       (error "Version %S lacks snapshot" current-version))
     (number-to-string num))
    (`(,num)
     ;; If the version is proper release then increase it and add a snapshot tag
     (format "%s-cvs" (1+ num)))))

(defun flycheck/set-version (new-version)
  "Set the version of Flycheck to NEW-VERSION."
  (with-temp-file flycheck/lib
    (insert-file-contents flycheck/lib)
    (goto-char (point-min))
    (re-search-forward (rx "Version:" (* (any space))
                           (group (1+ (any digit))
                                  (* (not (any space))))
                           eol))
    (replace-match new-version 'fixedcase 'literal nil 1)))

(defun flycheck/commit-tag-push-release (version)
  "Commit and tag a release with VERSION."
  (flycheck/git-or-fail "add" flycheck/lib flycheck/changelog)
  (flycheck/git-or-fail "commit" "-m" (format "Release version %s" version))
  (flycheck/git-or-fail "git" "tag" "--sign"
                        "-m" (format "Flycheck %s" version)
                        version)
  (flycheck/git-or-fail "push" "origin" "master")
  (flycheck/git-or-fail "push" "--tags" "origin"))

(defun flycheck/commit-and-push-master ()
  "Commit and push the new snapshot version for master."
  (flycheck/git-or-fail "add" flycheck/lib flycheck/changelog)
  (flycheck/git-or-fail "commit" "-m" "Bump version in master")
  (flycheck/git-or-fail "push" "origin" "master"))

(defun flycheck/finalize-release-in-changelog (current next date)
  "Finalise the changelog entry for CURRENT version.

NEXT is the version to use for the finalized header, and DATE is
a date to show in the header, i.e. the release date."
  (with-temp-file flycheck/changelog
    (insert-file-contents flycheck/changelog)
    (rst-mode)
    (goto-char (point-min))
    (unless (looking-at-p (regexp-quote (format "%s (in development)"
                                                current)))
      (error "Could not find snapshot version header in %s"
             flycheck/changelog))
    (kill-line)
    (insert (format "%s (%s)" next (format-time-string "%b %d, %Y" date)))
    (rst-adjust nil)))

(defun flycheck/add-snapshot-to-changelog (version)
  "Add a new snapshot header for VERSION to the changelog."
  (with-temp-file flycheck/changelog
    (insert-file-contents flycheck/changelog)
    (rst-mode)
    (goto-char (point-min))
    (insert (format "%s (in development)
====

" version))
    (goto-char (point-min))
    ;; Expand the adornment over the full line
    (rst-adjust nil)))

(defun flycheck/build-dist ()
  "Build the package for Flycheck."
  (let* ((default-directory flycheck/source-dir)
         (status (call-process "cask" nil nil nil "package")))
    (unless (eq status 0)
      (error "Failed to cask package with status %s" status))))

(defun flycheck/check-releasable ()
  "Check whether we can release in the current state.

Signals an error if

- not on the master branch
- dirty working tree
- untracked files
- Failed Travis CI build

Otherwise returns normally."
  (let ((branch (flycheck/current-branch)))
    (unless (equal branch "master")
      (user-error "Cannot make release from branch %s.  Switch to master"
                  branch)))
  (when (flycheck/working-tree-changed-p)
    (user-error "Cannot make release with dirty working directory.
Please commit or stash all changes"))
  (when (flycheck/untracked-files-p)
    (user-error "Cannot make release with untracked files.
Please commit or stash all changes"))
  (pcase-let ((`(,test-commit . ,passed) (flycheck/travis-build-state))
              (current-commit (car (process-lines "git" "rev-parse" "HEAD"))))
    (unless (equal test-commit current-commit)
      (user-error "HEAD not tested on Travis CI.
Please push your changes, wait for the Travis build and try again"))
    (unless passed
      (user-error "Travis CI build for HEAD failed.
Please fix the error, push, wait for the Travis CI build and try again"))))

(defun flycheck/make-release ()
  "Create and publish a Flycheck release."
  (flycheck/check-releasable)
  (let* ((current-version (flycheck/current-version))
         (new-version (flycheck/next-version current-version)))
    (unless (flycheck/snapshot-version-p current-version)
      (user-error "Cannot make a release from a non-snapshot version!"))
    (when (y-or-n-p (format "Releasing Flycheck %s, are you sure? "
                            new-version))
      ;; Bump version in Flycheck and in the changelog
      (flycheck/set-version new-version)
      (flycheck/finalize-release-in-changelog current-version new-version
                                              (current-time))
      (flycheck/commit-tag-push-release new-version)
      (flycheck/build-dist)
      (let ((new-snapshot (flycheck/next-version new-version)))
        (flycheck/set-version new-snapshot)
        (flycheck/add-snapshot-to-changelog new-snapshot)
        (flycheck/commit-and-push-master)
        (message "Flycheck %s out now; please announce in the flycheck/flycheck Gitter channel!
Please add information about the release to https://github.com/flycheck/flycheck/releases/edit/%s
and upload `dist/flycheck-%s.tar'.

Next version is %s" new-version new-version new-version new-snapshot)))))

(defun flycheck/make-release-batch ()
  "Make a release and exit Emacs."
  (flycheck/make-release)
  (kill-emacs 0))

;;; release.el ends here
