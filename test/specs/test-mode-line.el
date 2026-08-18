;;; test-mode-line.el --- Flycheck Specs: Mode Line  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Specs for mode line reporting.

;;; Code:

(require 'flycheck-buttercup)
(require 'flymake)
(require 'project)

(describe "Mode Line"
  (it "shows the number of errors, warnings, and infos"
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'warning "warning 1")
                 (flycheck-error-new-at 2 2 'warning "warning 2")
                 (flycheck-error-new-at 1 1 'error "error"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC:1|2|0")))

  (it "shows the number of infos"
    (let ((flycheck-current-errors
           (list (flycheck-error-new-at 1 1 'info "info"))))
      (expect (flycheck-mode-line-status-text 'finished)
              :to-equal " FlyC:0|0|1")))

  (it "includes the prefix"
    (let ((flycheck-mode-line-prefix "foobar")
          flycheck-current-errors)
      (expect (flycheck-mode-line-status-text 'finished) :to-equal " foobar:0")))

  (it "makes the error counts clickable"
    (let* ((flycheck-current-errors
            (list (flycheck-error-new-at 1 1 'error "boom")))
           (text (flycheck-mode-line-status-text 'finished))
           (counts-start (string-match ":" text)))
      (expect (get-text-property counts-start 'local-map text)
              :to-be flycheck-mode-line-counts-map)
      (expect (get-text-property counts-start 'help-echo text)
              :to-equal "mouse-1: list errors")))

  (it "explains a status that reports no counts, and offers a way to look"
    ;; These indicators are a single opaque character, which is exactly
    ;; when a user needs to be told what it means
    (dolist (status '(no-checker running errored interrupted suspicious))
      (let* ((text (flycheck-mode-line-status-text status))
             (pos (1- (length text))))
        (expect (get-text-property pos 'local-map text)
                :to-be flycheck-mode-line-status-map)
        (expect (get-text-property pos 'help-echo text)
                :to-match "mouse-1: check this buffer's setup")
        (expect (get-text-property pos 'help-echo text)
                :to-match (alist-get status flycheck-mode-line-status-help)))))

  (it "leaves the empty not-checked indicator alone"
    (let ((text (flycheck-mode-line-status-text 'not-checked)))
      (expect text :to-equal " FlyC")
      (expect (get-text-property (1- (length text)) 'local-map text)
              :to-be nil)))

  (it "keeps the error counts pointing at the error list, not the setup"
    (let* ((flycheck-current-errors
            (list (flycheck-error-new-at 1 1 'error "boom")))
           (text (flycheck-mode-line-status-text 'finished)))
      (expect (get-text-property (1- (length text)) 'local-map text)
              :to-be flycheck-mode-line-counts-map)))

  (it "binds mouse-1 of a countless status to the setup verification"
    (expect (lookup-key flycheck-mode-line-status-map [mode-line mouse-1])
            :to-be 'flycheck-mode-line-verify-setup)
    (expect (lookup-key flycheck-mode-line-status-map [header-line mouse-1])
            :to-be 'flycheck-mode-line-verify-setup))

  (it "binds mouse-1 to the error list"
    (expect (lookup-key flycheck-mode-line-counts-map [mode-line mouse-1])
            :to-be 'flycheck-mode-line-list-errors)
    (expect (lookup-key flycheck-mode-line-counts-map [header-line mouse-1])
            :to-be 'flycheck-mode-line-list-errors))

  (describe "project scope"

    (before-each
      (spy-on 'project-current :and-return-value nil)
      (clrhash flycheck--project-error-store)
      (clrhash flycheck--project-counts-cache))

    (it "counts the project's diagnostics instead of the buffer's"
      (flycheck-buttercup-with-temp-buffer
        (setq default-directory
              (file-name-as-directory (expand-file-name "/proj")))
        (flycheck--project-record-errors
         (list (flycheck-error-new-at 3 1 'error "elsewhere"
                                      :filename "/proj/other.go"
                                      :checker 'x)))
        (let ((flycheck-mode-line-scope 'project)
              (flycheck-current-errors nil))
          (expect (flycheck-mode-line-status-text 'finished)
                  :to-equal " FlyC:1|0|0"))))

    (it "colors the indicator by the project counts"
      (flycheck-buttercup-with-temp-buffer
        (setq default-directory
              (file-name-as-directory (expand-file-name "/proj")))
        (flycheck--project-record-errors
         (list (flycheck-error-new-at 3 1 'error "elsewhere"
                                      :filename "/proj/other.go"
                                      :checker 'x)))
        (let* ((flycheck-mode-line-scope 'project)
               (flycheck-current-errors nil)
               (text (flycheck-mode-line-status-text 'finished)))
          (expect (get-text-property 0 'face text) :to-equal 'error))))

    (it "refreshes the cached counts when the store changes"
      (flycheck-buttercup-with-temp-buffer
        (setq default-directory
              (file-name-as-directory (expand-file-name "/proj")))
        (flycheck--project-record-errors
         (list (flycheck-error-new-at 3 1 'error "one"
                                      :filename "/proj/a" :checker 'x)))
        (let ((flycheck-mode-line-scope 'project)
              (flycheck-current-errors nil))
          (expect (flycheck-mode-line-status-text 'finished)
                  :to-equal " FlyC:1|0|0")
          (flycheck--project-record-errors
           (list (flycheck-error-new-at 4 1 'warning "two"
                                        :filename "/proj/b" :checker 'x)))
          (expect (flycheck-mode-line-status-text 'finished)
                  :to-equal " FlyC:1|1|0"))))

    (it "keeps the buffer's counts under the default scope"
      (flycheck-buttercup-with-temp-buffer
        (setq default-directory
              (file-name-as-directory (expand-file-name "/proj")))
        (flycheck--project-record-errors
         (list (flycheck-error-new-at 3 1 'error "elsewhere"
                                      :filename "/proj/other.go"
                                      :checker 'x)))
        (let ((flycheck-current-errors nil))
          (expect (flycheck-mode-line-status-text 'finished)
                  :to-equal " FlyC:0"))))

    (it "hears about diagnostics eglot parks for unvisited files"
      ;; Those never pass through Flycheck, so a variable watcher on
      ;; flymake's parking spot keeps the cached counts honest.
      (let ((before flycheck--project-diagnostics-generation))
        (let ((flymake-list-only-diagnostics nil))
          (ignore flymake-list-only-diagnostics))
        (expect flycheck--project-diagnostics-generation
                :to-be-greater-than before))))

  (it "explains suppressed errors in the tooltip of the whole indicator"
    (flycheck-buttercup-with-temp-buffer
      (setq flycheck--suppressed-error-count 7)
      (let* ((flycheck-current-errors
              (list (flycheck-error-new-at 1 1 'error "boom")))
             (text (flycheck-mode-line-status-text 'finished))
             (counts-start (string-match ":" text))
             (plus-pos (string-match (rx "+" eos) text)))
        ;; The truncation marker is part of the clickable segment
        (expect (get-text-property plus-pos 'local-map text)
                :to-be flycheck-mode-line-counts-map)
        ;; The tooltip explains the suppression anywhere on the counts
        (dolist (pos (list counts-start plus-pos))
          (expect (get-text-property pos 'help-echo text)
                  :to-match "7 more errors not shown"))))))

;;; test-mode-line.el ends here
