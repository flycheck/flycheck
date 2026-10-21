;;; test-proselint.el --- Flycheck Specs: Proselint -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)
(require 'markdown-ts-mode nil t)

(describe "Language Proselint"

  (describe "flycheck--proselint-args"
    (before-each
      (clrhash flycheck--proselint-old-args-by-host))

    (it "probes a host once and caches the detected version"
      (spy-on 'flycheck--proselint-version :and-return-value "0.14.0")
      (let ((default-directory "/tmp/"))
        (expect (flycheck--proselint-args) :to-equal '("--json" "-"))
        (expect (flycheck--proselint-args) :to-equal '("--json" "-")))
      (expect 'flycheck--proselint-version :to-have-been-called-times 1))

    (it "detects the version independently on each host"
      ;; `file-remote-p' parses the prefix without connecting, so the probe
      ;; can be faked per host without a live remote.
      (spy-on 'flycheck--proselint-version :and-call-fake
              (lambda (&rest _)
                (if (file-remote-p default-directory) "0.16.0" "0.14.0")))
      (let ((default-directory "/tmp/"))
        (expect (flycheck--proselint-args) :to-equal '("--json" "-")))
      (let ((default-directory "/ssh:host:/tmp/"))
        (expect (flycheck--proselint-args)
                :to-equal '("check" "--output-format=json")))
      (expect 'flycheck--proselint-version :to-have-been-called-times 2)))

  (flycheck-buttercup-def-checker-test proselint (text markdown) nil
    (let ((flycheck-disabled-checkers '(markdown-markdownlint-cli markdown-markdownlint-cli2 markdown-mdl markdown-pymarkdown)))
      (flycheck-buttercup-with-env '(("LC_ALL" . nil))
        (flycheck-buttercup-should-syntax-check
         "language/text/text.txt" '(text-mode markdown-mode markdown-ts-mode)
         '(1 7 warning "Substitute 'damn' every time you're inclined to write 'very'; your editor will delete it and the writing will be just as it should be."
             :id "weasel_words.very"
             :checker proselint
             :end-line 1
             :end-column 12)
         '(2 4 warning "Redundancy. Use 'associate' instead of 'associate together'."
             :id "redundancy.garner"
             :checker proselint
             :end-line 3
             :end-column 1)
         '(3 5 warning "Gender bias. Use 'lawyer' instead of 'lady lawyer'."
             :id "sexism.misc"
             :checker proselint
             :end-line 3
             :end-column 17)))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test proselint "language/text/text.txt"
      '(1 1 warning "Substitute 'damn' every time you're inclined to write 'very'; your editor will delete it and the writing will be just as it should be." :id "weasel_words.very" :end-line 1 :end-column 1)
      '(1 1 warning "Redundancy. Use 'associate' instead of 'associate together'." :id "redundancy.misc.garner" :end-line 1 :end-column 1)
      '(1 1 warning "Gender bias. Use 'lawyer' instead of 'lady lawyer'." :id "social_awareness.sexism" :end-line 1 :end-column 1))))

;;; test-proselint.el ends here
