;;; local-checkdoc-variables.el -- Silence checkdoc warnings with file-local variables -*- lexical-binding: t; -*-

;;; Commentary:

;; Make sure that file-local checkdoc settings are properly propagated to
;; checkdoc process.

;;; Code:

(defun imperative-mood ()
  "Uses an imperative mood, which is wrong.")

(defun arguments-in-wrong-order (a b)
  "Sum B and A."
  (+ a b))

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; End:

(provide 'local-checkdoc-variables)
;;; local-checkdoc-variables.el ends here
