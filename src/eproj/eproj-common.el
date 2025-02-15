;; eproj-common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 February 2025
;; Description:

(require 'eproj-tag-index)

(defun eproj/default-authoritative-key-func (identifier tag)
  "Resonable default method to produce key to compare similar tags for
authoritativeness."
  (cl-assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (cons identifier (eproj-tag/type tag)))

(defun eproj/extract-tag-line (proj tag)
  "Fetch line where TAG is defined."
  (cl-assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (for-buffer-with-file
      (eproj-resolve-to-abs-path (eproj-tag/file tag) proj)
    (with-inhibited-field-text-motion
      (save-excursion
        (goto-line-dumb (eproj-tag/line tag))
        (current-line-with-properties)))))

(provide 'eproj-common)

;; Local Variables:
;; End:

;; eproj-common.el ends here
