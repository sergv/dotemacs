;; eproj-common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 February 2025
;; Description:

(declare-function eproj--resolve-to-abs-path-cached "eproj")

(require 'eproj-tag-index)

(defmacro eproj-resolve-to-abs-path (path proj)
  `(if (file-name-absolute-p ,path)
       ,path
     ,(if proj
          `(eproj--resolve-to-abs-path-cached ,path (eproj-project/root ,proj))
        `(error "Path is not absolute and no project available to resolve it: %s"
                ,path))))

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
