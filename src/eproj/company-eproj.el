;; company-eproj.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 15 February 2016
;; Description:

(require 'company-mode-setup)

(defparameter company-eproj-ignore-case nil)

(defun company-eproj (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (pcase command
    (`interacative (company-begin-backend 'company-eproj))
    (`prefix (and (not (company-in-string-or-comment))
                  (company-grab-symbol)))
    (`candidates
     (let* ((completion-ignore-case company-eproj-ignore-case)
            (proj (eproj-get-project-for-buf (current-buffer)))
            (related-projs (eproj-get-all-related-projects proj))
            (all-projs (cons proj related-projs))
            (effective-major-mode (eproj-symbnav/resolve-synonym-modes major-mode))
            (show-tag-kind-func (aif (gethash effective-major-mode eproj/languages-table)
                                  (eproj-language/show-tag-kind-procedure it)
                                  (error "Cannot find language definition for mode %s" effective-major-mode)))
            (tag-tables (-non-nil
                         (--map (cdr-safe (assq effective-major-mode (eproj-project/tags it)))
                                all-projs)))
            (all-matches (-mapcat (lambda (table)
                                    (-mapcat (lambda (completion)
                                               (let ((tags (gethash completion table)))
                                                 (-map (lambda (tag)
                                                         (let ((new-completion (copy-sequence completion)))
                                                           (set-text-properties
                                                            0
                                                            (length new-completion)
                                                            (list
                                                             :file (eproj-tag/file tag)
                                                             :line (eproj-tag/line tag)
                                                             ;; :tag tag
                                                             :kind (funcall show-tag-kind-func tag))
                                                            new-completion)
                                                           new-completion))
                                                       tags)))
                                             (all-completions arg table)))
                                  tag-tables)))
       all-matches
       )
     ;; (remove-if-not
     ;;  (lambda (c) (string-prefix-p arg c))
     ;;  sample-completions)
     )
    (`annotation
     (format " [%s]" (get-text-property 0 :kind arg)))
    ;; (`ignore-case company-eproj-ignore-case)
    ;; (`sorted t)
    ))

(provide 'company-eproj)

;; Local Variables:
;; End:

;; company-eproj.el ends here
