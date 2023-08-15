;; custom-predicates.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 27 June 2012
;; Description:

(save-match-data
  (dolist (command '(plusp
                     minusp
                     zerop
                     integerp
                     integer-or-markerp
                     y-or-n-p
                     vidgetp
                     virtualenvp
                     vectorp
                     unsafep
                     typep
                     timerp
                     tailp
                     subsetp
                     subrp
                     symbolp
                     sequencep
                     ring-p
                     ring-empty-p
                     random-state-p
                     processp
                     bufferp
                     characterp
                     string-prefix-p

                     buffer-modified-p
                     file-exists-p
                     file-executable-p
                     file-readable-p
                     file-writable-p
                     file-accessible-directory-p
                     file-ownership-preserved-p
                     file-newer-than-file-p
                     file-symlink-p
                     file-directory-p
                     file-regular-p
                     local-variable-p))
    (let ((old-name (symbol-name command)))
      (cl-assert (= (aref old-name (1- (length old-name)))
                    (string-to-char "p")))
      (let* ((new-name (replace-regexp-in-string "-?p$"
                                                 "?"
                                                 old-name))
             (new-command (intern new-name)))
        ;; (message "(defalias %s %s)" new-command command)
        (defalias new-command command)))))

(save-match-data
  (dolist (command '(eq
                     equal
                     string-match
                     looking-at
                     string=
                     char=))
    (let ((old-name (symbol-name command)))
      (let* ((new-name (replace-regexp-in-string "-*$"
                                                 "?"
                                                 old-name))
             (new-command (intern new-name)))
        ;; (message "(defalias %s %s)" new-command command)
        (defalias new-command command)))))

(provide 'custom-predicates)

;; Local Variables:
;; End:

;; custom-predicates.el ends here
