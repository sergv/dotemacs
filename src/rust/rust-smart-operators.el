;; rust-smart-operators.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 November 2019
;; Description:

(require 'smart-operators-utils)

(defvar rust-smart-operators--operator-chars
  (let ((tbl (make-hash-table :test #'eq)))
    (loop
      for c across "+-*/%^&|<>=" ;; ! - too special, used for macro calls
      do (puthash c t tbl))
    tbl)
  "Characters that may constitute operators.")

(defun rust-smart-operators--insert-char-surrounding-with-spaces (char)
  "Insert CHARacter while trying to surround it with spaces and
stick it to the previous operator on line."
  (rust-smart-operators--insert-char-optionally-surrounding-with-spaces char t))

(defun rust-smart-operators--insert-char-optionally-surrounding-with-spaces (char insert-space-after)
  (let* ((disable-smart-operators? current-prefix-arg)
         (pt (point))
         (before (char-before pt))
         (at-beginning-of-buffer? (null before)))
    (cond
      ((or disable-smart-operators?
           (smart-operators--literal-insertion?)
           (not (gethash char rust-smart-operators--operator-chars)))
       (insert-char char))
      (t
       (let ((whitespace-deleted? nil)
             (after (char-after pt)))
         ;; Decide whether to insert space before the operator.
         (if (and (not (smart-operators--on-empty-string?))
                  (not (memq char '(?\< ?\>)))
                  (if (char-equal char ?\|)
                      ;; Check if we're preceded by '...(|..._|_'.
                      (save-excursion
                        (skip-chars-backward "^|" (- (point) 1024))
                        (if (bobp)
                            t
                          (progn
                            (forward-char -1)
                            (not (or (bobp)
                                     (char-equal (char-before) ?\())))))
                    t)
                  (or
                   ;; At beginning of buffer.
                   at-beginning-of-buffer?
                   (and (not (char-equal before ?\s))
                        (not (char-equal before ?\())
                        (not (gethash before rust-smart-operators--operator-chars))
                        (if (char-equal char ?=)
                            (not (char-equal before ?!))
                          t))
                   ;; Do break with a space after balanced >.
                   (and (char-equal before ?\>)
                        (save-excursion
                          (forward-char -1)
                          (awhen (sp-get-enclosing-sexp)
                            (and (string-equal (plist-get it :op) "<")
                                 (= (plist-get it :end) (+ 1 (point)))))))))
             (insert-char ?\s)
           ;; Delete spaces backwards if there's operator or open
           ;; paren char before the spaces.
           (let ((delete-whitespace?
                  (save-excursion
                    (when (not (zerop (skip-syntax-backward " ")))
                      (let* ((pt-before-ws (point))
                             (char-before-spaces (char-before pt-before-ws)))
                        (and char-before-spaces ;; not at beginning of buffer
                             (if (char-equal char-before-spaces ?>)
                                 (save-excursion
                                   (forward-char -1)
                                   (aif (sp-get-enclosing-sexp)
                                       (if (= (plist-get it :end) (+ 1 (point)))
                                           (not (string-equal (plist-get it :op) "<"))
                                         t)
                                     t ;; No sexp - ok to delete.
                                     ))
                               t ;; Not a > before spaces - ok to delete.
                               )
                             (or (gethash char-before-spaces rust-smart-operators--operator-chars)
                                 (char-equal char-before-spaces ?\()
                                 (and (char-equal char ?=)
                                      (char-equal char-before-spaces ?!)))))))))
             (when delete-whitespace?
               (setf whitespace-deleted? (delete-whitespace-backward)))))

         (let ((before-insert (char-before)))
           (insert-char char)

           (when (and insert-space-after

                      (not (char-equal char ?\<))

                      (if (char-equal char ?\>)
                          (if (memq before-insert '(?= ?-))
                              t ;; Insert space after operators '->', '=>'
                            nil)
                        t)

                      (or at-beginning-of-buffer?
                          (not (char-equal before-insert ?\()))

                      (if (char-equal char ?\&)
                          (or whitespace-deleted?
                              (char-equal before-insert ?\&) ;; Insert space after operator '&&'
                              )
                        t)

                      (or (not after) ;; at end of buffer
                          (and (not (char-equal after ?\s))
                               (not (char-equal after ?\))))))
             (insert-char ?\s))))))))

;;;###autoload
(defun rust-smart-operators-self-insert (arg)
  "Insert charater and take care to surround it with spaces."
  (interactive "p")
  (cl-assert (characterp last-command-event)
             nil
             "Last event is not a character: %s" last-command-event)
  (rust-smart-operators--insert-char-surrounding-with-spaces last-command-event))

(provide 'rust-smart-operators)

;; Local Variables:
;; End:

;; rust-smart-operators.el ends here
