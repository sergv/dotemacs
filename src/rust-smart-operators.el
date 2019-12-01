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
             (after (char-after)))
         ;; Decide whether to insert space before the operator.
         (if (and (not (smart-operators--on-empty-string?))
                  (not (memq char '(?\< ?\>)))
                  (or
                   ;; At beginning of buffer.
                   at-beginning-of-buffer?
                   (and (not (char-equal before ?\s))
                        (not (char-equal before ?\())
                        (not (gethash before rust-smart-operators--operator-chars)))))
             (insert-char ?\s)
           ;; Delete spaces backwards if there's operator or open
           ;; paren char before the spaces.
           (let ((delete-whitespace?
                  (save-excursion
                    (skip-syntax-backward " ")
                    (let* ((pt-before-ws (point))
                           (char-before-spaces (char-before pt-before-ws)))
                      (and char-before-spaces ;; not at beginning of buffer
                           (or (gethash char-before-spaces rust-smart-operators--operator-chars)
                               (char-equal char-before-spaces ?\()
                               (and (char= char ?=)
                                    (char= char-before-spaces ?!))))))))
             (when delete-whitespace?
               (delete-whitespace-backward)
               (setf whitespace-deleted? t))))

         (insert-char char)

         (when (and insert-space-after
                    (not (memq char '(?\< ?\>)))
                    (not (and (not at-beginning-of-buffer?)
                              (char-equal before ?\())))
           (when (or (not after) ;; at end of buffer
                     (and (not (char-equal after ?\s))
                          (not (char-equal after ?\)))))
             (insert-char ?\s))))))))

;;;###autoload
(defun rust-smart-operators-self-insert (arg)
  "Insert charater and take care to surround it with spaces."
  (interactive "p")
  (unless (characterp last-command-event)
    (error "Last event is not a character: %s" last-command-event))
  (rust-smart-operators--insert-char-surrounding-with-spaces last-command-event))

(provide 'rust-smart-operators)

;; Local Variables:
;; End:

;; rust-smart-operators.el ends here
