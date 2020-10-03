;; rust-smart-operators.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 November 2019
;; Description:

(require 'smart-operators-utils)

(defconst rust-smart-operators--operator-chars
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

(defconst rust-smart-operators--chars-to-separate-from-ampersand
  '(?= ?> ?< ?| ?^ ?% ?/ ?- ?+ ?*))

(defconst rust-smart-operators--chars-to-separate-from-asterisk
  '(?= ?> ?< ?| ?^ ?% ?/ ?- ?+ ?* ?&))

(defun rust-smart-operators--insert-char-optionally-surrounding-with-spaces (char insert-space-after)
  (let ((disable-smart-operators? current-prefix-arg))
    (if (or disable-smart-operators?
            (smart-operators--literal-insertion?)
            (not (gethash char rust-smart-operators--operator-chars)))
        (insert-char char)
      (let ((whitespace-deleted? nil)
            (after (char-after (point))))
        ;; Delete spaces backwards if there's operator or open
        ;; paren char before the spaces.
        (when-let ((delete-whitespace?
                    (save-excursion
                      (when (not (zerop (skip-syntax-backward " ")))
                        (let* ((pt-before-ws (point))
                               (char-before-spaces (char-before pt-before-ws))
                               (char-before-spaces2 (char-before (- pt-before-ws 1))))
                          (and char-before-spaces ;; not at beginning of buffer
                               (cond
                                 ((and (char= char ?*)
                                       (memq char-before-spaces rust-smart-operators--chars-to-separate-from-asterisk))
                                  nil)
                                 ((and (char= char ?&)
                                       (memq char-before-spaces rust-smart-operators--chars-to-separate-from-ampersand))
                                  nil)
                                 ((char-equal char-before-spaces ?>)
                                  (if (char-equal char ?>)
                                      t ;; either it’s operator >> or generic type
                                    (save-excursion
                                      (forward-char -1)
                                      (aif (sp-get-enclosing-sexp)
                                          (if (= (plist-get it :end) (+ 1 (point)))
                                              (not (string-equal (plist-get it :op) "<"))
                                            t)
                                        t ;; No sexp - ok to delete.
                                        ))))
                                 (t
                                  t) ;; Not a > before spaces - ok to delete.
                                 )
                               (or (gethash char-before-spaces rust-smart-operators--operator-chars)
                                   (char-equal char-before-spaces ?\()
                                   (and (char-equal char ?=)
                                        (or (char-equal char-before-spaces ?!)
                                            (and (char-equal char-before-spaces ?.)
                                                 (char-equal char-before-spaces2 ?.)))))))))))
          (setf whitespace-deleted? (delete-whitespace-backward)))

        (let* ((pt (point))
               (before (char-before pt))
               (at-beginning-of-buffer? (null before))
               (before2 (and (not at-beginning-of-buffer?)
                             (char-before (1- pt)))))

          (let ((insert-space-before-char?
                 (and (not (smart-operators--on-empty-string?))
                      (not (memq char '(?\< ?\>)))
                      (if (char-equal char ?\|)
                          ;; Check if we're preceded by '...( *(move +)?|..._|_'.
                          (save-excursion
                            (skip-chars-backward "^|(" (line-beginning-position))
                            (cond
                              ((bobp)
                               t)
                              ((looking-at "move\\( *\\)")
                               ;; Don’t want a space after first pipe in a closure.
                               (setf insert-space-after nil)
                               (if (= (match-end 1)
                                      (match-beginning 1))
                                   t ;; no spaces => add some
                                 nil))
                              (t
                               (skip-chars-backward "| move" (line-beginning-position))
                               (cond
                                 ((bobp)
                                  nil)
                                 ((char-equal (char-before) ?\()
                                  nil)
                                 (t
                                  t)))))
                        t)
                      ;; ::*
                      (if (and (char-equal char ?*)
                               (char-equal before ?:)
                               (char-equal before2 ?:))
                          nil
                        t)
                      ;; ..=
                      (if (and (char-equal char ?=)
                               (char-equal before ?.)
                               (char-equal before2 ?.))
                          (progn
                            (setf insert-space-after nil)
                            nil)
                        t)
                      (if (and (char-equal char ?&)
                               (char-equal before ?\[))
                          nil
                        t)
                      (or
                       ;; At beginning of buffer.
                       at-beginning-of-buffer?
                       (and (not (memq before '(?\s ?\()))
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
                                     (= (plist-get it :end) (+ 1 (point)))))))
                       ;; =& and >& are not operators so add a space
                       (and (char= char ?*)
                            (memq before rust-smart-operators--chars-to-separate-from-asterisk))
                       (and (char= char ?&)
                            (memq before rust-smart-operators--chars-to-separate-from-ampersand))))))

            ;; Decide whether to insert space before the operator.
            (when insert-space-before-char?
              (insert-char ?\s)))

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

                       (if (char= char ?*)
                           (if (memq before rust-smart-operators--chars-to-separate-from-asterisk)
                               ;; We don’t want '= * _|_',
                               ;; we want '= *_|_' respectively
                               nil
                             whitespace-deleted?)
                         t)
                       (if (char= char ?\&)
                           (if (memq before rust-smart-operators--chars-to-separate-from-ampersand)
                               ;; We don’t want '= & _|_' or '-> & _|_'
                               ;; we want '= &_|_' or '-> &_|_' respectively
                               nil
                             (or whitespace-deleted?
                                 (char-equal before-insert ?\&) ;; Insert space after operator '&&'
                                 ))
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
