;; rust-smart-operators.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 November 2019
;; Description:

(require 'comment-util)
(require 'smart-operators-utils)

(defconst rust-smart-operators--operator-chars
  (coerce "+-*/%^&|<>=" 'list) ;; ! - too special, used for macro calls
  "Characters that may constitute operators.")

(defconst rust-smart-operators--numeric-literal-chars
  (coerce "+-" 'list)
  "Characters that are used in numeric literals, e.g. +5, -2.")

(defun rust-smart-operators--insert-char-surrounding-with-spaces (char)
  "Insert CHARacter while trying to surround it with spaces and
stick it to the previous operator on line."
  (rust-smart-operators--insert-char-optionally-surrounding-with-spaces char t))

(defconst rust-smart-operators--chars-to-separate-from-ampersand
  '(?= ?> ?| ?^ ?% ?/ ?- ?+ ?*))

(defconst rust-smart-operators--chars-to-separate-from-asterisk
  '(?= ?> ?| ?^ ?% ?/ ?- ?+ ?* ?&))

(defun rust-smart-operators--insert-char-optionally-surrounding-with-spaces (char insert-space-after)
  (let ((disable-smart-operators? current-prefix-arg)
        (char-is-smart-op? (memq char rust-smart-operators--operator-chars)))
    (if (or disable-smart-operators?
            (not char-is-smart-op?))
        (insert-char char)

      (let* ((ws-size nil)
             (pt-before-ws (save-excursion
                             (setf ws-size (skip-syntax-backward " "))
                             (point)))
             (char-before-spaces (char-before pt-before-ws)))
        (if (smart-operators--literal-insertion? (awhen *comment-util-current-format*
                                                   (and (memq char (comment-format-comment-chars it))
                                                        (memq char-before-spaces (comment-format-comment-chars it)))))
            (insert-char char)
          (let ((whitespace-deleted? nil)
                (after (char-after (point))))
            ;; Delete spaces backwards if there's operator or open
            ;; paren char before the spaces.
            (when-let ((delete-whitespace?
                        (when (and char-before-spaces ;; not at beginning of buffer
                                   (not (zerop ws-size)))
                          (or (and (char= char-before-spaces ?|)
                                   (char= char ?-))
                              (and (char= char-before-spaces ?-)
                                   (char= char ?>))
                              (and (cond
                                     ((and (char= char ?*)
                                           (memq char-before-spaces
                                                 rust-smart-operators--chars-to-separate-from-asterisk))
                                      nil)
                                     ((and (char= char ?&)
                                           (memq char-before-spaces
                                                 rust-smart-operators--chars-to-separate-from-ampersand))
                                      nil)
                                     ((and (char= char-before-spaces ?=)
                                           (not (char= char ?>)) ;; for =>
                                           char-is-smart-op?)
                                      nil)
                                     ((char= char-before-spaces ?>)
                                      (if (char= char ?>)
                                          t ;; either it’s operator >> or generic type
                                        (save-excursion
                                          (goto-char pt-before-ws)
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
                                   (let ((char-before-spaces2 (char-before (- pt-before-ws 1))))
                                     (or (if (char= char ?=)
                                             ;; So that we create arithmetic increments, e.g. +=
                                             (memq char-before-spaces '(?+ ?- ?* ?/ ?%))
                                           (memq char-before-spaces rust-smart-operators--operator-chars))
                                         (char= char-before-spaces ?\()
                                         (and (char= char ?=)
                                              (or (char= char-before-spaces ?!)
                                                  (and (char= char-before-spaces ?.)
                                                       (char= char-before-spaces2 ?.)))))))))))
              (setf whitespace-deleted? (delete-whitespace-backward)))

            (let* ((pt (point))
                   (before (char-before pt))
                   (at-beginning-of-buffer? (null before))
                   (before2 (and (not at-beginning-of-buffer?)
                                 (char-before (1- pt)))))

              (let ((insert-space-before-char?
                     (and (not (smart-operators--on-empty-string?))
                          (not (memq char '(?\< ?\>)))
                          (if (char= char ?\|)
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
                                     ((char= (char-before) ?\()
                                      nil)
                                     (t
                                      t)))))
                            t)
                          ;; ::*
                          (if (and (char= char ?*)
                                   (or (and (char= before ?:)
                                            (char= before2 ?:))
                                       (and (char= before ?&)
                                            (not (char= before2 ?&)))))
                              nil
                            t)
                          ;; ..=
                          (if (and (char= char ?=)
                                   (char= before ?.)
                                   (char= before2 ?.))
                              (progn
                                (setf insert-space-after nil)
                                nil)
                            t)
                          (if (and (char= char ?&)
                                   (char= before ?\[))
                              nil
                            t)
                          (if (and (char= char ?=)
                                   (not (char= before ?>)) ;; > may be close of type signature, e.g. let foo: Vec<i32> _|_= bar;
                                   (memq before rust-smart-operators--operator-chars))
                              nil
                            t)
                          (or
                           ;; At beginning of buffer.
                           at-beginning-of-buffer?
                           (and (not (memq before '(?\s ?\()))
                                (not (memq before rust-smart-operators--operator-chars))
                                (if (char= char ?=)
                                    (not (char= before ?!))
                                  t))
                           ;; Do break with a space after balanced >.
                           (and (char= before ?\>)
                                (save-excursion
                                  (forward-char -1)
                                  (awhen (sp-get-enclosing-sexp)
                                    (and (string-equal (plist-get it :op) "<")
                                         (= (plist-get it :end) (+ 1 (point)))))))
                           ;; =& and >& are not operators so add a space
                           (and (char= char ?*)
                                (memq before rust-smart-operators--chars-to-separate-from-asterisk))
                           (and (char= char ?&)
                                (memq before rust-smart-operators--chars-to-separate-from-ampersand))
                           (and (char= char ?+)
                                (not (char= before ?=))
                                (memq before rust-smart-operators--operator-chars))
                           (and (char= char ?-)
                                (char= char-before-spaces ?|))
                           (and (memq char rust-smart-operators--numeric-literal-chars)
                                (char= before ?=))))))

                ;; Decide whether to insert space before the operator.
                (when insert-space-before-char?
                  (insert-char ?\s)))

              (let ((before-insert (char-before)))
                (insert-char char)

                (when (and insert-space-after

                           (not (char= char ?\<))

                           (if (char= char ?\>)
                               (if (memq before-insert '(?= ?-))
                                   t ;; Insert space after operators '->', '=>'
                                 nil)
                             t)

                           (or at-beginning-of-buffer?
                               (not (char= before-insert ?\()))

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
                                     (char= before-insert ?\&) ;; Insert space after operator '&&'
                                     ))
                             t)

                           (if (and (char= char-before-spaces ?=)
                                    (memq char rust-smart-operators--numeric-literal-chars))
                               nil
                             t)

                           (or (not after) ;; at end of buffer
                               (and (not (char= after ?\s))
                                    (not (char= after ?\))))))
                  (insert-char ?\s))))))))))

;;;###autoload
(defun rust-smart-operators-self-insert (n)
  "Insert charater and take care to surround it with spaces."
  (interactive "p")
  (cl-assert (characterp last-command-event) nil
             "Last event is not a character: %s" last-command-event)
  (dotimes (_ n)
    (rust-smart-operators--insert-char-surrounding-with-spaces last-command-event)))

(provide 'rust-smart-operators)

;; Local Variables:
;; End:

;; rust-smart-operators.el ends here
