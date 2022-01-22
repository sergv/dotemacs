;; rust-smart-operators.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 November 2019
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'comment-util)
(require 'smart-operators-utils)

(defconst rust-smart-operators--operator-chars
  (cl-coerce "+-*/%^&|<>=" 'list) ;; ! - too special, used for macro calls
  "Characters that may constitute operators.")

(defconst rust-smart-operators--numeric-literal-chars
  (cl-coerce "+-" 'list)
  "Characters that are used in numeric literals, e.g. +5, -2.")

(defun rust-smart-operators--insert-char-surrounding-with-spaces (char)
  "Insert CHARacter while trying to surround it with spaces and
stick it to the previous operator on line."
  (rust-smart-operators--insert-char-optionally-surrounding-with-spaces char t))

(defconst rust-smart-operators--chars-to-separate-from-ampersand
  '(?= ?> ?| ?^ ?% ?/ ?- ?+ ?*))

(defconst rust-smart-operators--chars-to-separate-from-asterisk
  '(?= ?> ?| ?^ ?% ?/ ?- ?+ ?* ?&))

(defconst +rust-smart-operators--valid-operators+
  '("!" "!=" "%" "%=" "&" "&" "&" "&=" "&&" "*" "*=" "*" "*" "+" "+" "+=" "," "-" "-" "-=" "->" "." ".." "..=" ".." ".." "..." "/" "/=" ":" ":" ":" ";" ";" "<<" "<<=" "<" "<=" "=" "==" "=>" ">" ">=" ">>" ">>=" "@" "^" "^=" "|" "|" "|=" "||" "?"))

(defconst +rust-smart-operators--composite-operators+
  (--filter (< 1 (length it)) +rust-smart-operators--valid-operators+))

(defmacro rust-smart-operators--char-before-matches (specs idx get-char)
  "Specs is a list of strings."
  (declare (indent 2))
  (when (symbolp specs)
    (setq specs (eval specs)))
  (if specs
      (let ((entries (make-hash-table :test #'eq)))
        (dolist (entry specs)
          (when (not (string= entry ""))
            (let ((last-idx (1- (length entry))))
              (when-let ((last (aref entry last-idx)))
                (puthash last (cons (cl-subseq entry 0 last-idx)
                                    (gethash last entries))
                         entries)))))
        `(pcase ,(funcall get-char idx)
           ,@(--map (list (car it)
                          `(rust-smart-operators--char-before-matches
                               ,(--filter (< 0 (length it)) (cdr it))
                               ,(+ idx 1)
                             ,get-char))
                    (hash-table->alist entries))))
    `t))

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
                          (or (and (eq char-before-spaces ?|)
                                   (eq char ?-))
                              (and (eq char-before-spaces ?-)
                                   (eq char ?>))
                              (and (cond
                                     ((and (eq char ?*)
                                           (memq char-before-spaces
                                                 rust-smart-operators--chars-to-separate-from-asterisk))
                                      nil)
                                     ((and (eq char ?&)
                                           (memq char-before-spaces
                                                 rust-smart-operators--chars-to-separate-from-ampersand))
                                      nil)
                                     ((and (eq char-before-spaces ?=)
                                           (not (memq char '(?> ?=))) ;; for =>
                                           char-is-smart-op?)
                                      nil)
                                     ((eq char-before-spaces ?>)
                                      (if (eq char ?>)
                                          ;; Either it’s operator >> or generic type.
                                          t
                                        ;; If char before-before is a space then we consider
                                        ;; that ?\> before spaces is an operator.
                                        (let ((before2 (char-before (1- pt-before-ws))))
                                          (not (eq before2 ?\s)))))
                                     (t
                                      t) ;; Not a > before spaces - ok to delete.
                                     )
                                   (let ((char-before-spaces2 (char-before (- pt-before-ws 1))))
                                     (and (not (rust-smart-operators--char-before-matches
                                                +rust-smart-operators--composite-operators+
                                                0
                                                (lambda (n) `(char-before (- pt-before-ws ,n)))))
                                          (or (if (eq char ?=)
                                                  ;; So that we create arithmetic increments, e.g. +=
                                                  (memq char-before-spaces '(?+ ?- ?* ?/ ?% ?=))
                                                (memq char-before-spaces rust-smart-operators--operator-chars))
                                              (eq char-before-spaces ?\()
                                              (and (eq char ?=)
                                                   (or (eq char-before-spaces ?!)
                                                       (and (eq char-before-spaces ?.)
                                                            (eq char-before-spaces2 ?.))))))))))))
              (setf whitespace-deleted? (delete-whitespace-backward)))

            (let* ((pt (point))
                   (before (char-before pt))
                   (after (char-after pt))
                   (at-beginning-of-buffer? (null before))
                   (at-beginning-of-line? (or at-beginning-of-buffer?
                                              (eq before ?\n)))
                   (before2 (and (not at-beginning-of-buffer?)
                                 (char-before (1- pt)))))

              (let ((insert-space-before-char?
                     (and (not (smart-operators--on-empty-string?))
                          (not (memq char '(?\< ?\>)))
                          (if (eq char ?\|)
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
                                     ((eq (char-before) ?\()
                                      nil)
                                     (t
                                      t)))))
                            t)
                          ;; ::*
                          (if (and (eq char ?*)
                                   (or (and (eq before ?:)
                                            (eq before2 ?:))
                                       (and (eq before ?&)
                                            (not (eq before2 ?&)))
                                       (eq before ?\[)))
                              nil
                            t)
                          ;; ..=
                          (if (and (eq char ?=)
                                   (eq before ?.)
                                   (eq before2 ?.))
                              (progn
                                (setf insert-space-after nil)
                                nil)
                            t)
                          (if (and (eq char ?&)
                                   (eq before ?\[))
                              nil
                            t)
                          (if (and (eq char ?=)
                                   (not (eq before ?>)) ;; > may be close of type signature, e.g. let foo: Vec<i32> _|_= bar;
                                   (memq before rust-smart-operators--operator-chars))
                              nil
                            t)
                          (if (and (eq char ?/)
                                   (eq after ?/)
                                   at-beginning-of-line?)
                              (progn
                                (setf insert-space-after nil)
                                nil)
                            t)
                          (or
                           ;; At beginning of buffer.
                           at-beginning-of-buffer?
                           (and (not (memq before '(?\s ?\()))
                                (not (memq before rust-smart-operators--operator-chars))
                                (if (eq char ?=)
                                    (not (eq before ?!))
                                  t))
                           (and (eq before ?\>)
                                (not (eq before2 ?\s)))
                           ;; =& and >& are not operators so add a space
                           (and (eq char ?*)
                                (memq before rust-smart-operators--chars-to-separate-from-asterisk))
                           (and (eq char ?&)
                                (memq before rust-smart-operators--chars-to-separate-from-ampersand))
                           (and (eq char ?+)
                                (not (eq before ?=))
                                (memq before rust-smart-operators--operator-chars))
                           (and (eq char ?-)
                                (eq char-before-spaces ?|))
                           (and (memq char rust-smart-operators--numeric-literal-chars)
                                (eq before ?=))))))

                ;; Decide whether to insert space before the operator.
                (when insert-space-before-char?
                  (insert-char ?\s)))

              (let ((before-insert (char-before)))
                (insert-char char)

                (when (and insert-space-after

                           (not (eq char ?\<))

                           (if (eq char ?\>)
                               (if (memq before-insert '(?= ?-))
                                   t ;; Insert space after operators '->', '=>'
                                 nil)
                             t)

                           (or at-beginning-of-buffer?
                               (not (eq before-insert ?\()))

                           (if (eq char ?*)
                               (if (memq before rust-smart-operators--chars-to-separate-from-asterisk)
                                   ;; We don’t want '= * _|_',
                                   ;; we want '= *_|_' respectively
                                   nil
                                 whitespace-deleted?)
                             t)
                           (if (eq char ?\>)
                               (if (memq before-insert '(?= ?-))
                                   t ;; Insert space after operators '->', '=>'
                                 nil)
                             t)
                           (if (eq char ?\&)
                               (if (memq before rust-smart-operators--chars-to-separate-from-ampersand)
                                   ;; We don’t want '= & _|_' or '-> & _|_'
                                   ;; we want '= &_|_' or '-> &_|_' respectively
                                   nil
                                 (or whitespace-deleted?
                                     (eq before-insert ?\&) ;; Insert space after operator '&&'
                                     ))
                             t)

                           (if (and (eq char-before-spaces ?=)
                                    (memq char rust-smart-operators--numeric-literal-chars))
                               nil
                             t)

                           (or (not after) ;; at end of buffer
                               (and (not (eq after ?\s))
                                    (not (eq after ?\))))))
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
