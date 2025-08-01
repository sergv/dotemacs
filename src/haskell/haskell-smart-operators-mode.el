;; haskell-smart-operators-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 20 February 2017
;; Description:
;; Bind characters for operators that will automatically maintain whitespace
;; around themselves.
;;
;; Special thanks: Chris Done's magnificent `structured-haskell-mode', which
;; inspired this work.

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'haskell-ext-tracking)
(require 'haskell-ghc-support)
(require 'haskell-mode)
(require 'haskell-smart-operators-utils)
(require 'haskell-syntax-table)
(require 'pseudoparedit)
(require 'smart-operators-utils)

(require 'treesit)
(require 'treesit-setup)

(defconst haskell-smart-operators--operator-chars-str "!#$%&*+./:<=>?@\\^|~-")

(defconst haskell-smart-operators--operator-chars
  (let ((tbl (make-hash-table :test #'eq)))
    (dovector (c haskell-smart-operators--operator-chars-str)
      (puthash c t tbl))
    tbl)
  "Characters that may constitute operators.")

(defun haskell-smart-operators--on-a-line-with-guard? ()
  ;; Same as but should be leaner without regexen.
  ;; (save-excursion
  ;;   (beginning-of-line)
  ;;   (and (looking-at-p "^[ \t]+|")
  ;;        (point)))
  (let* ((p0 (point-at-bol))
         (p p0)
         (c (char-after p)))
    (while (or (eq c ?\s)
               (eq c ?\t))
      (cl-incf p)
      (setq c (char-after p)))
    (when (and
           ;; Check that there was at least one space.
           (not (eq p p0))
           (eq (char-after p) ?|))
      p0)))

(defvar haskell-quasiquoter-name-syntax-table
  (let ((tbl (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?. "w" tbl)
    (modify-syntax-entry ?' "w" tbl)
    (modify-syntax-entry ?_ "w" tbl)
    tbl)
  "Special syntax table for haskell that allows to recognize symbols that contain
both unicode and ascii characters.")

(defun haskell-smart-operators--insert-char-surrounding-with-spaces (char)
  "Insert CHARacter while trying to surround it with spaces and
stick it to the previous operator on line."
  (haskell-smart-operators--insert-char-optionally-surrounding-with-spaces char t))

(defsubst haskell-smart-operators--is-whitespace-char? (c)
  (or (eq c ?\s)
      (eq c ?\t)))

(defun haskell-smart-operators--insert-char-optionally-surrounding-with-spaces (char insert-space-after)
  (let* ((disable-smart-operators? current-prefix-arg)
         (line-start-pos (line-beginning-position))

         (pt (point))
         (prev-char (char-before pt))
         (at-beginning-of-buffer? (null prev-char))
         (pt-before-ws (point))

         (pt-preceded-by-two-dashes? nil)
         (handling-haddock-comment?
          (if disable-smart-operators?
              nil
            (save-excursion
              (skip-syntax-backward " ")
              (setf pt-before-ws (point))
              (let* ((before-ws (char-before pt-before-ws))
                     (pt-before-ws2 (1- pt-before-ws)))
                (when (and before-ws
                           (let ((before-ws2 (char-before pt-before-ws2)))
                             (and before-ws2
                                  (eq before-ws before-ws2)
                                  (eq before-ws ?-))))
                  (setf pt-preceded-by-two-dashes? t)
                  (= pt-before-ws2 (1+ line-start-pos)))))))

         (magic-hash? (and (eq char ?#)
                           (haskell-ext-tracking-have-magic-hash?)))

         (insert-trailing-space
          (lambda (whitespace-deleted? whitespace-inserted? before-pt before after)
            ;; Decide whether to insert a space after the operator.
            (when (and insert-space-after
                       (not (and (eq char ?\\)
                                 (not at-beginning-of-buffer?)
                                 (eq before ?\()))
                       (if magic-hash?
                           (not (eq after ?,))
                         t))
              (when (or (not after) ;; at end of buffer
                        ;; If the next thing is lambda then we don’t want to merge
                        ;; with its "\".
                        (eq after ?\\)
                        (and (not (or (haskell-smart-operators--is-whitespace-char? after)
                                      ;; Don’t insert space before backtick.
                                      (eq after ?\`)

                                      ;; Do not split "|]" token when we're inserting the "|".
                                      (and (eq char ?|)
                                           (eq after ?\]))

                                      (gethash after haskell-smart-operators--operator-chars)))
                             (cond
                               ;; Don’t insert space after "!" if it’s preceded by a space,
                               ;; "foo !x !y = ...".
                               ((and (eq char ?!)
                                     (or whitespace-inserted?
                                         (haskell-smart-operators--is-whitespace-char? before)))
                                nil)
                               ;; Special case for @ since it's part of as-patterns.
                               ((eq char ?@)
                                (or whitespace-deleted?
                                    (gethash before haskell-smart-operators--operator-chars)))
                               ((eq after ?\))
                                (let ((c (save-excursion
                                           (goto-char before-pt)
                                           (skip-chars-backward haskell-smart-operators--operator-chars-str)
                                           (char-before))))
                                  (or (null c)
                                      (haskell-smart-operators--is-whitespace-char? c))))
                               (t t))))
                (insert-char ?\s))))))
    (cond
      ;; Haddock comments must be treated specially because they're comments
      ;; and we generally disable smart operator insertion within comments.
      ((and handling-haddock-comment?
            (memq char '(?^ ?|)))
       (delete-region pt-before-ws (point))
       (let ((before-pt (point))
             (before (char-before)))
         (insert-char ?\s)
         (insert-char char)
         (funcall insert-trailing-space nil nil before-pt before (char-after))))
      ;; Must check for arrows here because otherwise
      ;; `haskell-smart-operators--literal-insertion?' will treat '--'
      ;; as a comment and not allow to do any meaningful work.
      ((and (not handling-haddock-comment?)
            pt-preceded-by-two-dashes?
            (memq char '(?< ?> ?-)))
       (delete-region pt-before-ws (point))
       (let ((before-pt (point))
             (before (char-before)))
         (insert-char char)
         (funcall insert-trailing-space nil nil before-pt before (char-after))))
      ((or disable-smart-operators?
           (haskell-smart-operators--literal-insertion?)
           (not (gethash char haskell-smart-operators--operator-chars)))
       (insert-char char))
      (t
       (let ((whitespace-deleted? nil)
             (whitespace-inserted? nil)
             (before-pt nil)
             (before nil)
             (after (char-after)))
         ;; Decide whether to insert space before the operator.
         (if (and (not (smart-operators--on-empty-string?))
                  ;; If inserting hash then we should not add a space
                  ;; if MagicHash is enabled.
                  (if magic-hash?
                      (not (eq char ?#)) ;; Stop considering whether to insert space.
                    t)
                  (or at-beginning-of-buffer?
                      ;; After | that is a potential guard.
                      (when (eq prev-char ?|)
                        (awhen (haskell-smart-operators--on-a-line-with-guard?)
                          (equal it (- pt 1))))
                      ;; Insert space between us and previous # if the char before # is
                      ;; not a space itself, which means that # ends identifier name.
                      (when (eq prev-char ?#)
                        (let ((prev-prev-char (char-before (1- pt))))
                          (not (or (null prev-prev-char)
                                   (eq prev-prev-char ?\n)
                                   (haskell-smart-operators--is-whitespace-char? prev-prev-char)))))
                      (and (eq char ?!)
                           (eq prev-char ?\\))
                      (and (not (eq prev-char ?\s))
                           (not (eq prev-char ?\())
                           ;; Do not insert spaces before @ since it's mostly used
                           ;; as as-patterns.
                           (not (eq char ?@))
                           (and (not (eq prev-char ?\`))
                                (or (null after)
                                    (not (eq after ?\`))))
                           (not (and (eq char ?|)
                                     (or
                                      ;; Do not split '[|' token when we're inserting the '['.
                                      (eq prev-char ?\[)
                                      ;; Do not split '[foo|' quasiquoter
                                      ;; when we're inserting the '['.
                                      (save-excursion
                                        (with-syntax-table haskell-quasiquoter-name-syntax-table
                                          (skip-syntax-backward "w" (line-beginning-position))
                                          (let ((before-far (char-before)))
                                            (eq before-far ?\[)))))))
                           (not (gethash prev-char haskell-smart-operators--operator-chars)))))
             (progn
               (setq before-pt (point)
                     before (char-before)
                     whitespace-inserted? t)
               (insert-char ?\s))
           ;; Delete spaces backwards if there's operator or open paren char
           ;; before the spaces.
           (let ((delete-whitespace?
                  (save-excursion
                    (skip-syntax-backward " ")
                    (let* ((pt-before-ws (point))
                           (char-before-spaces (char-before pt-before-ws)))
                      (and char-before-spaces ;; not at beginning of buffer
                           (if (eq char-before-spaces ?#)
                               (if-let ((char-before-char-before-spaces (char-before (1- pt-before-ws))))
                                   ;; If there’s space before previous # then it’s an operator so
                                   ;; delete whitespace backwards.
                                   (or (haskell-smart-operators--is-whitespace-char? char-before-char-before-spaces)
                                       (and (eq char ?#)
                                            (save-excursion
                                              (let ((hashes-count 0))
                                                (while (eq (char-before) ?#)
                                                  (setf hashes-count (+ hashes-count 1))
                                                  (forward-char -1))
                                                (when (< hashes-count 2)
                                                  (skip-syntax-backward "^ >")
                                                  (looking-at-p (rx (? (any ?+ ?-))
                                                                    (+ (any (?0 . ?9)))
                                                                    (? "."
                                                                       (+ (any (?0 . ?9))))
                                                                    (? (any ?e ?E)
                                                                       (? (any ?+ ?-))
                                                                       (+ (any (?0 . ?9))))
                                                                    "#")))))))
                                 ;; Beginning of buffer, and we’re
                                 ;; here "#_|_", hash is definitely
                                 ;; not part of a name.
                                 t)
                             t)
                           ;; Don’t delete whitespace if result would be "\!".
                           (if (eq char ?!)
                               (not (eq char-before-spaces ?\\))
                             t)
                           (or (gethash char-before-spaces haskell-smart-operators--operator-chars)
                               (eq char-before-spaces ?\()
                               ;; If inserting # in MagicHash mode
                               ;; then make it stick to the previous
                               ;; word as well as to operators.
                               (and magic-hash?
                                    (memq (char-syntax char-before-spaces) '(?w ?_))))
                           (if (eq char-before-spaces ?|)
                               ;; Check that it's not a guard.
                               (not (haskell-smart-operators--on-a-line-with-guard?))
                             t))))))
             (when delete-whitespace?
               (setf whitespace-deleted?
                     (delete-whitespace-backward))
               ;; (setf whitespace-deleted? t)
               )
             (setq before-pt (point)
                   before (char-before))))
         ;; Insert operator char.
         (insert-char char)
         (funcall insert-trailing-space whitespace-deleted? whitespace-inserted? before-pt before after))))))

;;;###autoload
(defun haskell-smart-operators-self-insert (n)
  "Insert charater and take care to surround it with spaces."
  (interactive "*p")
  (cl-assert (characterp last-command-event) nil
             "Last event is not a character: %s" last-command-event)
  (dotimes (_ n)
    (haskell-smart-operators--insert-char-surrounding-with-spaces last-command-event)))

;;;###autoload
(defun haskell-smart-operators-$ ()
  "Swap parens with a dollar."
  (interactive "*")
  (let ((start (point))
        (strip-next-parens?
         (save-excursion
           (skip-syntax-backward " ")
           (not (eq (char-before) ?\<))))
        (paren-start-pos nil))
    (when (and strip-next-parens?
               (save-excursion
                 (skip-syntax-forward " ")
                 (when (and (eq (char-after) ?\()
                            (save-excursion
                              (forward-char 1)
                              (skip-syntax-forward " ")
                              (not (eq (char-after) ?\`))))
                   (setf paren-start-pos (point))
                   t)))
      (goto-char paren-start-pos)
      ;; delete parenthesized sexp
      (save-excursion
        (forward-sexp)
        (delete-char -1))
      (delete-char 1)
      (delete-region start paren-start-pos))
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?\$)))

;;;###autoload
(defun haskell-smart-operators-hyphen ()
  "Insert hyphen surrounding with spaces. No surrounding within
strings or comments. Expand into {- _|_ -} if inside { *}."
  (interactive "*")
  (let ((pt (point)))
    (cl-destructuring-bind
        (pt-before pt-after is-before? is-after? is-surrounded?)
        (smart-operators--point-surrounded-by ?\{ ?\})
      (cond
        (is-surrounded?
         (progn
           (delete-region pt-before pt-after)
           (insert "-  -")
           (forward-char -2)))
        (is-before?
         (delete-region pt-before pt)
         (insert "- "))
        (is-after?
         (delete-region pt pt-after)
         (insert " -")
         (forward-char -2))
        (t
         (haskell-smart-operators--insert-char-surrounding-with-spaces ?-))))))

;; TODO consider merging ‘haskell-indentation-common-electric-command’
;; and ‘smart-operators-comma’ to define ‘haskell-smart-operators-comma’
;;;###autoload
(defalias 'haskell-smart-operators-comma #'smart-operators-comma)

;;;###autoload
(defalias 'haskell-cabal-smart-operators-comma #'smart-operators-comma)

;;;###autoload
(defun haskell-smart-operators-dot ()
  "Insert comma followed by space."
  (interactive "*")
  (let* ((pos-before-spaces
          (save-excursion
            (skip-syntax-backward " ")
            (point)))
         (preceded-by-operator?
          (awhen (char-before pos-before-spaces)
            (gethash it haskell-smart-operators--operator-chars))))
    (when preceded-by-operator?
      (delete-region pos-before-spaces (point)))
    (insert-char ?\.)
    (when (and preceded-by-operator?
               (let ((next-char (char-after)))
                 (and (not (memq next-char '(?\s ?\) ?\] ?\})))
                      (not (gethash next-char haskell-smart-operators--operator-chars)))))
      (insert-char ?\s))))

;;;###autoload
(defun haskell-smart-operators-hash ()
  "Smart insertion of #."
  (interactive "*")
  (cl-destructuring-bind
      (pt-pragma-start pt-pragma-end is-surrounded-for-pragma?)
      (smart-operators--point-surrounded-by2 ?\{ ?- ?- ?\})
    (cl-destructuring-bind
        (pt-c2hs-start pt-c2hs-end _is-before? _is-after? is-surrounded-for-c2hs?)
        (smart-operators--point-surrounded-by ?\{ ?\})
      (cond (is-surrounded-for-pragma?
             (delete-region (- pt-pragma-start 2) (+ pt-pragma-end 2))
             (haskell-abbrev+--insert-pragma nil))
            ;; for c2hs
            (is-surrounded-for-c2hs?
             (delete-region pt-c2hs-start pt-c2hs-end)
             (insert "##")
             (forward-char -1))
            ;; Don't surround with #includes, #lang/#opts abbrevs, etc with spaces
            ((= (point)
                (line-beginning-position))
             (insert-char ?\#))
            (t
             (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))))))

;;;###autoload
(defun haskell-smart-operators-exclamation-mark ()
  "Smart insertion for ! aimed at placing bangs within records."
  (interactive "*")
  (let* ((p (point))
         (p-before-ws
          (save-excursion
            (skip-syntax-backward " ")
            (point)))
         (p-after-ws
          (save-excursion
            (skip-syntax-forward " ")
            (point)))
         (preceded-by-double-colon?
          (save-excursion
            (goto-char p-before-ws)
            (preceded-by2 ?: ?:)))
         (preceded-by-arrow?
          (save-excursion
            (goto-char p-before-ws)
            (preceded-by2 ?- ?>)))
         (preceded-by-operator?
          (save-excursion
            (goto-char p-before-ws)
            (gethash (char-before) haskell-smart-operators--operator-chars)))
         (followed-by-operator?
          (save-excursion
            (goto-char p-after-ws)
            (gethash (char-after) haskell-smart-operators--operator-chars)))
         (ts-field-node-children
          (when (and (derived-mode-p 'haskell-ts-mode)
                     (or
                      ;; Don’t stick to preceding arrow since it’s most likely part of
                      ;; a funciton type.
                      preceded-by-arrow?
                      (not (or (and (not preceded-by-double-colon?)
                                    preceded-by-operator?)
                               followed-by-operator?))))
            (awhen (treesit-node-top-level
                    (treesit-node-at p)
                    (lambda (x) (string= (treesit-node-type x) "field")))
              (treesit-node-children it))))
         (ts-field-colon-node nil)
         (ts-field-type-node nil))
    (cond
      ((and ts-field-node-children
            (= (length ts-field-node-children) 3)
            (string= (treesit-node-type (car ts-field-node-children)) "field_name")
            (setf ts-field-colon-node (cadr ts-field-node-children))
            (string= (treesit-node-type ts-field-colon-node) "::")
            (setf ts-field-type-node (caddr ts-field-node-children))
            ;; Check for when inserting ! in:
            ;;
            ;; data Foo = Foo
            ;;   { foo :: Set Int
            ;;   , bar :: Map Int Double
            ;;   , baz :: _|_
            ;;   })

            ;; If there’s a node but it’s empty then there’s no node and treesitter
            ;; is messing with us.
            (/= (treesit-node-start ts-field-type-node)
                (treesit-node-end ts-field-type-node))

            ;; There should be no ! already
            (not (string= (treesit-node-type ts-field-type-node) "strict_field"))
            ;; That we’re after :: but before type end (i.e. could be
            ;; before type start but that’s ok).
            (<= (treesit-node-end ts-field-colon-node) p)
            (<= p (treesit-node-end ts-field-type-node)))
       (let ((has-parens? (string= (treesit-node-type ts-field-type-node) "parens")))
         (if has-parens?
             (progn
               (goto-char (treesit-node-start ts-field-type-node))
               (insert-char ?!))
           (let* ((start (treesit-node-start ts-field-type-node))
                  (end (treesit-node-end ts-field-type-node))
                  (should-add-parens?
                   (save-excursion
                     (save-restriction
                       (narrow-to-region start end)
                       (goto-char (point-min))
                       (search-forward " " nil t)))))
             ;; Start from the end so that we don’t need to recompute
             ;; positions due to moves.
             (when should-add-parens?
               (goto-char end)
               (insert-char ?\)))
             (goto-char start)
             (when (and preceded-by-double-colon?
                        (not (eq (char-before) ?\s)))
               (insert-char ?\s))
             (insert-char ?!)
             (when should-add-parens?
               (insert-char ?\())))))
      (preceded-by-double-colon?
       (unless (eq (char-before) ?\s)
         (insert-char ?\s))
       (insert-char ?!))
      (t
       (haskell-smart-operators--insert-char-surrounding-with-spaces ?!)))))

;;;###autoload
(defun haskell-smart-operators-quote ()
  (interactive "*")
  (if (haskell-smart-operators--literal-insertion?)
      (insert-char ?\')
    (let ((prev-char (char-before)))
      (insert-char ?\')
      (when (or (not prev-char)
                (not (or (eq (char-syntax prev-char) ?w)
                         (eq (char-syntax prev-char) ?_))))
        (insert-char ?\')
        (forward-char -1)))))

(defvar haskell-smart-operators-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dolist (key (list (kbd "=") (kbd "+") (kbd "*") (kbd "<") (kbd ">")
                       (kbd "%") (kbd "^") (kbd "&") (kbd "/")
                       (kbd "?") (kbd "|") (kbd "~")
                       (kbd "@") (kbd ":")))
      (define-key keymap key #'haskell-smart-operators-self-insert))
    (define-key keymap (kbd "!") #'haskell-smart-operators-exclamation-mark)
    (define-key keymap (kbd "-") #'haskell-smart-operators-hyphen)
    (define-key keymap (kbd "#") #'haskell-smart-operators-hash)
    (define-key keymap (kbd ",") #'haskell-smart-operators-comma)
    (define-key keymap (kbd ".") #'haskell-smart-operators-dot)
    (define-key keymap (kbd "$") #'haskell-smart-operators-$)
    keymap))

;;;###autoload
(defun haskell-smart-operators-open-paren ()
  (interactive)
  (let ((literal-insertion? (haskell-smart-operators--literal-insertion?))
        (checked-import-list? nil)
        (inside-import-list? nil))
    (smart-operators--insert-pair ?\(
                                  ?\)
                                  (lambda (before)
                                    (not (or literal-insertion?
                                             (eq before ?\()
                                             (eq before ?\[)
                                             (eq before ?\\)
                                             (eq before ?@)
                                             (if checked-import-list?
                                                 inside-import-list?
                                               (setf checked-import-list? t
                                                     inside-import-list? (haskell-smart-operators--in-import-list?))))))
                                  (lambda (after)
                                    (not (or literal-insertion?
                                             (eq after ?\))
                                             (eq after ?\])
                                             (eq after ?,)
                                             (if checked-import-list?
                                                 inside-import-list?
                                               (setf checked-import-list? t
                                                     inside-import-list? (haskell-smart-operators--in-import-list?)))))))))

;;;###autoload
(defun haskell-smart-operators-open-bracket ()
  (interactive)
  (let ((literal-insertion? (haskell-smart-operators--literal-insertion?)))
    (smart-operators--insert-pair ?\[
                                  ?\]
                                  (lambda (before)
                                    (not (or literal-insertion?
                                             (eq before ?\()
                                             (eq before ?\[)
                                             (eq before ?\\)
                                             (eq before ?@))))
                                  (lambda (after)
                                    (not (or literal-insertion?
                                             (eq after ?\))
                                             (eq after ?\])
                                             (eq after ?,)))))))

;;;###autoload
(defun haskell-smart-operators-open-brace ()
  (interactive)
  (let ((literal-insertion? (haskell-smart-operators--literal-insertion?))
        (is-hsc? (derived-mode-p 'haskell-hsc-mode))
        (p (point)))
    (when is-hsc?
      (save-excursion
        (when (and (not (zerop (skip-chars-backward " \t")))
                   (eq (char-before) ?#))
          (delete-region (point) p))))
    (smart-operators--insert-pair ?\{
                                  ?\}
                                  (lambda (before)
                                    (not (or literal-insertion?
                                             (eq before ?\()
                                             (eq before ?\[)
                                             (eq before ?\\)
                                             (if is-hsc?
                                                 (eq before ?#)
                                               nil))))
                                  (lambda (after)
                                    (not (or literal-insertion?
                                             (eq after ?\))
                                             (eq after ?\])
                                             (eq after ?,)))))))

;;;###autoload
(define-minor-mode haskell-smart-operators-mode
  "Toggle haskell-smart-operators-mode."
  :init-value nil
  :lighter nil
  :keymap nil
  :global nil)

(provide 'haskell-smart-operators-mode)

;; Local Variables:
;; End:

;; haskell-smart-operators-mode.el ends here
