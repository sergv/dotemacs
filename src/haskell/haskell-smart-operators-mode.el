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

(require 'haskell-completions)
(require 'haskell-ghc-support)
(require 'smart-operators-utils)

(defvar haskell-smart-operators--operator-chars
  (let ((tbl (make-hash-table :test #'eq)))
    (loop
      for c across "!#$%&*+-./:<=>?@\\^|~"
      do (puthash c t tbl))
    tbl)
  "Characters that may constitute operators.")

(defun haskell-smart-operators--on-a-line-with-guard? ()
  (save-excursion
    ;; Old version...
    ;; (not (string-match-p "^[ \t]*|"
    ;;                      (buffer-substring-no-properties
    ;;                       (line-beginning-position)
    ;;                       (- (point) 1))))
    (beginning-of-line)
    (and (looking-at-p "^[ \t]+|")
         (point))))

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

(defun haskell-smart-operators--insert-char-optionally-surrounding-with-spaces (char insert-space-after)
  (let* ((disable-smart-operators? current-prefix-arg)
         (line-start-pos (line-beginning-position))

         (pt (point))
         (before (char-before pt))
         (at-beginning-of-buffer? (null before))
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
                                  (char-equal before-ws before-ws2)
                                  (char-equal before-ws ?-))))
                  (setf pt-preceded-by-two-dashes? t)
                  (= pt-before-ws2 (1+ line-start-pos)))))))
         (insert-trailing-space
          (lambda (whitespace-deleted? after)
            ;; Decide whether to insert a space after the operator.
            (when (and insert-space-after
                       (not (and (char-equal char ?\\)
                                 (not at-beginning-of-buffer?)
                                 (char-equal before ?\())))
              (when (or (not after) ;; at end of buffer
                        ;; If the next thing is lambda then we don't want to merge
                        ;; with its \.
                        (char-equal after ?\\)
                        (and (not (char-equal after ?\s))
                             (not (char-equal after ?\)))
                             ;; Don't insert space before backtick.
                             (not (char-equal after ?\`))
                             ;; Do not split '|]' token when we're inserting the '|'.
                             (not (and (char-equal char ?|)
                                       (char-equal after ?\])))
                             ;; Special case for @ since it's part of as-patterns.
                             (if (char-equal char ?@)
                                 whitespace-deleted?
                               (not (gethash after haskell-smart-operators--operator-chars)))))
                (insert-char ?\s))))))
    (cond
      ;; Haddock comments must be treated specially because they're comments
      ;; and we generally disable smart operator insertion within comments.
      ((and handling-haddock-comment?
            (memq char '(?^ ?|)))
       (delete-region pt-before-ws (point))
       (insert-char ?\s)
       (insert-char char)
       (funcall insert-trailing-space nil (char-after)))
      ;; Must check for arrows here because otherwise
      ;; `smart-operators--literal-insertion?' will treat '--'
      ;; as a comment and not allow to do any meaningful work.
      ((and (not handling-haddock-comment?)
            pt-preceded-by-two-dashes?
            (memq char '(?< ?> ?-)))
       (delete-region pt-before-ws (point))
       (insert-char char)
       (funcall insert-trailing-space nil (char-after)))
      ((or disable-smart-operators?
           (smart-operators--literal-insertion?)
           (not (gethash char haskell-smart-operators--operator-chars)))
       (insert-char char))
      (t
       (let ((whitespace-deleted? nil)
             (after (char-after)))
         ;; Decide whether to insert space before the operator.
         (if (and (not (smart-operators--on-empty-string?))
                  ;; If inserting hash then we should not add a space
                  ;; if MagicHash is enabled.
                  (if (and (char-equal char ?#)
                           haskell-smart-operators-mode--have-magic-hash)
                      nil ;; Stop considering whether to inser space.
                    t)
                  (or
                   ;; At beginning of buffer.
                   at-beginning-of-buffer?
                   ;; After | that is a potential guard.
                   (when (char-equal before ?|)
                     (awhen (haskell-smart-operators--on-a-line-with-guard?)
                       (equal it (- pt 1))))
                   (and
                    ;; Do not insert spaces before @ since it's mostly used
                    ;; as as-patterns.
                    (not (char-equal char ?@))
                    (not (char-equal before ?\s))
                    (not (char-equal before ?\())
                    (and (not (char-equal before ?\`))
                         (or (null after)
                             (not (char-equal after ?\`))))
                    (not (and (char-equal char ?|)
                              (or
                               ;; Do not split '[|' token when we're inserting the '['.
                               (char-equal before ?\[)
                               ;; Do not split '[foo|' quasiquoter
                               ;; when we're inserting the '['.
                               (save-excursion
                                 (with-syntax-table haskell-quasiquoter-name-syntax-table
                                   (skip-syntax-backward "w" (line-beginning-position))
                                   (let ((before-far (char-before)))
                                     (and before-far
                                          (char-equal before-far ?\[))))))))
                    (not (gethash before haskell-smart-operators--operator-chars)))))
             (insert-char ?\s)
           ;; Delete spaces backwards if there's operator or open paren char
           ;; before the spaces.
           (let ((delete-whitespace?
                  (save-excursion
                    (skip-syntax-backward " ")
                    (let* ((pt-before-ws (point))
                           (char-before-spaces (char-before pt-before-ws)))
                      (and char-before-spaces ;; not at beginning of buffer
                           (or (gethash char-before-spaces haskell-smart-operators--operator-chars)
                               (char-equal char-before-spaces ?\()
                               ;; If inserting # in MagicHash mode
                               ;; then make it stick to the previous
                               ;; word as well as to operators.
                               (and (char-equal char ?#)
                                    haskell-smart-operators-mode--have-magic-hash
                                    (memq (char-syntax char-before-spaces) '(?w ?_))))
                           (if (char-equal char-before-spaces ?|)
                               ;; Check that it's not a guard.
                               (not (haskell-smart-operators--on-a-line-with-guard?))
                             t))))))
             (when delete-whitespace?
               (delete-whitespace-backward)
               (setf whitespace-deleted? t))))
         ;; Insert operator char.
         (insert-char char)
         (funcall insert-trailing-space whitespace-deleted? after))))))

;;;###autoload
(defun haskell-smart-operators-self-insert (arg)
  "Insert charater and take care to surround it with spaces."
  (interactive "p")
  (unless (characterp last-command-event)
    (error "Last event is not a character: %s" last-command-event))
  (haskell-smart-operators--insert-char-surrounding-with-spaces last-command-event))

;;;###autoload
(defun haskell-smart-operators-$ ()
  "Swap parens with a dollar."
  (interactive)
  (let ((start (point))
        (strip-next-parens?
         (save-excursion
           (skip-syntax-backward " ")
           (awhen (char-before)
             (not (char-equal it ?\<)))))
        (paren-start-pos nil))
    (when (and strip-next-parens?
               (save-excursion
                 (skip-syntax-forward " ")
                 (let ((next-char (char-after)))
                   (when (and next-char
                              (char= next-char ?\())
                     (setf paren-start-pos (point))
                     t))))
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
  (interactive)
  (let ((pt (point)))
    (destructuring-bind
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

;;;###autoload
(defalias 'haskell-smart-operators-comma #'smart-operators-comma)

;;;###autoload
(defun haskell-smart-operators-dot ()
  "Insert comma followed by space."
  (interactive)
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
               (not (memq (char-after) '(?\s ?\) ?\] ?\}))))
      (insert-char ?\s))))

;;;###autoload
(defun haskell-smart-operators-hash ()
  "Smart insertion of #."
  (interactive)
  (destructuring-bind
      (pt-pragma-start pt-pragma-end is-surrounded-for-pragma?)
      (smart-operators--point-surrounded-by2 ?\{ ?- ?- ?\})
    (destructuring-bind
        (pt-c2hs-start pt-c2hs-end is-before? is-after? is-surrounded-for-c2hs?)
        (smart-operators--point-surrounded-by ?\{ ?\})
      (cond (is-surrounded-for-pragma?
             (delete-region pt-pragma-start pt-pragma-end)
             (insert "#  #")
             (forward-char -2)
             (let ((pragma (ivy-completing-read "Pragma: "
                                                haskell-completions--pragma-names)))
               (insert pragma " ")
               (when (string= pragma "LANGUAGE")
                 (insert
                  (ivy-completing-read
                   "Language: "
                   haskell-ghc-supported-extensions)))))
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
  (interactive)
  (let ((preceded-by-double-colon?
         (save-excursion
           (skip-syntax-backward " ")
           (preceded-by2 ?: ?:))))
    (if preceded-by-double-colon?
        (let ((before (char-before)))
          (when (and before
                     (not (eq before ?\s)))
            (insert-char ?\s))
          (insert-char ?!))
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?!))))

(defun haskell-smart-operators-exclamation-mark--impl (insert-space-after)
  (haskell-smart-operators--insert-char-optionally-surrounding-with-spaces ?! insert-space-after))

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

(defvar-local haskell-smart-operators-mode--have-magic-hash nil
  "Whether MagicHash extension is enabled in current buffer.")

(defvar-local haskell-smart-operators-mode--magic-hash-updated nil
  "Whether MagicHash extension is enabled in current buffer.")


(defun haskell-smart-operators-mode--update-magic-hash ()
  (setf haskell-smart-operators-mode--magic-hash-updated t)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (setf haskell-smart-operators-mode--have-magic-hash
            (if (re-search-forward "\\_<MagicHash\\_>" nil t)
                t
              nil)))))

;;;###autoload
(define-minor-mode haskell-smart-operators-mode
  "Toggle haskell-smart-operators-mode."
  nil ;; init
  nil ;; modeline
  nil ;; keymap
  :global nil
  (progn
    (haskell-smart-operators-mode--update-magic-hash)
    (if haskell-smart-operators-mode
        (progn
          (dolist (hook '(after-save-hook after-revert-hook))
            (add-hook
             hook
             #'haskell-smart-operators-mode--update-magic-hash
             nil     ;; append
             t       ;; local
             )))
      (progn
        (dolist (hook '(after-save-hook after-revert-hook))
          (remove-hook
           hook
           #'haskell-smart-operators-mode--update-magic-hash
           t ;; local
           ))))))

(provide 'haskell-smart-operators-mode)

;; Local Variables:
;; End:

;; haskell-smart-operators-mode.el ends here
