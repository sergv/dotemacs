;; paredit-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  6 April 2013
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'macro-util))

(require 'el-patch)
(require 'paredit)

(require 'common)

(require 'comment-util)

;;;

;;;###autoload
(el-patch-feature paredit)

(defun paredit-forward-slurp-sexp--remove-initial-whitespace (&optional _)
  (when (and (lisp-pos-is-beginning-of-sexp? (- (point) 1))
             (whitespace-char? (char-after)))
    (delete-whitespace-forward)))

(defun paredit-backward-slurp-sexp--remove-initial-whitespace (&optional _)
  (when (and (lisp-pos-is-end-of-sexp? (point))
             (whitespace-char? (char-before)))
    (delete-whitespace-backward)))


(defun paredit-dummy-indent (_pos)
  0)

;; Configure global paredit without surprises which other modes could
;; override if needed.
(setf paredit-indent-sexp-function #'ignore
      paredit-indent-line-function #'ignore
      paredit-calculate-indent #'paredit-dummy-indent
      paredit-indent-region-function nil
      paredit-in-char-p-function #'paredit-never-in-char-p
      paredit-space-for-delimiter-predicates (list #'paredit-setup--put-space-before-open-paren?))

(defvar paredit--put-space-before-open-paren? nil)

(defun paredit-setup--put-space-before-open-paren? (endp delim)
  (if (eq delim ?\()
      paredit--put-space-before-open-paren?
    t))

(cl-defun prepare-paredit (&key indent-sexp indent-line calc-indent indent-region in-char-p (space-before-open-paren 'not-specified))
  (when indent-sexp
    (cl-assert (functionp indent-sexp))
    (setq-local paredit-indent-sexp-function indent-sexp))
  (when indent-line
    (cl-assert (functionp indent-line))
    (setq-local paredit-indent-line-function indent-line))
  (when calc-indent
    (cl-assert (functionp calc-indent))
    (setq-local paredit-calculate-indent calc-indent))
  (when indent-region
    (cl-assert (functionp indent-region))
    (setq-local paredit-indent-region-function indent-region))
  (when in-char-p
    (cl-assert (functionp in-char-p))
    (setq-local paredit-in-char-p-function in-char-p))
  (unless (eq space-before-open-paren 'not-specified)
    (setq-local paredit--put-space-before-open-paren? space-before-open-paren)))

;;;###autoload
(defun set-up-paredit ()
  (when comment-util-mode
    (awhen (comment-format-one-line (comment-util-current-format))
      (let ((one-line (concat it comment-util--spaces-after-comment)))
        (setq-local paredit-comment-prefix-toplevel one-line
                    paredit-comment-prefix-code one-line
                    paredit-comment-prefix-margin one-line))))

  ;; REALLY don’t want paredit keybindings. I make my own bindings.
  (paredit-mode -1))


(defun paredit-init ()
  (advice-add 'paredit-forward-slurp-sexp :after #'paredit-forward-slurp-sexp--remove-initial-whitespace)

  (advice-add 'paredit-backward-slurp-sexp :after #'paredit-backward-slurp-sexp--remove-initial-whitespace)

  (def-keys-for-map paredit-mode-map
    ;; ‘fill-paragraph’ is so much better
    ("M-q"       nil)
    ("C-k"       nil)
    ("<return>"  nil)
    ("C-<left>"  nil)
    ("C-<right>" nil)
    ("M-<left>"  paredit-backward-slurp-sexp)
    ("M-<right>" paredit-backward-barf-sexp))

  (comment-util-auto-comment-advice paredit-newline))

(eval-after-load 'paredit '(paredit-init))

;;; vimmized functions

;;;###autoload (autoload 'vim:splice-sexp-killing-backward "paredit-setup" "" t)
(vimmize-function paredit-splice-sexp-killing-backward :name vim:splice-sexp-killing-backward :call-n-times t)
;;;###autoload (autoload 'vim:splice-sexp-killing-forward "paredit-setup" "" t)
(vimmize-function paredit-splice-sexp-killing-forward :name vim:splice-sexp-killing-forward :call-n-times t)


;;;###autoload (autoload 'vim:backward-slurp-sexp "paredit-setup" "" t)
(vimmize-function paredit-backward-slurp-sexp :name vim:backward-slurp-sexp :call-n-times t)
;;;###autoload (autoload 'vim:backward-barf-sexp "paredit-setup" "" t)
(vimmize-function paredit-backward-barf-sexp :name vim:backward-barf-sexp :call-n-times t)
;;;###autoload (autoload 'vim:forward-slurp-sexp "paredit-setup" "" t)
(vimmize-function paredit-forward-slurp-sexp :name vim:forward-slurp-sexp :call-n-times t)
;;;###autoload (autoload 'vim:forward-barf-sexp "paredit-setup" "" t)
(vimmize-function paredit-forward-barf-sexp :name vim:forward-barf-sexp :call-n-times t)

;;;###autoload (autoload 'vim:splice-sexp "paredit-setup" "" t)
(vimmize-function paredit-splice-sexp :name vim:splice-sexp :call-n-times t)
;;;###autoload (autoload 'vim:split-sexp "paredit-setup" "" t)
(vimmize-function paredit-split-sexp :name vim:split-sexp :call-n-times t)
;;;###autoload (autoload 'vim:join-sexps "paredit-setup" "" t)
(vimmize-function paredit-join-sexps :name vim:join-sexps :call-n-times t)
;;;###autoload (autoload 'vim:raise-sexp "paredit-setup" "" t)
(vimmize-function paredit-raise-sexp :name vim:raise-sexp :call-n-times t)
;;;###autoload (autoload 'vim:wrap-sexp "paredit-setup" "" t)
(vimmize-function paredit-wrap-sexp :name vim:wrap-sexp :call-n-times t)
;;;###autoload (autoload 'vim:convolute-sexp "paredit-setup" "" t)
(vimmize-function paredit-convolute-sexp :name vim:convolute-sexp :call-n-times t)

;; :call-n-times nil because these two handle numeric arguments themselves
;;;###autoload (autoload 'vim:paredit-forward-kill "paredit-setup" "" t)
(vimmize-function paredit-forward-kill :name vim:paredit-forward-kill :call-n-times nil)
;;;###autoload (autoload 'vim:paredit-backward-kill "paredit-setup" "" t)
(vimmize-function paredit-backward-kill :name vim:paredit-backward-kill :call-n-times nil)

;;;###autoload (autoload 'vim:paredit-forward-kill-word "paredit-setup" "" t)
(vimmize-function paredit-forward-kill-word :name vim:paredit-forward-kill-word :call-n-times t)
;;;###autoload (autoload 'vim:paredit-backward-kill-word "paredit-setup" "" t)
(vimmize-function paredit-backward-kill-word :name vim:paredit-backward-kill-word :call-n-times t)

(defmacro vim--do-motion-with-amended-begin (&rest body)
  "Execute BODY and amend the motion object it returns to start at
the position before BODY executed.

This macro is similar to `vim:do-motion'."
  (declare (indent 0))
  (let ((current-pos '#:current-pos)
        (motion-var '#:motion-var))
    `(let ((,current-pos (point))
           (,motion-var (progn ,@body)))
       (unless (vim-motion-p ,motion-var)
         (error "vim:do-motion-with-fixed-motion: BODY hasn't returned motion structure"))
       (vim-change-motion-begin ,motion-var ,current-pos))))

;; note: plain lowercase *-word functions are doing fine without
;; vim--do-motion-with-amended-begin corrections
;;;###autoload (autoload 'vim:paredit-forward-word "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-word (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w)))
  (vim:motion-fwd-word:wrapper :count count))

;;;###autoload (autoload 'vim:paredit-forward-word-end "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-word-end (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w)))
  (vim:motion-fwd-word-end:wrapper :count count))

;;;###autoload (autoload 'vim:paredit-backward-word "paredit-setup" "" t)
(vim-defmotion vim:paredit-backward-word (inclusive count motion-result)
  (goto-char (paredit-skip-backward-for-kill
              (point)
              '(?\w)
              :forward-word (lambda (count)
                              (vim:motion-fwd-word-end:wrapper :count count))
              :backward-word (lambda (count)
                               (vim:motion-bwd-word-end:wrapper :count count))))
  (vim:motion-bwd-word:wrapper :count count))


;;;###autoload (autoload 'vim:paredit-forward-WORD "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-WORD (inclusive count motion-result)
  (vim--do-motion-with-amended-begin
      #'vim-change-motion-begin
    (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
    (vim:motion-fwd-WORD:wrapper :count count)))

;;;###autoload (autoload 'vim:paredit-forward-WORD-end "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-WORD-end (inclusive count motion-result)
  (vim--do-motion-with-amended-begin
      #'vim-change-motion-begin
    (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
    (vim:motion-fwd-WORD-end:wrapper :count count)))

;;;###autoload (autoload 'vim:paredit-backward-WORD "paredit-setup" "" t)
(vim-defmotion vim:paredit-backward-WORD (inclusive count motion-result)
  (vim--do-motion-with-amended-begin
      #'vim-change-motion-begin
    (goto-char (paredit-skip-backward-for-kill
                (point)
                '(?\w ?\_)
                :forward-word (lambda (count)
                                (vim:motion-fwd-WORD-end:wrapper :count count))
                :backward-word (lambda (count)
                                 (vim:motion-bwd-WORD-end:wrapper :count count))))
    (vim:motion-bwd-WORD:wrapper :count count)))

;; note: symbol-oriented functions are also working fine without
;; vim--do-motion-with-amended-begin corrections
;;;###autoload (autoload 'vim:paredit-inner-symbol "paredit-setup" "" t)
(vim-defmotion vim:paredit-inner-symbol (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-inner-symbol:wrapper :count count))

;;;###autoload (autoload 'vim:paredit-outer-symbol "paredit-setup" "" t)
(vim-defmotion vim:paredit-outer-symbol (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-outer-symbol:wrapper :count count))


;;;###autoload (autoload 'vim:paredit-forward-symbol "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-symbol (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-fwd-symbol:wrapper :count count))

;;;###autoload (autoload 'vim:paredit-forward-symbol-end "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-symbol-end (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point)
                                            '(?\w ?\_)))
  (vim:motion-fwd-symbol-end:wrapper :count count))

;;;###autoload (autoload 'vim:paredit-backward-symbol "paredit-setup" "" t)
(vim-defmotion vim:paredit-backward-symbol (inclusive count motion-result)
  (goto-char (paredit-skip-backward-for-kill
              (point)
              '(?\w ?\_)
              :forward-word (lambda (count)
                              (vim:motion-fwd-symbol-end:wrapper :count count))
              :backward-word (lambda (count)
                               (vim:motion-bwd-symbol:wrapper :count count))))
  (vim:motion-bwd-symbol:wrapper :count count))

(defun paredit-setup--wrap-or-insert (open close escape?)
  "Wrap the following expression or the active region with PAIR."
  (let* ((p (point))
         (start p)
         (end p))
    (if (region-active-p)
        (with-region-bounds-unadj start2 end2
          (setf start start2
                end end2))
      (when-let (sym-bounds (bounds-of-thing-at-point 'symbol))
        ;; don't wrap if we are at the end of symbol
        (unless (= p (cdr sym-bounds))
          (setf start (car sym-bounds)
                end (cdr sym-bounds)))))
    ;; If point is not in the symbol then don't wrap the next symbol, but
    ;; insert pair at point instead.
    (if (< p start)
        (progn
          (goto-char p)
          (insert open close))
      (progn
        (goto-char end)
        (insert-char close)
        (goto-char start)
        (insert-char open)

        (setq end (+ end 2))

        (if escape?
            (progn
              (setf end (paredit-setup--escape-region (+ start 1) (- end 1)))
              (paredit-indent-region start end ))
          (paredit-indent-region start end))))))

(defun paredit-setup--escape-region (start end)
  (save-excursion
    (goto-char start)
    (paredit-forward-for-quote end)))

(defun vim-wrap-parens ()
  "Wrap region in (...)."
  (interactive)
  (paredit-setup--wrap-or-insert ?\( ?\) nil))

(defun vim-wrap-braces ()
  "Wrap region in [...]."
  (interactive)
  (paredit-setup--wrap-or-insert ?\[ ?\] nil))

(defun vim-wrap-brackets ()
  "Wrap region in {...}."
  (interactive)
  (paredit-setup--wrap-or-insert ?\{ ?\} nil))

(defun vim-wrap-angles ()
  "Wrap region in <...>."
  (interactive)
  (paredit-setup--wrap-or-insert ?\< ?\> nil))

(defun vim-wrap-dquotes ()
  "Wrap region in \"...\"."
  (interactive)
  (paredit-setup--wrap-or-insert ?\" ?\" t))

(defun vim-wrap-typographical-single-quotes ()
  "Wrap region in ‘...’."
  (interactive)
  (paredit-setup--wrap-or-insert ?\‘ ?\’ nil))

(defun vim-wrap-typographical-double-quotes ()
  "Wrap region in “...”."
  (interactive)
  (paredit-setup--wrap-or-insert ?\“ ?\” nil))

(defun vim-wrap-backticks ()
  "Wrap region in `...`."
  (interactive)
  (paredit-setup--wrap-or-insert ?\` ?\` nil))

;; (defun paredit-forward-kill-symbol ()
;;   "Kill a word forward, skipping over intervening delimiters."
;;   (interactive)
;;   (paredit-kill-forward (lambda () (vim:cmd-delete:wrapper :motion
;;                                               (vim:motion-outer-symbol:wrapper :count 1)
;;                                               ;; (vim:motion-fwd-symbol-end:wrapper :count 1)
;;                                               ))
;;                         '(?\w ?\_)))
;;
;; (defun paredit-backward-kill-symbol ()
;;   "Kill a word backward, skipping over any intervening delimiters."
;;   (interactive)
;;   (paredit-backward-kill (lambda () (vim:cmd-delete:wrapper :motion
;;                                                (vim:motion-bwd-symbol:wrapper :count 1)))
;;                          '(?\w ?\_)
;;                          :forward-word (lambda (count)
;;                                          (vim:motion-fwd-symbol-end:wrapper :count 1))
;;                          :backward-word (lambda (count)
;;                                           (vim:motion-bwd-symbol:wrapper :count 1))))
;;
;;
;; (vimmize-function paredit-forward-kill-symbol
;;                   :name vim:paredit-forward-kill-symbol
;;                   :call-n-times t)
;;
;; (vimmize-function paredit-backward-kill-symbol
;;                   :name vim:paredit-backward-kill-symbol
;;                   :call-n-times t)

(provide 'paredit-setup)

;; Local Variables:
;; End:

;; paredit-setup.el ends here
