;; paredit-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  6 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)

;;;

(eval-after-load
    'paredit
  '(progn
     (defadvice paredit-forward-slurp-sexp
       (after
        paredit-forward-slurp-sexp-remove-initial-whitespace
        activate
        compile)
       (when (and (lisp-pos-is-beginning-of-sexp? (- (point) 1))
                  (whitespace-char? (char-after)))
         (delete-whitespace-forward)))

     (defadvice paredit-backward-slurp-sexp
       (after
        paredit-backward-slurp-sexp-remove-initial-whitespace
        activate
        compile)
       (when (and (lisp-pos-is-end-of-sexp? (point))
                  (whitespace-char? (char-before)))
         (delete-whitespace-backward)))

     ;; fix work in comments
     (redefun paredit-newline ()
       "Insert a newline and indent it.
This is like `newline-and-indent', but it not only indents the line
that the point is on but also the S-expression following the point,
if there is one.
Move forward one character first if on an escaped character.
If in a string, just insert a literal newline.
If in a comment and if followed by invalid structure, call
  `indent-new-comment-line' to keep the invalid structure in a
  comment."
       (interactive)
       (cond ((paredit-in-string-p)
              (newline))
             ((paredit-in-comment-p)
              (newline-and-indent)
              ;; Indent the following S-expression, but don't signal an
              ;; error if there's only a closing delimiter after the point.
              (paredit-ignore-sexp-errors
                (indent-sexp)))
             (t
              (if (paredit-in-char-p)
                (forward-char))
              (newline-and-indent)
              ;; Indent the following S-expression, but don't signal an
              ;; error if there's only a closing delimiter after the point.
              (paredit-ignore-sexp-errors (indent-sexp)))))

     ;; inhibit modification hooks
     (redefun paredit-insert-pair (n open close forward)
       (let ((inhibit-modification-hooks t))
         (let* ((regionp
                 (and (paredit-region-active-p)
                      (paredit-region-safe-for-insert-p)))
                (end
                 (and regionp
                      (not n)
                      (prog1 (region-end) (goto-char (region-beginning))))))
           (let ((spacep (paredit-space-for-delimiter-p nil open)))
             (if spacep (insert " "))
             (insert open)
             (save-excursion
               ;; Move past the desired region.
               (cond (n (funcall forward
                                 (save-excursion
                                   (forward-sexp (prefix-numeric-value n))
                                   (point))))
                     (regionp (funcall forward (+ end (if spacep 2 1)))))
               (insert close)
               (if (paredit-space-for-delimiter-p t close)
                 (insert " ")))))))

     (def-keys-for-map paredit-mode-map
       ("C-k"         nil)
       ("<return>"    nil)
       ("C-S-<left>"  paredit-backward-slurp-sexp)
       ("C-S-<right>" paredit-backward-barf-sexp))

     (defadvice:auto-comment paredit-newline)))

;;; vimmized functions

(vimmize-function paredit-splice-sexp-killing-backward
                  :name vim:splice-sexp-killing-backward
                  :call-n-times t)
(vimmize-function paredit-splice-sexp-killing-forward
                  :name vim:splice-sexp-killing-forward
                  :call-n-times t)


(vimmize-function paredit-backward-slurp-sexp
                  :name vim:backward-slurp-sexp
                  :call-n-times t)
(vimmize-function paredit-backward-barf-sexp
                  :name vim:backward-barf-sexp
                  :call-n-times t)
(vimmize-function paredit-forward-barf-sexp
                  :name vim:forward-barf-sexp
                  :call-n-times t)
(vimmize-function paredit-forward-slurp-sexp
                  :name vim:forward-slurp-sexp
                  :call-n-times t)

;; :call-n-times nil because these two handle numeric arguments themselves
(vimmize-function paredit-forward-kill
                  :name vim:paredit-forward-kill
                  :call-n-times nil)
(vimmize-function paredit-backward-kill
                  :name vim:paredit-backward-kill
                  :call-n-times nil)

(vimmize-function paredit-forward-kill-word
                  :name vim:paredit-forward-kill-word
                  :call-n-times t)
(vimmize-function paredit-backward-kill-word
                  :name vim:paredit-backward-kill-word
                  :call-n-times t)


(defmacro vim:do-motion-with-fixed-motion (fix-func &rest body)
  "Execute BODY and fix motion returned by it with FIX-FUNC that should take
two arguments: motion to be fixed and position, stored at the start of BODY's
execution.

This macro is similar to `vim:do-motion'."
  (declare (indent 1))
  (let ((current-pos (gensym))
        (motion-var (gensym)))
    `(let ((,current-pos (point))
           (,motion-var (progn ,@body)))
       (unless (vim:motion-p ,motion-var)
         (error "vim:do-motion-with-fixed-motion: BODY hasn't returned motion structure"))
       (funcall ,fix-func ,motion-var ,current-pos))))


;; note: plain lowercase *-word functions are doing fine without
;; vim:do-motion-with-fixed-motion corrections
(vim:defmotion vim:paredit-forward-word (inclusive count)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w)))
  (vim:motion-fwd-word :count count))

(vim:defmotion vim:paredit-forward-word-end (inclusive count)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w)))
  (vim:motion-fwd-word-end :count count))

(vim:defmotion vim:paredit-backward-word (inclusive count)
  (goto-char (paredit-skip-backward-for-kill
              (point)
              '(?\w)
              :forward-word (lambda (count)
                              (vim:motion-fwd-word-end :count count))
              :backward-word (lambda (count)
                               (vim:motion-bwd-word-end :count count))))
  (vim:motion-bwd-word :count count))


(vim:defmotion vim:paredit-forward-WORD (inclusive count)
  (vim:do-motion-with-fixed-motion #'vim:change-motion-begin
    (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
    (vim:motion-fwd-WORD :count count)))

(vim:defmotion vim:paredit-forward-WORD-end (inclusive count)
  (vim:do-motion-with-fixed-motion #'vim:change-motion-begin
    (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
    (vim:motion-fwd-WORD-end :count count)))

(vim:defmotion vim:paredit-backward-WORD (inclusive count)
  (vim:do-motion-with-fixed-motion #'vim:change-motion-begin
    (goto-char (paredit-skip-backward-for-kill
                (point)
                '(?\w ?\_)
                :forward-word (lambda (count)
                                (vim:motion-fwd-WORD-end :count count))
                :backward-word (lambda (count)
                                 (vim:motion-bwd-WORD-end :count count))))
    (vim:motion-bwd-WORD :count count)))


;; note: symbol-oriented functions are also working fine without
;; vim:do-motion-with-fixed-motion corrections
(vim:defmotion vim:paredit-inner-symbol (inclusive count)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-inner-symbol :count count))

(vim:defmotion vim:paredit-outer-symbol (inclusive count)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-outer-symbol :count count))


(vim:defmotion vim:paredit-forward-symbol (inclusive count)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-fwd-symbol :count count))

(vim:defmotion vim:paredit-forward-symbol-end (inclusive count)
  (goto-char (paredit-skip-forward-for-kill (point)
                                            '(?\w ?\_)))
  (vim:motion-fwd-symbol-end :count count))

(vim:defmotion vim:paredit-backward-symbol (inclusive count)
  (goto-char (paredit-skip-backward-for-kill
              (point)
              '(?\w ?\_)
              :forward-word (lambda (count)
                              (vim:motion-fwd-symbol-end :count count))
              :backward-word (lambda (count)
                               (vim:motion-bwd-symbol :count count))))
  (vim:motion-bwd-symbol :count count))



;; (defun paredit-forward-kill-symbol ()
;;   "Kill a word forward, skipping over intervening delimiters."
;;   (interactive)
;;   (paredit-kill-forward (lambda () (vim:cmd-delete :motion
;;                                               (vim:motion-outer-symbol :count 1)
;;                                               ;; (vim:motion-fwd-symbol-end :count 1)
;;                                               ))
;;                         '(?\w ?\_)))
;;
;; (defun paredit-backward-kill-symbol ()
;;   "Kill a word backward, skipping over any intervening delimiters."
;;   (interactive)
;;   (paredit-backward-kill (lambda () (vim:cmd-delete :motion
;;                                                (vim:motion-bwd-symbol :count 1)))
;;                          '(?\w ?\_)
;;                          :forward-word (lambda (count)
;;                                          (vim:motion-fwd-symbol-end :count 1))
;;                          :backward-word (lambda (count)
;;                                           (vim:motion-bwd-symbol :count 1))))
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
