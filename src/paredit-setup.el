;; paredit-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  6 April 2013
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'el-patch)
  (require 'macro-util))

(require 'common)
(require 'el-patch)
(require 'paredit)

;;;

;;;###autoload
(el-patch-feature paredit)

;; inhibit modification hooks
(el-patch-defun paredit-insert-pair (n open close forward)
  (el-patch-wrap 1 0
    (with-inhibited-modification-hooks
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
                             (paredit-scan-sexps-hack (point)
                                                      (prefix-numeric-value n))
                             ;; (el-patch-swap
                             ;;   (paredit-scan-sexps-hack (point)
                             ;;                            (prefix-numeric-value n))
                             ;;   (save-excursion
                             ;;     (forward-sexp (prefix-numeric-value n))
                             ;;     (point)))
                             ))
                 (regionp (funcall forward (+ end (if spacep 2 1)))))
           (if (and (not (paredit-in-string-p))
                    (paredit-in-comment-p))
               (newline))
           (insert close)
           (if (paredit-space-for-delimiter-p t close)
               (insert " "))))))))

(defun paredit-forward-slurp-sexp--remove-initial-whitespace ()
  (when (and (lisp-pos-is-beginning-of-sexp? (- (point) 1))
             (whitespace-char? (char-after)))
    (delete-whitespace-forward)))

(defun paredit-backward-slurp-sexp--remove-initial-whitespace ()
  (when (and (lisp-pos-is-end-of-sexp? (point))
             (whitespace-char? (char-before)))
    (delete-whitespace-backward)))

(defun paredit-init ()
  (advice-add 'paredit-forward-slurp-sexp :after #'paredit-forward-slurp-sexp--remove-initial-whitespace)

  (advice-add 'paredit-backward-slurp-sexp :after #'paredit-backward-slurp-sexp--remove-initial-whitespace)

  (def-keys-for-map paredit-mode-map
    ("C-k"         nil)
    ("<return>"    nil)
    ("C-S-<left>"  paredit-backward-slurp-sexp)
    ("C-S-<right>" paredit-backward-barf-sexp))

  (advices/auto-comment paredit-newline))

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
  (vim:motion-fwd-word :count count))

;;;###autoload (autoload 'vim:paredit-forward-word-end "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-word-end (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w)))
  (vim:motion-fwd-word-end :count count))

;;;###autoload (autoload 'vim:paredit-backward-word "paredit-setup" "" t)
(vim-defmotion vim:paredit-backward-word (inclusive count motion-result)
  (goto-char (paredit-skip-backward-for-kill
              (point)
              '(?\w)
              :forward-word (lambda (count)
                              (vim:motion-fwd-word-end :count count))
              :backward-word (lambda (count)
                               (vim:motion-bwd-word-end :count count))))
  (vim:motion-bwd-word :count count))


;;;###autoload (autoload 'vim:paredit-forward-WORD "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-WORD (inclusive count motion-result)
  (vim--do-motion-with-amended-begin
      #'vim-change-motion-begin
    (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
    (vim:motion-fwd-WORD :count count)))

;;;###autoload (autoload 'vim:paredit-forward-WORD-end "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-WORD-end (inclusive count motion-result)
  (vim--do-motion-with-amended-begin
      #'vim-change-motion-begin
    (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
    (vim:motion-fwd-WORD-end :count count)))

;;;###autoload (autoload 'vim:paredit-backward-WORD "paredit-setup" "" t)
(vim-defmotion vim:paredit-backward-WORD (inclusive count motion-result)
  (vim--do-motion-with-amended-begin
      #'vim-change-motion-begin
    (goto-char (paredit-skip-backward-for-kill
                (point)
                '(?\w ?\_)
                :forward-word (lambda (count)
                                (vim:motion-fwd-WORD-end :count count))
                :backward-word (lambda (count)
                                 (vim:motion-bwd-WORD-end :count count))))
    (vim:motion-bwd-WORD :count count)))

;; note: symbol-oriented functions are also working fine without
;; vim--do-motion-with-amended-begin corrections
;;;###autoload (autoload 'vim:paredit-inner-symbol "paredit-setup" "" t)
(vim-defmotion vim:paredit-inner-symbol (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-inner-symbol :count count))

;;;###autoload (autoload 'vim:paredit-outer-symbol "paredit-setup" "" t)
(vim-defmotion vim:paredit-outer-symbol (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-outer-symbol :count count))


;;;###autoload (autoload 'vim:paredit-forward-symbol "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-symbol (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point) '(?\w ?\_)))
  (vim:motion-fwd-symbol :count count))

;;;###autoload (autoload 'vim:paredit-forward-symbol-end "paredit-setup" "" t)
(vim-defmotion vim:paredit-forward-symbol-end (inclusive count motion-result)
  (goto-char (paredit-skip-forward-for-kill (point)
                                            '(?\w ?\_)))
  (vim:motion-fwd-symbol-end :count count))

;;;###autoload (autoload 'vim:paredit-backward-symbol "paredit-setup" "" t)
(vim-defmotion vim:paredit-backward-symbol (inclusive count motion-result)
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
