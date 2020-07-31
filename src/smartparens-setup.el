;; smartparens-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 August 2013
;; Description:

(eval-when-compile (require 'subr-x))

(require 'macro-util)
(require 'smart-operators-utils)

(smartparens-global-mode 1)

(setq-default sp-autoskip-closing-pair 'always)
;; do not autoinsert new pairs when in stringlike expression
(setq-default sp-autoskip-opening-pair t)

(setf sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil
      sp-navigate-consider-sgml-tags '(html-mode xhtml-mode xml-mode nxml-mode web-mode)
      ;; do not reindent on ups
      sp-navigate-reindent-after-up nil
      sp-wrap-entire-symbol 'globally
      sp-show-pair-delay 0.01 ;;625
      sp-ignore-modes-list '( ;; enable smartparens mode in minibuffer,
                             ;; and let it bind keys for currently active
                             ;; pairs, then auxiliary keys later in icicle setup
                             ;; minibuffer-inactive-mode
                             ibuffer-mode
                             ediff-mode
                             emms-playlist-mode
                             isearch-mode
                             debugger-mode
                             compilation-mode
                             grep-mode
                             occur-mode
                             completion-list-mode
                             doc-view-mode
                             browse-kill-ring-mode
                             magit-status-mode
                             magit-log-mode
                             magit-key-mode
                             magit-show-branches-mode
                             magit-branch-manager-mode
                             magit-commit-mode
                             magit-diff-mode
                             org-agenda-mode
                             image-mode
                             calendar-mode
                             select-mode
                             haskell-compilation-mode
                             rust-compilation-mode
                             clojure-compilation-mode))

(defun sp-wrap-or-insert (pair-open)
  "Wrap the following expression with PAIR.

This function is a non-interactive helper.  To use this function
interactively, bind the following lambda to a key:

 (lambda (&optional arg) (interactive \"P\") (sp-wrap-with-pair \"(\"))

This lambda accepts the same prefix arguments as
`sp-select-next-thing'.

If region is active and `use-region-p' returns true, the region
is wrapped instead.  This is useful with selection functions in
`evil-mode' to wrap regions with pairs."
  (let* ((p (point))
         (active-pair (--first (equal (car it) pair-open) sp-pair-list)))
    (destructuring-bind (start . end)
        (if (region-active-p)
            (cons (region-beginning) (region-end))
          (or (-when-let (sym-bounds (bounds-of-thing-at-point 'symbol))
                (if (= p (cdr sym-bounds))
                    nil ;; don't wrap if we are at the end of symbol
                  sym-bounds))
              (cons p p)))
      (with-marker (start-marker (copy-marker start))
        (with-marker (end-marker (copy-marker end))
          ;; If point is not in the symbol then don't wrap the next symbol, but
          ;; insert pair at point instead.
          (if (< p start)
              (progn
                (goto-char p)
                (sp-insert-pair pair-open))
            (progn
              (goto-char end)
              (insert (cdr active-pair))
              (goto-char start)
              (insert (car active-pair))
              (sp--indent-region start end))))))))

;; Expand foo {_|_} into
;;
;; foo {
;;     _|_
;; }
(defadvice sp-newline
    (around
     sp-newline-expand-braced-block
     activate
     compile)
  (destructuring-bind
      (start end _is-before? _is-after? is-surrounded?)
      (smart-operators--point-surrounded-by ?\{ ?\})
    (when is-surrounded?
      (delete-region start end))
    ad-do-it
    (when is-surrounded?
      (let ((indent (if indent-tabs-mode
                        "\t"
                      (make-string tab-width ?\s))))
        (newline-and-indent)
        (let ((line-indent (current-line-indentation-str)))
          (forward-line -1)
          (insert line-indent indent))))))

;; these two are the same ones used for paredit
(defadvice sp-forward-slurp-sexp
    (after
     sp-forward-slurp-sexp-remove-initial-whitespace
     activate
     compile)
  (when (and (lisp-pos-is-beginning-of-sexp? (- (point) 1))
             (whitespace-char? (char-after)))
    (delete-whitespace-forward)))

(defadvice sp-backward-slurp-sexp
    (after
     sp-backward-slurp-sexp-remove-initial-whitespace
     activate
     compile)
  (when (and (lisp-pos-is-end-of-sexp? (point))
             (whitespace-char? (char-before)))
    (delete-whitespace-backward)))

(defun sp-backward-up-sexp (&optional arg interactive)
  "Move backward one level of parenthesis.

With negative argument move forward, still one level out."
  (interactive "p\np")
  (sp-up-sexp (if (not (null? arg)) (- arg) -1) interactive))

(vimmize-motion sp-backward-up-sexp
                :name vim:backward-up-sexp
                :exclusive t
                :do-not-adjust-point t)

(vimmize-motion sp-up-sexp
                :name vim:up-sexp
                :exclusive t
                :do-not-adjust-point t)

(defun sp-in-minibuffer? (_id _action _context)
  (minibufferp))

;; do not autoinsert ' pair if the point is preceeded by word.  This
;; will handle the situation when ' is used as a contraction symbol in
;; natural language.  Nil for second argument means to keep the
;; original definition of closing pair.
(sp-pair "'" nil :unless '(sp-point-after-word-p sp-in-minibuffer?))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes sp-lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p) :unless '(sp-in-minibuffer?)))

;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
;; mode) as a first argument.  The macro `sp-with-modes' adds this
;; automatically.  If you want to call sp-local-pair outside this
;; macro, you MUST supply the major mode argument.

(eval-after-load "latex"    '(require 'smartparens-latex))
(eval-after-load "tex-mode" '(require 'smartparens-latex))
(eval-after-load "org-mode" '(require 'smartparens-latex))


(sp-local-pair 'awk-mode "/" "/")

(sp-pair "‘" "’" :actions '(insert wrap))
(sp-pair "“" "”" :actions '(insert wrap))

;; /* */ is needed by c mode (and related ones) only
(sp-pair "/*" "*/" :actions nil)

(defun cc-mode-open-block (_id action _context)
  (when (eq action 'insert)
    (newline)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(sp-with-modes '(c-mode
                 c++-mode
                 java-mode
                 awk-mode)
  (sp-local-pair "/*" "*/" :actions '(insert wrap))
  (sp-local-pair "{" "}"
                 :actions '(insert wrap)
                 :post-handlers '(:add cc-mode-open-block)))

(defun rust-create-braced-block (_id action _context)
  "Open a new brace or bracket expression, with relevant newlines and indent.

E.g. make a following structure

foo {
    _|_
}"
  (when (eq action 'insert)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))


(sp-with-modes '(haskell-mode
                 literate-haskell-mode
                 haskell-c-mode
                 haskell-cabal-mode
                 dante-repl-mode)
  (sp-local-pair "{-#" "#-}")
  (sp-local-pair "'" nil
                 :actions '(insert)
                 :unless '(sp-point-after-word-p
                           sp-haskell-strict-ignore-apostrophe-after-word)
                 :skip-match 'sp-haskell-skip-apostrophe)

  (sp-local-pair "\\(" nil :when '(sp-in-string-p))
  (sp-local-pair "\\\\(" nil :when '(sp-in-string-p))
  (sp-local-pair "`" "`" :actions '(insert wrap)))

(sp-with-modes '(markdown-mode)
  (sp-local-pair "`" "`" :actions '(insert wrap)))

(sp-with-modes '(shell-mode)
  (sp-local-pair "\"" "\"" :actions '(insert wrap)))

(vimmize-function sp-backward-slurp-sexp
                  :name vim:sp-backward-slurp-sexp)
(vimmize-function sp-backward-barf-sexp
                  :name vim:sp-backward-barf-sexp)

(vimmize-function sp-forward-slurp-sexp
                  :name vim:sp-forward-slurp-sexp)
(vimmize-function sp-forward-barf-sexp
                  :name vim:sp-forward-barf-sexp)

(vimmize-function sp-splice-sexp-killing-backward
                  :name vim:sp-splice-sexp-killing-backward)
(vimmize-function sp-splice-sexp-killing-forward
                  :name vim:sp-splice-sexp-killing-forward)
(vimmize-function sp-splice-sexp-killing-around
                  :name vim:sp-splice-sexp-killing-around)

(vimmize-function sp-kill-char
                  :name vim:sp-kill-char)
(vimmize-function sp-backward-kill-char
                  :name vim:sp-backward-kill-char)

(vimmize-function sp-split-sexp
                  :name vim:sp-split-sexp)
(vimmize-function sp-join-sexp
                  :name vim:sp-join-sexp)


;; After starting emacs the first input via minibuffer does not have
;; smartparens-mode enabled. This forces minibuffer to always have
;; smartparens enabled.
(defun smartparens-minibuffer-setup ()
  (smartparens-mode +1))

(add-hook 'minibuffer-setup-hook #'smartparens-minibuffer-setup)

(provide 'smartparens-setup)

;; Local Variables:
;; End:

;; smartparens-setup.el ends here
