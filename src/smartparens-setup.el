;; smartparens-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 August 2013
;; Description:

(require 'smartparens)
(require 'smartparens-autoloads)
(require 'macro-util)

(smartparens-global-mode 1)

(setq-default sp-autoskip-closing-pair 'always)
(setf sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil
      sp-navigate-consider-sgml-tags '(html-mode xhtml-mode xml-mode nxml-mode)
      ;; do not reindent on ups
      sp-navigate-reindent-after-up nil
      ;; following variable is buffer local, so it's set in mode-local setups
      ;; sp-autoskip-opening-pair
      ;; sp-autoskip-closing-pair
      sp-autoinsert-if-followed-by-word t
      sp-autoinsert-if-followed-by-same 0
      sp-ignore-modes-list '(;; enable smartparens mode in minibuffer,
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

                             clojure-compilation-mode))

;; these two are the same ones used for paredit
(defadvice sp-forward-slurp-sexp
  (after
   sp-forward-slurp-sexp-remove-initial-whitespace
   activate
   compile)
  (when (and (lisp-pos-is-beginning-of-sexp? (- (point) 1))
             (whitespace-charp (char-after)))
    (delete-whitespace-forward)))

(defadvice sp-backward-slurp-sexp
  (after
   sp-backward-slurp-sexp-remove-initial-whitespace
   activate
   compile)
  (when (and (lisp-pos-is-end-of-sexp? (point))
             (whitespace-charp (char-before)))
    (delete-whitespace-backward)))

(defun smartparens-buffer-local-setup ()
  "Set up buffer-local options for smartparens."
  ;; do not autoinsert new pairs when in stringlike expression
  (setq-local sp-autoskip-opening-pair t)
  (setq-local sp-autoskip-closing-pair 'always)
  (setq-local sp-navigate-consider-stringlike-sexp-in-buffer t))

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


(defun sp-in-minibuffer? (id action context)
  (not (null? (active-minibuffer-window))))

;; do not autoinsert ' pair if the point is preceeded by word.  This
;; will handle the situation when ' is used as a contraction symbol in
;; natural language.  Nil for second argument means to keep the
;; original definition of closing pair.
(sp-pair "'" nil :unless '(sp-point-after-word-p sp-in-minibuffer?))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p) :unless '(sp-in-minibuffer?)))

;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
;; mode) as a first argument.  The macro `sp-with-modes' adds this
;; automatically.  If you want to call sp-local-pair outside this
;; macro, you MUST supply the major mode argument.

;; LaTeX modes
(sp-with-modes '(tex-mode
                 plain-tex-mode
                 latex-mode
                 LaTeX-mode
                 ;; quite a math mode
                 org-mode)
  ;; math modes, yay.  The :actions are provided automatically if
  ;; these pairs do not have global definition.
  (sp-local-pair "$" "$")
  (sp-local-pair "\\[" "\\]")
  (sp-local-pair "`" "'" :unless '(sp-in-minibuffer?))
  (sp-local-tag "\\b" "\\begin{_}" "\\end{_}"))

;; html modes
(sp-with-modes '(sgml-mode
                 html-mode
                 xml-mode)
  (sp-local-pair "<" ">"))

(sp-with-modes '(nxml-mode
                 sgml-mode
                 html-mode
                 xml-mode)
  (sp-local-tag  "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))

(sp-local-pair 'awk-mode "/" "/")

;; /* */ is needed by c mode (and related ones) only
(sp-pair "/*" nil :actions nil)

(defun cc-mode-open-block (id action context)
  (when (eq action 'insert)
    (newline)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode)))

(sp-with-modes '(c-mode
                 c++-mode
                 java-mode
                 awk-mode)
  (sp-local-pair "/*" "*/" :actions '(insert wrap))
  (sp-local-pair "{" "}"
                 :actions '(insert wrap)
                 :post-handlers '(:add cc-mode-open-block)))

(sp-with-modes '(haskell-mode
                 haskell-c-mode
                 haskell-cabal-mode
                 haskell-interactive-mode
                 inferior-haskell-mode)
  ;; ' is identifier part in Haskell, and characters are rare enough
  ;; to make manual entering '' pair feasible
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "\\(" nil :when '(sp-in-string-p))
  (sp-local-pair "\\\\(" nil :when '(sp-in-string-p))
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

;; (defun smartparens-setup ()
;;   (def-keys-for-map ))

(provide 'smartparens-setup)

;; Local Variables:
;; End:

;; smartparens-setup.el ends here
