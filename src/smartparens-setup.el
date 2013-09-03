;; smartparens-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 August 2013
;; Description:

(require 'smartparens)
(require 'macro-util)

(smartparens-global-mode 1)

(setf sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil
      sp-navigate-consider-sgml-tags '(html-mode xhtml-mode xml-mode nxml-mode)
      sp-navigate-consider-stringlike-sexp '(latex-mode awk-mode)
      ;; do not reindent on ups
      sp-navigate-reindent-after-up nil
      sp-autoskip-closing-pair 'always
      sp-autoinsert-if-followed-by-word t
      sp-ignore-modes-list (append sp-ignore-modes-list
                                   '(ibuffer-mode
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
                                     minimap-mode
                                     tagged-buflist-mode

                                     clojure-compilation-mode
                                     nrepl-popup-buffer-mode
                                     nrepl-macroexpansion-minor-mode
                                     nrepl-interaction-mode)))

;; these two are the same ones used for paredit
(defadvice sp-forward-slurp-sexp
  (after
   sp-forward-slurp-sexp-remove-initial-whitespace
   activate
   compile)
  (when (and (lisp-pos-is-beginning-of-sexp? (- (point) 1))
             (whitespace-charp (char-after)))
    (delete-whitespaces-forward)))

(defadvice sp-backward-slurp-sexp
  (after
   sp-backward-slurp-sexp-remove-initial-whitespace
   activate
   compile)
  (when (and (lisp-pos-is-end-of-sexp? (point))
             (whitespace-charp (char-before)))
    (delete-whitespaces-backward)))



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


;; do not autoinsert ' pair if the point is preceeded by word.  This
;; will handle the situation when ' is used as a contraction symbol in
;; natural language.  Nil for second argument means to keep the
;; original definition of closing pair.
(sp-pair "'" nil :unless '(sp-point-after-word-p))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p)))

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
  (sp-local-pair "`" "'")
  (sp-local-tag "\\b" "\\begin{_}" "\\end{_}"))

;; html modes
(sp-with-modes '(sgml-mode
                 html-mode
                 xhtml-mode
                 nxml-mode
                 xml-mode)
  (sp-local-pair "<" ">")
  (sp-local-tag  "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))

(sp-local-pair 'awk-mode "/" "/")

;; /* */ is needed by c mode (and related ones) only
(sp-pair "/*" nil :actions nil)

(sp-with-modes '(c-mode
                 c++-mode
                 java-mode
                 awk-mode)
  (sp-local-pair "/*" "*/" :actions '(insert wrap)))

(def-keys-for-map smartparens-mode-map
  ("<return>" sp-newline))

;; (defun smartparens-setup ()
;;   (def-keys-for-map ))

(provide 'smartparens-setup)

;; Local Variables:
;; End:

;; smartparens-setup.el ends here
