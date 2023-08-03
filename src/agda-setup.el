;; agda-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 14 May 2014
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'agda-abbrev+)
(require 'common)
(require 'hydra-setup)

(require 'agda2)

(eval-when-compile
  (defvar agda-input-translations))

(declare-function agda-input-setup "agda-input")
(declare-function agda2-load "agda2-mode")
(declare-function agda2-make-case "agda2-mode")
(declare-function agda2-auto-maybe-all "agda2-mode")
(declare-function agda2-compute-normalised-maybe-toplevel "agda2-mode")
(declare-function agda2-give "agda2-mode")
(declare-function agda2-refine "agda2-mode")
(declare-function agda2-goal-and-context "agda2-mode")
(declare-function agda2-goal-and-context-and-inferred "agda2-mode")
(declare-function agda2-previous-goal "agda2-mode")
(declare-function agda2-next-goal "agda2-mode")
(declare-function eri-indent "eri")
(declare-function eri-indent-reverse "eri")
(declare-function agda2-go "agda2-mode")
(declare-function agda2-string-quote "agda2-mode")

(eval-after-load "agda-input"
  `(progn
     (push '("forall" "∀") agda-input-translations)
     (push '("cap" "∩") agda-input-translations)
     (push '("Cap" "⋂") agda-input-translations)
     (push '("cup" "∪") agda-input-translations)
     (push '("Cup" "⋃") agda-input-translations)
     (push '("bb" "𝔹") agda-input-translations)
     (push '("not" "¬") agda-input-translations)
     (push '("bottom" "⊥") agda-input-translations)
     (push '("top" "⊤") agda-input-translations)
     (agda-input-setup)))

(vim-defcmd vim:agda-load-file (nonrepeatable)
  (agda2-load))

(defhydra-ext hydra-agda-align (:exit t :foreign-keys nil :hint nil :base-map (make-sparse-keymap))
  "
_=_:  on equals
_->_: on arrows
_<-_: on left-arrows
_|_:  on guards
_,_:  on commas
_--_: on comments"
  ("="  haskell-align-on-equals)
  ("->" haskell-align-on-arrows)
  ("<-" haskell-align-on-left-arrows)
  ("|"  haskell-align-on-guards)
  (","  haskell-align-on-commas)
  ("--" haskell-align-on-comments))

(defhydra-derive hydra-agda-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign"
  ("a" hydra-agda-align/body))

(defhydra-ext hydra-agda (:exit t :foreign-keys warn :hint nil)
  "
_a_uto
_c_ase split
_e_valuate
_g_ive - Checks whether the term written in the current hole has the right type and, if it does, replaces the hole with that term.
_r_efine - Checks whether the return type of the expression e in the hole matches the expected type

_,_: zoom in into hole, show its goal and context
_._: zoom in into hole, show its goal, context, and infer their type
_b_: previous goal
_f_: next goal"
  ;; add more cases by splitting given argument
  ("c" agda2-make-case)
  ;; insert some value that matches hole’s type
  ("a" agda2-auto-maybe-all)
  ;; query expression and evaluate (normalise) it)
  ("e" agda2-compute-normalised-maybe-toplevel)
  ;; Give. Checks whether the term written in the current hole has the right
  ;; type and, if it does, replaces the hole with that term.
  ("g" agda2-give)
  ;; Refine. Checks whether the return type of the expression e in the hole
  ;; matches the expected type. If so, the hole is replaced by e { }1 ... { }n,
  ;; where a sufficient number of new holes have been inserted.
  ;; If the hole is empty, then the refine command instead inserts a lambda or
  ;; constructor (if there is a unique type-correct choice).
  ("r" agda2-refine)

  ;; zoom in into hole, show its goal and context
  ("," agda2-goal-and-context)
  ;; same as ", ," but tries to infer type of current hole’s contents
  ("." agda2-goal-and-context-and-inferred)
  ;; navigate between holes
  ("b" agda2-previous-goal)
  ("f" agda2-next-goal))

;;;###autoload
(defun agda-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci t
               :use-whitespace 'tabs-only)
  (setup-indent-size 2)
  (bind-tab-keys #'eri-indent
                 #'eri-indent-reverse
                 :enable-yasnippet t)
  (dolist (cmd '("load" "lo" "l"))
    (vim-local-emap cmd #'vim:agda-load-file))
  (def-keys-for-map vim-normal-mode-local-keymap
    (("C-l" "<f6>")    agda2-load)
    ("C-="             input-unicode)
    ("C-."             agda2-goto-definition-keyboard)
    ("C-,"             agda2-go-back)
    ("-"               hydra-agda/body)
    ("C-t"             agda2-previous-goal)
    ("C-h"             agda2-next-goal)

    ("<tab>"           eri-indent)
    (("S-<tab>" "S-<lefttab>" "S-<iso-lefttab>") eri-indent-reverse))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("- e" agda2-compute-normalised-region)
    ("g"   hydra-agda-vim-visual-g-ext/body))
  (agda-abbrev+-setup))

;;;###autoload
(add-hook 'agda2-mode-hook #'agda-setup)

(defun agda2-compute-normalised-region (&optional arg)
  "Computes the normal form of the given expression.
The scope used for the expression is that of the last point inside the current
top-level module.
With a prefix argument \"abstract\" is ignored during the computation."
  (interactive "P")
  (cl-assert (region-active-p))
  (with-region-bounds start end
    (let* ((expr (buffer-substring-no-properties start end))
           (cmd (concat "Cmd_compute_toplevel"
                        (if arg " True" " False")
                        " ")))
      (agda2-go t nil (concat cmd (agda2-string-quote expr))))))

(provide 'agda-setup)

;; Local Variables:
;; End:

;; agda-setup.el ends here
