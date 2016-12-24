;; agda-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 14 May 2014
;; Description:

(require 'agda-abbrev+)
(require 'common)

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

;;;###autoload
(require 'agda2)

;;;###autoload (autoload 'agda-align-on-equals "agda-setup.el" nil t)
(make-align-function agda-align-on-equals
                     "=[^=]"
                     :require-one-or-more-spaces t)
;;;###autoload (autoload 'agda-align-on-arrows "agda-setup.el" nil t)
(make-align-function agda-align-on-arrows
                     "\\(?:->\\|→\\)\\(?: \\|$\\)")
;;;###autoload (autoload 'agda-align-on-left-arrows "agda-setup.el" nil t)
(make-align-function agda-align-on-left-arrows
                     "\\(?:<-\\|←\\)\\(?: \\|$\\)")
;;;###autoload (autoload 'agda-align-on-pipes "agda-setup.el" nil t)
(make-align-function agda-align-on-pipes
                     "|\\(?:[^|]\\|$\\)"
                     :require-one-or-more-spaces t)
;;;###autoload (autoload 'agda-align-on-commas "agda-setup.el" nil t)
(make-align-function agda-align-on-commas
                     ",\\(?:[^,\)]\\|$\\)")
;;;###autoload (autoload 'agda-align-on-comments "agda-setup.el" nil t)
(make-align-function agda-align-on-comments
                     "--+\\(?: \\|$\\)"
                     :require-one-or-more-spaces t)
;;;###autoload (autoload 'agda-align-on-colons "agda-setup.el" nil t)
(make-align-function agda-align-on-colons
                     "\\(?::[^:]\\)")

(vim:defcmd vim:agda-load-file (nonrepeatable)
  (agda2-load))

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
  (vim:local-emap "load" #'vim:agda-load-file)
  (vim:local-emap "lo"   #'vim:agda-load-file)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f6>"            agda2-load)
    ("<tab>"           eri-indent)
    ("S-<tab>"         eri-indent-reverse)
    ("S-<lefttab>"     eri-indent-reverse)
    ("S-<iso-lefttab>" eri-indent-reverse)
    ("+"               input-unicode)
    ("C-."             agda2-goto-definition-keyboard)
    ("C-,"             agda2-go-back)
    ;; zoom in into hole, show its goal and context
    ("- ,"             agda2-goal-and-context)
    ;; same as ", ," but tries to infer type of current hole's contents
    ("- ."             agda2-goal-and-context-and-inferred)
    ;; add more cases by splitting given argument
    ("- c"             agda2-make-case)
    ;; insert some value that matches hole's type
    ("- a"             agda2-auto)
    ;; query expression and evaluate (normalise) it)
    ("j"               agda2-compute-normalised-maybe-toplevel)
    ;; Give. Checks whether the term written in the current hole has the right
    ;; type and, if it does, replaces the hole with that term.
    ("- g"             agda2-give)
    ;; Refine. Checks whether the return type of the expression e in the hole
    ;; matches the expected type. If so, the hole is replaced by e { }1 ... { }n,
    ;; where a sufficient number of new holes have been inserted.
    ;; If the hole is empty, then the refine command instead inserts a lambda or
    ;; constructor (if there is a unique type-correct choice).
    ("- r"             agda2-refine)
    ;; navigate between holes
    ("- b"             agda2-previous-goal)
    ("- f"             agda2-next-goal)
    ("C-t"             agda2-previous-goal)
    ("C-h"             agda2-next-goal))
  (def-keys-for-map vim:visual-mode-local-keymap
    ("j"               agda2-compute-normalised-region)
    ("g a ="           agda-align-on-equals)
    ("g a - >"         agda-align-on-arrows)
    ("g a < -"         agda-align-on-left-arrows)
    ("g a |"           agda-align-on-pipes)
    ("g a ,"           agda-align-on-commas)
    ("g a - -"         agda-align-on-comments)
    ("g a :"           agda-align-on-colons))
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
  (multiple-value-bind (start end) (get-region-bounds)
    (let* ((expr (buffer-substring-no-properties start end))
           (cmd (concat "Cmd_compute_toplevel"
                        (if arg " True" " False")
                        " ")))
      (agda2-go t nil (concat cmd (agda2-string-quote expr))))))

(provide 'agda-setup)

;; Local Variables:
;; End:

;; agda-setup.el ends here
