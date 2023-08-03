;; diff-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 28 August 2014
;; Description:

(eval-when-compile
  (require 'macro-util))

(declare-function diff-hunk-kill "diff-mode")
(declare-function server-edit "server")

(require 'common)
(require 'folding-setup)
(require 'hydra-setup)

(defhydra-derive hydra-diff-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_,_: kill hunk"
  ("," diff-hunk-kill))

(defconst diff--outline-headings
  '(("diff" . 1)
    ("@@" . 2)))
(defconst diff--outline-header-regexp
  (concat "^\\(?:"
          (mapconcat (lambda (x) (concat "\\(?:" (car x) "\\)"))
                     diff--outline-headings
                     "\\|")
          "\\)"))

(defun diff-up-heading ()
  "Move to current hunk start then to start of current file."
  (interactive)
  (if (outline-on-heading-p nil)
      (outline-up-heading 1 nil)
    (outline-back-to-heading nil)))

;;;###autoload
(defun diff-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-hl-line t
               :use-whitespace 'tabs-only
               :smerge nil)
  (hs-minor-mode-initialize
   :start (rx (or "[" "(" "{"))
   :comment-start-re (rx bol
                         (repeat 3 (or "-" "+"))
                         " "))

  ;; Enable outline by hand because diff mode outline is too specialized
  (setf outline-regexp diff--outline-header-regexp
        outline-heading-alist diff--outline-headings)
  (outline-minor-mode +1)

  (setup-folding t t)

  (def-keys-for-map vim-normal-mode-local-keymap
    (("<up>"     "C-t") diff-hunk-prev)
    (("<down>"   "C-h") diff-hunk-next)
    (("C-<up>"   "M-t") diff-file-prev)
    (("C-<down>" "M-h") diff-file-next)
    (("<return>" "SPC") diff-goto-source)
    ("g"                hydra-diff-vim-normal-g-ext/body)
    ("<tab>"            outline-cycle)
    ("<backtab>"        outline-cycle-buffer)
    ("'"                diff-up-heading)))

;;;###autoload
(add-hook 'diff-mode-hook #'diff-mode-setup)

(provide 'diff-mode-setup)

;; Local Variables:
;; End:

;; diff-mode-setup.el ends here
