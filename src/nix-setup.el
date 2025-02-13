;; nix-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 June 2018
;; Description:

(eval-when-compile
  (require 'macro-util)
  (require 'hydra-setup))

(require 'align-util)
(require 'hydra-setup)
(require 'indentation)
(require 'vim-setup)

(require 'nix-company)
(require 'nix-shebang)
(require 'nix-syntax-table)

(awhen (getenv "EMACS_NIX_STORE_DIR")
  (setf nix-store-dir it))

(awhen (getenv "EMACS_NIX_STATE_DIR")
  (setf nix-state-dir it))

(defconst nix-misc--bounds-of-symbol--base-word-chars "[:alnum:]_")

(defun nix-misc--bounds-of-symbol-impl ()
  (save-excursion
    (let ((init (point)))
      (skip-chars-backward (eval-when-compile (concat nix-misc--bounds-of-symbol--base-word-chars "-")))
      (let ((start (point)))
        (goto-char init)
        (skip-chars-forward (eval-when-compile (concat nix-misc--bounds-of-symbol--base-word-chars "'-")))
        (let ((end (point)))
          (unless (eq start end)
            (cons start end)))))))

;;;###autoload
(defun bounds-of-nix-symbol ()
  (nix-misc--bounds-of-symbol-impl))

;;;###autoload
(put 'nix-symbol 'bounds-of-thing-at-point #'bounds-of-nix-symbol)

(defun nix-align-generic ()
  (interactive)
  (generic-align-on-equals))

(defun nix--simple-indent-newline-same-col ()
  "Make a newline and go to the same column as the current line."
  (interactive "*")
  (delete-horizontal-space t)
  (let ((indent-size
         (save-excursion
           (let* ((start (line-beginning-position))
                  (end (progn
                         (goto-char start)
                         (skip-to-indentation)
                         (point))))
             (when (or (eobp)
                       (not (eq ?\s (char-after))))
               (- end start))))))
    (insert-char ?\n)
    (when indent-size
      (insert-char ?\s indent-size))))

(defun nix--simple-indent-newline-indent ()
  "Make a newline on the current column and indent on step."
  (interactive "*")
  (nix--simple-indent-newline-same-col)
  (insert-char ?\s vim-shift-width))

;;;###autoload
(defun nix-backward-up-indentation-or-sexp ()
  "Nix brother of ‘paredit-backward-up’ that considers both
sexps and indentation levels."
  (interactive)
  (indent-backward-up-indentation-or-sexp #'indent-on-blank-line?))

(vimmize-motion nix-backward-up-indentation-or-sexp
                :name vim:nix-backward-up-indentation-or-sexp
                :exclusive t
                :unadjusted t
                :raw-result t)

;;;###autoload
(defun nix-up-sexp ()
  "Nix brother of ‘paredit-forward-up’ that considers only sexps for now."
  (interactive)
  (paredit-forward-up))

(vimmize-motion nix-up-sexp
                :name vim:nix-up-sexp
                :exclusive t
                :unadjusted t
                :raw-result t)

(defhydra-ext hydra-nix-align (:exit t :foreign-keys nil :hint nil)
  "
_a_: generic
_=_: on equals"
  ("a" nix-align-generic)
  ("=" generic-align-on-equals))

(defhydra-derive hydra-nix-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign"
  ("a" hydra-nix-align/body))

(defhydra-derive hydra-nix-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
<tab>: format buffer"
  ("<tab>" nix-format-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\<flake\\.lock\\'" . json-mode))

;;;###autoload
(defun nix-setup ()
  (init-common :use-whitespace t)

  (setq-local vim-shift-width 2
              search-syntax-table nix-search-fixed-syntax-table)

  (setup-hideshow-yafolding t '(:header-symbol "#" :length-min 3))

  (yafolding-mode +1)

  (company-mode +1)
  (setq-local company-backends '(company-nix))

  (bind-tab-keys nil
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet nil)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("g" hydra-nix-vim-normal-g-ext/body))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("g" hydra-nix-vim-visual-g-ext/body))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-SPC"                           company-complete)
    ("C-<return>"                      nix--simple-indent-newline-indent)
    ("C-<tab>"                         indent-relative-forward)
    (("C-S-<tab>" "C-S-<iso-lefttab>") indent-relative-backward))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap)
    ("*"            search-for-nix-symbol-at-point-forward)
    ("C-*"          search-for-nix-symbol-at-point-forward-new-color)
    ("#"            search-for-nix-symbol-at-point-backward)
    ("C-#"          search-for-nix-symbol-at-point-backward-new-color))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap
                     vim-motion-mode-local-keymap
                     vim-operator-pending-mode-local-keymap)
    ("'" vim:nix-backward-up-indentation-or-sexp:interactive)
    ("q" vim:haskell-up-sexp:interactive)))

;;;###autoload
(add-hook 'nix-mode-hook #'nix-setup)

;;;###autoload
(defun nix-repl-setup ()
  (init-repl :use-whitespace t
             :bind-return t
             :create-keymaps t)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("<tab>"    completion-at-point)
    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)

    ("C-t"      comint-previous-prompt)
    ("C-h"      comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)

    ("C-SPC"    vim:comint-clear-buffer-above-prompt:interactive)
    ("M-p"      browse-comint-input-history)))

;;;###autoload
(add-hook 'nix-repl-mode-hook #'nix-repl-setup)

(provide 'nix-setup)

;; Local Variables:
;; End:

;; nix-setup.el ends here
