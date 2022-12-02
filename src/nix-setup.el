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

(awhen (getenv "EMACS_NIX_STORE_DIR")
  (setf nix-store-dir it))

(awhen (getenv "EMACS_NIX_STATE_DIR")
  (setf nix-state-dir it))

;;;###autoload
(nix-prettify-global-mode +1)

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

  (setq-local vim-shift-width 2)

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
    (("C-S-<tab>" "C-S-<iso-lefttab>") indent-relative-backward)))

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
