;; prolog-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  4 December 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'macro-util))

(require 'browse-kill-ring-setup)
(require 'dash)

;;;###autoload
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;;;###autoload
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;;;###autoload
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(defvar prolog-system)
(defvar prolog-inferior-mode-map)

(declare-function prolog-consult-predicate "prolog")
(declare-function prolog-indent-predicate "prolog")

(setf prolog-system 'swi)

;;;###autoload
(setf auto-mode-alist
      (cons '("\\.pl$" . prolog-mode)
            (cons '("\\.pro$" . prolog-mode)
                  (--filter (not (eq 'perl-mode (cdr it)))
                            auto-mode-alist))))
;; (add-to-list 'auto-mode-alist '("\\.m\\'" . mercury-mode))

(defhydra-ext hydra-prolog-dash (:exit t :foreign-keys nil :hint nil)
  "
_e_: consult predicate"
  ("e" prolog-consult-predicate))

(defhydra-derive hydra-prolog-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_<tab>_: reindent predicate"
  ("<tab>" prolog-indent-predicate))

;;;###autoload
(defun prolog-setup ()
  (init-common :use-yasnippet nil
               :use-render-formula t
               :use-whitespace 'tabs-only)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("g"       hydra-prolog-vim-normal-g-ext/body)
    ("-"       hydra-prolog-dash/body)

    ("SPC SPC" switch-to-prolog)

    ("<f6>"    prolog-consult-file)))

;;;###autoload (autoload 'switch-to-prolog "prolog-setup" nil t)
(define-switch-to-interpreter
  switch-to-prolog
  ("*prolog*")
  (run-prolog nil)
  :doc "Pop to prolog interpreter."
  :save-buffer t
  :error-msg "Can't switch to prolog interpreter")

;;;

;;;###autoload
(defun prolog-inferior-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci nil
               :smerge nil)
  (init-repl :create-keymaps nil)

  ;; changed in prolog.el itself
  ;; (modify-syntax-entry ?_ "_" prolog-mode-syntax-table)

  (vim-local-emap "clear" #'vim:comint-clear-buffer-above-prompt)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt)
    ("C-t"      comint-previous-prompt)
    ("C-h"      comint-next-prompt))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap
                     prolog-inferior-mode-map)
    ("C-SPC"    vim:comint-clear-buffer-above-prompt:interactive)
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)
    ("<tab>"    nil)

    ("M-p"      browse-comint-input-history)

    ("<return>" comint-send-input)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)))

;;;###autoload
(defun restart-prolog ()
  "Restart prolog process."
  (interactive)
  (run-prolog t))

;;;###autoload
(add-hook 'prolog-mode-hook #'prolog-setup)
;;;###autoload
(add-hook 'prolog-inferior-mode-hook #'prolog-inferior-setup)

(provide 'prolog-setup)

;; Local Variables:
;; End:

;; prolog-setup.el ends here
