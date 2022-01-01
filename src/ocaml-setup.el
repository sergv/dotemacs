;; ocaml-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  9 April 2013
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'macro-util))

(require 'common)
(require 'comint-setup)
(require 'indentation)

;;; Prelude

(defalias 'ocaml-mode #'tuareg-mode)

;;; Helper function

;;;###autoload (autoload 'switch-to-ocaml-repl "ocaml-setup" nil t)
(define-switch-to-interpreter
  switch-to-ocaml-repl
  ("*ocaml-repl*")
  (tuareg-run-ocaml)
  :doc "Pop to ocaml repl."
  :save-buffer t
  :error-msg "Can't switch to ocaml repl")

(defun ocaml-format-buffer ()
  (interactive)
  (save-excursion
    (indent-whole-buffer)))

(puthash 'tuareg-mode
         #'ocaml-format-buffer
         *mode-indent-functions-table*)

;;; ocaml-setup

;;;###autoload
(defun ocaml-setup ()
  (init-common :use-render-formula t
               :sp-slurp-sexp-insert-space nil)
  (bind-tab-keys #'indent-for-tab-command
                 nil
                 :enable-yasnippet t)
  (pretty-ligatures-install-safe!)
  (def-keys-for-map (vim:normal-mode-local-keymap)
    ("SPC SPC" switch-to-ocaml-repl)
    ("<f6>"    tuareg-eval-buffer)))

;;;###autoload
(add-hook 'tuareg-mode-hook #'ocaml-setup)
;;;###autoload
(add-hook 'tuareg-menhir-mode-hook #'ocaml-setup)

;;; ocaml repl

(defun ocaml-interactive-send-input ()
  "Send current line, appending ;; if necessary."
  (interactive)
  (unless (tuareg-interactive-end-of-phrase)
    (insert ";;"))
  (comint-send-input)
  (goto-char (point-max)))

;;;###autoload
(defun ocaml-interactive-setup ()
  (init-repl :bind-return nil
             :sp-slurp-sexp-insert-space nil)
  (pretty-ligatures-install-safe!)
  (vim:local-emap "clear" #'vim:comint-clear-buffer-above-prompt)
  (def-keys-for-map tuareg-interactive-mode-map
    ("C-SPC"      vim:comint-clear-buffer-above-prompt)
    ("<return>"   ocaml-interactive-send-input)
    ("C-<return>" sp-newline)
    ("SPC SPC"    comint-clear-prompt)
    ("<f6>"       tuareg-interrupt-ocaml)))

;;;###autoload
(add-hook 'tuareg-interactive-mode-hook #'ocaml-interactive-setup)

(provide 'ocaml-setup)

;; Local Variables:
;; End:

;; ocaml-setup.el ends here
