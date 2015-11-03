;; ocaml-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  9 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'comint-setup)

;;; Prelude

(add-to-load-path
  (concat +emacs-standalone-path+ "/tuareg"))
(load "tuareg-site-file")

(defalias 'ocaml-mode #'tuareg-mode)

;;; Helper function

(define-switch-to-interpreter
  switch-to-ocaml-repl
  ("*ocaml-toplevel*")
  (tuareg-run-ocaml)
  :doc "Pop to ocaml repl."
  :save-buffer t
  :error-msg "Can't switch to ocaml repl")

(defun ocaml-indent-buffer ()
  (interactive)
  (save-excursion
    (indent-whole-buffer)))

(add-to-list '*mode-buffer-indent-function-alist*
             (cons 'tuareg-mode #'ocaml-indent-buffer))

;;; ocaml-setup

(defun ocaml-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci t
               :sp-slurp-sexp-insert-space nil)
  (bind-tab-keys #'indent-for-tab-command
                 nil
                 :enable-yasnippet t)
  (def-keys-for-map tuareg-mode-map
    ("SPC SPC" switch-to-ocaml-repl)
    ("<f6>"    tuareg-eval-buffer)))

(add-hook 'tuareg-mode-hook #'ocaml-setup)

;;; ocaml repl

(defun ocaml-interactive-send-input ()
  "Send current line, appending ;; if necessary."
  (interactive)
  (unless (tuareg-interactive-end-of-phrase)
    (insert ";;"))
  (comint-send-input)
  (goto-char (point-max)))

(defun ocaml-interactive-setup ()
  (init-repl :bind-return nil
             :sp-slurp-sexp-insert-space nil)
  (linum-mode 1)
  (def-keys-for-map tuareg-interactive-mode-map
    ("<return>"   ocaml-interactive-send-input)
    ("C-<return>" sp-newline)
    ("SPC SPC"    comint-clear-prompt)
    ("C-SPC"      comint-clear-buffer-above-prompt)
    ("<f6>"       tuareg-interrupt-ocaml)))

(add-hook 'tuareg-interactive-mode-hook #'ocaml-interactive-setup)

;;; end

(provide 'ocaml-setup)

;; Local Variables:
;; End:

;; ocaml-setup.el ends here
