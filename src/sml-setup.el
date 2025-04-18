;; sml-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 26 April 2016
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'indentation)

(defvar sml-indent-level)

(setf sml-indent-level 2)

;;;###autoload (autoload 'switch-to-sml-repl "sml-setup" nil t)
(define-switch-to-interpreter
  switch-to-sml-repl
  ("*sml-repl*")
  (sml-run)
  :doc "Pop to sml repl."
  :save-buffer t
  :error-msg "Can't switch to sml repl")

;;;###autoload
(defun sml-format-buffer ()
  (interactive)
  (indent-whole-buffer))

(puthash 'sml-mode
         #'sml-format-buffer
         *mode-indent-functions-table*)

;;;###autoload
(defun sml-mode-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci t)
  (bind-tab-keys #'indent-for-tab-command
                 nil
                 :enable-yasnippet t)
  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC" switch-to-sml-repl))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("j" sml-send-region)))

;;;###autoload
(add-hook 'sml-mode-hook #'sml-mode-setup)

;;;###autoload
(defun inferior-sml-mode-setup ()
  (init-repl :bind-return nil)
  (vim-local-emap "clear" #'vim:comint-clear-buffer-above-prompt))

;;;###autoload
(add-hook 'inferior-sml-mode-hook #'inferior-sml-mode-setup)

(provide 'sml-setup)

;; Local Variables:
;; End:

;; sml-setup.el ends here
