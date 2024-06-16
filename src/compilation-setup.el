;; compilation-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 29 February 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'subr-x)

  (require 'macro-util))

(require 'common)
(require 'compile)
(require 'compilation-navigation)
(require 'xterm-color)

(setf compilation-always-kill t
      ;; don't ask - just save
      compilation-ask-about-save nil
      compilation-max-output-line-length nil)


(defun compilation--apply-ansi-colours-filter (f proc string)
  "Turn ANSI colour codes into colourful text!"
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'compilation--apply-ansi-colours-filter)

;;;###autoload
(defun compilation-jump-to-next-error ()
  "Jump to next error in the compilation buffer."
  (interactive)
  (if (compilation-buffer-p (current-buffer))
      (if-let (pos (text-property-jump-forward 'compilation-message nil t nil))
          pos
        (error "No compilation errors"))
    (error "Not in a compilation buffer")))

;;;###autoload
(defun compilation-jump-to-prev-error ()
  "Jump to previous error in the compilation buffer."
  (interactive)
  (if (compilation-buffer-p (current-buffer))
      (if-let (pos (text-property-jump-backward 'compilation-message nil t nil))
          pos
        (error "No compilation errors"))
    (error "Not in a compilation buffer")))

;;; compilation info

(defun compilation-init-after-load ()
  (def-keys-for-map compilation-button-map
    ("C-m" nil)))

(eval-after-load "compile"
  '(compilation-init-after-load))

(defun compilation-mode-setup ()
  (vim:bind-local-keymaps)
  (setup-hl-paren)
  (hl-line-mode +1)

  (def-keys-for-map compilation-mode-map
    ("C-c C-c" kill-compilation))

  (def-keys-for-map vim-normal-mode-local-keymap
    (("<up>"   "C-t")   compilation-jump-to-prev-error)
    (("<down>" "C-h")   compilation-jump-to-next-error)
    ("q"                remove-buffer)
    (("C-m" "<f9>" "H") recompile)
    ("<return>"         compilation/goto-error)
    ("SPC"              compilation/goto-error-other-window)))

(add-hook 'compilation-mode-hook #'compilation-mode-setup)

(vim-defcmd vim:recompile (nonrepeatable)
  (recompile))

(provide 'compilation-setup)

;; Local Variables:
;; End:

;; compilation-setup.el ends here
