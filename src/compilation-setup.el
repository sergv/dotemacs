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
      compilation-ask-about-save nil)


(defun compilation--apply-ansi-colours-filter (f proc string)
  "Turn ANSI colour codes into colourful text!"
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'compilation--apply-ansi-colours-filter)

;;;###autoload
(defun compilation-jump-to-next-error ()
  "Jump to next error in the compilation buffer."
  (interactive)
  (if (compilation-buffer-p (current-buffer))
      (text-property-jump-forward 'compilation-message nil t nil)
    (error "Not in a compilation buffer")))

;;;###autoload
(defun compilation-jump-to-prev-error ()
  "Jump to previous error in the compilation buffer."
  (interactive)
  (if (compilation-buffer-p (current-buffer))
      (text-property-jump-backward 'compilation-message nil t nil)
    (error "Not in a compilation buffer")))

;;; compilation info

(eval-after-load "compile"
  '(progn
     (def-keys-for-map compilation-mode-map
       +vi-keys+
       +vim-special-keys+
       +vim-search-keys+
       +vim-word-motion-keys+
       (("<up>"   "t")     compilation-jump-to-prev-error)
       (("<down>" "h")     compilation-jump-to-next-error)
       ("M-p"              nil)
       ("q"                remove-buffer)
       ("C-c C-c"          kill-compilation)
       ("m"                pseudovim-motion-jump-item)
       ("0"                pseudovim-motion-beginning-of-line-or-digit-argument)
       ("^"                pseudovim-motion-first-non-blank)
       ("$"                pseudovim-motion-end-of-line)

       (("C-v" "v")        set-mark-command)
       (("C-y" "y")        copy-region-as-kill)

       (("C-m" "<f9>" "H") recompile)
       ("<return>"         compilation/goto-error)
       ("SPC"              compilation/goto-error-other-window))

     (def-keys-for-map compilation-button-map
       ("C-m" nil))))

(defun compilation-mode-setup ()
  (hl-line-mode +1))

(add-hook 'compilation-mode-hook #'compilation-mode-setup)

(vim-defcmd vim:recompile (nonrepeatable)
  (recompile))

(provide 'compilation-setup)

;; Local Variables:
;; End:

;; compilation-setup.el ends here
