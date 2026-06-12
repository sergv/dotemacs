;; special-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 13 June 2026
;; Description:

(eval-when-compile
  (require 'macro-util))

(defun special-buffer-erase ()
  (interactive)
  (with-inhibited-read-only
   (erase-buffer)))

;;;###autoload
(defun special-mode-setup ()
  (hl-line-mode +1)

  (def-keys-for-map special-mode-map
    ("C-SPC" special-buffer-erase)))

;;;###autoload
(add-hook 'special-mode-hook #'special-mode-setup)

(provide 'special-setup)

;; Local Variables:
;; End:

;; special-setup.el ends here
