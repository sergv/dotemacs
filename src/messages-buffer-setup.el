;; messages-buffer-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 January 2022
;; Description:

(eval-when-compile
  (require 'macro-util))

(defun message-buffer-erase ()
  (interactive)
  (with-inhibited-read-only
   (erase-buffer)))

;;;###autoload
(defun messages-buffer-setup ()
  (hl-line-mode +1)

  (def-keys-for-map messages-buffer-mode-map
    ("C-SPC" message-buffer-erase)))

;;;###autoload
(add-hook 'messages-buffer-mode-hook #'messages-buffer-setup)

(provide 'messages-buffer-setup)

;; Local Variables:
;; End:

;; messages-buffer-setup.el ends here
