;; flycheck-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 May 2016
;; Description:

(require 'flycheck-autoloads)

(autoload 'flycheck-haskell-setup "flycheck-haskell" nil nil)

;; Don't show errors on fringes.
(setf flycheck-indication-mode nil
      ;; Highlight whole line with error
      flycheck-highlighting-mode 'lines
      flycheck-completing-read-function #'ido-completing-read
      flycheck-display-errors-delay 0)

(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(defun flycheck-error-list-setup ()
  (def-keys-for-map flycheck-error-list-mode-map
    +vim-search-keys+
    +vim-special-keys+
    (("h" "<down>") flycheck-error-list-next-error)
    (("t" "<up>")   flycheck-error-list-previous-error)
    ("H"            flycheck-error-list-check-source)
    ("<escape>"     quit-window)))

(add-hook 'flycheck-error-list-mode-hook #'flycheck-error-list-setup)

(defadvice flycheck-display-error-messages
    (before
     flycheck-display-error-messages-set-mode-in-error-buffer
     activate
     compile)
  (unless (buffer-live-p flycheck-error-message-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-message-buffer)
      (text-mode))))

(provide 'flycheck-setup)

;; Local Variables:
;; End:

;; flycheck-setup.el ends here
