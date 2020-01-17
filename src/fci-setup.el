;; fci-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 22 September 2015
;; Description:

(require 'fill-column-indicator)

(setf fci-rule-column 100)

(defun toggle-fci ()
  (when fci-mode
    (fci-mode -1)
    (fci-mode +1)))

(defun toggle-fci-everywhere ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (toggle-fci))))

;; Tuesday, 15 September 2015 NB: it's somewhat expensive to toggle fci on every
;; buffer switch, try to disable it in the future and see whether it's needed
;; any more.
(defadvice switch-to-buffer (after switch-to-buffer-toggle-fci activate compile)
  "Check syntax with GHC when switching to a haskell-mode buffer."
  (toggle-fci))

(add-hook 'sessions/load-buffers-hook #'toggle-fci-everywhere)
(add-hook 'after-change-major-mode-hook #'toggle-fci)

(provide 'fci-setup)

;; Local Variables:
;; End:

;; fci-setup.el ends here
