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
      flycheck-completing-read-function #'ido-completing-read)

(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(provide 'flycheck-setup)

;; Local Variables:
;; End:

;; flycheck-setup.el ends here
