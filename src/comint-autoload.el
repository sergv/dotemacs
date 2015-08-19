;; comint-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 19 August 2015
;; Description:

(setf comint-input-ignoredups t)
(setq-default comint-input-ignoredups t)

(autoload 'comint-setup "comint-setup")
(autoload 'comint-clear-prompt "comint-setup" nil t)
(autoload 'comint-clear-buffer-above-prompt "comint-setup" nil t)

(add-hook 'comint-mode-hook #'comint-setup)

(eval-after-load "shell" '(require 'comint-setup))
(eval-after-load "comint" '(require 'comint-setup))

(provide 'comint-autoload)

;; Local Variables:
;; End:

;; comint-autoload.el ends here
