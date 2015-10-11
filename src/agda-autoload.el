;; agda-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 11 October 2015
;; Description:

(require 'agda2)

(autoload 'agda-setup "agda-setup" nil nil)
(add-hook 'agda2-mode-hook #'agda-setup)

(autoload 'agda-align-on-equals "agda-setup" nil t)
(autoload 'agda-align-on-arrows "agda-setup" nil t)
(autoload 'agda-align-on-left-arrows "agda-setup" nil t)
(autoload 'agda-align-on-pipes "agda-setup" nil t)
(autoload 'agda-align-on-commas "agda-setup" nil t)
(autoload 'agda-align-on-comments "agda-setup" nil t)
(autoload 'agda-align-on-colons "agda-setup" nil t)
(autoload 'agda2-compute-normalised-region "agda-setup" nil t)

(provide 'agda-autoload)

;; Local Variables:
;; End:

;; agda-autoload.el ends here
