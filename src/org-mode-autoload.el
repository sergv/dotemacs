;; org-mode-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile
  (require 'cl))

(require 'set-up-paths)
(require 'org-loaddefs)

(defvar session-globals-exclude nil)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(autoload 'org-toggle-display-style "org-mode-setup" "" t)
(autoload 'org-toggle-inline-images-and-formulae "org-mode-setup" "" t)
(autoload 'org-mode-up-heading "org-mode-setup" "" t)

(autoload 'org-mode-setup "org-mode-setup")
(add-hook 'org-mode-hook #'org-mode-setup)

(autoload 'org-agenda-mode-setup "org-mode-setup")
(add-hook 'org-agenda-mode-hook #'org-agenda-mode-setup)

(autoload 'htmlize-buffer "htmlize" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'htmlize-file "htmlize" nil t)
(autoload 'htmlize-many-files "htmlize" nil t)
(autoload 'htmlize-many-files-dired "htmlize" nil t)

(eval-after-load "org"
  '(progn
     (require 'org-mode-setup)))

(provide 'org-mode-autoload)

;; Local Variables:
;; End:

;; org-mode-autoload.el ends here
