;; org-mode-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'set-up-paths)

(add-to-list 'load-path (concat +emacs-standalone-elc-path+
                                "/org"))
(add-to-list 'load-path (concat +emacs-standalone-elc-path+
                                "/org/emacs/site-lisp/org"))

(require 'org-loaddefs)


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(autoload 'org-toggle-display-style "org-mode-setup" "" t)
(autoload 'org-toggle-inline-images-and-formulae "org-mode-setup" "" t)
(autoload 'org-mode-up-heading "org-mode-setup" "" t)

(autoload 'org-mode-setup "org-mode-setup")
(add-hook 'org-mode-hook #'org-mode-setup)

(autoload 'org-agenda-mode-setup "org-mode-setup")
(add-hook 'org-agenda-mode-hook #'org-agenda-mode-setup)

(provide 'org-mode-autoload)

;; Local Variables:
;; End:

;; org-mode-autoload.el ends here
