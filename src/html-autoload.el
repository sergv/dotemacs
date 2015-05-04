;; html-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(autoload 'web-mode "web-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.x?html?\\'" . web-mode))

(autoload 'hl-tags-mode "hl-tags-mode" nil t)
(autoload 'sgml-skip-tag-backward "sgml-mode" nil t)
(autoload 'sgml-skip-tag-forward "sgml-mode" nil t)

(add-to-list 'magic-mode-alist '("<[ ?]*xml " . nxml-mode))

(dolist (mode '(nxml-mode web-mode))
  (setf hs-special-modes-alist
        (cons `(,mode
                "<[^/>]>\\|<[^>]*"
                "</"
                "<!--" ;; won't work on its own; uses syntax table
                my-nxml-forward-element
                nil)
              (assq-delete-all mode hs-special-modes-alist))))

(autoload 'vim:motion-jump-tag "html-setup" "" t)
(autoload 'vim:nxml-backward-up-element "html-setup" "" t)
(autoload 'markup-forward-up-element "html-setup" "" t)
(autoload 'vim:markup-forward-up-element "html-setup" "" t)
(autoload 'my-nxml-forward-element "html-setup" nil nil)

(autoload 'markup-setup "html-setup")
(autoload 'html-setup "html-setup")
(autoload 'nxml-reindent-enclosing-tag "html-setup" "" t)
(autoload 'nxhtml-setup "html-setup")
(autoload 'nxml-setup "html-setup")
(autoload 'web-mode-setup "html-setup")

(autoload 'hl-tags-context-nxml-mode "hl-tags-mode" nil nil)
(autoload 'nxml-tokenize-forward "nxml-mode" nil nil)

(add-hook 'html-mode-hook #'html-setup)
(add-hook 'sgml-mode-hook #'html-setup)
(add-hook 'nxhtml-mode-hook #'nxhtml-setup)
(add-hook 'nxml-mode-hook #'nxml-setup)
(add-hook 'web-mode-hook #'web-mode-setup)

(provide 'html-autoload)

;; Local Variables:
;; End:

;; html-autoload.el ends here
