;; base-emacs-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 23 July 2026
;; Description:

(autoload 'auto-revert-notify-rm-watch "autorevert")
(autoload 'comint-add-to-input-history "comint")
(autoload 'comint-after-pmark-p "comint")
(autoload 'comint-bol "comint" nil t)
(autoload 'comint-next-input "comint" nil t)
(autoload 'comint-next-prompt "comint" nil t)
(autoload 'comint-output-filter "comint")
(autoload 'comint-previous-input "comint" nil t)
(autoload 'comint-previous-prompt "comint" nil t)
(autoload 'comint-replace-by-expanded-history "comint")
(autoload 'comint-send-input "comint" nil t)
(autoload 'comint-snapshot-last-prompt "comint")
(autoload 'comint-update-fence "comint")
(autoload 'comint-write-input-ring "comint")
(autoload 'company-grab-symbol "company")
(autoload 'eshell-send-input "esh-mode" nil t)
(autoload 'flyspell-goto-next-error "flyspell" nil t)
(autoload 'hl-line-highlight "hl-line")
(autoload 'pcomplete-entries "pcomplete")
(autoload 'server-edit "server" nil t)
(autoload 'shell-eval-command "shell")
(autoload 'shell-mode "shell")
(autoload 'smerge-next "smerge-mode" nil t)
(autoload 'smerge-prev "smerge-mode" nil t)
(autoload 'xref-pulse-momentarily "xref")

(autoload 'outline-cycle "outline" nil t)
(autoload 'outline-cycle-buffer "outline" nil t)
(autoload 'outline-hide-body "outline" nil t)
(autoload 'outline-hide-other "outline" nil t)
(autoload 'outline-hide-subtree "outline" nil t)
(autoload 'outline-previous-heading "outline" nil t)
(autoload 'outline-show-all "outline" nil t)
(autoload 'outline-show-children "outline" nil t)
(autoload 'outline-show-subtree "outline" nil t)
(autoload 'outline-up-heading "outline" nil t)

(autoload 'outline-on-heading-p "outline")
(autoload 'outline-back-to-heading "outline")

(provide 'base-emacs-autoload)

;; Local Variables:
;; End:

;; base-emacs-autoload.el ends here
