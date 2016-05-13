;; shell-autoloads.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:

(add-to-list 'auto-mode-alist '("\\.xbindkeysrc$" . shell-script-mode))

(add-to-list 'display-buffer-alist
             '("^\\*Async Shell Command\\*" . (display-buffer-no-window)))

(add-to-list 'display-buffer-alist
             '("^\\*shell.*\\*" . (display-buffer-same-window)))


(autoload 'shell-setup "shell-setup")
(add-hook 'shell-mode-hook #'shell-setup)

(autoload 'shell-script-setup "shell-setup")

(dolist (mode '(cmake-mode-hook
                shell-script-mode-hook
                sh-mode-hook
                sh-script-mode-hook
                conf-space-mode-hook
                conf-mode-hook
                conf-xdefaults-mode-hook))
  (add-hook mode #'shell-script-setup))

(autoload 'pcomplete/git "shell-completion" nil t)

(autoload 'pcomplete/runghc "shell-completion" nil t)
(autoload 'pcomplete/runhaskell "shell-completion" nil t)
(autoload 'pcomplete-ghc-flags "shell-completion" nil)
(autoload 'pcomplete/ghc "shell-completion" nil t)
(autoload 'pcomplete/cabal "shell-completion" nil t)
(autoload 'pcomplete/hp2ps "shell-completion" nil t)
(autoload 'pcomplete/hp2pdf "shell-completion" nil t)
(autoload 'pcomplete/hp2pretty "shell-completion" nil t)
(autoload 'pcomplete/hp2svg "shell-completion" nil nil)
(autoload 'pcomplete/stack "shell-completion" nil t)
(autoload 'pcomplete/hlint "shell-completion" nil t)

(autoload 'pcomplete/nm "shell-completion" nil t)
(autoload 'pcomplete/gcc "shell-completion" nil t)
(autoload 'pcomplete/cp "shell-completion" nil t)
(autoload 'pcomplete/ls "shell-completion" nil t)
(autoload 'pcomplete/cat "shell-completion" nil t)
(autoload 'pcomplete/mv "shell-completion" nil t)
(autoload 'pcomplete/bash "shell-completion" nil t)
(autoload 'pcomplete/diff "shell-completion" nil t)
(autoload 'pcomplete/source "shell-completion" nil t)
(autoload 'pcomplete/ssh-add "shell-completion" nil t)
(autoload 'pcomplete/find "shell-completion" nil t)
(autoload 'pcomplete/du "shell-completion" nil t)
(autoload 'pcomplete/busybox "shell-completion" nil t)
(autoload 'pcomplete/untar "shell-completion" nil t)

(defalias 'pcomplete/l 'pcomplete/ls)
(defalias 'pcomplete/la 'pcomplete/ls)
(defalias 'pcomplete/ll 'pcomplete/ls)

(unless (getenv "SHELL")
  (setenv "SHELL" shell-file-name))

;; Local Variables:
;; End:

;; shell-autoloads.el ends here
