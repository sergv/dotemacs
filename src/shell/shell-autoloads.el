;; shell-autoloads.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:

(add-to-list 'auto-mode-alist '("\\.xbindkeysrc$" . shell-script-mode))

(autoload 'shell-setup "shell-setup")
(add-hook 'shell-mode-hook #'shell-setup)

(autoload 'shell-script-setup "shell-setup")

(mapc (lambda (x) (add-hook x #'shell-script-setup))
      '(makefile-mode-hook
        cmake-mode-hook
        shell-script-mode-hook
        sh-mode-hook
        sh-script-mode-hook
        conf-space-mode-hook
        conf-mode-hook
        conf-xdefaults-mode-hook))

(autoload 'pcomplete/git "shell-completion" nil t)
(autoload 'pcomplete/runghc "shell-completion" nil t)
(autoload 'pcomplete/runhaskell "shell-completion" nil t)
(autoload 'pcomplete/ghc "shell-completion" nil t)
(autoload 'pcomplete/cabal "shell-completion" nil t)
(autoload 'pcomplete/nm "shell-completion" nil t)
(autoload 'pcomplete/gcc "shell-completion" nil t)
(autoload 'pcomplete/cp "shell-completion" nil t)
(autoload 'pcomplete/ls "shell-completion" nil t)
(autoload 'pcomplete/cat "shell-completion" nil t)
(autoload 'pcomplete/mv "shell-completion" nil t)


;; Local Variables:
;; End:

;; shell-autoloads.el ends here
