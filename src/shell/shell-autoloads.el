;; shell-autoloads.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:

(autoload 'shell-setup "shell-setup")

(add-hook 'shell-mode-hook #'shell-setup)

(autoload 'shell-script-setup "shell-setup")

(mapc #'(lambda (x) (add-hook x #'shell-script-setup))
      '(makefile-mode-hook
        cmake-mode-hook
        shell-script-mode-hook
        sh-mode-hook
        sh-script-mode-hook
        conf-space-mode-hook
        conf-mode-hook
        conf-xdefaults-mode-hook))


;; Local Variables:
;; End:

;; shell-autoloads.el ends here
