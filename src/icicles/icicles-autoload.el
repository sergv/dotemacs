;; icicles-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(eval-after-load "icicles" '(progn (require 'icicles-setup)))

(autoload 'icy-mode "icicles" "" t)
(autoload 'icicle-file "icicles" "" t)
(autoload 'icicle-locate-file "icicles" "" t)
(autoload 'icicle-locate-file-other-window "icicles" "" t)
(autoload 'icicle-buffer "icicles" "" t)
(autoload 'icicle-read-file-name "icicles" "" t)
(autoload 'icicle-completing-read "icicles" "" t)
(autoload 'icicle-read-string "icicles" "" t)
(autoload 'icicle-shell-command "icicles" "" t)

(autoload 'completion-list-setup "icicles-setup")
(add-hook 'completion-list-mode-hook #'completion-list-setup)


(provide 'icicles-autoload)

;; Local Variables:
;; End:

;; icicles-autoload.el ends here
