;; icicles-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(autoload 'icy-mode "icicles" "" t)
(autoload 'icicle-file "icicles" "" t)
(autoload 'icicle-locate-file "icicles" "" t)
(autoload 'icicle-locate-file-other-window "icicles" "" t)
(autoload 'icicle-buffer "icicles" "" t)
(autoload 'icicle-read-file-name "icicles" "" t)
(autoload 'icicle-completing-read "icicles" "" t)
(autoload 'icicle-read-string "icicles" "" t)
(autoload 'icicle-shell-command "icicles" "" t)
(autoload 'icicle-pp-eval-expression "icicles" "" t)
(autoload 'icicle-pp-eval-expression-in-minibuffer "icicles" "" t)
(autoload 'icicle-delete-window "icicles" "" t)

(autoload 'completion-list-setup "icicles-setup")
(add-hook 'completion-list-mode-hook #'completion-list-setup)

(eval-after-load "icicles" '(progn (require 'icicles-setup)))
(load-library "icicles-opt")

(provide 'icicles-autoload)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; icicles-autoload.el ends here
