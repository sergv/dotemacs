;; eproj-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 29 May 2013
;; Description:

(autoload 'eproj-symbnav/go-to-symbol-home "eproj" "" t)
(autoload 'eproj-symbnav/go-back "eproj" "" t)
(autoload 'setup-eproj-symbnav "eproj")
(autoload 'eproj-update-buffer-tags "eproj")

(eval-after-load "eproj"
  '(progn
     (setf *ctags-exec*
           (platform-dependent-executable (concat +execs-path+ "/ctags")))))

(provide 'eproj-setup)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; eproj-setup.el ends here
