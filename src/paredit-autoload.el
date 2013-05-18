;; paredit-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(eval-after-load 'paredit '(progn (require 'paredit-setup)))

(autoload 'vim:splice-sexp-killing-backward "paredit-setup" "" t)
(autoload 'vim:splice-sexp-killing-forward "paredit-setup" "" t)

(autoload 'vim:backward-slurp-sexp "paredit-setup" "" t)
(autoload 'vim:backward-barf-sexp "paredit-setup" "" t)
(autoload 'vim:forward-barf-sexp "paredit-setup" "" t)
(autoload 'vim:forward-slurp-sexp "paredit-setup" "" t)

(autoload 'vim:paredit-forward-kill "paredit-setup" "" t)
(autoload 'vim:paredit-backward-kill "paredit-setup" "" t)
(autoload 'vim:paredit-forward-kill-word "paredit-setup" "" t)
(autoload 'vim:paredit-backward-kill-word "paredit-setup" "" t)



(autoload 'paredit-insert-space-after-reader-sharp? "paredit-setup")

(autoload 'vim:paredit-forward-word "paredit-setup" "" t)
(autoload 'vim:paredit-forward-word-end "paredit-setup" "" t)
(autoload 'vim:paredit-backward-word "paredit-setup" "" t)

(autoload 'vim:paredit-forward-WORD "paredit-setup" "" t)
(autoload 'vim:paredit-forward-WORD-end "paredit-setup" "" t)
(autoload 'vim:paredit-backward-WORD "paredit-setup" "" t)

(autoload 'vim:paredit-inner-symbol "paredit-setup" "" t)
(autoload 'vim:paredit-outer-symbol "paredit-setup" "" t)

(autoload 'vim:paredit-forward-symbol "paredit-setup" "" t)
(autoload 'vim:paredit-forward-symbol-end "paredit-setup" "" t)
(autoload 'vim:paredit-backward-symbol "paredit-setup" "" t)

(provide 'paredit-autoload)

;; Local Variables:
;; End:

;; paredit-autoload.el ends here
