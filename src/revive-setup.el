;; revive-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 22 January 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'revive+)
(require 'revive)

(setf revive-plus:all-frames t
      revive-plus:wconf-archive-file (concat +emacs-config-path+ "/wconf-archive")
      revive-plus:last-wconf-file (concat +emacs-config-path+ "/last-wconf"))

(setf revive:save-variables-mode-local-private
      '((c++-mode c-indentation-style c-basic-offset)))

(provide 'revive-setup)

;; Local Variables:
;; End:

;; revive-setup.el ends here
