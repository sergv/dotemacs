;; smerge-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 18 January 2022
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'smerge-mode)
(require 'vim-setup)

(setf smerge-auto-leave nil)

(vim-define-keymap smerge-mode "smerge mode")

;;;###autoload (autoload 'vim-activate-smerge-mode "smerge-setup" nil t)
(vim-define-mode smerge "VIM smerge mode for resolving merge conflicts\n\nsmerge mode keymap:\n\\{vim-smerge-mode-keymap}"
  :ident "S"
  ;; :message "-- SMERGE --"
  :keymaps '(vim-smerge-mode-keymap
             vim-operator-pending-mode-keymap
             vim-motion-mode-keymap
             ;; vim-override-keymap
             )
  :command-function #'vim--normal-mode-command
  :cursor 'hbar)

(defhydra-ext hydra-smerge (:exit t :foreign-keys nil :hint nil)
  "
_h_: go to next confict       _e_diff
_t_: go to previous conflict  _r_efine
_<_: keep upper
_>_: keep lower
_b_: keep base
_c_: keep current
_,_: kill current
"
  ("<" vim:smerge-keep-upper:interactive)
  (">" vim:smerge-keep-lower:interactive)
  ("b" vim:smerge-keep-base:interactive)
  ("c" vim:smerge-keep-current:interactive)
  (","              vim:smerge-kill-current:interactive)

  ("e" smerge-ediff)
  ("r" smerge-refine))

(vimmize-function smerge-keep-upper   :name vim:smerge-keep-upper :has-count nil)
(vimmize-function smerge-keep-lower   :name vim:smerge-keep-lower :has-count nil)
(vimmize-function smerge-keep-base    :name vim:smerge-keep-base :has-count nil)
(vimmize-function smerge-keep-current :name vim:smerge-keep-current :has-count nil)
(vimmize-function smerge-kill-current :name vim:smerge-kill-current :has-count nil)

(vim-defcmd vim:smerge-mode-exit (nonrepeatable)
  "Deactivates visual mode, returning to normal-mode."
  (vim-activate-normal-mode))

(def-keys-for-map vim-smerge-mode-keymap
  +vim-normal-mode-navigation-keys+
  +vim-search-keys+
  +vim-search-extended-keys+
  +vim-special-keys+

  ("."              vim:cmd-repeat:interactive)

  ("h"              smerge-next)
  ("t"              smerge-prev)

  ("<"              vim:smerge-keep-upper:interactive)
  (">"              vim:smerge-keep-lower:interactive)
  ("b"              vim:smerge-keep-base:interactive)
  ("c"              vim:smerge-keep-current:interactive)
  (","              vim:smerge-kill-current:interactive)

  ("e"              smerge-ediff)

  ("-"              hydra-smerge/body)

  ("<down>"         vim:motion-down:interactive)
  ("<up>"           vim:motion-up:interactive)
  (("q" "<escape>") vim:smerge-mode-exit:interactive))

(provide 'smerge-setup)

;; Local Variables:
;; End:

;; smerge-setup.el ends here
