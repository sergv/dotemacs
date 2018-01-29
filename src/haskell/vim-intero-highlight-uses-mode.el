;; vim-intero-highlight-uses-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 29 January 2018
;; Description:

(require 'intero)
(require 'vim)

(vim:defcmd vim:highlight-uses-quit (nonrepeatable)
  "Stop `vim:intero-highlight-uses-mode'."
  (unwind-protect
      (intero-highlight-uses-mode -1)
    (unless intero-highlight-uses-mode
      (vim:activate-normal-mode))))

(vim:define-keymap intero-highlight-uses-mode "highlight uses mode")

(vim:define-mode intero-highlight-uses "VIM adapter for intero's highlight uses\n\nMode keymap:\n\\{vim:intero-highlight-uses-mode-keymap}"
  :ident "H"
  ;; :message "-- Highlight uses --"
  :keymaps '(vim:intero-highlight-uses-mode-keymap
             vim:operator-pending-mode-keymap
             vim:motion-mode-keymap
             ;; vim:override-keymap
             )
  :command-function #'vim:normal-mode-command
  :cursor 'hbar)

(def-keys-for-map (vim:intero-highlight-uses-mode-keymap
                   intero-highlight-uses-mode-map)
  +vim-interbuffer-navigation-keys+
  +vim-normal-mode-navigation-keys+
  +vim-search-keys+
  +vim-search-extended-keys+
  +vim-special-keys+
  (("h" "<down>" "<tab>") intero-highlight-uses-mode-next)
  (("t" "<up>" "S-<tab>" "<backtab>" "S-<lefttab>" "S-<iso-lefttab>") intero-highlight-uses-mode-prev)

  ("<return>"       intero-highlight-uses-mode-stop-here)
  ("r"              intero-highlight-uses-mode-replace)
  (("q" "<escape>") vim:highlight-uses-quit))

;;;###autoload
(add-hook 'intero-highlight-uses-mode-hook #'vim:activate-intero-highlight-uses-mode)

(provide 'vim-intero-highlight-uses-mode)

;; Local Variables:
;; End:

;; vim-intero-highlight-uses-mode.el ends here
