;; vim-edmacro-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 14 January 2022
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'hydra-setup)
(require 'vim-setup)

(defhydra-derive hydra-vim-edmacro-normal-g hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  ""
  ("#" vim-edmacro-finish))

;;;###autoload
(defun vim-edmacro-setup ()
  (init-common :use-whitespace 'tabs-only
               :smerge nil)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("g" hydra-git-rebase-from-vim-normal/body)))

;;;###autoload
(add-hook 'vim-edmacro-mode-hook #'vim-edmacro-setup)

(provide 'vim-edmacro-setup)

;; Local Variables:
;; End:

;; vim-edmacro-setup.el ends here
