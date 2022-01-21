;; j-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  2 September 2021
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'hydra-setup)

(defhydra-ext hydra-j (:exit t :foreign-keys warn :hint nil)
  "
_d_oc for a symbol

execute _l_ine
execute _b_uffer
"
  ("d" j-help-lookup-symbol)
  ("l" j-console-execute-line)
  ("b" j-console-execute-buffer))

(defun j-setup--console-execute-region ()
  (interactive)
  (with-region-bounds start end
    (j-console-execute-region start end)))

(defhydra-ext hydra-j-visual (:exit t :foreign-keys warn :hint nil)
  "
execute _r_egion
"
  ("r" j-setup--console-execute-region))

;;;###autoload
(defun j-setup ()
  (init-common :use-render-formula nil
               :use-yasnippet t
               :use-whitespace t)
  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC"      j-console)
    (("C-l" "<f6>") j-console-execute-buffer)
    ("-"            hydra-j/body))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("-"            hydra-j-visual/body)))

;;;###autoload
(add-hook 'j-mode-hook #'j-setup)

(provide 'j-setup)

;; Local Variables:
;; End:

;; j-setup.el ends here
