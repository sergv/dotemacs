;; gdb-script-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  7 December 2025
;; Description:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gdb\\'" . gdb-script-mode))

;;;###autoload
(defun gdb-script-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-render-formula nil
               :use-whitespace nil)
  (setq-local hs-allow-nesting t)
  (setup-indent-size 2)
  (setup-hideshow-yafolding t nil))

;;;###autoload
(add-hook 'gdb-script-mode-hook #'gdb-script-mode-setup)

(provide 'gdb-script-mode-setup)

;; Local Variables:
;; End:

;; gdb-script-mode-setup.el ends here
