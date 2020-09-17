;; text-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 25 June 2020
;; Description:

;;;###autoload
(defun text-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-render-formula nil
               :use-whitespace nil
               :sp-slurp-sexp-insert-space nil)
  (setq-local hs-allow-nesting t)
  (setup-folding t nil))

;;;###autoload
(add-hook 'text-mode-hook #'text-mode-setup)

(provide 'text-mode-setup)

;; Local Variables:
;; End:

;; text-mode-setup.el ends here
