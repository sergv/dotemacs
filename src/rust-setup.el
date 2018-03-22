;; rust-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 March 2018
;; Description:

(require 'common)

;;;###autoload
(defun rust-setup ()
  (init-common :use-render-formula nil
               :sp-slurp-sexp-insert-space nil
               :use-yasnippet t
               :use-whitespace 'tabs-only)
  (fontify-conflict-markers!)
  (setup-hs-minor-mode)
  (company-mode +1)

  (setq-local whitespace-line-column 80)
  (setq-local whitespace-style '(face tabs lines-tail)))

(add-hook 'rust-mode-hook #'rust-setup)

(provide 'rust-setup)

;; Local Variables:
;; End:

;; rust-setup.el ends here
