;; toml-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 March 2018
;; Description:

(require 'common)

;;;###autoload
(defun toml-setup ()
  (init-common :use-render-formula nil
               :sp-slurp-sexp-insert-space nil
               :use-yasnippet t
               :use-whitespace 'tabs-only)
  (fontify-conflict-markers!)

  (setq-local whitespace-line-column 80)
  (setq-local whitespace-style '(face tabs lines-tail)))

(add-hook 'toml-mode-hook #'toml-setup)

(provide 'toml-setup)

;; Local Variables:
;; End:

;; toml-setup.el ends here
