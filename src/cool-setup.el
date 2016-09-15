;; cool-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 May 2014
;; Description:

;;;###autoload
(defun cool-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-render-formula t
               :sp-slurp-sexp-insert-space nil
               :use-whitespace 'tabs-only
               :use-fci t)
  (hs-minor-mode-setup))

;;;###autoload
(add-hook 'cool-mode-hook #'cool-setup)

(provide 'cool-setup)

;; Local Variables:
;; End:

;; cool-setup.el ends here
