;; vhdl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 12 October 2013
;; Description:

(require 'common)

;;;###autoload
(defun vhdl-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-whitespace 'tabs-only
               :use-fci t
               :sp-slurp-sexp-insert-space nil)
  (bind-tab-keys #'vhdl-electric-tab
                 nil
                 :enable-yasnippet t))

;;;###autoload
(add-hook 'vhdl-mode-hook #'vhdl-setup)

(provide 'vhdl-setup)

;; Local Variables:
;; End:

;; vhdl-setup.el ends here
