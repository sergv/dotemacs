;; vhdl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 12 October 2013
;; Description:

(require 'common)


(defun vhdl-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-whitespace t
               :sp-slurp-sexp-insert-space nil))

(add-hook 'vhdl-mode-hook #'vhdl-setup)

(provide 'vhdl-setup)

;; Local Variables:
;; End:

;; vhdl-setup.el ends here
