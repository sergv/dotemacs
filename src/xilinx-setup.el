;; xilinx-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 13 October 2013
;; Description:

;; Set ups for various xilinx-specific FPGA-oriented file formats

(require 'common)

(autoload 'ucf-mode "ucf-mode" nil t)
(add-to-list 'auto-mode-alist '(".ucf\\'" . ucf-mode))

(defun ucf-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula nil
               :use-whitespace t
               :sp-slurp-sexp-insert-space nil))

(add-hook 'ucf-mode-hook #'ucf-setup)

(provide 'xilinx-setup)

;; Local Variables:
;; End:

;; xilinx-setup.el ends here
