;; verilog-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 12 October 2013
;; Description:

(declare-function electric-verilog-tab "verilog-mode")

(require 'common)

(setf verilog-indent-level             4
      verilog-indent-level-module      4
      verilog-indent-level-declaration 4
      verilog-indent-level-behavioral  0
      verilog-indent-level-directive   0
      verilog-case-indent              0
      verilog-auto-newline             t
      verilog-auto-indent-on-newline   t
      verilog-tab-always-indent        t
      verilog-auto-endcomments         t
      verilog-minimum-comment-distance 40
      verilog-indent-begin-after-if    t
      verilog-auto-lineup              'all
      verilog-highlight-p1800-keywords t)

;;;###autoload
(defun verilog-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-whitespace 'tabs-only
               :use-fci t
               :sp-slurp-sexp-insert-space nil)
  (bind-tab-keys #'electric-verilog-tab
                 nil
                 :enable-yasnippet t))

;;;###autoload
(add-hook 'verilog-mode-hook #'verilog-setup)

(provide 'verilog-setup)

;; Local Variables:
;; End:

;; verilog-setup.el ends here
