;; verilog-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 12 October 2013
;; Description:

(declare-function electric-verilog-tab "verilog-mode")

(defvar verilog-indent-level)
(defvar verilog-indent-level-module)
(defvar verilog-indent-level-declaration)
(defvar verilog-indent-level-behavioral)
(defvar verilog-indent-level-directive)
(defvar verilog-case-indent)
(defvar verilog-auto-newline)
(defvar verilog-auto-indent-on-newline)
(defvar verilog-tab-always-indent)
(defvar verilog-auto-endcomments)
(defvar verilog-minimum-comment-distance)
(defvar verilog-indent-begin-after-if)
(defvar verilog-auto-lineup)
(defvar verilog-highlight-p1800-keywords)

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
               :use-fci t)
  (bind-tab-keys #'electric-verilog-tab
                 nil
                 :enable-yasnippet t))

;;;###autoload
(add-hook 'verilog-mode-hook #'verilog-setup)

(provide 'verilog-setup)

;; Local Variables:
;; End:

;; verilog-setup.el ends here
