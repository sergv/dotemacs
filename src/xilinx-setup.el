;; xilinx-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 13 October 2013
;; Description:

;; Set ups for various xilinx-specific FPGA-oriented file formats

(require 'common)

;;;###autoload
(autoload 'ucf-mode "ucf-mode" nil t)
;;;###autoload
(add-to-list 'auto-mode-alist '(".ucf\\'" . ucf-mode))

;;;###autoload
(defun ucf-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula nil
               :use-whitespace 'tabs-only
               :use-fci t)
  (bind-tab-keys #'tab-to-tab-stop
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t))

;;;###autoload
(add-hook 'ucf-mode-hook #'ucf-setup)

(provide 'xilinx-setup)

;; Local Variables:
;; End:

;; xilinx-setup.el ends here
