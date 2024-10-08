;; tcl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 12 October 2013
;; Description:

(require 'common)

;;;###autoload
(defun tcl-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-whitespace 'tabs-only
               :use-fci t)
  (bind-tab-keys #'tab-to-tab-stop
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t))

;;;###autoload
(add-hook 'tcl-mode-hook #'tcl-setup)

(provide 'tcl-setup)

;; Local Variables:
;; End:

;; tcl-setup.el ends here
