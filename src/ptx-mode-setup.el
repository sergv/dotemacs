;; ptx-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 23 August 2018
;; Description:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ptx\\'" . ptx-mode))

;;;###autoload
(add-hook 'ptx-mode-hook #'ptx-mode-setup)

;;;###autoload
(defun ptx-mode-setup ()
  (init-common
   :use-yasnippet nil
   :use-comment t
   :use-fci t))

(provide 'ptx-mode-setup)

;; Local Variables:
;; End:

;; ptx-mode-setup.el ends here
