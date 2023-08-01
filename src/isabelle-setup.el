;; isabelle-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 August 2023
;; Description:

(require 'isar-mode)

;;;###autoload
(defun isar-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci t
               :use-whitespace 'tabs-only)
  (setup-indent-size 2))

;;;###autoload
(add-hook 'isar-mode-hook #'isar-setup)

(provide 'isabelle-setup)

;; Local Variables:
;; End:

;; isabelle-setup.el ends here
