;; cuda-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 21 July 2013
;; Description:

(require 'common)
(require 'cc-setup)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.c[ul]h?\\'" . cuda-mode))

;;;###autoload
(defun cuda-setup ()
  (cc-setup :define-special-keys nil)
  (setup-folding 'enable-cpp '(:header-symbol "/" :length-min 3))
  (cc-setup/set-up-c-basic-offset)
  (setup-eproj-symbnav))

;;;###autoload
(add-hook 'cuda-mode-hook #'cuda-setup)

(provide 'cuda-setup)

;; Local Variables:
;; End:

;; cuda-setup.el ends here
