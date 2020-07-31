;; toml-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 28 July 2020
;; Description:

;;;###autoload
(defun toml-mode-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-whitespace t
               :use-fci t
               :use-hl-line t)
  (setup-folding t '(:header-symbol "#" :length-min 3)))

;;;###autoload
(add-hook 'toml-mode-hook #'toml-mode-setup)

(provide 'toml-mode-setup)

;; Local Variables:
;; End:

;; toml-mode-setup.el ends here
