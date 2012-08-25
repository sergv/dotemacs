;; yaml-mode-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 20 April 2012
;; Description:

(autoload 'yaml-mode "yaml-mode.el" "Simple mode to edit YAML." t)


(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(defun yaml-setup ()
  (init-common :use-render-formula t))

(add-hook 'yaml-mode-hook #'yaml-setup)

(provide 'yaml-mode-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;; yaml-mode-setup.el ends here
