;; d-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 29 March 2012
;; Description:

(autoload 'd-mode "d-mode" "Mode for editing CLISP *.d files." t)

(add-to-list 'auto-mode-alist '("\\.d$" . d-mode))

(defun d-mode-setup ()
  (init-common :use-yasnippet nil)
  ;; turn off jit-lock
  (set (make-local-variable 'font-lock-support-mode)
       nil)
  )

(add-hook 'd-mode-hook #'d-mode-setup)

(provide 'd-mode-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;; d-setup.el ends here
