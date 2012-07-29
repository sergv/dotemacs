;;; debsources-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 25 November 2011
;; Keywords:
;; Requirements:
;; Status:


(autoload 'debsources-mode "debsources" "Start debsources mode.")

(push '("sources\\.list$" . debsources-mode)
      auto-mode-alist)

(defun debsources-setup ()
  (init-common)
  (autopair-mode))

(add-hook 'debsources-mode-hook #'debsources-setup)


;;; debsources-setup.el ends here
