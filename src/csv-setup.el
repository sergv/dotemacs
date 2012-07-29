;;; csv-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 13 January 2012
;; Keywords:
;; Requirements:
;; Status:


(autoload 'csv-mode "csv-mode" nil t)

(defun csv-setup ()
  )

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(add-hook 'csv-mode-hook #'csv-setup)

(provide 'csv-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; csv-setup.el ends here
