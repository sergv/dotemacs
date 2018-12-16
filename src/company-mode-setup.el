;; company-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 15 February 2016
;; Description:

(require 'company-statistics)
(require 'persistent-store)

;;;###autoload
(autoload 'company-mode "company" nil t)
;;;###autoload
(autoload 'company-begin-backend "company" nil t)

(setf company-idle-delay nil ;; disable auto-completion
      company-tooltip-minimum-width 40
      company-tooltip-align-annotations t
      company-tooltip-offset-display 'lines
      company-statistics-size 512)

(company-statistics-mode +1)

(defadvice company-statistics--save (around
                                     company-statistics--save/use-persistent-store
                                     activate
                                     compile)
  (persistent-store-put 'company-statistics--scores company-statistics--scores)
  (persistent-store-put 'company-statistics--log company-statistics--log)
  (persistent-store-put 'company-statistics--index company-statistics--index))

(defadvice company-statistics--load (around
                                     company-statistics--load/use-persistent-store
                                     activate
                                     compile)
  "Restore statistics."
  (setf company-statistics--scores (persistent-store-get 'company-statistics--scores)
        company-statistics--log (persistent-store-get 'company-statistics--log)
        company-statistics--index (persistent-store-get 'company-statistics--index))
  (and company-statistics--scores
       company-statistics--log
       company-statistics--index))

;; Workaround to make company-mode play nicely with fci mode,
;; cf https://github.com/company-mode/company-mode/issues/180
(defvar-local company-fci-mode-on-p nil)

;;;###autoload
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

;;;###autoload
(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

;;;###autoload
(add-hook 'company-completion-started-hook #'company-turn-off-fci)
;;;###autoload
(add-hook 'company-completion-finished-hook #'company-maybe-turn-on-fci)
;;;###autoload
(add-hook 'company-completion-cancelled-hook #'company-maybe-turn-on-fci)

(provide 'company-mode-setup)

;; Local Variables:
;; End:

;; company-mode-setup.el ends here
