;; company-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 15 February 2016
;; Description:

;;;###autoload
(autoload 'company-mode "company" nil t)
;;;###autoload
(autoload 'company-begin-backend "company" nil t)

(setf company-idle-delay nil ;; disable auto-completion
      company-tooltip-minimum-width 40
      company-tooltip-align-annotations t
      company-tooltip-offset-display 'lines)


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
