;; company-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 15 February 2016
;; Description:

(eval-when-compile
  (require 'macro-util))

(provide 'company-mode-setup)

;; (require 'company-posframe)
(require 'company-statistics)
(require 'persistent-store)

;;;###autoload
(autoload 'company-mode "company" nil t)
;;;###autoload
(autoload 'company-begin-backend "company" nil t)

(setf company-idle-delay nil ;; disable auto-completion
      company-tooltip-minimum-width 40
      company-tooltip-align-annotations t
      company-tooltip-offset-display 'lines)

;;;###autoload
(defun company-statistics--save/use-persistent-store ()
  (persistent-store-put 'company-statistics--scores company-statistics--scores)
  (persistent-store-put 'company-statistics--log company-statistics--log)
  (persistent-store-put 'company-statistics--index company-statistics--index))

;;;###autoload
(advice-add 'company-statistics--save :override #'company-statistics--save/use-persistent-store)

;;;###autoload
(defun company-statistics--load/use-persistent-store ()
  (setf company-statistics--scores (persistent-store-get 'company-statistics--scores)
        company-statistics--log (persistent-store-get 'company-statistics--log)
        company-statistics--index (persistent-store-get 'company-statistics--index))
  (and company-statistics--scores
       company-statistics--log
       company-statistics--index))

;;;###autoload
(advice-add 'company-statistics--load :override #'company-statistics--load/use-persistent-store)

;; (def-keys-for-map company-posframe-active-map
;;   ("<escape>" company-abort))

(company-statistics-mode +1)
;; (company-posframe-mode +1)

(provide 'company-mode-setup)

;; Local Variables:
;; End:

;; company-mode-setup.el ends here
