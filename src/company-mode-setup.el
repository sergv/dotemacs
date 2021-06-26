;; company-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 15 February 2016
;; Description:

(require 'company-posframe)
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

(company-statistics-mode +1)
;; (company-posframe-mode +1)

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

(def-keys-for-map company-posframe-active-map
  ("<escape>" company-abort))

(provide 'company-mode-setup)

;; Local Variables:
;; End:

;; company-mode-setup.el ends here
