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
(require 'persistent-store)

;;;###autoload
(autoload 'company-mode "company" nil t)
;;;###autoload
(autoload 'company-begin-backend "company" nil t)

(setf company-idle-delay nil ;; disable auto-completion
      company-tooltip-minimum-width 40
      company-tooltip-align-annotations t
      company-tooltip-offset-display 'lines)

;; (def-keys-for-map company-posframe-active-map
;;   ("<escape>" company-abort))

;; (company-posframe-mode +1)

(defsubst delete-duplicate-candidates--introduced-by-company-dabbrev-code? (x)
  (eq (get-text-property 0 'company-backend x)
      'company-dabbrev-code))

(defun delete-duplicate-candidates-from-company-dabbrev-code (xs)
  "Remove candidates introduced by ‘company-dabbrev-code’ backend if they were
introduced by some other backend."
  (if (cdr xs)
      ;; Only invoke if there’s more than one candidate
      (let ((introduced-by-other (make-hash-table :test #'equal)))
        (dolist (x xs)
          (unless (delete-duplicate-candidates--introduced-by-company-dabbrev-code? x)
            (puthash x t introduced-by-other)))
        (inplace-delete-if! (lambda (x)
                              (and (delete-duplicate-candidates--introduced-by-company-dabbrev-code? x)
                                   (gethash x introduced-by-other)))
                            xs))
    xs))

(provide 'company-mode-setup)

;; Local Variables:
;; End:

;; company-mode-setup.el ends here
