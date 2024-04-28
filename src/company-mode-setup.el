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

(push (cons 'company-statistics--scores #'company-statistics--merge-scores)
      persistent-store-merge-handlers)

(defun company-statistics--merge-scores (old-entry new-entry)
  (cl-assert (consp old-entry))
  (cl-assert (consp new-entry))
  (let ((old (cdr old-entry))
        (new (cdr new-entry)))
    (cl-assert (hash-table-p (cdr old-entry)))
    (cl-assert (hash-table-p (cdr new-entry)))
    (let ((merged (copy-hash-table old)))
      (cl-loop
       for k being the hash-keys of new using (hash-values v)
       do
       (if-let ((old-entry (gethash k old)))
           (puthash k (company-statistics--merge-scores-entries old-entry v) merged)
         (puthash k v merged)))
      (cons (car old-entry) merged))))

(defun company-statistics--merge-scores-entries (old new)
  (company-statistics--alist-update old new #'max))

;; (def-keys-for-map company-posframe-active-map
;;   ("<escape>" company-abort))

(company-statistics-mode +1)
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
