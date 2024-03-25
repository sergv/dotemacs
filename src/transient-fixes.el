;; transient-fixes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 30 April 2023
;; Description:

(eval-when-compile
  (require 'cl))

(defvar transient-values)
(defvar transient-levels)
(defvar transient-history)

(require 'persistent-store)

;;; Make transient.el use persistent store.

;;;###autoload
(defun transient--read-file-contents/use-persistent-store (old-read-file-contents file)
  (cond
    ((equal file transient-values-file)
     (persistent-store-get 'transient-values))
    ((equal file transient-levels-file)
     (persistent-store-get 'transient-levels))
    ((equal file transient-history-file)
     (persistent-store-get 'transient-history))
    (t
     (funcall old-read-file-contents file))))

;;;###autoload
(advice-add 'transient--read-file-contents :around #'transient--read-file-contents/use-persistent-store)

;;;###autoload
(defun transient-save-value/use-persistent-store ()
  (persistent-store-put 'transient-values transient-values))

;;;###autoload
(advice-add 'transient-save-value :override #'transient-save-value/use-persistent-store)

;;;###autoload
(defun transient-save-levels/use-persistent-store ()
  (persistent-store-put 'transient-levels transient-levels))

;;;###autoload
(advice-add 'transient-save-levels :override #'transient-save-levels/use-persistent-store)

;;;###autoload
(defun transient-save-history/use-persistent-store ()
  (persistent-store-put 'transient-history
                        (cl-sort (mapcar (pcase-lambda (`(,key . ,val))
                                           (cons key (seq-take (delete-dups val)
                                                               transient-history-limit)))
                                         transient-history)
                                 #'string<
                                 :key #'car)))

;;;###autoload
(advice-add 'transient-save-history :override #'transient-save-history/use-persistent-store)

(defun transient-history--merge-entries (old new)
  (cl-assert (and (consp old) (symbolp (car old))) nil "Invalid old transient-history contents: %s" old)
  (cl-assert (and (consp new) (symbolp (car new))) nil "Invalid new transient-history contents: %s" new)
  (let ((old-key (car old))
        (new-key (car new))
        (old-normalised
         (cl-sort (mapcar (lambda (x)
                            (pcase x
                              (`(,key . ,val)
                               (cons key
                                     (cl-sort val #'string-list<)))
                              (_
                               (error "Invalid transient-history element in old contents: %s" x))))
                          (cdr old))
                  #'string<
                  :key #'car))
        (new-normalised
         (cl-sort (mapcar (lambda (x)
                            (pcase x
                              (`(,key . ,val)
                               (cons key
                                     (cl-sort val #'string-list<)))
                              (_
                               (error "Invalid transient-history element in new contents: %s" x))))
                          (cdr new))
                  #'string<
                  :key #'car)))
    (when (equal old-normalised new-normalised)
      (cons old-key old-normalised))))

(push (cons 'transient-history #'transient-history--merge-entries)
      persistent-store-merge-handlers)

(provide 'transient-fixes)

;; Local Variables:
;; End:

;; transient-fixes.el ends here
