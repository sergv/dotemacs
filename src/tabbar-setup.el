;; tabbar-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 August 2012
;; Description:

(require 'common)
(require 'more-scheme)
(require 'buffer-groups)
(require 'tabbar)

(defun tabbar-buffer-groups+ (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (cond ((eq? major-mode 'slime-repl-mode)
           '("lisp" "slime"))
          ;; "interpret" +buffer-groups+
          ((invisible-buffer? buffer)
           '("#invisible#"))
          (else
           (let ((bufname (buffer-name)))
             (labels
               ((matches-definition? (definition)
                  (cond ((null? definition)
                         nil)
                        ((eq? (car definition) 'or)
                         (matches-definition? (cdr definition)))
                        ((eq? (caar definition) 'predicate)
                         ;; eval predicate body
                         (or (eval (cdar definition))
                             (matches-definition? (cdr definition))))
                        ((eq? (caar definition) 'mode)
                         (or (eq? major-mode (cdar definition))
                             (matches-definition? (cdr definition))))
                        ((eq? (caar definition) 'name)
                         (or (string-match-pure? (cdar definition) bufname)
                             (matches-definition? (cdr definition))))))
                (find-group (groups)
                  (if (null? groups)
                    '("text")
                    (let ((group (caar groups))
                          (definition (cadar groups)))
                      (if (matches-definition? definition)
                        ;; tabbar expects group to be a list
                        (list group)
                        (find-group (cdr groups)))))))
               (find-group +buffer-groups+)))))))

(defun tabbar-buffer-list+ ()
  (filter (lambda (buf)
            (and (or (not (null? (buffer-file-name buf)))
                     (not (char=? ?\s (aref (buffer-name buf) 0))))
                 (not (invisible-buffer? buf))))
          (buffer-list)))

(setf tabbar-buffer-groups-function 'tabbar-buffer-groups+
      tabbar-buffer-list-function 'tabbar-buffer-list+)
(tabbar-mode t)

(provide 'tabbar-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;; tabbar-setup.el ends here
