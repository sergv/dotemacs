;; tabbar-buffer-groups.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 April 2012
;; Description:

(require 'more-scheme)
(require 'buffer-groups)

(defun tabbar-buffer-groups+ (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (cond ((eq? major-mode 'slime-repl-mode)
           '("lisp" "slime"))
          ;; "interpret" +buffer-groups+
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


(provide 'tabbar-buffer-groups)

;; Local Variables:
;; lexical-binding: t
;; End:

;; tabbar-buffer-groups.el ends here
