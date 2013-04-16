;; tabbar-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 August 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom)
(require 'common)
(require 'more-scheme)
(require 'buffer-groups)
(require 'tabbar)

(setf tabbar-cycling-scope (quote tabs)
      tabbar-home-function nil
      tabbar-home-help-function nil
      tabbar-inhibit-functions '(tabbar-default-inhibit-function)
      tabbar-scroll-left-function nil
      tabbar-scroll-left-help-function nil
      tabbar-scroll-right-function nil
      tabbar-scroll-right-help-function nil)


(defun tabbar-move-selected-tab-left ()
  (interactive)
  (let* ((tabset    (tabbar-current-tabset t))
         (selected  (tabbar-selected-tab tabset))
         (tabs      (tabbar-tabs tabset))
         (len       (1- (length tabs)))
         (sel-index (position selected tabs))
         (tabs1     (move-element-left sel-index tabs)))
    (set tabset tabs1)
    (tabbar-set-template tabset nil)
    (tabbar-click-on-tab selected)))

(defun tabbar-move-selected-tab-right ()
  (interactive)
  (let* ((tabset    (tabbar-current-tabset t))
         (selected  (tabbar-selected-tab tabset))
         (tabs      (tabbar-tabs tabset))
         (len       (1- (length tabs)))
         (sel-index (position selected tabs))
         (tabs1     (move-element-right sel-index tabs)))
    (set tabset tabs1)
    (tabbar-set-template tabset nil)
    (tabbar-click-on-tab selected)))

(defun tabbar-buffer-groups+ (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (cond ((eq? major-mode 'slime-repl-mode)
           '("lisp" "slime"))
          ((invisible-buffer? buffer)
           '("#invisible#"))
          ;; "interpret" +buffer-groups+
          (else
           (let ((bufname (buffer-name)))
             (letrec
                 ((matches-definition?
                   (lambda (definition)
                     (cond ((null? definition)
                            nil)
                           ((eq? (car definition) 'or)
                            (funcall matches-definition? (cdr definition)))
                           ((eq? (caar definition) 'predicate)
                            ;; eval predicate body
                            (or (eval (cdar definition))
                                (funcall matches-definition? (cdr definition))))
                           ((eq? (caar definition) 'mode)
                            (or (eq? major-mode (cdar definition))
                                (funcall matches-definition? (cdr definition))))
                           ((eq? (caar definition) 'name)
                            (or (string-match-pure? (cdar definition) bufname)
                                (funcall matches-definition? (cdr definition)))))))
                  (find-group
                   (lambda (groups)
                     (if (null? groups)
                       '("text")
                       (let ((group (caar groups))
                             (definition (cadar groups)))
                         (if (funcall matches-definition? definition)
                           ;; tabbar expects group to be a list
                           (list group)
                           (funcall find-group (cdr groups))))))))
               (funcall find-group +buffer-groups+)))))))

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
;; End:

;; tabbar-setup.el ends here
