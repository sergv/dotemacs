;; hydra-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 17 March 2020
;; Description:

(require 'common-whitespace)
(require 'hydra)
;; (require 'posframe)
(require 'simple)

;; (setf hydra-hint-display-type (if (posframe-workable-p)
;;                                   'posframe
;;                                 'lv))
(setf hydra-hint-display-type 'lv)

(defmacro defhydra-ext (name args &optional docstring &rest heads)
  "Like `defhydra' but also binds standard keys I expect to find across
all hydras in my setup."
  (declare (indent defun) (doc-string 3))
  (let ((args-ext
         ;; Supplying (vim--remember-this-command-keys!) for :body-pre makes
         ;; it record key for vim’s repeat facility when `name' is invoked.
         (cons :body-pre (cons '(vim--remember-this-command-keys!) args))))
    `(defhydra ,name ,args-ext
       ,docstring
       ,@heads
       ("<escape>" nil))))

(defmacro defhydra-derive (name parent args &optional docstring &rest heads)
  (declare (indent defun) (doc-string 4))
  `(defhydra-ext ,name ,args
     ,(concat (hydra--prop parent "/docstring")
              (if (and docstring
                       (< 0 (length docstring)))
                  (concat "\n\n" (trim-whitespace-left docstring))
                ""))
     ,@(cl-delete-duplicates
        (append (hydra--prop parent "/heads") heads)
        :key #'car
        :test #'equal)))

(defhydra-ext hydra-toggle (:exit nil :foreign-keys nil :hint nil)
  "
Toggle:
_c_olour theme       %(identity *current-color-theme*)
_d_ebug on error     %`debug-on-error
auto _f_ill          %(if auto-fill-function t nil)
searh _h_ighlighting %(not (null (search-highlighting-is-enabled?)))
debug on _q_uit      %`debug-on-quit
_t_runate lines      %`truncate-lines
_w_hitespace mode    %`whitespace-mode"
  ("c" solarized-toggle)
  ("d" toggle-debug-on-error)
  ("f" auto-fill-mode)
  ("h" search-toggle-highlighting :exit t)
  ("q" toggle-debug-on-quit)
  ("t" toggle-truncate-lines)
  ("w" whitespace-mode))

(defalias 'toggle #'hydra-toggle/body)

(provide 'hydra-setup)

;; Local Variables:
;; End:

;; hydra-setup.el ends here
