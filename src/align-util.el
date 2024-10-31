;; align-util.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 April 2022
;; Description:

(cl-defmacro defalign (func
                       align-str
                       &key
                       (repeat nil)
                       (require-one-or-more-spaces nil)
                       (put-align-spaces-after-str nil))
  (declare (indent 1))
  (let ((spaces-re (concat "\\([ \t]"
                           (if require-one-or-more-spaces
                               "+"
                             "*")
                           "\\)"))
        (align-re (cond
                    ((stringp align-str)
                     (concat "\\(?:"
                             align-str
                             "\\)"))
                    ((symbolp align-str)
                     align-str)
                    (t
                     (macroexpand-all align-str))))
        (indent-region-func (string->symbol (format "%s-indent-region" func))))
    `(progn
       (defun ,indent-region-func (start end)
         (with-marker (end-marker (copy-marker end))
           (unless indent-tabs-mode
             (remove-tabs-in-region! start end))
           (let ((indent-tabs-mode nil))
             (align-regexp start
                           end-marker
                           (eval-when-compile
                             ,(if put-align-spaces-after-str
                                  `(concat ,align-re ,spaces-re)
                                `(concat ,spaces-re ,align-re)))
                           1
                           1
                           ,repeat))))
       (defun ,func ()
         (interactive "*")
         (when (region-active-p)
           (with-region-bounds start end
             (when prettify-symbols-mode
               (prettify-symbols-decompose-region start end))
             (,indent-region-func start end)
             (when prettify-symbols-mode
               (font-lock-flush start end))))))))

;;;###autoload (autoload 'generic-align-on-equals "align-util.el" nil t)
(defalign generic-align-on-equals "=")

(provide 'align-util)

;; Local Variables:
;; End:

;; align-util.el ends here
