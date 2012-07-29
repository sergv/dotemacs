

(defmacro make-if-pred (pred error-msg &optional pred-name)
  (let ((name (or pred-name
                  (intern (concat "if-" (symbol-name pred)))))
        (arg-name (gensym)))
    `(defmacro ,name (,arg-name &rest body)
       (declare (indent defun))
       (when body
         (list 'if (list 'funcall '#',pred ,arg-name)
               `(progn ,@body)
               (list 'error
                     (format
                           ,(concat (symbol-name name) ": " error-msg ": %s")
                           ,arg-name)))
         ;; `(if (funcall (function ,,pred) ,,arg-name)
         ;; (progn ,@body)
         ;; (error (concat
         ;; ,,(concat (symbol-name name) ": " error-msg ": %s")
         ;; ,,arg-name)))
         ))))

(make-if-pred symbolp "symbol argument required")

(make-if-pred stringp "string argument required")

(make-if-pred (lambda (xs) (and (listp xs) xs))
              "non-empty list required"
              if-list-nonempty)

(defun make-joined-name (orig-symbol suffix-str &optional prefix-str)
  (if-symbolp orig-symbol
    (if-stringp suffix-str
      (intern (concat prefix-str (symbol-name orig-symbol) suffix-str)))))


(defmacro util:eval-if-symbol (x)
  "Evaluate x if it's symbos. Intended to be used inside defmacro."
  `(if (symbolp ,x)
       (eval ,x)
       ,x))




(defmacro haskell:make-query-to-inferior (query-name query-func &optional use-prefix-arg)
  `(defun ,query-name ,(when use-prefix-arg '(&optional x))
     ,(concat "Perform `"
              (symbol-name query-func)
              "' with current haskell identifier at point.")
     (interactive ,(when use-prefix-arg '(list current-prefix-arg)))
     (let ((sym (haskell-ident-at-point)))
       (when (> (length sym) 0)
         ,(append `(,query-func sym)
                  (when use-prefix-arg '(x)))))))

(provide 'macro-util)


;; Local Variables:
;; lexical-binding: t
;; End:

