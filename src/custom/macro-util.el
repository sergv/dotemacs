;; macro-util.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defmacro util/eval-if-symbol (x)
  "Evaluate x if it's symbos. Intended to be used inside defmacro."
  `(if (symbolp ,x)
       (eval ,x)
     ,x))

(defmacro* vimmize-motion (func
                           &key
                           (name nil)
                           (exclusive t)
                           (doc nil)
                           (do-not-adjust-point nil))
  "Embed FUNC into vim framework of motions. FUNC may be symbol or
actual call to function."
  (let* ((func-name
          (cond
            ((listp func)
             (symbol-name (car func)))
            ((symbolp func)
             (symbol-name func))
            (t
             (error "FUNC should be call to function (list) or symbol: %s"
                    func))))
         (motion-name (if name
                          name
                        (intern (concat "vim:" func-name)))))
    `(vim:defmotion ,motion-name (,(if exclusive 'exclusive 'inclusive)
                                  ,@(if do-not-adjust-point
                                        '(do-not-adjust-point)
                                      '()))
       ,(if doc doc (concat "See `" func-name "'."))
       ,(if (listp func)
            func
          (list func)))))

(defmacro* vimmize-function (func
                             &key
                             (name nil)
                             (doc nil)
                             (has-count t)
                             (call-n-times nil)
                             (repeatable t)
                             (keep-visual nil))
  "Embed FUNC into vim framework of actions. FUNC may be symbol or
actual call to function. If FUNC is a symbol and CALL-N-TIMES is nil
then symbol should name function of one argument - prefix argument count.

Non-nil CALL-N-TIMES causes resulting function to call FUNC as
many times as was specified by the prefix argument.

Therefore, if FUNC is a call to some function (e.g. (foo bar baz)) then
CALL-N-TIMES should be non nil to cause this call to be applied n times."
  (let* ((func-name
          (cond
            ((listp func)
             (symbol-name (car func)))
            ((symbolp func)
             (symbol-name func))
            (t
             (error "FUNC should be call to function (list) or symbol: %s"
                    func))))
         (action-name (if name name (intern (concat "vim:" func-name)))))
    `(vim:defcmd ,action-name ,(append (if has-count
                                           '(count)
                                         '())
                                       (if repeatable
                                           '()
                                         '(nonrepeatable))
                                       (if keep-visual
                                           '(keep-visual)
                                         '()))
       ,(if doc doc (format "Vimmized version of `%s'." func-name))
       ,(cond
          ((and has-count
                (not (null? call-n-times)))
           (let ((counter '#:counter))
             `(dotimes (,counter (or count 1))
                ,(if (symbolp func)
                     `(funcall #',func)
                   func))))
          ((symbolp func)
           `(funcall #',func
                     ,@(append
                        (if has-count
                            '(count)
                          '()))))
          (t
           func)))))

(defmacro defun-nested-caching (func args reset-cache-func cache-args &rest body)
  "Defun new function FUNC that automatically caches it's output depending of values of
CACHE-ARGS, which should be a list.

NB does not expect to cache values of ARGS that are nil."
  (cl-assert (symbol? func))
  (cl-assert (symbol? reset-cache-func))
  (cl-assert (list? cache-args))
  (cl-assert (-all? #'symbol? cache-args))
  (cl-assert (equal? cache-args
                     (intersection args cache-args :test #'equal?))
             nil
             "defun-caching: CACHE-ARGS must be a subset of ARGS")
  (let ((cache-var (gentemp "cache"))
        (query-var '#:query)
        (hash-table-var '#:hash-table)
        (value-var '#:value)
        (empty-table-expr '(make-hash-table :test #'equal)))
    `(progn
       (defvar ,cache-var ,empty-table-expr)
       (defun ,reset-cache-func ()
         (setf ,cache-var ,empty-table-expr))
       (defun ,func ,args
         (let ((,query-var
                ,(first
                  (foldl (lambda (hash-table-expr-struct x)
                           (destructuring-bind (hash-table-expr may-be-null?)
                               hash-table-expr-struct
                             (list
                              (if may-be-null?
                                  `(let ((,hash-table-var ,hash-table-expr))
                                     (and ,hash-table-var
                                          (gethash ,x ,hash-table-var)))
                                `(gethash ,x ,hash-table-expr))
                              t
                              )))
                         ;; state: hash table and whether it may be null
                         (list cache-var nil)
                         cache-args))))
           (if ,query-var
               ,query-var
             (let ((,value-var (progn ,@body)))
               ,(funcall (foldr (lambda (x mk-value-to-put)
                                  (let ((table-var '#:table))
                                    (lambda (table)
                                      `(let ((,table-var ,table))
                                         (puthash ,x
                                                  ,(funcall mk-value-to-put
                                                            `(gethash ,x
                                                                      ,table-var
                                                                      ,empty-table-expr))
                                                  ,table-var)
                                         ;; return table we've been assigning to
                                         ;; so it may be accessed one level
                                         ;; above current one
                                         ,table-var))))
                                (lambda (unused-table)
                                  value-var)
                                cache-args)
                         cache-var)
               ,value-var)))))))

(defmacro defun-caching (func args reset-cache-func mk-cache-key &rest body)
  "Defun new function FUNC that automatically caches it's output
depending of value of MK-CACHE-KEY, which should be an expression
that returnsn a value to use as a caching key.

NB does not expect to cache values of ARGS that are nil. Also will recompute
BODY if it returns nil."
  (declare (indent 4))
  (cl-assert (symbol? func))
  (cl-assert (symbol? reset-cache-func))
  (let ((cache-var (gentemp "defun-caching--cache"))
        (query-var '#:query)
        (hash-table-var '#:hash-table)
        (value-var '#:value)
        (cache-arg-var '#:cache-key)
        (uninitialized '#:uninitialized)
        (empty-table-expr '(make-hash-table :test #'equal)))
    `(progn
       (defvar ,cache-var ,empty-table-expr)
       (defun ,reset-cache-func ()
         (setf ,cache-var ,empty-table-expr))
       (defun ,func ,args
         (let* ((,cache-arg-var ,mk-cache-key)
                (,query-var
                 (gethash ,cache-arg-var ,cache-var ',uninitialized)))
           (if (eq ,query-var ',uninitialized)
               (let ((,value-var (progn ,@body)))
                 (puthash ,cache-arg-var ,value-var ,cache-var)
                 ,value-var)
             ,query-var))))))

(defmacro defun-once (name &rest body)
  "Define function NAME with body BODY that will call BODY only once and return it's original
value on all subsequent invokations."
  (declare (indent 1))
  (let ((value (gentemp "value"))
        (uninitialized '#:uninitialized))
    `(defalias ',name
       (let ((,value ',uninitialized))
         (lambda ()
           (if (eq ,value ',uninitialized)
               (setf ,value (progn ,@body))
             ,value))))))

;;; other macros

(defmacro when-buffer-has-file (&rest body)
  "Execute BODY if current buffer has file assigned."
  (declare (indent 0))
  `(when buffer-file-name
     ,@body))


(defun quoted? (x)
  (eq 'quote (car-safe x)))

(defun def-keys-for-map--expand-key (key)
  "Expand key definition akin to `kbd' but may also add some
compatibility mappings for unexpected environments."
  (cl-assert (or (stringp key)
                 (vectorp key))
             nil
             "Invalid key definition: %s" key)
  (let ((kbd-expanded (eval `(kbd ,key))))
    ;; Rebind "C-h" for terminals that refuse to send "C-h" and
    ;; send "C-<backspace>" instead.
    (if (equal kbd-expanded (kbd "C-h"))
        (list kbd-expanded
              (kbd "C-<backspace>"))
      (list kbd-expanded))))

(defmacro def-keys-for-map (mode-map &rest key-command-list)
  "Bind keys specified by KEY-COMMAND-LIST into map MODE-MAP. MODE-MAP can be
either a single map or a list of maps.

KEY-COMMAND-LIST can be list of the following:
1. Symbols - they will be treated as variables and their contents treated as
another KEY-COMMAND-LIST spliced in place of a variable;
2. Entries - 2-element lists (KEYS COMMAND) where:
2.a. KEYS    - string or list of string defining keys, in `kbd' format;
2.b. COMMAND - symbol or inline interactive lambda to be invoked on pressing a key.
"
  (declare (indent nil))
  (letrec ((def-key
             (lambda (map key command)
               (cl-assert (or (string? key)
                              (vector? key)
                              (symbol? key))
                          nil
                          "Invalid key: %s"
                          key)
               `(define-key ,map
                  ,key
                  ,(cond
                     ((and (list? command)
                           (or (eq? 'function (car command))
                               (eq? 'quote (car command))))
                      command)
                     ((and (list? command)
                           (eq? 'lambda (car command)))
                      (list 'function command))
                     (t
                      (list 'quote command))))))
           (process-key-command-list
            (lambda (map-var key-command-list)
              (loop
                for entry in key-command-list
                appending (if (symbol? entry)
                              (funcall process-key-command-list map-var (eval entry))
                            (destructuring-bind (key command)
                                (if (quoted? entry)
                                    (eval entry)
                                  entry)
                              (cond
                                ((list? key)
                                 (list
                                  `(dolist (key ',(-mapcat #'def-keys-for-map--expand-key key))
                                     ,(funcall def-key map-var 'key command))))
                                ((string? key)
                                 (list (funcall def-key map-var (eval `(kbd ,key)) command)))
                                (t
                                 (error "Invalid key: %s" key)))))))))
    (let* ((map-var '#:keymap)
           (bindings
            `(dolist (,map-var
                      (list
                       ,@(cond
                           ((list? mode-map)
                            mode-map)
                           (t (list mode-map)))))
               (when (null? ,map-var)
                 ;; don't silently ignore potential problems
                 (error ,(format "warning: map %s is nil" map-var)))
               ,@(funcall process-key-command-list map-var key-command-list))))
      (unless bindings
        (error "No keys bound for %S using following key-command-list %S"
               mode-map
               key-command-list))
      `(prog1 nil
         ,bindings))))

(defmacro make-highlight-procedure (name regexp after-found-predicate)
  `(defun ,name (limit)
     (let (match-data-to-set)
       (save-match-data
         (while (and (null match-data-to-set)
                     (re-search-forward ,regexp limit t))
           (when (funcall ,after-found-predicate)
             (setf match-data-to-set (match-data)))))
       (when match-data-to-set
         (set-match-data match-data-to-set)
         (goto-char (match-end 0))
         t))))

(defmacro* define-repeated-function (orig-func
                                     &key
                                     (prefix "-n")
                                     (default-count 1))
  `(defun ,(make-symbol (concat (symbol-name orig-func)
                                prefix))
       (&optional count)
     ,(format "This function applies `%s' COUNT times."
              (symbol-name orig-func))
     (interactive "p")
     (setf count (or count ,default-count))
     (dotimes (i count)
       (funcall #',orig-func))))

(defmacro* define-switch-to-interpreter (name
                                         (&rest buffer-names)
                                         run-interpreter-command
                                         &key
                                         (doc "")
                                         (test-if-already-running nil)
                                         (save-buffer t)
                                         (error-msg nil)
                                         (sleep-time 1)
                                         (try-count 10))
  "NAME - name of the switch procedure.

BUFFER-NAMES - list of symbols/strings/lists. Symbol stands for variable
which contains buffer name, string stands for buffer name and list
stands for piece of code that may return symbol/string or nil. Nil would
mean that this piece of code failed to yield proper buffer name at the
moment of call so it would be skipped on current iteration. Piece
of code may be called more than once."
  (cl-assert (< 0 try-count))
  (let ((tries '#:tries)
        (done-block '#:done-block)
        (runned '#:runned)
        (tmp '#:tmp))
    `(defun ,name ()
       ,doc
       (interactive)
       ,(when save-buffer
          '(save-buffer-if-modified))
       (let ((,runned nil))
         (block ,done-block

           (dotimes (,tries ,try-count)
             (cond
               ,@(loop
                   for buf in buffer-names
                   collecting
                   (if (listp buf)
                       `((let ((,tmp ,buf))
                           (and ,tmp
                                (buffer-live-p (get-buffer ,tmp))))
                         (pop-to-buffer (get-buffer ,buf) t)
                         (return-from ,done-block))
                     `((buffer-live-p (get-buffer ,buf))
                       (pop-to-buffer (get-buffer ,buf) t)
                       (return-from ,done-block))))

               ((not ,runned)
                (save-window-excursion
                  (save-excursion
                    ,(if (not (null test-if-already-running))
                         `(unless ,test-if-already-running
                            ,run-interpreter-command
                            (setf ,runned t))
                       `(progn
                          ,run-interpreter-command
                          (setf ,runned t))))))

               ,(when (and (not (null sleep-time))
                           (< 0 sleep-time))
                  `(t
                    (sleep-for ,sleep-time)))))

           (error ,(if error-msg
                       error-msg
                     `(format "Can't switch to any buffer of %s"
                              ,(mapconcat (lambda (name)
                                            (if (symbolp name)
                                                (symbol-name name)
                                              name))
                                          buffer-names
                                          ", ")))))))))

(defmacro* defalign (func
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
         (align-regexp start
                       end
                       (eval-when-compile
                         ,(if put-align-spaces-after-str
                              `(concat ,align-re ,spaces-re)
                            `(concat ,spaces-re ,align-re)))
                       1
                       1
                       ,repeat))
       (defun ,func ()
         (interactive "*")
         (when (region-active-p)
           (with-region-bounds start end
             (when prettify-symbols-mode
               (with-silent-modifications
                 (remove-text-properties start end
                                         '(composition
                                           prettify-symbols-start
                                           prettify-symbols-end))))
             (,indent-region-func start end)
             (when prettify-symbols-mode
               (font-lock-flush start end))))))))

(defmacro save-current-line (&rest body)
  "Save current line (but not column), execute BODY and go to saved line."
  (declare (indent 0))
  (let ((line-var '#:line))
    `(let ((,line-var (count-lines-dumb (point-min) (point))))
       (unwind-protect
           (progn ,@body)
         (goto-line-dumb ,line-var)))))

(defmacro save-current-line-column (&rest body)
  "Save current line and column, execute BODY and go to saved line and column."
  (declare (indent 0))
  (let ((column-var '#:column))
    `(save-current-line
      (let ((,column-var (current-column)))
        (unwind-protect
            (progn ,@body)
          (move-to-column ,column-var))))))

(defmacro for-buffer-with-file (filename &rest body)
  "Execute BODY in buffer with contents of FILENAME. If FILENAME is already
opened in some buffer, then reuse it, and insert its contents in temporary
buffer if no such buffer exists."
  (declare (indent 1))
  (let ((buf-var '#:buf)
        (exec-func '#:exec-func))
    `(let ((,exec-func (lambda () ,@body)))
       (if-let (,buf-var (get-file-buffer ,filename))
           (with-current-buffer ,buf-var
             (save-excursion
               (funcall ,exec-func)))
         (with-temp-buffer
           (insert-file-contents ,filename
                                 t ;; make current buffer visit inserted file
                                 )
           (funcall ,exec-func))))))

(defmacro with-first-matching-item (item-var pred-value-var pred items &rest body)
  "Execute BODY with ITEM-VAR and PRED-VALUE-VAR bound to first and second
result of `find-first-matching' respectively, if such result is non-nil, and
return nil otherwise."
  (declare (indent 4))
  (let ((res-var '#:result))
    `(when-let (,res-var (find-first-matching ,pred ,items))
       (multiple-value-bind (,item-var ,pred-value-var) ,res-var
         ,@body))))

;;; with-* macro

(defmacro with-current-frame (frame &rest body)
  (declare (indent 1))
  (let ((selected '#:selected))
    `(let ((,selected ,frame))
       (select-frame ,frame)
       (unwind-protect
           (progn
             ,@body)
         (select-frame ,selected)))))

(defmacro with-disabled-undo (&rest body)
  (declare (indent 0))
  (let ((store '#:store))
    `(let ((,store buffer-undo-list)
           ;; this disables further undo recording
           (buffer-undo-list t))
       ,@body)))

(defmacro with-preserved-buffer-modified-p (&rest body)
  "Execute BODY and restore `buffer-modified-p' flag after its done."
  (declare (indent 0))
  (let ((store '#:store))
    `(let ((,store (buffer-modified-p)))
       (unwind-protect
           (progn
             ,@body)
         (set-buffer-modified-p ,store)))))

(defmacro with-inhibited-modification-hooks (&rest body)
  "Execute BODY and restore `inhibit-modification-hooks' after its done."
  (declare (indent 0))
  `(let ((inhibit-modification-hooks t))
     ,@body))

(defmacro with-inhibited-read-only (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro with-inhibited-redisplay (&rest body)
  (declare (indent 0))
  `(let ((inhibit-redisplay t))
     ,@body))

(defmacro* with-marker ((marker-var marker-init) &rest body)
  (declare (indent 1))
  `(let ((,marker-var ,marker-init))
     (unwind-protect
         (progn
           ,@body)
       (set-marker ,marker-var nil))))

(defmacro with-expanded-invisible-overlays (start end &rest body)
  (declare (indent 2))
  `(let ((invisible-ovs nil))
     (unwind-protect
         (progn
           (dolist (ov (overlays-in ,start ,end))
             (when (overlay-get ov 'invisible)
               (overlay-put ov 'invisible nil)
               (push ov invisible-ovs)))
           ,@body)
       (dolist (ov invisible-ovs)
         (overlay-put ov 'invisible t)))))

(defmacro with-temporary-file (var prefix suffix contents &rest body)
  "Create temporary file with PREFIX and SUFFIX in it's name (see
`make-temp-file' for meaning of these parameters). If CONTENTS is
non-nil, write it to the new file. Name of the created file will be bound to VAR.
Temporary file will be removed after BODY finishes."
  (declare (indent 4))
  `(let ((,var (make-temp-file ,prefix nil ,suffix ,contents)))
     (unwind-protect
         (progn
           ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

;;; aif, awhen

(defmacro aif (condition true-branch &optional false-branch)
  "Anaphoric if, binds evaluated condition to variable it."
  (declare (indent 2))
  `(let ((it ,condition))
     (if it
         ,true-branch
       ,false-branch)))

(defmacro awhen (condition &rest body)
  "Anaphoric if, binds evaluated condition to variable it."
  `(let ((it ,condition))
     (when it
       ,@body)))

;; defparameter, defparameter-local

(defmacro defparameter (var &optional value doc)
  "Just like CL's defparameter, sets variable value when evaluated."
  `(prog1 nil
     (set (defvar ,var nil ,doc) ,value)))

(defmacro defparameter-local (var &optional value doc)
  "Similar to `defparameter' but defines buffer-local variables."
  (let ((tmp-var '#:store))
    `(progn
       (make-variable-buffer-local ',var)
       (setf ,tmp-var ,value)
       (if (boundp ',var)
           (progn
             (setq-default ,var ,tmp-var)
             (setf ,var ,tmp-var))
         (defvar-local ,var ,tmp-var ,doc))
       nil)))

(defmacro time-it (msg &rest body)
  "Count how many seconds BODY executes and report it with format
MSG which should have one %s or %f place."
  (declare (indent 1))
  (let ((start '#:start)
        (end '#:end)
        (res '#:res))
    `(let ((,start (current-time)))
       (let ((,res (progn ,@body)))
         (let ((,end (current-time)))
           (message ,msg (float-time (time-subtract ,end ,start)))
           ,res)))))

;;; end

(provide 'macro-util)

;; Local Variables:
;; End:

;; macro-util.el ends here
