;; macro-util.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'subr-x))

(defmacro car-sure (x)
  (if (fboundp #'comp-hint-cons)
      `(car (comp-hint-cons ,x))
    `(car ,x)))

(defmacro cdr-sure (x)
  (if (fboundp #'comp-hint-cons)
      `(cdr (comp-hint-cons ,x))
    `(cdr ,x)))

(defmacro cdar-sure (x)
  `(cdr-sure (car-sure ,x)))

(defmacro cddr-sure (x)
  `(cdr-sure (cdr-sure ,x)))

(defmacro cadr-sure (x)
  "Extract second list element."
  `(car-sure (cdr-sure ,x)))

(defmacro caddr-sure (x)
  "Extract third list element."
  `(car-sure (cdr-sure (cdr-sure ,x))))

(defmacro cadddr-sure (x)
  "Extract fourth list element."
  `(car-sure (cdr-sure (cdr-sure (cdr-sure ,x)))))

(defmacro caddddr-sure (x)
  "Extract fourth list element."
  `(car-sure (cdr-sure (cdr-sure (cdr-sure (cdr-sure ,x))))))

(defmacro setcar-sure (x y)
  (if (fboundp #'comp-hint-cons)
      `(setcar (comp-hint-cons ,x) ,y)
    `(setcar ,x ,y)))

(defmacro setcdr-sure (x y)
  (if (fboundp #'comp-hint-cons)
      `(setcdr (comp-hint-cons ,x) ,y)
    `(setcdr ,x ,y)))

;; TODO: test and use, add -nfilter
(defmacro --nfilter (form list)
  (let ((res '#:res)
        (tmp '#:tmp)
        (lst '#:lst))
    `(let (,res
           ,tmp
           (,lst ,list))
       ;; Find first cell for res
       (while (and ,lst
                   (not ,res))
         (let ((it (car ,lst)))
           (when ,form
             (setf ,tmp (setf ,res ,lst))))
         (setf ,lst (cdr lst)))
       ;; Continue filtetring, now we have a cons cell to modify
       (while ,lst
         (let ((it (car ,lst)))
           (when ,form
             (setf tmp (setcdr-sure tmp lst))))
         (setf ,lst (cdr lst)))
       ,res)))

(defmacro util/eval-if-symbol (x)
  "Evaluate x if it's symbos. Intended to be used inside defmacro."
  `(if (symbolp ,x)
       (eval ,x)
     ,x))

(cl-defmacro vimmize-motion (func
                             &key
                             (name nil)
                             (exclusive t)
                             (doc nil)
                             (unadjusted nil)
                             (raw-result nil))
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
    `(vim-defmotion ,motion-name (,(if exclusive 'exclusive 'inclusive)
                                  ,@(when unadjusted
                                        '(unadjusted))
                                  ,@(when raw-result
                                        '(raw-result)))
       ,(if doc doc (concat "See `" func-name "'."))
       ,(if (listp func)
            func
          (list func)))))

(cl-defmacro vimmize-function (func
                               &key
                               (name nil)
                               (doc nil)
                               (has-count t)
                               (call-n-times nil)
                               (repeatable t)
                               (keep-visual nil)
                               (unadjusted nil))
  "Embed FUNC into vim framework of actions. FUNC may be symbol or
actual call to function. If FUNC is a symbol and CALL-N-TIMES is nil
then symbol should name function of one argument - prefix argument count.

Non-nil CALL-N-TIMES causes resulting function to call FUNC as
many times as was specified by the prefix argument.

Therefore, if FUNC is a call to some function (e.g. (foo bar baz)) then
CALL-N-TIMES should be non nil to cause this call to be applied n times."
  (cl-assert (symbolp func))
  (when (eq func name)
    (error "Function to be vimmized and it’s vim name should be different: %s" func))
  (let* ((func-name (symbol-name func))
         (action-name (if name name (intern (concat "vim:" func-name)))))
    `(vim-defcmd ,action-name ,(append (when has-count
                                           '(count))
                                       (if repeatable
                                           '()
                                         '(nonrepeatable))
                                       (when keep-visual
                                         '(keep-visual))
                                       (when unadjusted
                                         '(unadjusted)))
       ,(or doc (format "Vimmized version of ‘%s’." func-name))
       ,(if (and has-count call-n-times)
            `(dotimes (_ (or count 1))
               (,func))
          `(,func ,@(if has-count '(count) '()))))))

(defmacro defun-nested-caching (func args reset-cache-func cache-args &rest body)
  "Defun new function FUNC that automatically caches it's output depending of values of
CACHE-ARGS, which should be a list.

NB does not expect to cache values of ARGS that are nil."
  (cl-assert (symbolp func))
  (cl-assert (symbolp reset-cache-func))
  (cl-assert (list? cache-args))
  (cl-assert (-all? #'symbolp cache-args))
  (cl-assert (equal? cache-args
                     (intersection args cache-args :test #'equal?))
             nil
             "defun-caching: CACHE-ARGS must be a subset of ARGS")
  (let ((cache-var (string->symbol (concat (symbol->string func) "--internal--cache")))
        (query-var '#:query)
        (hash-table-var '#:hash-table)
        (value-var '#:value)
        (empty-table-expr '(make-hash-table :test #'equal)))
    `(progn
       (defvar ,cache-var ,empty-table-expr)
       (defun ,reset-cache-func ()
         (clrhash ,cache-var))
       (defun ,func ,args
         (let ((,query-var
                ,(first
                  (foldl (lambda (hash-table-expr-struct x)
                           (cl-destructuring-bind (hash-table-expr may-be-null?)
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
                                (lambda (_)
                                  value-var)
                                cache-args)
                         cache-var)
               ,value-var)))))))

(defmacro defun-caching (func args reset-cache-func mk-cache-key &rest body)
  "Defun new function FUNC that automatically caches it's output
depending of value of MK-CACHE-KEY expression, which should be an expression
that returnsn a value to use as a caching key.

NB does not expect to cache values of ARGS that are nil. Also will recompute
BODY if it returns nil."
  (declare (indent 4) (doc-string 5))
  `(defun-caching-extended ,func ,args ,nil ,(string->symbol (format "%s/make-cache" func)) ,reset-cache-func ,mk-cache-key ,@body))

(defmacro defun-caching-extended (func args func-with-explicit-cache make-cache-func reset-cache-func mk-cache-key &rest body)
  "Defun new function FUNC that automatically caches it's output
depending of value of MK-CACHE-KEY, which should be an expression
that returnsn a value to use as a caching key.

NB does not expect to cache values of ARGS that are nil. Also will recompute
BODY if it returns nil."
  (declare (indent 6) (doc-string 7))
  (cl-assert (symbolp func))
  (cl-assert (or (symbolp func-with-explicit-cache) (null func-with-explicit-cache)))
  (cl-assert (symbolp make-cache-func))
  (cl-assert (or (symbolp reset-cache-func) (null reset-cache-func)))
  (let ((cache-var (string->symbol (concat (symbol->string func) "--internal--cache")))
        (cache-arg '#:cache)
        (query-var '#:query)
        (value-var '#:value)
        (cache-arg-var '#:cache-key)
        (uninitialized '#:uninitialized))
    `(progn
       (defun ,make-cache-func ()
         (make-hash-table :test #'equal))
       (defvar ,cache-var (,make-cache-func))
       ,@(awhen reset-cache-func
           (list
            `(defun ,it ()
               ,(format "Reset cache used by ‘%s’" func)
               (clrhash ,cache-var))))
       ,@(awhen func-with-explicit-cache
           `((defun ,it ,(cons cache-arg args)
               ,(format "Similar to ‘%s’ but takes cache variable explicitly." func)
               (let* ((,cache-arg-var ,mk-cache-key)
                      (,query-var
                       (gethash ,cache-arg-var ,cache-arg ',uninitialized)))
                 (if (eq ,query-var ',uninitialized)
                     (let ((,value-var (progn ,@body)))
                       (puthash ,cache-arg-var ,value-var ,cache-arg)
                       ,value-var)
                   ,query-var)))))
       (defun ,func ,args
         ,@(when (stringp (car body))
             (list (car body)))
         (let* ((,cache-arg-var ,mk-cache-key)
                (,query-var
                 (gethash ,cache-arg-var ,cache-var ',uninitialized)))
           (if (eq ,query-var ',uninitialized)
               (let ((,value-var (progn ,@(if (stringp (car body))
                                              (cdr body)
                                            body))))
                 (puthash ,cache-arg-var ,value-var ,cache-var)
                 ,value-var)
             ,query-var))))))

(defmacro defun-once (name &rest body)
  "Define function NAME with body BODY that will call BODY only once and return it's original
value on all subsequent invokations."
  (declare (indent 1))
  (let ((value (cl-gentemp "value"))
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
  (cond
    ((vectorp key)
     (list key))
    ((stringp key)
     (let ((kbd-expanded (eval `(kbd ,key))))
       ;; Rebind "C-h" for terminals that refuse to send "C-h" and
       ;; send "C-<backspace>" instead.
       (if (equal kbd-expanded (kbd "C-h"))
           (list kbd-expanded
                 (kbd "C-<backspace>"))
         (list kbd-expanded))))
    (t
     (error "Cannot expand key: %s" key))))

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
               (cl-assert (or (stringp key)
                              (vectorp key)
                              (symbolp key))
                          nil
                          "Invalid key: %s"
                          key)
               `(define-key ,map
                  ,key
                  ,(cond
                     ((and (listp command)
                           (or (eq 'function (car command))
                               (eq 'quote (car command))))
                      command)
                     ((and (listp command)
                           (eq 'lambda (car command)))
                      (list 'function command))
                     (t
                      (list 'quote command))))))
           (process-key-command-list
            (lambda (map-var key-command-list)
              (cl-loop
                for entry in key-command-list
                appending (if (symbolp entry)
                              (funcall process-key-command-list map-var (eval entry))
                            (cl-destructuring-bind (key command)
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

(cl-defmacro define-repeated-function (orig-func
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

(cl-defmacro define-switch-to-interpreter (name
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
         (cl-block ,done-block

           (dotimes (,tries ,try-count)
             (cond
               ,@(cl-loop
                   for buf in buffer-names
                   collecting
                   (if (listp buf)
                       `((let ((,tmp ,buf))
                           (and ,tmp
                                (buffer-live-p (get-buffer ,tmp))))
                         (pop-to-buffer (get-buffer ,buf) t)
                         (cl-return-from ,done-block))
                     `((buffer-live-p (get-buffer ,buf))
                       (pop-to-buffer (get-buffer ,buf) t)
                       (cl-return-from ,done-block))))

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

(defmacro save-position-unsafe (&rest body)
  "Remember position of point and restore it after BODY finishes normally. If body
exits via signal then no restoration will take place, hence the unsafety."
  (let ((start-var '#:satrt))
    `(let ((,start-var (point)))
       (prog1
           (progn ,@body)
         (goto-char ,start-var)))))

(defmacro save-current-line (&rest body)
  "Save current line (but not column), execute BODY and go to saved line."
  (declare (indent 0))
  (let ((line-var '#:line))
    `(let ((,line-var (count-lines-fixed (point-min) (point))))
       (unwind-protect
           (progn ,@body)
         (goto-line-dumb ,line-var)))))

(defmacro save-current-column (&rest body)
  "Save and column, execute BODY and go to saved column."
  (declare (indent 0))
  (let ((column-var '#:column))
    `(let ((,column-var (current-column-fixed)))
       (unwind-protect
           (progn ,@body)
         (move-to-column ,column-var)))))

(defmacro save-current-line-column (&rest body)
  "Save current line and column, execute BODY and go to saved line and column."
  (declare (indent 0))
  (let ((line-var '#:line)
        (column-var '#:column))
    `(let ((,line-var (count-lines-fixed (point-min) (point)))
           (,column-var (current-column-fixed)))
       (unwind-protect
           (progn ,@body)
         (progn
           (goto-line-dumb ,line-var)
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

(cl-defmacro with-marker ((marker-var marker-init) &rest body)
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

(defmacro with-region-bounds (start end &rest body)
  "Call BODY with variables START and END bound to current region bounds. Does include final
newline in vim’s linewise visual mode."
  (declare (indent 2))
  (cl-assert (symbolp start))
  (cl-assert (symbolp end))
  `(progn
     (unless (region-active-p)
       (error "Region not active"))
     (let ((,start nil)
           (,end nil))
       (if (and (vim-visual-mode-p)
                (eq vim-visual--mode-type 'linewise))
           (setf ,start (save-excursion
                          (goto-char (region-beginning))
                          (line-beginning-position))
                 ,end (save-excursion
                        (goto-char (region-end))
                        ;; Include newline and the end of line.
                        (+ 1 (line-end-position))))
         (setf ,start (region-beginning)
               ,end (region-end)))
       (setf end (min end (point-max)))
       ,@body)))

(defmacro with-region-bounds-unadj (start end &rest body)
  "Call BODY with variables START and END bound to current region bounds. Does not include
final newline in vim’s linewise visual mode."
  (declare (indent 2))
  (cl-assert (symbolp start))
  (cl-assert (symbolp end))
  `(progn
     (unless (region-active-p)
       (error "Region not active"))
     (let ((,start nil)
           (,end nil))
       (if (and (vim-visual-mode-p)
                (eq vim-visual--mode-type 'linewise))
           (setf ,start (save-excursion
                          (goto-char (region-beginning))
                          (line-beginning-position))
                 ,end (save-excursion
                        (goto-char (region-end))
                        (line-end-position)))
         (setf ,start (region-beginning)
               ,end (region-end)))
       (setf ,end (min ,end (point-max)))
       ,@body)))

;;; wrap-search-around

(defmacro fold-direction (direction if-forward if-backward)
  (declare (indent 1))
  (pcase direction
    (`forward  if-forward)
    (`backward if-backward)
    (invalid   (error "Invalid direction: %s" invalid))))

(defmacro fold-direction-at-runtime (direction if-forward if-backward)
  (declare (indent 1))
  `(pcase ,direction
     (`forward  ,if-forward)
     (`backward ,if-backward)
     (invalid   (error "Invalid direction: %s" invalid))))

(cl-defmacro wrap-search-around
    (direction do-search &key not-found-message count)
  "Wrap DO-SEARCH action in current buffer buffer. DO-SEARCH should be
an expression that performs the search, moves the point
and returns a boolean, t when something was found and nil
otherwise. An example DO_SEARCH is `re-search-forward' with
some regexp.

DIRECTION must be a symbol, either 'forward or 'backward (don’t
quote it for macro’s sake).
"
  (declare (indent 1))
  (cl-assert (memq direction '(forward backward)))
  (let ((pt '#:pt)
        (res '#:res))
    `(let ((,pt (point))
           (,res nil)
           (message-log-max nil))
       (dotimes (_ ,(or count 1))
         (setf ,res (or ,do-search
                        (progn
                          ;; Go to boundary and redo the search.
                          (goto-char
                           (fold-direction ,direction (point-min) (point-max)))
                          (if ,do-search
                              (progn
                                (message ,(format "Wrapped at %s"
                                                  (fold-direction-at-runtime direction "bottom" "top")))
                                t)
                            (progn
                              (princ ,(or not-found-message "Nothing found") t)
                              (goto-char ,pt)
                              nil))))))
       ,res)))

;;;

(defmacro with-buffer (buf &rest body)
  (declare (indent 1))
  (let ((buf-var '#:buf))
    `(let ((,buf-var ,buf))
       (if (eq ,buf-var (current-buffer))
           (progn
             ,@body)
         (with-current-buffer ,buf-var
           ,@body)))))

;;;

(defmacro syntax-ppss-update! (var)
  "Reuse result of previous call to ‘syntax-ppss’ if it’s stored in the variable VAR."
  `(setq ,var (syntax-ppss)))

(defmacro syntax-ppss-cached (var)
  "Reuse result of previous call to ‘syntax-ppss’ if it’s stored in the variable VAR."
  `(or ,var
       (syntax-ppss-update! ,var)))

;;;

(defmacro let-alist-static (val names &rest body)
  (declare (indent 2))
  (cl-assert (-all? #'symbolp names))
  (let ((tmp '#:tmp))
    `(let ,names
       (dolist (,tmp ,val)
         (cl-case (car ,tmp)
           ,@(-map (lambda (x)
                     `((,x)
                       (setf ,x (cdr ,tmp))))
                   names)))
       ,@body)))

;;;

(defmacro dovector (args &rest body)
  "Like ‘dolist’, but on vectors.

Use as one of:
(dovector (x xs) ...)
(dovector ((x idx) xs) ...)"
  (declare (indent 1))
  (cl-assert (= 2 (length args)))
  (let ((matcher (car args)))
    (cl-assert (or (and (listp matcher)
                        (= 2 (length matcher))
                        (symbolp (cl-first matcher))
                        (symbolp (cl-second matcher)))
                   (symbolp (car args))))
    (let* ((val (if (listp matcher)
                    (cl-first matcher)
                  matcher))
           (vec (cadr args))
           (i (if (listp matcher)
                  (cl-second matcher)
                '#:i))
           (v '#:vec))
      `(let ((,v ,vec))
         (dotimes (,i (length ,v))
           (let ((,val (aref ,v ,i)))
             ,@body))))))

;;;

(defmacro with-optional-syntax-table (table &rest body)
  "Like ‘with-syntax-table’ but TABLE may be nil."
  (declare (debug t) (indent 1))
  (let ((old-table (make-symbol "table"))
        (old-buffer (make-symbol "buffer"))
        (new-table (make-symbol "new-table")))
    `(let ((,old-table (syntax-table))
           (,old-buffer (current-buffer))
           (,new-table ,table))
       (unwind-protect
           (progn
             (when ,new-table
               (set-syntax-table ,new-table))
             ,@body)
         (when ,new-table
           (with-current-buffer ,old-buffer
             (set-syntax-table ,old-table)))))))

;;; end

(provide 'macro-util)

;; Local Variables:
;; End:

;; macro-util.el ends here
