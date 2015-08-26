;; macro-util.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile (require 'cl-lib))

(defun util/make-joined-name (orig-symbol suffix-str &optional prefix-str)
  (intern (concat prefix-str (symbol-name orig-symbol) suffix-str)))

(defmacro util/eval-if-symbol (x)
  "Evaluate x if it's symbos. Intended to be used inside defmacro."
  `(if (symbolp ,x)
     (eval ,x)
     ,x))

(defmacro rxx (definitions &rest main-expr)
  "Return `rx' invokation of main-expr that has symbols defined in
DEFINITIONS substituted by definition body. DEFINITIONS is list
of let-bindig forms, (<symbol> <body>). No recursion is permitted -
no defined symbol should show up in body of its definition or in
body of any futher definition."
  (awhen (find-if (lambda (def) (not (= 2 (length def)))) definitions)
    (error "rxx: every definition should consist of two elements: (name def), offending definition: %s"
           it))
  `(rx ,@(reduce (lambda (def expr)
                   (subst (cadr def) (car def) expr
                          :test #'eq))
                 definitions
                 :initial-value main-expr
                 :from-end t)))

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
                             (call-n-times nil)
                             (repeatable t))
  "Embed FUNC into vim framework of actions. FUNC may be symbol or
actual call to function. If FUNC is a symbol and CALL-N-TIMES is nil
then symbol should name function of one argument - prefix argument count.

Non-nil CALL-N-TIMES causes resulting function to call FUNC as
many times as was specified by prefix argument.

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
    `(vim:defcmd ,action-name ,(append '(count)
                                       (if repeatable
                                         '()
                                         '(nonrepeatable)))
       ,(if doc doc (format "Vimmized version of `%s'." func-name))
       ,(cond
          ((not (null? call-n-times))
           (let ((counter (gensym)))
             `(dotimes (,counter (or count 1))
                ,(if (symbolp func)
                   `(funcall #',func)
                   func))))
          ((symbolp func)
           `(funcall #',func count))
          (t
           func)))))


(defmacro redefun (func args &rest body)
  "Redefine function FUNC. Arguments as in `defun'."
  (declare (indent defun))
  (let ((new-name (intern (concat (symbol-name func) "+"))))
    `(prog1 nil
       (defun ,new-name ,args
         ,@body)
       (fset ',func ',new-name))))

(defmacro defun-caching (func args reset-cache-func cache-args &rest body)
  "Defun new function FUNC that automatically caches it's output depending of values of
CACHE-ARGS, which should be a list.

NB does not expect to cache values of ARGS that are nil."
  (assert (symbol? func))
  (assert (symbol? reset-cache-func))
  (assert (list? cache-args))
  (assert (all? #'symbol? cache-args))
  (assert (equal? cache-args
                  (intersection args cache-args :test #'equal?))
          nil
          "defun-caching: CACHE-ARGS must be a subset of ARGS")
  (let ((cache-var (gentemp "cache"))
        (query-var (gensym "query"))
        (hash-table-var (gensym "hash-table"))
        (value-var (gensym "value"))
        (not-present-sym `(quote ,(gensym "not-present")))
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
                                  (let ((table-var (gensym "table")))
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

;;; circular jumps

(defmacro define-circular-jumps (forward-name
                                 backward-name
                                 regex
                                 &optional
                                 init)
  "Define two functions, FORWARD-NAME and BACKWARD-NAME to perform
jumps on REGEX with wraparound in buffer. INIT form will be executed
before performing any jumps."
  (declare (indent 2))
  (let ((forward-search `(re-search-forward
                          ,regex
                          nil
                          t))
        (backward-search `(re-search-backward
                           ,regex
                           nil
                           t))
        (found-var (string->symbol "found?"))
        (original-pos-var (string->symbol "pt")))
    `(progn
       (defun ,forward-name ()
         "Jump forward between regexp matches with wraparound."
         (interactive)
         ,init
         (let ((,found-var nil)
               (,original-pos-var (point)))
           (save-match-data
             ;; this rather complicated check checks for case of first prompt in
             ;; the buffer
             (when (or (= 1 (forward-line 1))
                       (eobp)
                       (beginning-of-line)
                       (not (setf ,found-var ,forward-search)))
               (goto-char (point-min))
               (setf ,found-var ,forward-search))
             (if ,found-var
               (goto-char (match-beginning 0))
               (progn
                 (goto-char ,original-pos-var)
                 (next-line))))))

       (defun ,backward-name ()
         "Jump backward between regexp matches with wraparound."
         (interactive)
         ,init
         (let ((,found-var nil)
               (,original-pos-var (point)))
           (save-match-data
             (when (or (= -1 (forward-line -1))
                       (bobp)
                       (end-of-line)
                       (not (setf ,found-var ,backward-search)))
               (goto-char (point-max))
               (setf ,found-var ,backward-search))
             (if ,found-var
               (goto-char (match-beginning 0))
               (progn
                 (goto-char ,original-pos-var)
                 (previous-line)))))))))

(defmacro* define-circular-prompt-property-jumps (forward-name
                                                  backward-name
                                                  property
                                                  value
                                                  &key
                                                  (init nil)
                                                  (move-to-property-end t))
  "Define two functions, FORWARD-NAME and BACKWARD-NAME to perform
jumps over text property PROPERTY with value VALUE with wraparound in
current buffer. INIT form will be executed before performing any jumps."
  `(progn
     (defun ,forward-name ()
       "Jump forward between prompts with wraparound."
       (interactive)
       ,init
       ;; (search-property 'forward
       ;;                  t
       ;;                  ,property
       ;;                  ,value)
       (search-property-forward ,property
                                ,value
                                'cycle
                                t ;; no error
                                )
       ,(when move-to-property-end
          '(let ((change-pos (next-single-property-change
                              (point)
                              'field
                              (current-buffer)
                              (point-max))))
             (if change-pos
               (goto-char change-pos)
               (goto-char (point-max))))))

     (defun ,backward-name ()
       "Jump backward between prompts with wraparound."
       (interactive)
       ,init
       (search-property-backward ,property
                                 ,value
                                 'cycle
                                 t)
       ;; (search-property 'backward
       ;;                  t
       ;;                  ,property
       ;;                  ,value)
       ,(when move-to-property-end
          '(let ((change-pos (next-single-property-change
                              (point)
                              'field
                              (current-buffer)
                              (point-max))))
             (if change-pos
               (goto-char change-pos)
               (goto-char (point-max))))))))

;;; other macros

(defmacro if-buffer-has-file (&rest body)
  "Execute BODY if current buffer has file assigned."
  (declare (indent 0))
  `(when (buffer-file-name)
     ,@body))

(defmacro if-has-makefile-command (&rest body)
  "Execute BODY if current file is listed in some makefile
in the same directory the current file is."
  (let ((fname-var (gensym))
        (fname-re-var (gensym)))
    `(if-buffer-has-file
       (let* ((,fname-var (file-name-nondirectory buffer-file-name))
              (,fname-re-var (concat "\\<" ,fname-var)))
         (when (some (lambda (makefile)
                       (file-contents-matches-re makefile ,fname-re-var))
                     '("makefile" "Makefile" "MAKEFILE"))
           ,@body)))))


(defmacro def-keys-for-map (mode-map &rest key-command-list)
  (declare (indent nil))
  (letrec ((def-key
             (lambda (map key command)
               `(define-key ,map
                  ,(eval `(kbd ,key))
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
            (lambda (map key-command-list)
              (loop
                for entry in key-command-list
                if (symbol? entry)
                for (key command) = (if (or (quoted? entry)
                                            (symbol? entry))
                                      (eval entry)
                                      entry)
                appending (if (symbol? entry)
                            (funcall process-key-command-list map (eval entry))
                            (destructuring-bind (key command)
                                (if (quoted? entry)
                                  (eval entry)
                                  entry)
                              (list (funcall def-key map key command))))))))
    (let* ((map-var (gensym "kmap"))
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

(defmacro run-if-fbound (func)
  `(and (fboundp (quote ,func))
        (,func)))


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

(defmacro* define-print-info-skeleton
    (name
     &key
     (doc nil)
     (insert-entity-name-procedure
      (lambda (beginning)
        (save-excursion
          (condition-case nil
              (progn
                (goto-char beginning)
                (save-excursion
                  ;; this throws error if no enclosing list found
                  (backward-up-list))
                (beginning-of-defun)
                (vim:motion-fwd-WORD)
                (concat
                 (upcase (symbol-name (symbol-at-point)))
                 ": "))
            (error "")))))
     (print-begin "(format t ")
     (print-end ")")
     (indent-after-func #'prog-indent-sexp)
     (make-variable-list #'join-lines)
     (msg-transform nil)
     ;; can be either some constant or function of one argument - user input
     (format-print-value "~a")
     (format-string-start "\"")
     (format-string-end "~%\"")
     (insert-newline-before-var-list t)
     (name-value-delimiter " = " ;; former ": "
                           )
     (variable-delimiter ", ")
     (message-delimiter "; "))
  "Now, here's quite complicated stuff but very general too. This macro can
solve most of debug print problems of mine.

I want to stress one point here: this macro does not and would never ever
support debug printing for C++ using cout et al.
Update: now it can support C++'s std::cout and other, check out
`shell-script-info-message-skeleton' for inspiration.

MSG-TRANSFORM - function to apply to entered text before inserting it into string,
will be applied to both variables and messages.

NAME-VALUE-DELIMITER string to be inserted between variable name and it's value."
  (let ((initial-state `(list (point) nil nil))
        (start-position #'car)
        (previously-inserted-message #'cadr)
        (variable-names #'caddr)
        (make-func-call (lambda (name)
                          (cond
                            ((functionp name)
                             `(function ,name))
                            ((list? name)
                             `(function ,(eval name)))
                            (t
                             name)))))
    `(define-skeleton ,name
       ,doc
       nil
       ;; store start of skeleton
       '(setq v1 ,initial-state)

       ,print-begin
       ,format-string-start
       ;; get name of function containing point
       ;; this form evaluates to string which would be function name
       (funcall ,(funcall make-func-call insert-entity-name-procedure)
                (,start-position v1))
       ((let* ((x (read-string-no-default "Variable or message starting with space: "
                                          nil
                                          nil
                                          ""))
               (message? (and (not (zerop (length x)))
                              (or (char= ?\s (aref x 0))
                                  (char= ?\t (aref x 0)))))
               (result nil))
          (if (< 0 (length x))
            (progn
              ;; so, if str starts with a space thes it's hardly a variable name or
              ;; some other form with value. Therefore it's a message to print,
              ;; without associated value
              (if message?
                (setf result
                      (funcall ,(if msg-transform
                                  (funcall make-func-call msg-transform)
                                  '(function identity))
                               (replace-regexp-in-string "^[ \t]+" "" x)))
                (let ((msg ,(if msg-transform
                              `(funcall ,(funcall make-func-call msg-transform)
                                        x)
                              'x))
                      (var-name x))
                  (push x (,variable-names v1))
                  (setf result
                        ,(if (functionp format-print-value)
                           `(funcall #',format-print-value x)
                           `(concat msg
                                    ,name-value-delimiter
                                    ,format-print-value)))))
              ;; if anything was inserted previously then prepend
              ;; ", " or "; " to result
              (when (,previously-inserted-message v1)
                (setf result (concat (case (,previously-inserted-message v1)
                                       (message
                                        (if message?
                                          ,variable-delimiter
                                          ,message-delimiter))
                                       (variable
                                        (if message?
                                          ,message-delimiter

                                          ,variable-delimiter)))
                                     result)))
              (setf (,previously-inserted-message v1)
                    (if message? 'message 'variable))
              ;; made up format chunk, return it so that str reference can use it
              result)
            ;; done entering values - communicate that to skeleton mode
            ""))
        ;; reference to just prompted message/variable name, would be immediately
        ;; inserted by skeleton into buffer
        str)
       ,format-string-end
       ,(when insert-newline-before-var-list
          ;; print \n only if there are any variable names
          `(when (,variable-names v1) '\n))
       (funcall ,(funcall make-func-call make-variable-list)
                (nreverse
                 (remove-if (lambda (str)
                              (= 0 (length str)))
                            (,variable-names v1))))
       ,print-end
       ,(when indent-after-func
          `(save-excursion
             (goto-char (,start-position v1))
             (funcall ,(funcall make-func-call indent-after-func)))))))

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
  (assert (< 0 try-count))
  (let ((switch (gensym))
        (tries (gensym))
        (called-interpreter (gensym))
        (done-block (gensym))
        (runned (gensym))
        (tmp (gensym)))
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

(defmacro* make-align-function (func
                                align-str
                                &key
                                (repeat nil)
                                (require-one-or-more-spaces nil)
                                (put-align-spaces-after-str nil))
  (let ((spaces-re (concat "\\([ \t]"
                           (if require-one-or-more-spaces
                             "+"
                             "*")
                           "\\)"))
        (align-re (if (string? align-str)
                    (concat "\\(?:"
                            align-str
                            "\\)")
                    (macroexpand-all align-str))))
    `(defun ,func ()
       (interactive)
       (when (region-active?)
         (multiple-value-bind (start end) (get-region-bounds)
           (align-regexp start
                         end
                         ,(if put-align-spaces-after-str
                            (concat align-re spaces-re)
                            (concat spaces-re align-re))
                         1
                         1
                         ,repeat))))))

(defmacro save-current-line-column (&rest body)
  "Save current line and column, execute BODY and go to saved line and column."
  (let ((line-var (gensym "line"))
        (column-var (gensym "column")))
    `(let ((,line-var (count-lines1 (point-min) (point)))
           (,column-var (current-column)))
       (unwind-protect
           (progn ,@body)
         (goto-line1 ,line-var)
         (move-to-column ,column-var)))))

(defmacro for-buffer-with-file (filename &rest body)
  "Execute BODY in buffer with contents of FILENAME. If FILENAME is already
opened in some buffer, then reuse it, and insert its contents in temporary
buffer if no such buffer exists."
  (declare (indent 1))
  (let ((buf-var (gensym))
        (exec-func (gensym)))
    `(let ((,exec-func (lambda () ,@body)))
       (if-let (,buf-var (get-file-buffer ,filename))
         (with-current-buffer ,buf-var
           (funcall ,exec-func))
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
  (let ((res-var (gensym)))
    `(when-let (,res-var (find-first-matching ,pred ,items))
       (multiple-value-bind (,item-var ,pred-value-var) ,res-var
         ,@body))))

;;; with-* macro

(defmacro with-current-frame (frame &rest body)
  (declare (indent 1))
  (let ((selected (gensym)))
    `(let ((,selected ,frame))
       (select-frame ,frame)
       (unwind-protect
           (progn
             ,@body)
         (select-frame ,selected)))))

(defmacro with-disabled-undo (&rest body)
  (declare (indent 0))
  (let ((store (gensym)))
    `(let ((,store buffer-undo-list)
           ;; this disables further undo recording
           (buffer-undo-list t))
       ,@body)))

(defmacro with-preserved-buffer-modified-p (&rest body)
  "Execute BODY and restore `buffer-modified-p' flag after its done."
  (declare (indent 0))
  (let ((store (gensym)))
    `(let ((,store (buffer-modified-p)))
       (unwind-protect
           (begin
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

;;; aif, awhen, if-let

(defmacro aif (condition true-branch &optional false-branch)
  "Anaphoric if, binds evaluated condition to variable it."
  `(let ((it ,condition))
     (if it
       ,true-branch
       ,false-branch)))

(defmacro awhen (condition &rest body)
  "Anaphoric if, binds evaluated condition to variable it."
  `(let ((it ,condition))
     (when it
       ,@body)))

(defmacro if-let (condition true-branch &optional false-branch)
  "E.g. (if-let (x (assoc 'foo bar)) baz quux)"
  (assert (and (list? condition)
               (= 2 (length condition)))
          nil
          "if-let error: invalid condition: %s" condition)
  (if (list? condition)
    (let ((tmp-var (gensym "cond-var"))
          (cond-var (first condition))
          (expr (first (rest condition))))
      `(let ((,tmp-var ,expr))
         (if ,tmp-var
           (let ((,cond-var ,tmp-var))
             ,true-branch)
           ,false-branch)))
    (error "not implemented yet")))

(defmacro when-let (condition &rest body)
  (declare (indent 1))
  `(if-let ,condition (progn ,@body)))

(defmacro when-let* (conditions &rest body)
  (declare (indent 1))
  (assert (and (list? conditions)
               (evenp (length conditions))))
  (if (null? conditions)
    `(progn ,@body)
    `(when-let (,(first conditions)
                ,(second conditions))
       (when-let* ,(cddr conditions)
                  ,@body))))

(defmacro if-let* (conditions true-branch &optional false-branch)
  "E.g.
(if-let (foo 1
         bar 2
         baz 3)
  (+ foo bar baz)
  'failed)))))"
  (declare (indent 1))
  (assert (and (list? conditions)
               (evenp (length conditions))))
  (if (null? conditions)
    true-branch
    `(if-let (,(first conditions)
              ,(second conditions))
       (if-let* ,(cddr conditions)
                ,true-branch
                ,false-branch)
       ,false-branch)))

(defmacro defparameter (var &optional value doc)
  "Just like CL's defparameter, sets variable value when evaluated."
  (let ((tmp-var (gensym "store")))
    `(progn
       (setf ,tmp-var ,value)
       (if (boundp ',var)
         (setf ,var ,tmp-var)
         (defvar ,var ,tmp-var ,doc))
       nil)))

;;; compatibility defines

(unless (symbol-function 'defvar-local)
  ;; taken verbatim from subr.el of emacs 24.3
  (defmacro defvar-local (var val &optional docstring)
    "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
    (declare (debug defvar) (doc-string 3))
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'progn (list 'defvar var val docstring)
          (list 'make-variable-buffer-local (list 'quote var)))))

;;; end

(provide 'macro-util)

;; Local Variables:
;; End:

;; macro-util.el ends here
