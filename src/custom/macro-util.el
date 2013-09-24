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

(defmacro rxx (definitions &rest main-expr)
  "Return `rx' invokation of main-expr that has symbols defined in
DEFINITIONS substituted by definition body. DEFINITIONS is list
of the form of let-bindigs, (symbol body). No recursion is permitted -
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

;;;; circular jumps

(defmacro define-circular-jumps (forward-name
                                 backward-name
                                 regex
                                 &optional
                                 init)
  "Define two functions, FORWARD-NAME and BACKWARD-NAME to perform
jumps on REGEX with wraparound in buffer. INIT form will be executed
before performing any jumps."
  (declare (indent 2))
  (symbol-macrolet ((forward-search `(re-search-forward
                                      ,regex
                                      nil
                                      t))
                    (backward-search `(re-search-backward
                                       ,regex
                                       nil
                                       t)))
    `(progn
       (defun ,forward-name ()
         "Jump forward between regexp matches with wraparound."
         (interactive)
         ,init
         (save-match-data
           ;; this rather complicated check checks for case of first prompt in
           ;; the buffer
           (when (or (= 1 (forward-line 1))
                     (eobp)
                     (beginning-of-line)
                     (not ,forward-search))
             (goto-char (point-min))
             ,forward-search)
           (goto-char (match-beginning 0))))

       (defun ,backward-name ()
         "Jump backward between regexp matches with wraparound."
         (interactive)
         ,init
         (save-match-data
           (when (or (= -1 (forward-line -1))
                     (bobp)
                     (end-of-line)
                     (not ,backward-search))
             (goto-char (point-max))
             ,backward-search)
           (goto-char (match-beginning 0)))))))

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
                                t)
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

;;;; other macros

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
                     (else
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
    (let ((bindings
           (loop
             for map in (cond
                          ((quoted? mode-map)
                           (eval mode-map))
                          ((list? mode-map)
                           mode-map)
                          (else (list mode-map)))
             collecting `(if (not (null? ,map))
                           (progn
                             ,@(funcall process-key-command-list map key-command-list))
                           ;; don't silently ignore potential problems
                           (error ,(format "warning: map %s is nil" map))))))
      (unless bindings
        (error "No keys bound for %S using following key-command-list %S"
               mode-map
               key-command-list))
      `(prog1 nil
         ,@bindings))))

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
                           ))
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
                            (else
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
               (message? (and (< 0 (length x))
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
                      ,(if msg-transform
                         `(funcall ,msg-transform
                                   (replace-regexp-in-string "^[ \t]+" "" x))
                         '(replace-regexp-in-string "^[ \t]+" "" x)))
                (let ((msg ,(if msg-transform
                              `(funcall ,(funcall make-func-call msg-transform)
                                        x)
                              'x))
                      (var-name x))
                  (push x (,variable-names v1))
                  (setf result
                        (concat msg
                                ,name-value-delimiter
                                ,(if (functionp format-print-value)
                                   `(funcall ,format-print-value x)
                                   'x)))))
              ;; if anything was inserted previously then prepend
              ;; ", " or "; " to result
              (when (,previously-inserted-message v1)
                (setf result (concat (case (,previously-inserted-message v1)
                                       (message
                                        (if message?
                                          ", "
                                          "; "))
                                       (variable
                                        (if message?
                                          "; "
                                          ", ")))
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
          '(if-buffer-has-file
             (when (buffer-modified-p)
               (save-buffer))))
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
       (when (or (region-active-p)
                 (run-if-fbound vim:visual-mode-p))
         (align-regexp (region-beginning)
                       (region-end)
                       ,(if put-align-spaces-after-str
                          (concat align-re spaces-re)
                          (concat spaces-re align-re))
                       1
                       1
                       ,repeat)))))


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

;; this function would not define make-NAME if :consturctor argument was supplied
(defmacro defstruct* (struct &rest descs)
  "Define a struct type.
This macro defines a new data type called NAME that stores data
in SLOTs.  It defines a `make-NAME' constructor, a `copy-NAME'
copier, a `NAME-p' predicate, and slot accessors named `NAME-SLOT'.
You can use the accessors to set the corresponding slots, via `setf'.

NAME may instead take the form (NAME OPTIONS...), where each
OPTION is either a single keyword or (KEYWORD VALUE).
See Info node `(cl)Structures' for a list of valid keywords.

Each SLOT may instead take the form (SLOT SLOT-OPTS...), where
SLOT-OPTS are keyword-value pairs for that slot.  Currently, only
one keyword is supported, `:read-only'.  If this has a non-nil
value, that slot cannot be set via `setf'.

\(fn NAME SLOTS...)"
  (let* ((name (if (consp struct) (car struct) struct))
         (opts (cdr-safe struct))
         (slots nil)
         (defaults nil)
         (conc-name (concat (symbol-name name) "-"))
         (constructor (intern (format "make-%s" name)))
         (constrs nil)
         (copier (intern (format "copy-%s" name)))
         (predicate (intern (format "%s-p" name)))
         (print-func nil) (print-auto nil)
         (safety (if (cl-compiling-file) cl-optimize-safety 3))
         (include nil)
         (tag (intern (format "cl-struct-%s" name)))
         (tag-symbol (intern (format "cl-struct-%s-tags" name)))
         (include-descs nil)
         (side-eff nil)
         (type nil)
         (named nil)
         (forms nil)
         pred-form pred-check)
    (if (stringp (car descs))
      (push (list 'put (list 'quote name) '(quote structure-documentation)
                  (pop descs)) forms))
    (setq descs (cons '(cl-tag-slot)
                      (map (function (lambda (x) (if (consp x) x (list x))))
                           descs)))
    (while opts
      (let ((opt (if (consp (car opts)) (caar opts) (car opts)))
            (args (cdr-safe (pop opts))))
        (cond ((eq opt :conc-name)
               (if args
                 (setq conc-name (if (car args)
                                   (symbol-name (car args)) ""))))
              ((eq opt :constructor)
               (if (cdr args)
                 (progn
                   (setq constructor nil)
                   (push args constrs)
                   ;; If this defines a constructor of the same name as
                   ;; the default one, don't define the default.
                   ;; (if (eq (car args) constructor)
                   ;;   (setq constructor nil))
                   ;; (push args constrs)
                   )
                 (if args (setq constructor (car args)))))
              ((eq opt :copier)
               (if args (setq copier (car args))))
              ((eq opt :predicate)
               (if args (setq predicate (car args))))
              ((eq opt :include)
               (setq include (car args)
                     include-descs (map (function
                                         (lambda (x)
                                           (if (consp x) x (list x))))
                                        (cdr args))))
              ((eq opt :print-function)
               (setq print-func (car args)))
              ((eq opt :type)
               (setq type (car args)))
              ((eq opt :named)
               (setq named t))
              ((eq opt :initial-offset)
               (setq descs (nconc (make-list (car args) '(cl-skip-slot))
                                  descs)))
              (t
               (error "Slot option %s unrecognized" opt)))))
    (if print-func
      (setq print-func (list 'progn
                             (list 'funcall (list 'function print-func)
                                   'cl-x 'cl-s 'cl-n) t))
      (or type (and include (not (get include 'cl-struct-print)))
          (setq print-auto t
                print-func (and (or (not (or include type)) (null print-func))
                                (list 'progn
                                      (list 'princ (format "#S(%s" name)
                                            'cl-s))))))
    (if include
      (let ((inc-type (get include 'cl-struct-type))
            (old-descs (get include 'cl-struct-slots)))
        (or inc-type (error "%s is not a struct name" include))
        (and type (not (eq (car inc-type) type))
             (error ":type disagrees with :include for %s" name))
        (while include-descs
          (setcar (memq (or (assq (caar include-descs) old-descs)
                            (error "No slot %s in included struct %s"
                                   (caar include-descs) include))
                        old-descs)
                  (pop include-descs)))
        (setq descs (append old-descs (delq (assq 'cl-tag-slot descs) descs))
              type (car inc-type)
              named (assq 'cl-tag-slot descs))
        (if (cadr inc-type) (setq tag name named t))
        (let ((incl include))
          (while incl
            (push (list 'pushnew (list 'quote tag)
                        (intern (format "cl-struct-%s-tags" incl)))
                  forms)
            (setq incl (get incl 'cl-struct-include)))))
      (if type
        (progn
          (or (memq type '(vector list))
              (error "Invalid :type specifier: %s" type))
          (if named (setq tag name)))
        (setq type 'vector named 'true)))
    (or named (setq descs (delq (assq 'cl-tag-slot descs) descs)))
    (push (list 'defvar tag-symbol) forms)
    (setq pred-form (and named
                         (let ((pos (- (length descs)
                                       (length (memq (assq 'cl-tag-slot descs)
                                                     descs)))))
                           (if (eq type 'vector)
                             (list 'and '(vectorp cl-x)
                                   (list '>= '(length cl-x) (length descs))
                                   (list 'memq (list 'aref 'cl-x pos)
                                         tag-symbol))
                             (if (= pos 0)
                               (list 'memq '(car-safe cl-x) tag-symbol)
                               (list 'and '(consp cl-x)
                                     (list 'memq (list 'nth pos 'cl-x)
                                           tag-symbol))))))
          pred-check (and pred-form (> safety 0)
                          (if (and (eq (caadr pred-form) 'vectorp)
                                   (= safety 1))
                            (cons 'and (cdddr pred-form)) pred-form)))
    (let ((pos 0) (descp descs))
      (while descp
        (let* ((desc (pop descp))
               (slot (car desc)))
          (if (memq slot '(cl-tag-slot cl-skip-slot))
            (progn
              (push nil slots)
              (push (and (eq slot 'cl-tag-slot) (list 'quote tag))
                    defaults))
            (if (assq slot descp)
              (error "Duplicate slots named %s in %s" slot name))
            (let ((accessor (intern (format "%s%s" conc-name slot))))
              (push slot slots)
              (push (nth 1 desc) defaults)
              (push (list*
                     'defsubst* accessor '(cl-x)
                     (append
                      (and pred-check
                           (list (list 'or pred-check
                                       `(error "%s accessing a non-%s"
                                               ',accessor ',name))))
                      (list (if (eq type 'vector) (list 'aref 'cl-x pos)
                                (if (= pos 0) '(car cl-x)
                                    (list 'nth pos 'cl-x)))))) forms)
              (push (cons accessor t) side-eff)
              (push (list 'define-setf-method accessor '(cl-x)
                          (if (cadr (memq :read-only (cddr desc)))
                            (list 'progn '(ignore cl-x)
                                  `(error "%s is a read-only slot"
                                          ',accessor))
                            ;; If cl is loaded only for compilation,
                            ;; the call to cl-struct-setf-expander would
                            ;; cause a warning because it may not be
                            ;; defined at run time.  Suppress that warning.
                            (list 'with-no-warnings
                                  (list 'cl-struct-setf-expander 'cl-x
                                        (list 'quote name) (list 'quote accessor)
                                        (and pred-check (list 'quote pred-check))
                                        pos))))
                    forms)
              (if print-auto
                (nconc print-func
                       (list (list 'princ (format " %s" slot) 'cl-s)
                             (list 'prin1 (list accessor 'cl-x) 'cl-s)))))))
        (setq pos (1+ pos))))
    (setq slots (nreverse slots)
          defaults (nreverse defaults))
    (and predicate pred-form
         (progn (push (list 'defsubst* predicate '(cl-x)
                            (if (eq (car pred-form) 'and)
                              (append pred-form '(t))
                              (list 'and pred-form t))) forms)
                (push (cons predicate 'error-free) side-eff)))
    (and copier
         (progn (push (list 'defun copier '(x) '(copy-sequence x)) forms)
                (push (cons copier t) side-eff)))
    (if constructor
      (push (list constructor
                  (cons '&key (delq nil (copy-sequence slots))))
            constrs))
    (while constrs
      (let* ((name (caar constrs))
             (args (cadr (pop constrs)))
             (anames (cl-arglist-args args))
             (make (mapcar* (function (lambda (s d) (if (memq s anames) s d)))
                            slots defaults)))
        (push (list 'defsubst* name
                    (list* '&cl-defs (list 'quote (cons nil descs)) args)
                    (cons type make)) forms)
        (if (cl-safe-expr-p (cons 'progn (map 'second descs)))
          (push (cons name t) side-eff))))
    (if print-auto (nconc print-func (list '(princ ")" cl-s) t)))
    (if print-func
      (push `(push
              ;; The auto-generated function does not pay attention to
              ;; the depth argument cl-n.
              (lambda (cl-x cl-s ,(if print-auto '_cl-n 'cl-n))
                (and ,pred-form ,print-func))
              custom-print-functions)
            forms))
    (push (list 'setq tag-symbol (list 'list (list 'quote tag))) forms)
    (push (list* 'eval-when '(compile load eval)
                 (list 'put (list 'quote name) '(quote cl-struct-slots)
                       (list 'quote descs))
                 (list 'put (list 'quote name) '(quote cl-struct-type)
                       (list 'quote (list type (eq named t))))
                 (list 'put (list 'quote name) '(quote cl-struct-include)
                       (list 'quote include))
                 (list 'put (list 'quote name) '(quote cl-struct-print)
                       print-auto)
                 (map (function (lambda (x)
                                  (list 'put (list 'quote (car x))
                                        '(quote side-effect-free)
                                        (list 'quote (cdr x)))))
                      side-eff))
          forms)
    (cons 'progn (nreverse (cons (list 'quote name) forms)))))

(defmacro bind (bindings &rest body)
  (cond
    ((null bindings)
     `(progn ,@body))
    ((listp (first (car bindings)))
     `(multiple-value-bind ,(first (car bindings))
          ,(second (car bindings))
        (bind ,(cdr bindings) ,@body)))
    (t
     (multiple-value-bind (symbol-bindings other-bindings)
         (split-with-pred (lambda (b) (symbolp (car b)))
                          bindings)
       `(cl:let ,symbol-bindings
                (bind ,other-bindings ,@body))))))


;;;; with-* macro

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

;;;; aif, awhen, if-let

(defmacro aif (condition if-branch &optional else-branch)
  "Anaphoric if, binds evaluated condition to variable it."
  `(let ((it ,condition))
     (if it
       ,if-branch
       ,else-branch)))

(defmacro awhen (condition &rest body)
  "Anaphoric if, binds evaluated condition to variable it."
  `(let ((it ,condition))
     (when it
       ,@body)))

(defmacro if-let (condition if-branch &optional else-branch)
  (assert (and (or (list? condition)
                   (vector? condition))
               (= 2 (length condition)))
          nil
          "if-let error: invalid condition: %s" condition)
  (if (list? condition)
    (let ((tmp-var (gensym))
          (cond-var (first condition))
          (expr (first (rest condition))))
      `(let ((,tmp-var ,expr))
         (if ,tmp-var
           (let ((,cond-var ,tmp-var))
             ,if-branch)
           ,else-branch)))
    (error "not implemented yet")))

(defmacro when-let (condition &rest body)
  (assert (and (or (list? condition)
                   (vector? condition))
               (= 2 (length condition))))
  (if (list? condition)
    (let ((tmp-var (gensym))
          (cond-var (first condition))
          (expr (first (rest condition))))
      `(let ((,tmp-var ,expr))
         (when ,tmp-var
           (let ((,cond-var ,tmp-var))
             ,@body))))
    (error "not implemented yet")))

;;;; end

(provide 'macro-util)

;; Local Variables:
;; End:

;; macro-util.el ends here
