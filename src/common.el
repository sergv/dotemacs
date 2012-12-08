;; common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  5 November 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'custom)
(require 'custom-predicates)
(require 'more-scheme)

(defalias 'defconstant 'defconst)

(defvar *ex-commands-re-cache* nil
  "This variable contains optimized regexp that matches
currently defined ex commands. Should be updated with
`ex-commands-re-cache-update' when new ex commands being defined.")


(defun ex-commands-re-cache-update ()
  "Updates `*ex-commands-re-cache*' with current ex-commands."
  (setf *ex-commands-re-cache*
        (concat "\\("
                (regexp-opt (mapcar #'car vim:ex-commands))
                "\\)\\(!\\)?")))



(defsubst remap-interval (a b c d x)
  "Remap x from [a, b] into [c, d]"
  (+ c
     (* (- x a)
        (/ (coerce (- d c) 'float)
           (coerce (- b a) 'float)))))

(defun make-random-gen (init)
  (let ((a 0)
        (b #x7fff)
        (seed init))
    #'(lambda (begin end)
        (setf seed (logand (1+ (* seed 69069))
                           b))
        (remap-interval a b begin end seed))))


;; yeilds values in range [0..1)
(defun make-tausworthe-random-gen (seed1 seed2 seed3)
  (let ((2-to-32 (expt 2 32)))
    (let ((tausworthe
            #'(lambda (s a b c d)
                (mod (logxor (ash (logand s c) (- d))
                             (ash (logxor s
                                          (ash s (- a)))
                                  b))
                     2-to-32)))
          (a seed1)
          (b seed2)
          (c seed3))
      #'(lambda ()
          (setf a (funcall tausworthe
                           a
                           13
                           19
                           4294967294
                           12)
                b (funcall tausworthe
                           b
                           2
                           25
                           4294967288
                           4)
                c (funcall tausworthe
                           c
                           3
                           11
                           4294967280
                           17))
          (/ (logxor a b c) (float 2-to-32))))))

(defun make-tausworthe-random-generator ()
  "Return tausworthe random generator obtained
by seeding `make-tausworthe-random-gen' with
current time and"
  (multiple-value-bind (a b microsec)
      (current-time)
    (let ((c (+ (* 65536 a) b)))
      (make-tausworthe-random-gen
       (+ (min c microsec) 2)
       (+ (emacs-pid) 8)
       (+ (max c microsec) 16)))))

(defvar *tausworthe-random-gen* (make-tausworthe-random-generator)
  "Global random generator")

(defun random-shuffle (vect random-gen)
  "Randoly shuffle vector VECT inplace with supply
of random numbers from RANDOM-GEN."
  (typep vect 'vector)
  (loop
    for i downfrom (1- (length vect)) to 1
    ;; may yield i
    for j = (round (* i (funcall random-gen)))
    do (psetf (aref vect i) (aref vect j)
              (aref vect j) (aref vect i)))
  vect)

(defun shuffle-lines (begin end)
  (interactive "r")
  (save-excursion
   (goto-char begin)
   (setf begin (line-beginning-position))
   (goto-char end)
   (setf end (line-end-position))
   (let ((lines (list->vector
                 (split-string (buffer-substring-no-properties begin end)
                               "\n"
                               t))))

     (delete-region begin end)
     (when (= (char-after) ?\n)
       (delete-char 1))
     (random-shuffle lines *tausworthe-random-gen*)
     (goto-char begin)
     (loop
       for line across lines
       do  (insert line)
           (insert "\n")))))


;;;; file utilities

(defun strip-trailing-slash (path)
  (if (char= ?\/ (aref path (1- (length path))))
    (subseq path 0 -1)
    path))


(defun path-concat (&rest args)
  "Connect paths with standard delimiter"
  (mapconcat #'strip-trailing-slash args "/"))

(defalias 'paths-concat 'path-concat)



(defun* get-directory-contents (dir &key (full t))
  (remove-if #'(lambda (x) (member (file-name-nondirectory x) '("." "..")))
             (directory-files dir full)))

(defun version-control-directory? (filename)
  (string-match-pure? (rx (or "SCCS"
                              "RCS"
                              "CVS"
                              "MCVS"
                              ".svn"
                              ".git"
                              ".hg"
                              ".bzr"
                              "_MTN"
                              "_darcs"
                              "{arch}")
                          (? "/"))
                      filename))

(defun* find-rec (path
                  &key
                  (filep #'(lambda (p) t))
                  (dirp  #'(lambda (p) nil))
                  (do-not-visitp
                      (lambda (p)
                        (version-control-directory?
                         (file-name-nondirectory p)))))
  "Collect files and/or directories under PATH recursively.

Collect files and directories which satisfy FILEP and
DIRP respectively in directories which don't satisfy DO-NOT-VISITP.
By default, version-control specific directories are omitted, e.g. .git etc.

All predicates are called with full absolute paths."
  (when (stringp filep)
    (setf filep
          (let ((regexp-local filep))
            #'(lambda (p) (string-match-p regexp-local p)))))
  (when (stringp dirp)
    (setf dirp
          (let ((regexp-local dirp))
            #'(lambda (p) (string-match-p regexp-local p)))))
  (when (stringp do-not-visitp)
    (setf do-not-visitp
          (let ((regexp-local do-not-visitp))
            #'(lambda (p) (string-match-p regexp-local p)))))

  (letrec ((collect-rec
             (lambda (path accum)
               (cond
                 ((and (file-directory-p path)
                       (not (funcall do-not-visitp path)))
                  (reduce #'(lambda (acc p)
                              (funcall collect-rec p acc))
                          (get-directory-contents path :full t)
                          :initial-value (if (funcall dirp path)
                                           (cons path accum)
                                           accum)))
                 ((funcall filep path)
                  (cons path accum))
                 (t
                  accum)))))
    (nreverse (funcall collect-rec path nil))))

(defun read-and-insert-filename ()
  "Read filename with completion from user and insert it at point.
Of course directory names are also supported."
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (y-or-n-p "Insert link? "))
    (insert "[[file:"
            (expand-file-name (read-file-name "" nil ""))
            "][]]")
    (insert (expand-file-name (read-file-name "" nil "")))))

(defun find-filename-in-tree-recursive ()
  "Read filename regexp and try to find it in current dir's tree or in trees
obtained by following upward in filesystem"
  (interactive)
  (let* ((filename-re (read-string "filename regexp: " ""))
         (path (reverse (split-string (file-name-directory
                                       (buffer-file-name (current-buffer)))
                                      "/"
                                      t)))
         (subdirs-visited '())
         (found? nil)
         (files nil) ;; found files
         )
    (letrec ((path-join (lambda (path)
                          (concat "/" (mapconcat #'identity (reverse path) "/")))))
      (while (and (not found?)
                  (not (null? path)))
        (let ((subdir (funcall path-join path)))
          (message "searching in %s" subdir)
          (setf files
                (find-rec subdir
                          :filep
                          (lambda (p)
                            (string-match-pure? filename-re
                                                (file-name-nondirectory p)))
                          :do-not-visitp
                          (lambda (p)
                            (or (version-control-directory?
                                 (file-name-nondirectory p))
                                (any? (lambda (subdir)
                                        (string-prefix? subdir p))
                                      subdirs-visited)))))
          (when (not (null? files))
            (setf found? t))
          (push subdir subdirs-visited)
          (setf path (cdr path))))
      (if found?
        (progn
          (assert (not (null? files)))
          (if (= 1 (length files))
            (find-file (car files))
            (select-start-selection files
                                    :on-selection
                                    (lambda (idx)
                                      (select-exit)
                                      (find-file (nth idx files)))
                                    :predisplay-function
                                    (lambda (x)
                                      (concat "file: " (file-name-nondirectory x) "\n"
                                              x "\n"))
                                    :preamble-function
                                    (lambda ()
                                      "Multiple files found\n\n")
                                    :separator-function
                                    (lambda () ""))))
        (error "No file found for \"%s\" regexp" filename-re)))))

;;;;

(defmacro rxx (definitions &rest main-expr)
  "Return `rx' invokation of main-expr that has symbols defined in
DEFINITIONS substituted by definition body. DEFINITIONS is list
of the form of let-bindigs, (symbol body). No recursion is permitted -
no defined symbol should show up in body of its definition or in
body of any futher definition."
  (aif (find-if (lambda (def) (not (= 2 (length def)))) definitions)
    (error "rxx: every definition should consist of two elements: (name def), offending definition: %s"
           it))
  `(rx ,@(reduce (lambda (def expr)
                   (subst (cadr def) (car def) expr
                          :test #'eq))
                 definitions
                 :initial-value main-expr
                 :from-end t)))


(defun completing-read-vanilla (prompt
                                collection
                                &optional predicate
                                          require-match
                                          initial-input
                                          hist-m@%=!$+&^*z
                                          def
                                          inherit-input-method)
  "Perform `completing-read' of icicles using vanilla completion."
  (let ((icicle-TAB-completion-methods '(vanilla))
        (icicle-hide-common-match-in-Completions-flag nil))
    (completing-read prompt
                     collection
                     predicate
                     require-match
                     initial-input
                     hist-m@%=!$+&^*z
                     def
                     inherit-input-method)))


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
                             (call-n-times nil))
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
    `(vim:defcmd ,action-name (count)
       ,(if doc doc (format "See `%s'." func-name))
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
  (let ((new-name (intern (concat (symbol-name func) "+"))))
    `(prog1 nil
       (defun ,new-name ,args
         ,@body)
       (fset ',func ',new-name))))

(put 'redefun 'doc-string-elt 3)
(font-lock-add-keywords 'emacs-lisp-mode '("redefun"))



(defmacro define-circular-jumps (forward-name
                                 backward-name
                                 regex
                                 &optional
                                 init)
  "Define two functions, FORWARD-NAME and BACKWARD-NAME to perform
jumps on REGEX with wraparound in buffer. INIT form will be executed
before performing any jumps."
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
jumps on text property PROPERTY with value VALUE with wraparound in
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




(defun emacs-forget-buffer-process ()
  "Emacs will not query about this process when killing."
  (let ((p (get-buffer-process (current-buffer))))
    (when p
      (set-process-query-on-exit-flag p nil))))

(defun* use-repl-modeline (&key (show-column t) (show-directory nil))
  "Set up `mode-line' for use in vairous repl."
  (setf mode-line-format
        `(" %[%b%] "
          ("(%m"
           mode-line-process
           ")")
          (:eval
           (when (buffer-narrowed?)
             "(Narrowed)"))
          " "
          (line-number-mode
           ("%l/"
            (:eval (number-to-string
                    (count-lines (point-min)
                                 (point-max))))
            "(%p)"))
          (column-number-mode
           (2 " %c"))
          ,@(when show-directory
              '("  "
                (:eval default-directory))))))

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
     (make-variable-list (lambda (list)
                           (mapconcat #'identity list "\n")))
     (use-upcase t)
     (format-print-value "~a")
     (format-string-start "\"")
     (format-string-end "~%\"")
     (insert-newline-before-var-list t))
  "Now, here's quite complicated stuff but very general too. This macro can
solve most debug print problems.

I want to stress one point here: this macro does not and would never ever
support debug printing for C++ using cout et al."
  `(define-skeleton ,name
     ,doc
     nil
     ;; store start of skeleton
     '(setq
       v1 (point)
       ;; car - whether any messages were inserted and if so
       ;; then becomes type of previously inserted message -
       ;; either 'message or 'variable
       ;; cdr - list of variable names to print
       v2 (cons nil nil))
     ,print-begin
     ,format-string-start
     ;; get name of function containing point
     ;; this form evaluates to string which would be function name
     (funcall #',insert-entity-name-procedure v1)
     ((let* ((x (read-string "Variable or message starting with space: "))
             (message-p (and (< 0 (length x))
                             (char= ?\s (aref x 0))))
             (result nil))
        (if (< 0 (length x))
          (progn
            ;; so, if str starts with a space thes it's hardly a variable name or
            ;; some other form with value. Therefore it's a message to print,
            ;; without associated value
            (if message-p
              (setf result
                    ,(if use-upcase
                       '(upcase (replace-regexp-in-string "^[ \t]+" "" x))
                       '(replace-regexp-in-string "^[ \t]+" "" x)))
              (progn
                (push x (cdr v2))
                (setf result
                      (concat ,(if use-upcase '(upcase x) 'x) ": " ,format-print-value))))
            ;; if anything was inserted previously then prepend
            ;; ", " or "; " to result
            (when (car v2)
              (setf result (concat (case (car v2)
                                     (message
                                      (if message-p
                                        ", "
                                        "; "))
                                     (variable
                                      (if message-p
                                        "; "
                                        ", ")))
                                   result)))
            (setf (car v2) (if message-p 'message 'variable))
            ;; made up format chunk, return in so that str reference can use it
            result)
          ;; done entering values - communicate that to skeleton mode
          ""))
      ;; reference to just prompted message/variable name, would be immediately
      ;; inserted by skeleton into buffer
      str)
     ,format-string-end
     ,(when insert-newline-before-var-list
        ;; print \n only if there are any variable names
        '(when (cdr v2) '\n))
     (funcall #',make-variable-list
              (nreverse
               (remove-if (lambda (str)
                            (= 0 (length str)))
                          (cdr v2))))
     ,print-end
     ,(when indent-after-func
        `(save-excursion
          (goto-char v1)
          (funcall #',indent-after-func)))))


(defun re-group-matchedp (n)
  "Return non-nil if Nth group matched."
  (match-beginning n))

(defun re-group-matched? (n)
  "Return non-nil if Nth group matched."
  (match-beginning n))


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


(defun constantly (x)
  (let ((tmp x))
    (lambda (&rest y)
      (declare (ignore y))
      tmp)))


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
        (align-re (concat "\\(?:"
                          align-str
                          "\\)")))
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

(redefun read-passwd (prompt &optional confirm default)
  "Read a password, prompting with PROMPT, and return it.
If optional CONFIRM is non-nil, read the password twice to make sure.
Optional DEFAULT is a default password to use instead of empty input.

This function echoes `.' for each character that the user types.

The user ends with RET, LFD, or ESC.  DEL or C-h rubs out.
C-y yanks the current kill.  C-u kills line.
C-g quits; if `inhibit-quit' was non-nil around this function,
then it returns nil if the user types C-g, but `quit-flag' remains set.

Once the caller uses the password, it can erase the password
by doing (clear-string STRING)."
  (with-local-quit
      (if confirm
        (let (success)
          (while (not success)
            (let ((first (read-passwd prompt nil default))
                  (second (read-passwd "Confirm password: " nil default)))
              (if (equal first second)
                (progn
                  (and (arrayp second) (clear-string second))
                  (setq success first))
                (and (arrayp first) (clear-string first))
                (and (arrayp second) (clear-string second))
                (message "Password not repeated accurately; please start over")
                (sit-for 1))))
          success)
        (let ((pass nil)
              ;; Copy it so that add-text-properties won't modify
              ;; the object that was passed in by the caller.
              (prompt (copy-sequence prompt))
              (c 0)
              (echo-keystrokes 0)
              (cursor-in-echo-area t)
              (message-log-max nil)
              (stop-keys (list 'return ?\r ?\n ?\e))
              (rubout-keys (list 'backspace ?\b ?\177)))
          (add-text-properties 0 (length prompt)
                               minibuffer-prompt-properties prompt)
          (while (progn (message "%s%s"
                                 prompt
                                 (make-string (length pass) ?.))
                        (setq c (read-key))
                        (not (memq c stop-keys)))
            (clear-this-command-keys)
            (cond ((memq c rubout-keys) ; rubout
                   (when (> (length pass) 0)
                     (let ((new-pass (substring pass 0 -1)))
                       (and (arrayp pass) (clear-string pass))
                       (setq pass new-pass))))
                  ((eq c ?\C-g) (keyboard-quit))
                  ((not (numberp c)))
                  ((= c ?\C-u) ; kill line
                   (and (arrayp pass) (clear-string pass))
                   (setq pass ""))
                  ((or (= c ?\C-y)
                       (= c ?\M-y)
                       (= c ?\C-p)
                       (= c ?\M-p)) ; yank
                   (let* ((str (condition-case nil
                                   (current-kill 0)
                                 (error nil)))
                          new-pass)
                     (when str
                       (setq new-pass
                             (concat pass
                                     (substring-no-properties str)))
                       (and (arrayp pass) (clear-string pass))
                       (setq c ?\0)
                       (setq pass new-pass))))
                  ((characterp c) ; insert char
                   (let* ((new-char (char-to-string c))
                          (new-pass (concat pass new-char)))
                     (and (arrayp pass) (clear-string pass))
                     (clear-string new-char)
                     (setq c ?\0)
                     (setq pass new-pass)))))
          (message nil)
          (or pass default "")))))

;; This function would not define make-NAME if :consturctor argument was supplied
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
                      (mapcar (function (lambda (x) (if (consp x) x (list x))))
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
                     include-descs (mapcar (function
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
        (if (cl-safe-expr-p (cons 'progn (mapcar 'second descs)))
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
                 (mapcar (function (lambda (x)
                           (list 'put (list 'quote (car x))
                                 '(quote side-effect-free)
                                 (list 'quote (cdr x)))))
                         side-eff))
          forms)
    (cons 'progn (nreverse (cons (list 'quote name) forms)))))


;;;; some ring functions

(defun* ring-delete-all (item ring &key (test #'equal))
  "Remove all elements of RING for which TEST invoked with ITEM returns t."
  ;; elements should be removed starting from the end
  (dotimes (i (ring-length ring))
    (let ((j (- (ring-length ring) (1+ i))))
      (when (funcall test item (ring-ref ring j))
        (ring-remove ring j)))))

(defun* ring-member (item ring &key (test #'equal))
  (let ((i 0)
        (member nil))
    (while (and (< i (ring-length ring))
                (not member))
      (setf member (funcall test
                            item
                            (ring-ref ring i)))
      (incf i))
    member))


(defmacro with-current-frame (frame &rest body)
  (let ((selected (gensym)))
    `(let ((,selected ,frame))
       (select-frame ,frame)
       (unwind-protect
            (progn
              ,@body)
         (select-frame ,selected)))))



;;;;;

(defun factorial (x)
  (if (= 0 x)
    1
    (* x (factorial (1- x)))))

(defun choose (n k)
  "Compute binomial coefficient."
  (/ (factorial n) (* (factorial k)
                      (factorial (- n k)))))

(defun permutations (list)
  (if (null list)
    (list nil)
    (mapcan (lambda (item)
              (mapcar (lambda (x)
                        (cons item x))
                      (permutations (remove item list))))
            list)))


(defun combinations (n k)
  "Construct all
 / n \\
 \\ k /
combinations"
  (letrec ((collect
             (lambda (start end)
               (if (< start 0)
                 (list ())
                 (loop
                   for i from start to end
                   nconcing
                      (mapcar (lambda (rest)
                                (cons i rest))
                              (funcall collect (1- start) (1- i))))))))
    (funcall collect (1- k) (1- n))))


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


(defun split-at (n list)
  "Return two elements: list of first N elements of LIST and
LIST sans it's first N elements."
  (cond
    ((null list)
     (values nil nil))
    ((= n 0)
     (values nil list))
    (t
     (bind (((head tail) (split-at (1- n) (cdr list))))
       (values (cons (car list) head) tail)))))


(defun* sum (seq &key (key #'identity) (start 0) (end nil))
  (reduce #'+
          seq
          :initial-value 0
          :key key
          :start start
          :end end))


;;;;

(defun point-inside-string-or-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
   (let* ((end (point))
          (begin (line-beginning-position)))
     (when begin
       (let ((state (parse-partial-sexp begin end)))
         (or (elt state 3)
             (elt state 4)))))))


;;;;

(defun assoc-value (key alist)
  (cadr (assoc key alist)))

(defun keyword-arglist-get-value (key keyword-arglist &optional default)
  "Get value of keyword argument named KEY from KEYWORD-ARGLIST, with
structure like this (:arg1 value1 :arg2 value2 ... :argN valueN)"
  (cond
    ((null? keyword-arglist)
     default)
    ((eq? key (car keyword-arglist))
     (cadr keyword-arglist))
    (t
     (keyword-arglist-get-value key
                                (cddr keyword-arglist)
                                default))))

;;;;

(defun string-trim-whitespace (str)
  "Trim whitespaces from string"
  (flet ((find-bound (&optional from-end)
           (position-if-not (lambda (char) (member* char (string->list " \t\n\r\v\f")
                                                    :test #'char=))
                            str :from-end from-end)))
    (let ((start (find-bound))
          (end (find-bound t)))
      (if start
        (subseq str start (1+ end))
        ""))))

;;;;

(defmacro with-disabled-undo (&rest body)
  (let ((store (gensym)))
    `(let ((,store buffer-undo-list)
           ;; this disables further undo recording
           (buffer-undo-list t))
       ,@body)))

(defmacro with-preserved-buffer-modified-p (&rest body)
  "Execute BODY and restore `buffer-modified-p' flag after its done."
  (let ((store (gensym)))
    `(let ((,store (buffer-modified-p)))
       (unwind-protect
            (begin
              ,@body)
         (set-buffer-modified-p ,store)))))

(defmacro with-inhibited-modification-hooks (&rest body)
  "Execute BODY and restore `inhibit-modification-hooks' after its done."
  `(let ((inhibit-modification-hooks t))
     ,@body))

(defmacro with-inhibited-read-only (&rest body)
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro with-inhibited-redisplay (&rest body)
  `(let ((inhibit-redisplay t))
     ,@body))


;;;;

(defun extract-unicode ()
  ;; note - every single bit of this function is made to let this function
  ;; work as fast as it can an as large buffers as possible
  ;; (on 2k lines performance is acceptable)
  (let ((filter*
          (lambda (pred items)
            (let ((result nil))
              (dolist (item items)
                (if (funcall pred item)
                  (push item result)))
              result))))
    (let ((chars (string->list (buffer-substring-no-properties (point-min)
                                                               (point-max)))))
      (sort (remove-duplicates (funcall filter* (lambda (c) (< 127 c)) chars)) '<))))

(defun input-unicode ()
  (interactive)
  (let* ((symbs (mapcar 'char->string (extract-unicode)))
         (symb (completing-read-vanilla "> " symbs)))
    (remove-text-properties 0 (length symb) '(font-lock-face nil) symb)
    (insert symb)))

;;;;

(defmacro aif (condition if-branch &optional else-branch)
  "Anaphoric if, binds evaluated condition to variable it."
  `(let ((it ,condition))
     (if it
       ,if-branch
       ,else-branch)))

;;;;

(defvar *invisible-buffers* '()
  "List of buffer name regexps than should not be visible in e.g. ibuffer,
tabbar, etc")

(defun add-invisible-buffer (buf-re)
  (assert (string? buf-re))
  (add-to-list '*invisible-buffers* buf-re))

(defun invisible-buffer? (buf)
  "Returns t if buffer BUF should be regarded as invisible, see also
`*invisible-buffers*'."
  (cond ((string? buf)
         (any? (lambda (re)
                 (string-match-pure? re buf))
               *invisible-buffers*))
        ((buffer? buf)
         (any? (lambda (re)
                 (string-match-pure? re (buffer-name buf)))
               *invisible-buffers*))
        (else
         (error "wrong argument type - not a string nor a buffer: %s"
                buf))))

(add-invisible-buffer "^\\*Completions\\*$")
(add-invisible-buffer "^#.+#$")
(add-invisible-buffer "^\\*Ibuffer\\*$")

;;;;

(defun for-buffers-with-mode (mode func)
  (for-each func
            (filter (lambda (buf)
                      (with-current-buffer buf
                        (eq? major-mode mode)))
                    (buffer-list))))

;;;;

(defun hash-table->alist (table)
  "Translate hash table into alist of (<key> . <value>) pairs."
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result)) table)
    result))

;;;;

;; this may be useful for something
;;
;; (defun make-ntree-node (value children)
;;   (cons 'ntree-node
;;         (cons value children)))
;;
;; (defun ntree-node? (item)
;;   (and (list? item) (not (null? item)) (eq? (car item) 'ntree-node)))
;;
;; (defun ntree-node-value (node)
;;   (assert (ntree-node? node))
;;   (cadr node))
;;
;; (defun ntree-node-children (node)
;;   (assert (ntree-node? node))
;;   (cddr node))
;;
;;
;; (defun print-ntree (node)
;;   (assert (ntree-node? node))
;;   (letrec ((print-node
;;              (lambda (node prefix)
;;                (insert prefix
;;                        (format "%s" (ntree-node-value node))
;;                        "\n")))
;;            (print-rec
;;              (lambda (node offset line-beg)
;;                (let* ((children (ntree-node-children node))
;;                       (last (car-safe (last children)))
;;                       (offset-delta 2)
;;                       (children-offset (+ offset offset-delta))
;;                       (children-line-beg
;;                         (concat line-beg
;;                                 "|"
;;                                 (make-string offset-delta ?\s))))
;;                  (mapc (lambda (child)
;;                          (funcall print-node child (concat line-beg "+--"))
;;                          (funcall print-rec
;;                                   child
;;                                   children-offset
;;                                   children-line-beg))
;;                        (butlast children))
;;                  (when last
;;                    (funcall print-node last (concat line-beg "`--"))
;;                    (funcall print-rec
;;                             last
;;                             children-offset
;;                             children-line-beg))))))
;;     (funcall print-node node "/")
;;     (funcall print-rec node 0 "")))
;;
;;
;; (print-ntree (make-ntree-node 'root
;;                               (list (make-ntree-node 'lvl1-1
;;                                                      (list (make-ntree-node 'lvl2-1 '())
;;                                                            (make-ntree-node 'lvl2-2 '())
;;                                                            (make-ntree-node 'lvl2-3 '())))
;;                                     (make-ntree-node 'lvl1-2 '())
;;                                     (make-ntree-node 'lvl1-3 '()))))
;;

;;;;

(defun bisect (item items start end eq? less?)
  "Binary search. Returns index into vector ITEMS.
LESS? is predicate on elements of ITEMS."
  (let ((orig-start start)
        (orig-end end))
    (while (< start end)
      (let* ((mid (/ (+ end start) 2))
             (mid-item (aref items mid)))
        (cond ((funcall less? item mid-item)
               (setf end mid))
              ((funcall eq? item mid-item)
               (setf start mid
                     end mid))
              (t
               (setf start (+ mid 1))))))
    (if (and (<= orig-start start)
             (< start orig-end))
      start
      nil)))

;;;;

(defun string-suffix? (string1 string2 &optional ignore-case)
  "Return t if STRING1 is a suffix of STRING2."
  (and (<= (length string1) (length string2))
       (eq t (compare-strings string1 0 nil
                              string2 (- (length string2) (length string1)) nil
                              ignore-case))))

;;;;

(defun merge-emacs-configs (new-config-dir curr-config-dir)
  "Merge changes from NEW-CONFIG-DIR into CURR-CONFIG-DIR by successively calling
ediff on files that differ and saving files in CURR-CONFIG-DIR that were updated
while executing ediff.

Use like this to pick changes that will go into CURR-CONFIG-DIR:
\(merge-emacs-configs \"/home/sergey/emacs.new\" \"/home/sergey/emacs\"\)
"
  (dolist (p (mapcar (lambda (p)
                       (file-relative-name p new-config-dir))
                     (find-rec new-config-dir
                               :filep
                               (lambda (p)
                                 (let ((fname (file-name-nondirectory p)))
                                   (and (string-match-pure? ".*\\.el$"
                                                            fname)
                                        ;; emacs locks?
                                        (not (string-match-pure? "^\\.#.*"
                                                                 fname))))))))
    (let* ((new      (concat new-config-dir "/" p))
           (new-buf  (find-file-noselect new))
           (curr     (concat curr-config-dir "/" p))
           (curr-buf (find-file-noselect curr)))
      (message "Files %s and %s" new curr)
      (assert (file-exist? new))
      (if (file-exist? curr)
        (if (string=?
             (with-current-buffer new-buf
               (buffer-substring-no-properties (point-min) (point-max)))
             (with-current-buffer curr-buf
               (buffer-substring-no-properties (point-min) (point-max))))
          (progn
            (message "Files %s and %s are the same, skipping" new curr)
            (kill-buffer new-buf)
            (kill-buffer curr-buf))
          (progn
            (ediff-diff-files-recursive-edit new curr :read-only nil)
            (kill-buffer new-buf)
            (with-current-buffer curr-buf
              (save-buffer))
            (redisplay t)))
        (when (y-or-n? (format "Copy %s to %s?" new curr))
          (copy-file new curr nil t t t))))))

;;;;


(provide 'common)

;; Local Variables:
;; End:


;; common.el ends here
