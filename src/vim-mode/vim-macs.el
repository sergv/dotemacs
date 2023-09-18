;; vim-macs.el - Basic macros for vim-mode. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(cl-defmacro vim-defcmd (name (&rest args) &rest body)
  "Defines a new VIM-command.

Vim-mode commands are defined using the macro vim-defcmd, which has the following form.

  (vim-defcmd command-name ((count [count-name])
                            (motion [motion-name])
                            (register [register-name])
                            (argument[:{char,text,file,buffer,...}] [arg-name])
                            [nonrepeatable]
                            [noninteractive]
                            [keep-visual]
                            [unadjusted])
    body ...)

The first three arguments are keyword arguments similar to
defun*. The fourth and fifth argument are not real arguments,
i.e., they do not bind a value. Instead they define special
attributes of the command.

Each of the arguments is optional. An argument is either given as
two element list (parameter parameter-name) or without explicit
name just parameter. When no parameter name is specified the
name is the same as the parameter itself, i.e., the parameter
count defines the variable count whereas the parameter
(count cnt) defines the variable cnt.

The parameters have the following meanings.

count:
  A numeric argument usually defining how many times a certain
  command should be repeated. If no explicit count has been
  given, the parameter has the value nil.

motion:
  If the command operates on a certain region, the region is
  usually defined by a motion and this argument should be
  specified. When the command is executed in normal-mode vim-mode
  will switch to operator-pending to wait for the motion to be
  specified. Afterwards the command is executed with this motion.
  Note that usually count is nil if the command takes a motion
  because the repeat count will be used by the motion. If this
  parameter is not present the command will not take a motion but
  will be executed without switching to operator-pending mode.

argument:
  Some commands take another argument besides the motion and the
  count. There are several types of arguments, the type is
  specified by appending a colon and the type-name after
  argument, i.e., argument:char defines an argument which is a
  single character, argument:text a general string, argument:file
  a file-path and argument:buffer a buffer-name. If no explicit
  type is given the argument type will be char. Note that the
  only allowed argument type for commands bound in another mode
  than ex-mode (using vim-emap or vim-local-emap) is char,
  i.e., when calling the command an additional character is read
  an passed to the function. An example for a command like this
  is the r command of Vim. All other argument types make only
  sense for ex-mode commands.

register:
  If specified the command can operator on a register. The name
  of the register, which is a character, is passed in this
  argument.

nonrepeatable:
  If specified the command cannot be repeated by the repeat
  command bound to '.' by default. This is usually the case for
  scrolling or window commands.

noninteractive:
  If specified then NAME:interactive version of the command (i.e.
  the one with the interactive spec) will not be defined.

keep-visual:
  If specified the command does not end visual-mode when executed
  in visual-mode. This is usually the case for scrolling or
  window commands. Note that most editing commands do disable
  visual-mode.

unadjusted:
  Do not adjust point after interactive command finishes.

As described above vim-defcmd can be used to define commands for
both normal-mode and ex-mode. Each command should place (point) at
the correct position after the operation.

In order to call a command from lisp-code, one has to use keyword
arguments, e.g.,

  (vim:cmd-delete-line:wrapper :count 5)

deletes five lines. Note that the keyword used to call a commands
are always :count, :motion, :register or :argument no matter which
parameter names are used to define the command.

For more information about the vim:motion struct look at vim-core.el."
  (declare (indent defun) (doc-string 3))
  (cl-assert (symbolp name))
  (let* ((name-str (symbol->string name))
         (name-interactive (string->symbol (concat name-str ":interactive")))
         (name-wrapper (string->symbol (concat name-str ":wrapper")))
         (count nil)
         (register nil)
         (motion nil)
         (argument nil)
         (keep-visual nil)
         (repeatable t)
         (force nil)
         (params nil)
         (named-params nil)
         (doc nil)
         (interactive t)
         (unadjusted nil))

    ;; extract documentation string
    (if (and (consp body)
             (cdr body)
             (stringp (car body)))
        (setq doc (car body)
              body (cdr body))
      (setq doc (format "VIM - command (%s %s)" name args)))

    ;; collect parameters
    (dolist (arg args)
      (pcase (if (consp arg) (car arg) arg)
        (`count
         (setq count t)
         (push '(count nil) params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'count)))
           (push `(,(cadr arg) count) named-params)))

        (`register
         (setq register t)
         (push '(register nil) params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'register)))
           (push `(,(cadr arg) register) named-params)))

        (`motion
         (when motion
           (error "%s: only one motion argument may be specified: %s" 'vim-defcmd arg))
         (setq motion t)
         (push 'motion params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'motion)))
           (push `(,(cadr arg) motion) named-params)))

        (`motion:optional
         (when motion
           (error "%s: only one motion argument may be specified: %s" 'vim-defcmd arg))
         (setq motion 'optional)
         (push 'motion params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'motion)))
           (push `(,(cadr arg) motion) named-params)))

        (`unadjusted (setq unadjusted t))
        (`keep-visual (setq keep-visual t))
        (`do-not-keep-visual (setq keep-visual nil))
        (`repeatable (setq repeatable t))
        (`nonrepeatable (setq repeatable nil))
        (`noninteractive (setq interactive nil))
        (`force
         (when force
           (error "%s: only one force argument may be specified: %s" 'vim-defcmd arg))
         (setq force t)
         (push 'force params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'force)))
           (push `(,(cadr arg) force) named-params)))

        (_
         (let ((arg-name (symbol-name (if (consp arg) (car arg) arg))))
           (save-match-data
             (unless (string-match "^argument\\(?::\\([[:word:]]+\\)\\)?$" arg-name)
               (error "vim-defcmd: Unexpected argument: %s" arg))
             (when argument
               (error "vim-defcmd: only one argument may be specified: %s" arg))
             (let ((arg-type (if (match-beginning 1)
                                 (string->symbol (match-string-no-properties 1 arg-name))
                               'text)))
               (setq argument arg-type)
               (push 'argument params)
               (when (and (consp arg)
                          (not (eq (cadr arg) 'argument)))
                 (push `(,(cadr arg) argument) named-params))))))))

    (let* ((args `(,@(when params `(&key ,@params))
                   ;; ,@(when named-params `(&aux ,@named-params))
                   ))
           (has-args? (and args t))
           (worker-args
            ;; (argument force &optional motion count register)
            ;; (argument force motion &optional count register)

            (list (if motion
                      'motion
                    '_ignored-motion)
                  (if count
                      'count
                    '_ignored-count)
                  (if argument
                      'argument
                    '_ignored-argument)
                  (if force
                      'force
                    '_ignored-force)
                  (if register
                      'register
                    '_ignored-register))))
      `(progn
         (put ',name 'type ',(if motion 'complex 'simple))
         (put ',name 'count ,count)
         (put ',name 'motion ',motion)
         (put ',name 'argument ',argument)
         (put ',name 'register ,register)
         (put ',name 'keep-visual ,keep-visual)
         (put ',name 'repeatable ,repeatable)
         (put ',name 'force ,force)

         ;; This does all the work.
         (defun ,name (,@worker-args)
           ,doc
           (let (,@named-params)
             ,@body))

         ;; Name for use from elisp.
         (cl-defmacro ,name-wrapper (,@args)
           ,doc
           (list ',name
                 ,(when motion
                    'motion)
                 ,(when count
                    'count)
                 ,(when argument
                    'argument)
                 ,(when force
                    'force)
                 ,(when register
                    'register)))

         ,@(when interactive
             (list
              `(progn
                 (put ',name-interactive 'vim--is-cmd? t)
                 ;; Name to bind to keys.
                 (defun ,name-interactive ,(if has-args? '(&rest args) '())
                   ,(format "Interactive version of ‘%s’" name)
                   (interactive)
                   (let ,(if unadjusted
                             '((vim-do-not-adjust-point t))
                           '())
                     (if vim-active-mode
                         (vim-execute-command #',name)
                       ,(if has-args?
                            `(error ,(format "Command %s takes arguments and cannot be called outside vim mode" name))
                          ;; `(apply #',name args)
                          `(,name))))

                   ;; (if ;; Since in minibuffer vim-mode may be inactive but
                   ;;     ;; we may still want to execute the desired command.
                   ;;     ;; And command may not have a mock alternative,
                   ;;     ;; e.g. ‘vim:cmd-paste-before’.
                   ;;     vim-active-command-function
                   ;;     (vim-active-mode vim-active-command-function #',name)
                   ;;   (apply #',name args))
                   ))))))))

(cl-defmacro vim-defmotion (name (&rest args) &rest body)
  "Vim-mode motions can be defined with the macro vim-defmotion.
Similar to commands motions have several keyword-like optional
parameters and a view attributes. The general form is as follows.

 (vim-defmotion motion-name ((count [count-name])
                             (argument [argument-name])
                             { inclusive
                             | exclusive
                             | linewise
                             | block
                             | unadjusted
                             | motion-result
                             | raw-result
                             }*)
    body ...)

The count and argument parameters are optional and can have a
special variable-name. Exactly one of the attributes inclusive,
exclusive, linewise, block must be specified.

count:
  The number of times the motion should be repeated.

argument:
  An extra character argument to be given after the motion
  command has been initiated. Examples are the motions f F t T of
  Vim.

inclusive, exclusive, linewise, block:
  This is the default motion type of this motion command.

unadjusted:
  if specified then after executing this motion point located at
  the end of line would not be adjusted, see also
  `vim--adjust-point'

Each motion command should return an object of type vim:motion \(see
below\). If the function does not return such an object explicitly,
it is automatically created implicitly based on the position of
(point) before and after execution of the motion and the specified
motion-type. This means when a motion is called from lisp code it
returns *always* a vim:motion object.

For more information about the vim:motion struct and motion types
look at vim-core.el."
  (declare (indent defun) (doc-string 3))
  (let* ((name-str (symbol->string name))
         (name-interactive (string->symbol (concat name-str ":interactive")))a
         (name-raw (string->symbol (concat name-str ":raw")))
         (name-wrapper (string->symbol (concat name-str ":wrapper")))
         (type nil)
         (count nil)
         (argument nil)
         (params nil)
         (named-params nil)
         (doc nil)
         (unadjusted nil)
         (raw-result nil)
         (motion-result nil)
         (ret '#:ret))

    ;; extract documentation string
    (if (and (consp body)
             (cdr body)
             (stringp (car body)))
        (setq doc (car body)
              body (cdr body))
      (setq doc (format "VIM - motion (%s %s)" name args)))

    ;; collect parameters
    (dolist (arg args)
      (pcase (if (consp arg) (car arg) arg)
        ((or `inclusive `exclusive `linewise `block)
         (setq type arg))

        (`unadjusted
         (setf unadjusted t))

        (`raw-result
         (setf raw-result t))

        (`motion-result
         (setf motion-result t))

        (`count
         (setq count t)
         (push '(count nil) params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'count)))
           (push `(,(cadr arg) count) named-params)))

        ((or `argument `argument:char)
         (when argument
           (error "vim-defcmd: only one argument may be specified: %s" arg))
         (setq argument 'char)
         (push 'argument params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'argument)))
           (push `(,(cadr arg) argument) named-params)))

        (_ (error "vim-defmotion: Unexpected argument: %s" arg))))

    (unless type
      (error "vim-defmotion: Motion type must be specified"))

    (when (and raw-result
               motion-result)
      (error "Only one fo raw-result or motion-result may be specified"))

    (let ((worker-args
           (list '_ignored-motion
                 (if count
                     'count
                   '_ignored-count)
                 (if argument
                     'argument
                   '_ignored-argument)
                 '_ignored-force
                 '_ignored-register)))
      `(progn
         (put ',name 'type ',type)
         (put ',name 'count ,count)
         (put ',name 'argument ',argument)

         ,@(when raw-result
             (list
              `(defun ,name-raw (,@worker-args)
                 ,doc
                 (let (,@named-params)
                   ,@body))))

         (defun ,name (,@worker-args)
           ,doc
           ,(cond
              (raw-result
               `(vim-wrap-motion ,type
                  (,name-raw nil                        ;; motion
                             ,(when count 'count)       ;; count
                             ,(when argument 'argument) ;; arg
                             nil                        ;; force
                             nil                        ;; register
                             )))
              (motion-result
               `(let (,@named-params)
                  (let ((,ret (progn ,@body)))
                    (cl-assert (equal (vim-motion-type ,ret)
                                      ',type))
                    ,ret)))
              (t
               `(let (,@named-params)
                  (vim-do-motion ,type
                    ,@body)))))

         (cl-defmacro ,name-wrapper (,@(when params `(&key ,@params)))
           ,doc
           (list ',name
                 nil
                 ,(when count
                    'count)
                 ,(when argument
                    'argument)
                 nil
                 nil))

         (defun ,name-interactive ()
           ,(format "Interactive version of ‘%s’" name)
           (interactive)
           (let ,(if unadjusted
                     '((vim-do-not-adjust-point t))
                   '())
             (if vim-active-mode
                 (vim-execute-command #',name)
               (,(if raw-result
                     name-raw
                   name)
                nil
                nil
                nil
                nil
                nil))))))))

(provide 'vim-macs)

;; Local Variables:
;; End:

;; vim-macs.el ends here
