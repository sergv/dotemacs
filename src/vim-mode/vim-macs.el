;; vim-macs.el - Basic macros for vim-mode. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defmacro vim:deflocalvar (name &rest args)
  "Defines a buffer-local variable."
  (declare (indent defun))
  `(progn
     (defvar ,name ,@args)
     (make-variable-buffer-local ',name)))

(defmacro* vim:defcmd (name (&rest args) &rest body)
  "Defines a new VIM-command.

Vim-mode commands are defined using the macro vim:defcmd, which has the following form.

  (vim:defcmd command-name ((count [count-name])
                            (motion [motion-name])
                            (register [register-name])
                            (argument[:{char,text,file,buffer,...}] [arg-name])
                            [nonrepeatable]
                            [keep-visual])
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

count: A numeric argument usually defining how many times a certain
       command should be repeated. If no explicit count has been
       given, the parameter has the value nil.

motion: If the command operates on a certain region, the region is
        usually defined by a motion and this argument should be
        specified. When the command is executed in normal-mode
        vim-mode will switch to operator-pending to wait for the
        motion to be specified. Afterwards the command is executed
        with this motion. Note that usually count is nil if the
        command takes a motion because the repeat count will be
        used by the motion. If this parameter is not present the
        command will not take a motion but will be executed without
        switching to operator-pending mode.

argument: Some commands take another argument besides the motion
          and the count. There are several types of arguments, the
          type is specified by appending a colon and the type-name
          after argument, i.e., argument:char defines an argument
          which is a single character, argument:text a general
          string, argument:file a file-path and argument:buffer a
          buffer-name. If no explicit type is given the argument
          type will be char. Note that the only allowed argument
          type for commands bound in another mode than ex-mode
          (using vim:emap or vim:local-emap) is char, i.e., when
          calling the command an additional character is read an
          passed to the function. An example for a command like
          this is the r command of Vim. All other argument types
          make only sense for ex-mode commands.

register: If specified the command can operator on a register. The
          name of the register, which is a character, is passed in
          this argument.

nonrepeatable: If specified the command cannot be repeated by the
               repeat command bound to '.' by default. This is
               usually the case for scrolling or window commands.

keep-visual: If specified the command does not end visual-mode when
             executed in visual-mode. This is usually the case for
             scrolling or window commands. Note that most editing
             commands do disable visual-mode.

As described above vim:defcmd can be used to define commands for
both normal-mode and ex-mode. Each command should place (point) at
the correct position after the operation.

In order to call a command from lisp-code, one has to use keyword
arguments, e.g.,

  (vim:cmd-delete-line :count 5)

deletes five lines. Note that the keyword used to call a commands
are always :count, :motion, :register or :argument no matter which
parameter names are used to define the command.

For more information about the vim:motion struct look at vim-core.el."
  (declare (indent defun))
  (let ((count nil)
        (register nil)
        (motion nil)
        (argument nil)
        (keep-visual nil)
        (repeatable t)
        (force nil)
        (params nil)
        (named-params nil)
        (doc nil))

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
           (error "%s: only one motion argument may be specified: %s" 'vim:defcmd arg))
         (setq motion t)
         (push 'motion params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'motion)))
           (push `(,(cadr arg) motion) named-params)))

        (`motion:optional
         (when motion
           (error "%s: only one motion argument may be specified: %s" 'vim:defcmd arg))
         (setq motion ''optional)
         (push 'motion params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'motion)))
           (push `(,(cadr arg) motion) named-params)))

        (`keep-visual (setq keep-visual t))
        (`do-not-keep-visual (setq keep-visual nil))
        (`repeatable (setq repeatable t))
        (`nonrepeatable (setq repeatable nil))
        (`force
         (when force
           (error "%s: only one force argument may be specified: %s" 'vim:defcmd arg))
         (setq force t)
         (push 'force params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'force)))
           (push `(,(cadr arg) force) named-params)))

        (_
         (let ((arg-name (symbol-name (if (consp arg) (car arg) arg))))
           (unless (string-match "^argument\\(?::\\([[:word:]]+\\)\\)?$" arg-name)
             (error "%s: Unexpected argument: %s" 'vim:defcmd arg))
           (when argument
             (error "%s: only one argument may be specified: %s" 'vim:defcmd arg))
           (let ((arg-type (if (match-beginning 1)
                             `',(intern (match-string 1 arg-name))
                             ''text)))
             (setq argument arg-type)
             (push 'argument params)
             (when (and (consp arg)
                        (not (eq (cadr arg) 'argument)))
               (push `(,(cadr arg) argument) named-params)))))))

    `(progn
       (put ',name 'type ',(if motion 'complex 'simple))
       (put ',name 'count ,count)
       (put ',name 'motion ,motion)
       (put ',name 'argument ,argument)
       (put ',name 'register ,register)
       (put ',name 'keep-visual ,keep-visual)
       (put ',name 'repeatable ,repeatable)
       (put ',name 'force ,force)
       (put ',name 'function
            (function*
             (lambda
               (,@(when params `(&key ,@params))
                ,@(when named-params `(&aux ,@named-params)))
               ,@body)))
       (defun* ,name (&rest args)
         ,doc
         (interactive)
         (if (and (vim:called-interactively-p)
                  ;; since in minibuffer vim-mode may be inactive but
                  ;; we may still want to execute the desired command
                  ;; also command may not have it's mock alternative,
                  ;; as for example vim:cmd-paste-before
                  (not (null? vim:active-command-function)))
           (funcall vim:active-command-function ',name)
           (apply (get ',name 'function) args))))))

(defmacro* vim:defmotion (name (&rest args) &rest body)
  "Vim-mode motions can be defined with the macro vim:defmotion.
Similar to commands motions have several keyword-like optional
parameters and a view attributes. The general form is as follows.

 (vim:defmotion motion-name ((count [count-name])
                             (argument [argument-name])
                             { inclusive
                             | exclusive
                             | linewise
                             | block
                             | do-not-adjust-point
                             }*)
    body ...)

The count and argument parameters are optional and can have a
special variable-name. Exactly one of the attributes inclusive,
exclusive, linewise, block must be specified.

count: The number of times the motion should be repeated.

argument: An extra character argument to be given after the motion
          command has been initiated. Examples are the motions f F
          t T of Vim.

inclusive, exclusive, linewise, block: This is the default motion
                                       type of this motion command.

do-not-adjust-point: if specified then after executing this motion
                     point located at the end of line would not
                     be adjusted, see also `vim:adjust-point'

Each motion command should return an object of type vim:motion \(see
below\). If the function does not return such an object explicitly,
it is automatically created implicitly based on the position of
(point) before and after execution of the motion and the specified
motion-type. This means when a motion is called from lisp code it
returns *always* a vim:motion object.

For more information about the vim:motion struct and motion types
look at vim-core.el."
  (declare (indent defun))
  (let ((type nil)
        (count nil)
        (argument nil)
        (params nil)
        (named-params nil)
        (doc nil)
        (do-not-adjust-point nil))

    ;; extract documentation string
    (if (and (consp body)
             (cdr body)
             (stringp (car body)))
      (setq doc (car body)
            body (cdr body))
      (setq doc (format "VIM - motion (%s %s)" name args)))

    ;; collect parameters
    (dolist (arg args)
      (pcase (if (cons? arg) (car arg) arg)
        ((or `inclusive `exclusive `linewise `block)
         (setq type arg))

        (`do-not-adjust-point
         (setf do-not-adjust-point t))

        (`count
         (setq count t)
         (push '(count nil) params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'count)))
           (push `(,(cadr arg) count) named-params)))

        ((or `argument `argument:char)
         (when argument
           (error "%s: only one argument may be specified: %s" 'vim:defcmd arg))
         (setq argument ''char)
         (push 'argument params)
         (when (and (consp arg)
                    (not (eq (cadr arg) 'argument)))
           (push `(,(cadr arg) argument) named-params)))

        (_ (error "%s: Unexpected argument: %s" 'vim:defmotion arg))))

    (unless type
      (error "%s: Motion type must be specified" 'vim:defmotion))

    `(progn
       (put ',name 'type ',type)
       (put ',name 'count ,count)
       (put ',name 'argument ,argument)
       (put ',name 'function
            (function* (lambda (,@(when params `(&key ,@params))
                                ,@(when named-params `(&aux ,@named-params)))
                         (vim:do-motion ',type (progn ,@body)))))
       (defun* ,name (&rest args)
         ,doc
         (interactive)
         (let ,(if do-not-adjust-point
                 '((*vim:do-not-adjust-point* t))
                 '())
           (if (vim:called-interactively-p)
             (vim:execute-command ',name)
             (apply (get ',name 'function) args)))))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '("vim:deflocalvar" "vim:defcmd" "vim:defmotion"))


(provide 'vim-macs)

;; Local Variables:
;; End:

;; vim-macs.el ends here
