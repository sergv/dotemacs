;;; vim-node.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.2.0
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"


(provide 'vim-vim)

(vim:deflocalvar vim:repeat-events nil
  "The sequence of events for the repeat command.")

(vim:deflocalvar vim:current-register nil
  "The register of the current command.")

(vim:deflocalvar vim:current-cmd-count nil
  "The count of the current command.")

(vim:deflocalvar vim:current-cmd nil
  "The node of the current command.")

(vim:deflocalvar vim:current-cmd-arg nil
  "The argument of the current command.")

(vim:deflocalvar vim:current-motion-count nil
  "The count of the current motion.")

(vim:deflocalvar vim:current-motion nil
  "The node of the current motion.")

(vim:deflocalvar vim:current-motion-arg nil
  "The argument of the current motion.")

(vim:deflocalvar vim:current-motion-type nil
  "The type of the current motion (inclusive, exclusive, linewise).")

(defun vim:toplevel-execution ()
  "Returns t iff this is a toplevel execution, not a mapping or repeat."
  (not executing-kbd-macro))


(defadvice vim:reset-key-state (before vim:vim-reset-key-state)
  "Resets the current state of the keymap."
  (setq vim:current-register nil
        vim:current-cmd-count nil
        vim:current-cmd nil
        vim:current-cmd-arg nil
        vim:current-motion-count nil
        vim:current-motion nil
        vim:current-motion-arg nil
        vim:current-motion-type nil))
(ad-activate 'vim:reset-key-state)


(defmacro* vim:defcmd (name (&rest args) &rest body)
  "Defines a new VIM-command."
  (let ((count nil)
        (motion nil)
        (argument nil)
        (keep-visual nil)
        (repeatable t)
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
      (case (if (consp arg) (car arg) arg)
        ('count (setq count t)
                (push '(count nil) params)
                (when (and (consp arg)
                           (not (eq (cadr arg) 'count)))
                  (push `(,(cadr arg) count) named-params)))

        ('motion (setq motion t)
                 (push 'motion params)
                (when (and (consp arg)
                           (not (eq (cadr arg) 'motion)))
                  (push `(,(cadr arg) motion) named-params)))
        
        ('argument (setq argument t)
                   (push 'argument params)
                   (when (and (consp arg)
                              (not (eq (cadr arg) 'argument)))
                     (push `(,(cadr arg) argument) named-params)))

        ('keep-visual (setq keep-visual t))
        ('do-not-keep-visual (setq keep-visual nil))
        ('repeatable (setq repeatable t))
        ('nonrepeatable (setq repeatable nil))
        
        (t (error "%s: Unexpected argument: %s" 'vim:defcmd arg))))

    `(progn
       (put 'type ',name ',(if motion 'complex 'simple))
       (put 'count ',name ,count)
       (put 'motion ',name ,motion)
       (put 'argument ',name ,argument)
       (put 'keep-visual ',name ,keep-visual)
       (put 'repeatable ',name ,repeatable)
       (put 'function ',name
            (function* (lambda (,@(when params `(&key ,@params))
                                ,@(when named-params `(&aux ,@named-params)))
                         ,@body)))
       (defun* ,name (&rest args)
         ,doc
         (interactive)
         (if (called-interactively-p)
             (funcall vim:active-command-function ',name)
           (apply (get 'function ',name) args))))))


(defmacro* vim:defmotion (name (&rest args) &rest body)
  (let ((type nil)
        (count nil)
        (argument nil)
        (params nil)
        (named-params nil)
        (doc nil))

    ;; extract documentation string
    (if (and (consp body)
               (cdr body)
               (stringp (car body)))
        (setq doc (car body)
              body (cdr body))
      (setq doc (format "VIM - motion (%s %s)" name args)))
    
    ;; collect parameters
    (dolist (arg args)
      (case (if (consp arg) (car arg) arg)
        ((inclusive exclusive linewise block)
         (setq type arg))
        
        ('count (setq count t)
                (push '(count nil) params)
                (when (and (consp arg)
                           (not (eq (cadr arg) 'count)))
                  (push `(,(cadr arg) count) named-params)))
        
        ('argument (setq argument t)
                   (push 'argument params)
                   (when (and (consp arg)
                              (not (eq (cadr arg) 'argument)))
                     (push `(,(cadr arg) argument) named-params)))
        
        (t (error "%s: Unexpected argument: %s" 'vim:defmotion arg))))

    (unless type
      (error "%s: Motion type must be specified" 'vim:defmotion))

    `(progn
       (put 'type ',name ',type)
       (put 'count ',name ,count)
       (put 'argument ',name ,argument)
       (put 'function ',name
            (function* (lambda (,@(when params `(&key ,@params))
                                ,@(when named-params `(&aux ,@named-params)))
                         ,@body)))
       (defun* ,name (&rest args)
         ,doc
         (interactive)
         (if (called-interactively-p)
             (funcall vim:active-command-function ',name)
           (apply (get 'function ',name) args))))))


;;(defmacro* vim:defspecial (name (param) &body body)
;;  `(progn
;;     (put 'type ',name 'special)
;;     (defun ,name (,param) ,@body)))


(defun vim:cmd-count-p (cmd)
  "Returns non-nil iff command cmd takes a count."
  (get 'count cmd))

(defun vim:cmd-motion-p (cmd)
  "Returns non-nil iff command `cmd' takes a motion parameter."
  (get 'motion cmd))

(defun vim:cmd-arg-p (cmd)
  "Returns non-nil iff command cmd takes an argument."
  (get 'argument cmd))
  
(defun vim:cmd-repeatable-p (cmd)
  "Returns non-nil iff command cmd is repeatable."
  (get 'repeatable cmd))

(defun vim:cmd-keep-visual-p (cmd)
  "Returns non-nil iff command cmd should stay in visual mode."
  (get 'keep-visual cmd))
  
(defun vim:cmd-type (cmd)
  "Returns the type of command cmd."
  (get 'type cmd))

(defun vim:cmd-function (cmd)
  "Returns the function of command `cmd'."
  (get 'function cmd))


(defmacro vim:apply-save-buffer (&rest args)
  "Like `apply' but stores the current buffer."
  (let ((ret (gensym)))
  `(progn
     (save-current-buffer
       (let ((,ret (apply ,@args)))
         (setq vim:new-buffer (current-buffer))
         ,ret)))))


(defmacro vim:funcall-save-buffer (&rest args)
  "Like `funcall' but stores the current buffer."
  (let ((ret (gensym)))
  `(progn
     (save-current-buffer
       (let ((,ret (funcall ,@args)))
         (setq vim:new-buffer (current-buffer))
         ,ret)))))


(defun vim:get-current-motion ()
  (if (null vim:current-motion)
      nil
    (let ((cmd vim:current-motion)
          (count (if (or vim:current-cmd-count
                         vim:current-motion-count)
                     (* (or vim:current-cmd-count 1)
                        (or vim:current-motion-count 1))
                   nil))
          (parameters nil))

      ;; build the parameter-list
      (when (vim:cmd-arg-p cmd)
        (push vim:current-motion-arg parameters)
        (push :argument parameters))
      (when (vim:cmd-count-p cmd)
        (push count parameters)
        (push :count parameters))

      (let* ((motion (vim:apply-save-buffer (vim:cmd-function cmd) parameters))
             (type (vim:cmd-type motion)))
        ;; check if the motion overwrites its default type
        (when (and (consp motion)
                   (symbolp (car motion)))
          (setq type (car motion))
          (setq motion (cdr motion)))
                   
        ;; block-motions return a pair of points or a pair of pairs
        (if (or (and (eq (vim:cmd-type cmd)'block) (consp (car motion)))
                (and (not (eq (vim:cmd-type cmd) 'block)) (consp motion)))
            (vim:make-motion :begin (car motion)
                             :end (cdr motion)
                             :type (vim:cmd-type cmd))
          (vim:make-motion :begin nil
                           :end motion
                           :type (vim:cmd-type cmd)))))))


(defun vim:get-current-cmd-motion ()
  "Returns the motion range for the current command w.r.t. inclusive/exclusive/linewise."
  (if vim:current-motion
      
      (let ((motion (vim:get-current-motion)))

        ;; if begin is nil, set it to point
        (unless (vim:motion-begin motion)
          (if (eq (vim:motion-type motion) 'block)
              (setf (vim:motion-begin motion)
                    (cons (line-number-at-pos (point))
                          (current-column)))
            (setf (vim:motion-begin motion) (point))))

        ;; order the motion
        (when (> (vim:motion-begin motion)
                 (vim:motion-end motion))
          (setq motion (vim:make-motion :begin (vim:motion-end motion)
                                        :end (vim:motion-begin motion)
                                        :type (vim:motion-type motion))))
        
        (case (vim:motion-type motion)
          ('inclusive
           (setq vim:current-motion-type 'inclusive)
           (vim:make-motion :begin (vim:motion-begin motion)
                            :end (vim:adjust-end-of-line-position (vim:motion-end motion))
                            :type 'inclusive))

          ('exclusive
           (if (save-excursion
                 (goto-char (vim:motion-end motion))
                 (bolp))
               
               (if (save-excursion
                     (goto-char (vim:motion-begin motion))
                     (looking-back "^[[:space:]]*"))
                   ;; motion becomes linewise(-exclusive)
                   (progn
                     (setq vim:current-motion-type 'linewise)
                     (vim:make-motion :begin (line-number-at-pos (vim:motion-begin motion))
                                      :end (1- (line-number-at-pos (vim:motion-end motion)))
                                      :type 'linewise))
                 
                 ;; motion becomes inclusive
                 (progn
                   (setq vim:current-motion-type 'inclusive)
                   (vim:make-motion :begin (vim:motion-begin motion)
                                    :end (1- (vim:motion-end motion)) ; will move to the previous end-of-line
                                    :type 'inclusive)))
                                    
             ;; usual exclusive motion; in this case the end-of-motion
             ;; will not be on the first character in a line, so (1-
             ;; (vim:motion-end motion)) is save
             (setq vim:current-motion-type 'exclusive)
             (vim:make-motion :begin (vim:motion-begin motion)
                              :end (1- (vim:motion-end motion))
                              :type 'inclusive)))

          ('linewise
           (setq vim:current-motion-type 'linewise)
           (vim:make-motion :begin (line-number-at-pos (vim:motion-begin motion))
                            :end (line-number-at-pos (vim:motion-end motion))
                            :type 'linewise))

          ('block
              ;; TODO: here more work is required
           (setq vim:current-motion-type 'block)
           motion)))

    ;; no motion -> return nil
    nil))

