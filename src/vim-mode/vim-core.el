;; vim-core.el - Core variables and functions. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; In Vim there several types of motions. The four basic types are
;;
;; # characterwise, inclusive
;; # characterwise, exclusive
;; # linewise
;; # blockwise
;;
;; The motion type has no influence when the motion is just used to
;; move (point) but influences on which region of the buffer a certain
;; command that requires a motion parameter works. If the motions
;; starts at point beg and ends at point end, characterwise inclusive
;; motions work on all characters from beg to end including end while
;; characterwise exclusive motions work on all characters from beg up
;; to end - 1 excluding end. Examples for the first type are e % for
;; the second type w l. Linewise motions always operate on whole lines
;; from the line containing beg up to the line containing end.
;; Blockwise motions operate on the rectangular region defined by row
;; and column of beg and end.
;;
;; A motion returns an object of type vim:motion either implicitly or
;; explicitly and a command gets an object of type vim:motion passed
;; in its motion parameter when called. The command should the work on
;; the region specified by the begin and end position of the motion
;; w.r.t. the motion type, e.g., linewise motions should work on whole
;; lines.
;;
;; The vim:motion object is a structure with four fields:
;;
;; type: The motion type, inclusive, exclusive, linewise, block.
;;
;; has-begin: If non nil the motion defines both an explicit end
;;            position and an explicit start position of the region.
;;            If it is nil the motion only defines an explicit end
;;            position and the start position is implicitly set to
;;            (point). All usual motions should set this field to nil
;;            and specify the new position of (point) as end position
;;            whereas text-objects, e.g., iw aw ib ab should set it to
;;            t and specify both positions.
;;
;; begin: The beginning of the motion, if not given it is set to
;;        (point).
;;
;; end: The end position of the motion.
;;
;; Most usual motions do not need to create the vim:motion object
;; explicitly. Just move (point) to the desired position and the
;; vim:motion object will be created automatically.
;;
;; Note that an explicitly returned vim:motion object may have a
;; different type than the default motion type. The default motion
;; type is only important for simple motions that do not return an
;; explicit vim:motion.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'register)

(require 'vim-macs)
(require 'vim-defs)
(require 'vim-modes)
(require 'vim-keymap)
(require 'vim-compat)

(defvar-local vim:new-buffer nil
  "The buffer the be made current at the end of the execution of
  the current command.

When executing a command, the current buffer can be changed.
Because vim-mode depends on many buffer local variable, the
current buffer must not be changed until all code dealing with
command execution is finished, e.g., recording of key events in
registers. When a command changes the current buffer, the new buffer
is stored in this variable and then the buffer which was current before
the execution of the command becomes current again. At the very end
of the command handling code the buffer in vim:new-buffer is made current.")

(defvar-local vim:this-column nil
  "The resulting column of the current motion.")

(defvar-local vim:last-column nil
  "The resulting column of the previous motion.")


(defsubst vim:use-last-column ()
  "This function should by called by a motion not changing the column."
  (setq vim:this-column vim:last-column))


(defsubst vim:toplevel-execution ()
  "Returns t iff this is a toplevel execution, not a mapping or repeat."
  (not executing-kbd-macro))


(defun vim:reset-key-state ()
  "Resets the current state of the keymap."
  (setq vim:current-register nil
        vim:current-cmd-count nil
        vim:current-cmd nil
        vim:current-cmd-arg nil
        vim:current-motion-count nil
        vim:current-motion nil
        vim:current-motion-arg nil
        vim:current-motion-type nil
        vim:current-force-motion-type nil))

(defsubst vim:clear-key-sequence ()
  "Clears the internal log of key-sequences."
  (setq vim:current-key-sequence nil))

(defsubst vim:remember-this-command-keys ()
  "Adds keys that invoked current command to `vim:current-key-sequence'."
  (setf vim:current-key-sequence
        (vconcat vim:current-key-sequence (this-command-keys-vector))))

(defsubst vim:cmd-count-p (cmd)
  "Returns non-nil iff command cmd takes a count."
  (get cmd 'count))

(defsubst vim:cmd-register-p (cmd)
  "Returns non-nil iff command may take a register."
  (get cmd 'register))

(defsubst vim:cmd-motion-p (cmd)
  "Returns non-nil iff command `cmd' takes a motion parameter."
  (get cmd 'motion))

(defsubst vim:cmd-arg (cmd)
  "Returns the type of command's argument."
  (get cmd 'argument))

(defsubst vim:cmd-arg-p (cmd)
  "Returns non-nil iff command cmd takes an argument of arbitrary type."
  (not (null (get cmd 'argument))))

(defsubst vim:cmd-text-arg-p (cmd)
  "Returns non-nil iff command cmd takes a text argument."
  (eq (vim:cmd-arg cmd) t))

(defsubst vim:cmd-char-arg-p (cmd)
  "Returns non-nil iff command cmd takes a char argument."
  (eq (vim:cmd-arg cmd) 'char))

(defsubst vim:cmd-file-arg-p (cmd)
  "Returns non-nil iff command cmd takes a file argument."
  (eq (vim:cmd-arg cmd) 'file))

(defsubst vim:cmd-buffer-arg-p (cmd)
  "Returns non-nil iff command cmd takes a buffer argument."
  (eq (vim:cmd-arg cmd) 'buffer))

(defsubst vim:cmd-repeatable-p (cmd)
  "Returns non-nil iff command cmd is repeatable."
  (get cmd 'repeatable))

(defsubst vim:cmd-keep-visual-p (cmd)
  "Returns non-nil iff command cmd should stay in visual mode."
  (get cmd 'keep-visual))

(defsubst vim:cmd-force-p (cmd)
  "Returns non-nil iff command cmd takes a force argument."
  (not (null (get cmd 'force))))

(defsubst vim:cmd-type (cmd)
  "Returns the type of command cmd."
  (get cmd 'type))

(defsubst vim:cmd-function (cmd)
  "Returns the function of command `cmd'."
  (get cmd 'function))


(defmacro vim:apply-save-buffer (func &rest args)
  "Like `apply' but stores the current buffer."
  `(save-current-buffer
     (prog1 (apply ,func ,@args)
       (setq vim:new-buffer (current-buffer)))))

(defmacro vim:funcall-save-buffer (func &rest args)
  "Like `funcall' but stores the current buffer."
  `(save-current-buffer
     (prog1 (funcall ,func ,@args)
       (setq vim:new-buffer (current-buffer)))))

(defun vim:get-register (register)
  "Returns the content of `register', signals error on fail."
  (let ((txt (get-register register)))
    (unless txt
      (error "Register '%c' empty" register))
    txt))

(defsubst vim:set-register (register value)
  (set-register register value))


;; This structure is passed to operators taking a motion. A motion
;; command can also return this structure to create a more advanced
;; motion like text object selections.
(defstruct (vim:motion
            (:constructor vim:make-motion-struct))
  has-begin ;; t iff the motion defined an explicit begin
  begin     ;; first point in this motion
  end       ;; last point in this motion
  type      ;; 'inclusive, 'exclusive, 'linewise, 'block
  )


(defun* vim:make-motion (&key
                         has-begin
                         (begin (point))
                         (end (point))
                         type)
  "Creates a new motion with `begin' and `end' always
positions within (point-min) and (point-max) and not at
 (line-end-position) (if possible)."
  (unless type
    (setq type (if (<= begin end) 'inclusive 'exclusive)))

  (letrec
      ((shrink-to (lambda (pos lower upper)
                    (max lower (min upper pos))))

       (normalize-pos (lambda (pos)
                        (let ((pos (funcall shrink-to pos (point-min) (point-max))))
                          (funcall shrink-to pos
                                   (save-excursion
                                     (goto-char pos)
                                     (line-beginning-position))
                                   (save-excursion
                                     (goto-char pos)
                                     (- (line-end-position)
                                        (if (eq type 'inclusive) 1 0))))))))

    (vim:make-motion-struct :has-begin has-begin
                            :begin (funcall normalize-pos begin)
                            :end (funcall normalize-pos end)
                            :type type)))

(defun vim:change-motion-begin (motion new-begin)
  (let ((result (copy-vim:motion motion)))
    (setf (vim:motion-begin result) new-begin)
    result))

(defun vim:change-motion-end (motion new-end)
  (let ((result (copy-vim:motion motion)))
    (setf (vim:motion-end result) new-end)
    result))


(defun vim:motion-line-count (motion)
  "Returns the number of lines the `motion' covers."
  (1+ (- (vim:motion-last-line motion)
         (vim:motion-first-line motion))))

(defun vim:motion-first-line (motion)
  "Returns the first line covered by `motion'."
  (min (line-number-at-pos (vim:motion-begin motion))
       (line-number-at-pos (vim:motion-end motion))))

(defun vim:motion-last-line (motion)
  "Returns the last line covered by `motion'."
  (max (line-number-at-pos (vim:motion-begin motion))
       (line-number-at-pos (vim:motion-end motion))))

(defun vim:motion-first-col (motion)
  "Returns the first column covered by `motion'."
  (min (save-excursion
         (goto-char (vim:motion-begin motion))
         (current-column))
       (save-excursion
         (goto-char (vim:motion-end motion))
         (current-column))))

(defun vim:motion-last-col (motion)
  "Returns the last column covered by `motion'."
  (max (save-excursion
         (goto-char (vim:motion-begin motion))
         (current-column))
       (save-excursion
         (goto-char (vim:motion-end motion))
         (current-column))))

(defun vim:motion-begin-pos (motion)
  "Returns the smaller position covered by `motion'.
The result is modified depending on the motion type to
return the correct start-position of emacs-ranges, i.e.
  - if motion is inclusive or exclusive, nothing is changed
  - if motion is line-wise, is always bol of the first line in the motion,
  - if motion is block 1 is added if and only if the begin column
    is larget than the end column."
  (pcase (vim:motion-type motion)
    (`linewise
     (save-excursion
       (goto-line-dumb (vim:motion-first-line motion))
       (line-beginning-position)))
    (`block
        (let ((b (min (vim:motion-begin motion) (vim:motion-end motion)))
              (e (max (vim:motion-begin motion) (vim:motion-end motion))))
          (if (> (save-excursion (goto-char b) (current-column))
                 (save-excursion (goto-char e) (current-column)))
              (1+ b)
            b)))
    (_ (min (vim:motion-begin motion) (vim:motion-end motion)))))

(defun vim:motion-end-pos (motion)
  "Returns the larger position covered by `motion'.
The result is modified depending on the motion type to
return the correct end-position of emacs-ranges, i.e.
  - if motion is inclusive, 1 is added,
  - if motion is exclusive, nothing is change,
  - if motion is line-wise, is always eol of the last line in the motion,
  - if motion is block 1 is added if and only if the end column
    is larger than or equal to the begin column and char at the end is not
    newline."
  (pcase (vim:motion-type motion)
    (`linewise
     (save-excursion
       (goto-line-dumb (vim:motion-last-line motion))
       (line-end-position)))
    (`block
        (let ((b (min (vim:motion-begin motion) (vim:motion-end motion)))
              (e (max (vim:motion-begin motion) (vim:motion-end motion))))
          (if (and (>= (save-excursion (goto-char e) (current-column))
                       (save-excursion (goto-char b) (current-column)))
                   (not (char=? (char-after e) ?\n)))
              (1+ e)
            e)))
    (`inclusive
     (1+ (max (vim:motion-begin motion) (vim:motion-end motion))))
    (_ (max (vim:motion-begin motion) (vim:motion-end motion)))))

(defmacro vim:do-motion (type expression)
  "Executes a motion body, ensuring the return of a valid vim:motion object.
This function is called to execute a motion function. When the
motion command returns a vim:motion struct, this struct is just
returned. Otherwise a new vim:motion is created depending on the
position of (point) before and after executing the motion command
and the (default) type of the motion."
  (declare (indent 1))
  (let ((current-pos '#:current-pos)
        (motion '#:motion))
    `(let ((,current-pos (point))
           (,motion ,expression))
       (if (vim:motion-p ,motion)
           ,motion
         (progn
           (when vim:this-column
             (move-to-column vim:this-column))
           (vim:make-motion :has-begin nil
                            :begin ,current-pos
                            :end (point)
                            :type ,type))))))

(defun vim:adjust-end-of-line-position (pos)
  "If pos is an end-of-line returns pos - 1 and pos otherwise."
  (save-excursion
    (goto-char pos)
    (max (line-beginning-position)
         (min (1- (line-end-position)) pos))))


(defvar-local *vim:do-not-adjust-point* nil
  "If equals to t then no adjustment of point at end of line would
take place. I.e. this enables to position point at the \\n character
at the end of line, whereas in Vim this is prohibited. This probably
should be set to t when working in repl buffers.

Assuming point is |, setting this option to t renders this possible
    foo bar|
but with nil, point will be repositioned at r:
    foo ba|r
")

(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  ;; TODO: should we check modes directly?
  (unless (vim:insert-mode-p)
    (when vim:this-column
      (move-to-column vim:this-column))

    ;; always stop at the last character (not the newline)
    (when (and (not (vim:visual-mode-p))
               (not *vim:do-not-adjust-point*)
               (eolp)
               (not (bolp)))
      (backward-char)))

  (setq vim:last-column (or vim:this-column
                            (current-column)))
  (setq vim:this-column nil))


(defun vim:execute-command (cmd)
  "Executes the vim-command `cmd'.
If an error occures, this function switches back to normal-mode.
Since all vim-mode commands go through this function, this is
the perfect point to do some house-keeping."
  ;; note: this is for brief debugging only since vim's visual mode
  ;; depends on the version with condition-case (if you dare to figure
  ;; that out then you may change this function whatever you like)

  ;; (unwind-protect
  ;;     (funcall vim:active-command-function cmd)
  ;;   (vim:reset-key-state)
  ;;   (vim:clear-key-sequence)
  ;;   (vim:adjust-point)
  ;;   (vim:activate-normal-mode))

  (condition-case err
      (funcall vim:active-command-function cmd)
    (error
     (vim:reset-key-state)
     (vim:clear-key-sequence)
     (vim:adjust-point)
     (vim:activate-normal-mode)
     (message "vim:execute-command: error: %s\nvim:active-command-function: %s\ncmd: %s"
              err
              vim:active-command-function
              cmd)
     (signal (car err) (cdr err)))))

(defun vim:execute-current-motion ()
  "Executes the current motion and returns the representing
vim:motion object."
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
      (when (vim:cmd-char-arg-p cmd)
        (push vim:current-motion-arg parameters)
        (push :argument parameters))
      (when (vim:cmd-count-p cmd)
        (push count parameters)
        (push :count parameters))

      (vim:apply-save-buffer cmd parameters))))

(defun vim:get-current-cmd-motion ()
  "Returns the motion range for the current command w.r.t.
command-specific transformations."
  (let ((motion (save-excursion (vim:execute-current-motion))))
    (when vim:current-force-motion-type
      (setf (vim:motion-type motion)
            (if (eq vim:current-force-motion-type 'char)
                (pcase (vim:motion-type motion)
                  (`exclusive 'inclusive)
                  (_ 'exclusive))
              vim:current-force-motion-type)))

    (when (and (eq (vim:motion-type motion) 'exclusive)
               (save-excursion
                 (goto-char (vim:motion-end-pos motion))
                 (bolp)))

      ;; exclusive motions may be modified
      (let ((end (vim:adjust-end-of-line-position (1- (vim:motion-end-pos motion)))))
        (if (< (vim:motion-begin motion)
               (vim:motion-end motion))
            (setf (vim:motion-end motion) end)
          (setf (vim:motion-begin motion) end)))

      (if (save-excursion
            (goto-char (vim:motion-begin-pos motion))
            (vim:looking-back "^\\s-*"))
          ;; motion becomes linewise(-exclusive)
          (setf (vim:motion-type motion) 'linewise)

        ;; motion becomes inclusive
        (setf (vim:motion-type motion) 'inclusive)))
    motion))


(defconst vim:emacs-keymap (vim:make-keymap)
  "Keymap for EMACS mode.")

(vim:define-mode emacs "VIM emacs-mode"
  :ident "E"
  ;; :message "-- EMACS --"
  :keymaps '(vim:emacs-keymap)
  :command-function 'vim:normal-mode-command)

;; from viper
(defsubst vim:ESC-event-p (event)
  (let ((ESC-keys '(?\e (control \[) escape))
        (key (event-basic-type event)))
    (member key ESC-keys)))

;; from viper
(defun vim:escape-to-emacs (events)
  "Executes some `events' in emacs."

  (let* ((unread-command-events events)
         (keys (read-key-sequence nil))
         (event (elt (listify-key-sequence keys) 0)))

    (when (vim:ESC-event-p event)
      (let ((unread-command-events keys))
        (setq keys (read-key-sequence nil))))

    (let ((command (key-binding keys)))
      (setq this-command command)
      (setq last-command-event (elt keys (1- (length keys))))
      (command-execute command)
      (when (memq command '(digit-argument
                            universal-argument))
        (vim:escape-to-emacs nil)))))

;; The following special keybinding ensures we can always return to
;; normal mode by pressing ESC three times.
;; Fix: two times or using Alt+Esc
(defun vim:exit-to-normal-mode ()
  "Exits any VIM mode and returns to normal-mode."
  (interactive)
  (vim:activate-normal-mode)
  (ding))

(vim:map (kbd "ESC ESC")
         #'vim:exit-to-normal-mode
         :keymap vim:override-keymap)

(provide 'vim-core)

;; Local Variables:
;; End:

;; vim-core.el ends here
