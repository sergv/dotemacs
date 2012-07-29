;;; lisp-jit-lock.el --- just-in-time fontification

;; Copyright (C) 1998, 2000-2012 Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Keywords: faces files
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; NOTE: this file is really stolen jit-lock.el from standard emacs
;; distribution. The only tweaked function is `lisp-jit-lock-after-change'.
;; The need for this file is in having separate mode for lisp fontification
;; which would remove need of tweaking original jit-lock to account for
;; lisp-related mechanism of fontification updates

;; Just-in-time fontification, triggered by C redisplay code.

;;; Code:


(eval-when-compile
 (require 'cl)

 (defmacro with-buffer-prepared-for-lisp-jit-lock (&rest body)
   "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
   ;; (declare (debug t))
   `(let ((inhibit-point-motion-hooks t))
      (with-silent-modifications
          ,@body))))

;;; Customization.

(defgroup lisp-jit-lock nil
  "Font Lock support mode to fontify just-in-time."
  :version "21.1"
  :group 'font-lock)

;; (defcustom lisp-jit-lock-chunk-size 200
;;   "Jit-lock fontifies chunks of at most this many characters at a time.
;;
;; This variable controls both display-time and stealth fontification."
;;   :type 'integer
;;   :group 'lisp-jit-lock)


;; (defcustom lisp-jit-lock-stealth-time nil
;;   "Time in seconds to wait before beginning stealth fontification.
;; Stealth fontification occurs if there is no input within this time.
;; If nil, stealth fontification is never performed.
;;
;; The value of this variable is used when JIT Lock mode is turned on."
;;   :type '(choice (const :tag "never" nil)
;;           (number :tag "seconds" :value 16))
;;   :group 'lisp-jit-lock)


;; (defcustom lisp-jit-lock-stealth-nice 0.5
;;   "Time in seconds to pause between chunks of stealth fontification.
;; Each iteration of stealth fontification is separated by this amount of time,
;; thus reducing the demand that stealth fontification makes on the system.
;; If nil, means stealth fontification is never paused.
;; To reduce machine load during stealth fontification, at the cost of stealth
;; taking longer to fontify, you could increase the value of this variable.
;; See also `lisp-jit-lock-stealth-load'."
;;   :type '(choice (const :tag "never" nil)
;;           (number :tag "seconds"))
;;   :group 'lisp-jit-lock)


;; (defcustom lisp-jit-lock-stealth-load
;;   (if (condition-case nil (load-average) (error)) 200)
;;   "Load in percentage above which stealth fontification is suspended.
;; Stealth fontification pauses when the system short-term load average (as
;; returned by the function `load-average' if supported) goes above this level,
;; thus reducing the demand that stealth fontification makes on the system.
;; If nil, means stealth fontification is never suspended.
;; To reduce machine load during stealth fontification, at the cost of stealth
;; taking longer to fontify, you could reduce the value of this variable.
;; See also `lisp-jit-lock-stealth-nice'."
;;   :type (if (condition-case nil (load-average) (error))
;;           '(choice (const :tag "never" nil)
;;             (integer :tag "load"))
;;           '(const :format "%t: unsupported\n" nil))
;;   :group 'lisp-jit-lock)


;; (defcustom lisp-jit-lock-stealth-verbose nil
;;   "If non-nil, means stealth fontification should show status messages."
;;   :type 'boolean
;;   :group 'lisp-jit-lock)


(defvaralias 'lisp-jit-lock-defer-contextually 'lisp-jit-lock-contextually)
(defcustom lisp-jit-lock-contextually 'syntax-driven
  "If non-nil, means fontification should be syntactically true.
If nil, means fontification occurs only on those lines modified.  This
means where modification on a line causes syntactic change on subsequent lines,
those subsequent lines are not refontified to reflect their new context.
If t, means fontification occurs on those lines modified and all
subsequent lines.  This means those subsequent lines are refontified to reflect
their new syntactic context, after `lisp-jit-lock-context-time' seconds.
If any other value, e.g., `syntax-driven', means syntactically true
fontification occurs only if syntactic fontification is performed using the
buffer mode's syntax table, i.e., only if `font-lock-keywords-only' is nil.

The value of this variable is used when JIT Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
          (const :tag "always" t)
          (other :tag "syntax-driven" syntax-driven))
  :group 'lisp-jit-lock)

(defcustom lisp-jit-lock-context-time 0.5
  "Idle time after which text is contextually refontified, if applicable."
  :type '(number :tag "seconds")
  :group 'lisp-jit-lock)

(defcustom lisp-jit-lock-defer-time nil ;; 0.25
  "Idle time after which deferred fontification should take place.
If nil, fontification is not deferred."
  :group 'lisp-jit-lock
  :type '(choice (const :tag "never" nil)
          (number :tag "seconds")))

;;; Variables that are not customizable.

(defvar lisp-jit-lock-mode nil
  "Non-nil means Just-in-time Lock mode is active.")
(make-variable-buffer-local 'lisp-jit-lock-mode)

(defvar lisp-jit-lock-functions nil
  "Functions to do the actual fontification.
They are called with two arguments: the START and END of the region to fontify.")
(make-variable-buffer-local 'lisp-jit-lock-functions)

(defvar lisp-jit-lock-context-unfontify-pos nil
  "Consider text after this position as contextually unfontified.
If nil, contextual fontification is disabled.")
(make-variable-buffer-local 'lisp-jit-lock-context-unfontify-pos)


(defvar lisp-jit-lock-stealth-timer nil
  "Timer for stealth fontification in Just-in-time Lock mode.")
(defvar lisp-jit-lock-stealth-repeat-timer nil
  "Timer for repeated stealth fontification in Just-in-time Lock mode.")
(defvar lisp-jit-lock-context-timer nil
  "Timer for context fontification in Just-in-time Lock mode.")
(defvar lisp-jit-lock-defer-timer nil
  "Timer for deferred fontification in Just-in-time Lock mode.")

(defvar lisp-jit-lock-defer-buffers nil
  "List of buffers with pending deferred fontification.")
(defvar lisp-jit-lock-stealth-buffers nil
  "List of buffers that are being fontified stealthily.")

;;; JIT lock mode

(defun lisp-jit-lock-mode (arg)
  "Toggle Just-in-time Lock mode.
Turn Just-in-time Lock mode on if and only if ARG is non-nil.
Enable it automatically by customizing group `font-lock'.

When Just-in-time Lock mode is enabled, fontification is different in the
following ways:

- Demand-driven buffer fontification triggered by Emacs C code.
  This means initial fontification of the whole buffer does not occur.
  Instead, fontification occurs when necessary, such as when scrolling
  through the buffer would otherwise reveal unfontified areas.  This is
  useful if buffer fontification is too slow for large buffers.

- Stealthy buffer fontification if `lisp-jit-lock-stealth-time' is non-nil.
  This means remaining unfontified areas of buffers are fontified if Emacs has
  been idle for `lisp-jit-lock-stealth-time' seconds, while Emacs remains idle.
  This is useful if any buffer has any deferred fontification.

- Deferred context fontification if `lisp-jit-lock-contextually' is
  non-nil.  This means fontification updates the buffer corresponding to
  true syntactic context, after `lisp-jit-lock-context-time' seconds of Emacs
  idle time, while Emacs remains idle.  Otherwise, fontification occurs
  on modified lines only, and subsequent lines can remain fontified
  corresponding to previous syntactic contexts.  This is useful where
  strings or comments span lines.

Stealth fontification only occurs while the system remains unloaded.
If the system load rises above `lisp-jit-lock-stealth-load' percent, stealth
fontification is suspended.  Stealth fontification intensity is controlled via
the variable `lisp-jit-lock-stealth-nice'."
  (setq lisp-jit-lock-mode arg)
  (cond (;; Turn Just-in-time Lock mode on.
         lisp-jit-lock-mode

         ;; Mark the buffer for refontification.
         (lisp-jit-lock-refontify)

         ;; Install an idle timer for stealth fontification.
         ;; (when (and lisp-jit-lock-stealth-time (null lisp-jit-lock-stealth-timer))
         ;;   (setq lisp-jit-lock-stealth-timer
         ;;         (run-with-idle-timer lisp-jit-lock-stealth-time t
         ;;                              'lisp-jit-lock-stealth-fontify)))

         ;; Create, but do not activate, the idle timer for repeated
         ;; stealth fontification.
         ;; (when (and lisp-jit-lock-stealth-time (null lisp-jit-lock-stealth-repeat-timer))
         ;;   (setq lisp-jit-lock-stealth-repeat-timer (timer-create))
         ;;   (timer-set-function lisp-jit-lock-stealth-repeat-timer
         ;;                       'lisp-jit-lock-stealth-fontify '(t)))

         ;; Init deferred fontification timer.
         (when (and lisp-jit-lock-defer-time (null lisp-jit-lock-defer-timer))
           (setq lisp-jit-lock-defer-timer
                 (run-with-idle-timer lisp-jit-lock-defer-time t
                                      'lisp-jit-lock-deferred-fontify)))

         ;; Initialize contextual fontification if requested.
         (when (eq lisp-jit-lock-contextually t)
           (unless lisp-jit-lock-context-timer
             (setq lisp-jit-lock-context-timer
                   (run-with-idle-timer lisp-jit-lock-context-time t
                                        'lisp-jit-lock-context-fontify)))
           (setq lisp-jit-lock-context-unfontify-pos
                 (or lisp-jit-lock-context-unfontify-pos (point-max))))

         ;; Setup our hooks.
         (add-hook 'after-change-functions 'lisp-jit-lock-after-change nil t)
         (add-hook 'fontification-functions 'lisp-jit-lock-function))

        ;; Turn Just-in-time Lock mode off.
        (t
         ;; Cancel our idle timers.
         ;; (when (and (or lisp-jit-lock-stealth-timer lisp-jit-lock-defer-timer
         ;;                lisp-jit-lock-context-timer)
         ;;            ;; Only if there's no other buffer using them.
         ;;            (not (catch 'found
         ;;                   (dolist (buf (buffer-list))
         ;;                     (with-current-buffer buf
         ;;                       (when lisp-jit-lock-mode (throw 'found t)))))))
         ;;   (when lisp-jit-lock-stealth-timer
         ;;     (cancel-timer lisp-jit-lock-stealth-timer)
         ;;     (setq lisp-jit-lock-stealth-timer nil))
         ;;   (when lisp-jit-lock-context-timer
         ;;     (cancel-timer lisp-jit-lock-context-timer)
         ;;     (setq lisp-jit-lock-context-timer nil))
         ;;   (when lisp-jit-lock-defer-timer
         ;;     (cancel-timer lisp-jit-lock-defer-timer)
         ;;     (setq lisp-jit-lock-defer-timer nil)))

         ;; Remove hooks.
         (remove-hook 'after-change-functions 'lisp-jit-lock-after-change t)
         (remove-hook 'fontification-functions 'lisp-jit-lock-function))))

(defun lisp-jit-lock-register (fun &optional contextual)
  "Register FUN as a fontification function to be called in this buffer.
FUN will be called with two arguments START and END indicating the region
that needs to be (re)fontified.
If non-nil, CONTEXTUAL means that a contextual fontification would be useful."
  (add-hook 'lisp-jit-lock-functions fun nil t)
  (when (and contextual lisp-jit-lock-contextually)
    (set (make-local-variable 'lisp-jit-lock-contextually) t))
  (lisp-jit-lock-mode t))

(defun lisp-jit-lock-unregister (fun)
  "Unregister FUN as a fontification function.
Only applies to the current buffer."
  (remove-hook 'lisp-jit-lock-functions fun t)
  (unless lisp-jit-lock-functions (lisp-jit-lock-mode nil)))

;; This function is used to prevent font-lock-fontify-buffer from
;; fontifying eagerly the whole buffer.  This is important for
;; things like CWarn mode which adds/removes a few keywords and
;; does a refontify (which takes ages on large files).
(defun lisp-jit-lock-refontify (&optional beg end)
  "Force refontification of the region BEG..END (default whole buffer)."
  (with-buffer-prepared-for-lisp-jit-lock
      (save-restriction
       (widen)
       (put-text-property (or beg (point-min)) (or end (point-max))
                          'fontified nil))))

;;; On demand fontification.

(defun lisp-jit-lock-function (start)
  "Fontify current buffer starting at position START.
This function is added to `fontification-functions' when `lisp-jit-lock-mode'
is active."
  (when (and lisp-jit-lock-mode (not memory-full))
    ;; (backward-up-list)
    (lisp-jit-lock-up-list-until-match *lisp-jit-lock-form-regexp*)
    (skip-syntax-forward "^(")
    (message "REFONTIFY NOW: %S"
             (buffer-substring-no-properties
              (point)
              (end-of-sexp-at-point)))
    (lisp-jit-lock-fontify-now (point)
                               (end-of-sexp-at-point))

    ;; (save-excursion
    ;;  (let ((lines (count-lines (point-min) start))
    ;;        (done nil))
    ;;    (goto-char start)
    ;;    (lisp-jit-lock-up-list-until-match *lisp-jit-lock-form-regexp*)
    ;;    ;; (while (not (= 0 (current-column)))
    ;;    ;;   (backward-up-list))
    ;;    (let ((sexp-start (point)))
    ;;      (while (and (not done)
    ;;                  (<= (- (count-lines start (point))
    ;;                         lines)
    ;;                      42))
    ;;        ;; ensure that we're at top level
    ;;        (let ((end-of-sexp (scan-sexps (point) 1)))
    ;;          (if end-of-sexp
    ;;            (goto-char end-of-sexp)
    ;;            (progn
    ;;              (goto-char (point-max))
    ;;              (setf done t)))))
    ;;      (lisp-jit-lock-fontify-now sexp-start ;; start
    ;;                                 (point)))))


    ;; (let ((end-of-sexp (or (scan-sexps (point) 1) (point-max))))
    ;;   (lisp-jit-lock-fontify-now start end-of-sexp))

    ;; (lisp-jit-lock-fontify-now start (point-max))

    ;; (if (null lisp-jit-lock-defer-timer)
    ;;   ;; No deferral.
    ;;   (lisp-jit-lock-fontify-now start (+ start lisp-jit-lock-chunk-size))
    ;;   ;; Record the buffer for later fontification.
    ;;   (progn
    ;;     (unless (memq (current-buffer) lisp-jit-lock-defer-buffers)
    ;;       (push (current-buffer) lisp-jit-lock-defer-buffers))
    ;;     ;; Mark the area as defer-fontified so that the redisplay engine
    ;;     ;; is happy and so that the idle timer can find the places to fontify.
    ;;     (with-buffer-prepared-for-lisp-jit-lock
    ;;         (put-text-property start
    ;;                            (next-single-property-change
    ;;                             start 'fontified nil
    ;;                             (min (point-max)
    ;;                                  (+ start lisp-jit-lock-chunk-size)))
    ;;                            'fontified 'defer))))
    ))

(defun lisp-jit-lock-fontify-now (&optional start end)
  "Fontify current buffer from START to END.
Defaults to the whole buffer.  END can be out of bounds."
  (with-buffer-prepared-for-lisp-jit-lock
      (save-excursion
       (unless start (setq start (point-min)))
       (setq end (if end (min end (point-max)) (point-max)))
       ;; This did bind `font-lock-beginning-of-syntax-function' to
       ;; nil at some point, for an unknown reason.  Don't do this; it
       ;; can make highlighting slow due to expensive calls to
       ;; `parse-partial-sexp' in function
       ;; `font-lock-fontify-syntactically-region'.  Example: paging
       ;; from the end of a buffer to its start, can do repeated
       ;; `parse-partial-sexp' starting from `point-min', which can
       ;; take a long time in a large buffer.
       (let ((orig-start start) next)
         (save-match-data
          ;; Fontify chunks beginning at START.  The end of a
          ;; chunk is either `end', or the start of a region
          ;; before `end' that has already been fontified.
          (while (and start (< start end))
            ;; Determine the end of this chunk.
            (setq next (or (text-property-any start end 'fontified t)
                           end))

            ;; Decide which range of text should be fontified.
            ;; The problem is that START and NEXT may be in the
            ;; middle of something matched by a font-lock regexp.
            ;; Until someone has a better idea, let's start
            ;; at the start of the line containing START and
            ;; stop at the start of the line following NEXT.
            (goto-char next)  (setq next (line-beginning-position 2))
            (goto-char start) (setq start (line-beginning-position))

            ;; Make sure the contextual refontification doesn't re-refontify
            ;; what's already been refontified.
            (when (and lisp-jit-lock-context-unfontify-pos
                       (< lisp-jit-lock-context-unfontify-pos next)
                       (>= lisp-jit-lock-context-unfontify-pos start)
                       ;; Don't move boundary forward if we have to
                       ;; refontify previous text.  Otherwise, we risk moving
                       ;; it past the end of the multiline property and thus
                       ;; forget about this multiline region altogether.
                       (not (get-text-property start 'lisp-jit-lock-defer-multiline)))
              (setq lisp-jit-lock-context-unfontify-pos next))

            ;; Fontify the chunk, and mark it as fontified.
            ;; We mark it first, to make sure that we don't indefinitely
            ;; re-execute this fontification if an error occurs.
            (put-text-property start next 'fontified t)
            (condition-case err
                (run-hook-with-args 'lisp-jit-lock-functions start next)
              ;; If the user quits (which shouldn't happen in normal on-the-fly
              ;; lisp-jit-locking), make sure the fontification will be performed
              ;; before displaying the block again.
              (quit (put-text-property start next 'fontified nil)
                    (funcall 'signal (car err) (cdr err))))

            ;; The redisplay engine has already rendered the buffer up-to
            ;; `orig-start' and won't notice if the above lisp-jit-lock-functions
            ;; changed the appearance of any part of the buffer prior
            ;; to that.  So if `start' is before `orig-start', we need to
            ;; cause a new redisplay cycle after this one so that any changes
            ;; are properly reflected on screen.
            ;; To make such repeated redisplay happen less often, we can
            ;; eagerly extend the refontified region with
            ;; lisp-jit-lock-after-change-extend-region-functions.
            (when (< start orig-start)
              (run-with-timer 0 nil 'lisp-jit-lock-force-redisplay
                              (current-buffer) start orig-start))

            ;; Find the start of the next chunk, if any.
            (setq start (text-property-any next end 'fontified nil))))))))

(defun lisp-jit-lock-force-redisplay (buf start end)
  "Force the display engine to re-render buffer BUF from START to END."
  (with-current-buffer buf
    (with-buffer-prepared-for-lisp-jit-lock
        ;; Don't cause refontification (it's already been done), but just do
        ;; some random buffer change, so as to force redisplay.
        (put-text-property start end 'fontified t))))



;;; Stealth fontification.

;; (defsubst lisp-jit-lock-stealth-chunk-start (around)
;;   "Return the start of the next chunk to fontify around position AROUND.
;; Value is nil if there is nothing more to fontify."
;;   (if (zerop (buffer-size))
;;     nil
;;     (save-restriction
;;      (widen)
;;      (let* ((next (text-property-not-all around (point-max) 'fontified t))
;;             (prev (previous-single-property-change around 'fontified))
;;             (prop (get-text-property (max (point-min) (1- around))
;;                                      'fontified))
;;             (start (cond
;;                      ((null prev)
;;                       ;; There is no property change between AROUND
;;                       ;; and the start of the buffer.  If PROP is
;;                       ;; non-nil, everything in front of AROUND is
;;                       ;; fontified, otherwise nothing is fontified.
;;                       (if (eq prop t)
;;                         nil
;;                         (max (point-min)
;;                              (- around (/ lisp-jit-lock-chunk-size 2)))))
;;                      ((eq prop t)
;;                       ;; PREV is the start of a region of fontified
;;                       ;; text containing AROUND.  Start fontifying a
;;                       ;; chunk size before the end of the unfontified
;;                       ;; region in front of that.
;;                       (max (or (previous-single-property-change prev 'fontified)
;;                                (point-min))
;;                            (- prev lisp-jit-lock-chunk-size)))
;;                      (t
;;                       ;; PREV is the start of a region of unfontified
;;                       ;; text containing AROUND.  Start at PREV or
;;                       ;; chunk size in front of AROUND, whichever is
;;                       ;; nearer.
;;                       (max prev (- around lisp-jit-lock-chunk-size)))))
;;             (result (cond ((null start) next)
;;                           ((null next) start)
;;                           ((< (- around start) (- next around)) start)
;;                           (t next))))
;;        result))))

;; (defun lisp-jit-lock-stealth-fontify (&optional repeat)
;;   "Fontify buffers stealthily.
;; This function is called repeatedly after Emacs has become idle for
;; `lisp-jit-lock-stealth-time' seconds.  Optional argument REPEAT is expected
;; non-nil in a repeated invocation of this function."
;;   ;; Cancel timer for repeated invocations.
;;   (unless repeat
;;     (cancel-timer lisp-jit-lock-stealth-repeat-timer))
;;   (unless (or executing-kbd-macro
;;               memory-full
;;               (window-minibuffer-p (selected-window))
;;               ;; For first invocation set up `lisp-jit-lock-stealth-buffers'.
;;               ;; In repeated invocations it's already been set up.
;;               (null (if repeat
;;                       lisp-jit-lock-stealth-buffers
;;                       (setq lisp-jit-lock-stealth-buffers (buffer-list)))))
;;     (let ((buffer (car lisp-jit-lock-stealth-buffers))
;;           (delay 0)
;;           minibuffer-auto-raise
;;           message-log-max
;;           start)
;;       (if (and lisp-jit-lock-stealth-load
;;                (> (car (load-average)) lisp-jit-lock-stealth-load))
;;         ;; Wait a little if load is too high.
;;         (setq delay lisp-jit-lock-stealth-time)
;;         (if (buffer-live-p buffer)
;;           (with-current-buffer buffer
;;             (if (and lisp-jit-lock-mode
;;                      (setq start (lisp-jit-lock-stealth-chunk-start (point))))
;;               ;; Fontify one block of at most `lisp-jit-lock-chunk-size'
;;               ;; characters.
;;               (with-temp-message (if lisp-jit-lock-stealth-verbose
;;                                    (concat "JIT stealth lock "
;;                                            (buffer-name)))
;;                 (lisp-jit-lock-fontify-now start
;;                                            (+ start lisp-jit-lock-chunk-size))
;;                 ;; Run again after `lisp-jit-lock-stealth-nice' seconds.
;;                 (setq delay (or lisp-jit-lock-stealth-nice 0)))
;;               ;; Nothing to fontify here.  Remove this buffer from
;;               ;; `lisp-jit-lock-stealth-buffers' and run again immediately.
;;               (setq lisp-jit-lock-stealth-buffers (cdr lisp-jit-lock-stealth-buffers))))
;;           ;; Buffer is no longer live.  Remove it from
;;           ;; `lisp-jit-lock-stealth-buffers' and run again immediately.
;;           (setq lisp-jit-lock-stealth-buffers (cdr lisp-jit-lock-stealth-buffers))))
;;       ;; Call us again.
;;       (when lisp-jit-lock-stealth-buffers
;;         (timer-set-idle-time lisp-jit-lock-stealth-repeat-timer (current-idle-time))
;;         (timer-inc-time lisp-jit-lock-stealth-repeat-timer delay)
;;         (timer-activate-when-idle lisp-jit-lock-stealth-repeat-timer t)))))


;;; Deferred fontification.

(defun lisp-jit-lock-deferred-fontify ()
  "Fontify what was deferred."
  (when (and lisp-jit-lock-defer-buffers (not memory-full))
    ;; Mark the deferred regions back to `fontified = nil'
    (dolist (buffer lisp-jit-lock-defer-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; (message "Jit-Defer %s" (buffer-name))
          (with-buffer-prepared-for-lisp-jit-lock
              (let ((pos (point-min)))
                (while
                    (progn
                      (when (eq (get-text-property pos 'fontified) 'defer)
                        (put-text-property
                         pos (setq pos (next-single-property-change
                                        pos 'fontified nil (point-max)))
                         'fontified nil))
                      (setq pos (next-single-property-change pos 'fontified)))))))))
    (setq lisp-jit-lock-defer-buffers nil)
    ;; Force fontification of the visible parts.
    (let ((lisp-jit-lock-defer-timer nil))
      ;; (message "Jit-Defer Now")
      (sit-for 0)
      ;; (message "Jit-Defer Done")
      )))


(defun lisp-jit-lock-context-fontify ()
  "Refresh fontification to take new context into account."
  (unless memory-full
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when lisp-jit-lock-context-unfontify-pos
          ;; (message "Jit-Context %s" (buffer-name))
          (save-restriction
           (widen)
           (when (and (>= lisp-jit-lock-context-unfontify-pos (point-min))
                      (< lisp-jit-lock-context-unfontify-pos (point-max)))
             ;; If we're in text that matches a complex multi-line
             ;; font-lock pattern, make sure the whole text will be
             ;; redisplayed eventually.
             ;; Despite its name, we treat lisp-jit-lock-defer-multiline here
             ;; rather than in lisp-jit-lock-defer since it has to do with multiple
             ;; lines, i.e. with context.
             (when (get-text-property lisp-jit-lock-context-unfontify-pos
                                      'lisp-jit-lock-defer-multiline)
               (setq lisp-jit-lock-context-unfontify-pos
                     (or (previous-single-property-change
                          lisp-jit-lock-context-unfontify-pos
                          'lisp-jit-lock-defer-multiline)
                         (point-min))))
             (with-buffer-prepared-for-lisp-jit-lock
                 ;; Force contextual refontification.
                 (remove-text-properties
                  lisp-jit-lock-context-unfontify-pos (point-max)
                  '(fontified nil lisp-jit-lock-defer-multiline nil)))
             (setq lisp-jit-lock-context-unfontify-pos (point-max)))))))))

(defvar lisp-jit-lock-start) (defvar lisp-jit-lock-end) ; Dynamically scoped variables.
(defvar lisp-jit-lock-after-change-extend-region-functions nil
  "Hook that can extend the text to refontify after a change.
This is run after every buffer change.  The functions are called with
the three arguments of `after-change-functions': START END OLD-LEN.
The extended region to refontify is returned indirectly by modifying
the variables `lisp-jit-lock-start' and `lisp-jit-lock-end'.

Note that extending the region this way is not strictly necessary, except
that the nature of the redisplay code tends to otherwise leave some of
the rehighlighted text displayed with the old highlight until the next
redisplay (see comment about repeated redisplay in `lisp-jit-lock-fontify-now').")



(defsubst lisp-jit-lock-up-list-until-match (regexp)
  (while (and (not (= 0 (current-column)))
              (not (looking-at-p regexp)))
    (backward-up-list)))

(defvar *lisp-jit-lock-form-regexp*
  "(\\(?:\\sw\\|\\s_\\)+\\_>"
  "Regexp used to find form start relative to current position to
initiate fontification update.")

(defun refresh-lisp-jit-lock-re-cache ()
  (setf *lisp-jit-lock-form-regexp*
        (concat "("
                (regexp-opt *sexpy-lisp-head-regexps*)
                "\\_>")))



(defun lisp-jit-lock-after-change (start end old-len)
  "START, END - positions of the beginning and end of the
range of changed text."
  (when (and lisp-jit-lock-mode (not memory-full))
    (let ((lisp-jit-lock-start start)
          (lisp-jit-lock-end end)
          (inhibit-point-motion-hooks t))
      (with-silent-modifications
          (save-excursion
           (condition-case nil
               (progn
                 (backward-up-list)
                 (let ((end-of-current-sexp (end-of-sexp-at-point)))
                   (lisp-jit-lock-up-list-until-match *lisp-jit-lock-form-regexp*)
                   (setf lisp-jit-lock-start (point)
                         lisp-jit-lock-end (end-of-sexp-at-point)
                         ;; end-of-current-sexp
                         )
                   (message "REFONTIFY: %S"
                            (buffer-substring-no-properties
                             lisp-jit-lock-start
                             lisp-jit-lock-end))))
             (error)))
        ;; Make sure we change at least one char (in case of deletions).
        (setf lisp-jit-lock-end (min (max lisp-jit-lock-end (1+ start)) (point-max)))
        ;; Request refontification.
        (put-text-property lisp-jit-lock-start lisp-jit-lock-end 'fontified nil))
      ;; Mark the change for deferred contextual refontification.
      (when lisp-jit-lock-context-unfontify-pos
        (setq lisp-jit-lock-context-unfontify-pos
              ;; Here we use `start' because nothing guarantees that the
              ;; text between start and end will be otherwise refontified:
              ;; usually it will be refontified by virtue of being
              ;; displayed, but if it's outside of any displayed area in the
              ;; buffer, only lisp-jit-lock-context-* will re-fontify it.
              (min lisp-jit-lock-context-unfontify-pos lisp-jit-lock-start))))))


;;;

(eval-after-load
 "font-lock"
 '(progn
   (redefun font-lock-turn-on-thing-lock ()
     (case (font-lock-value-in-major-mode font-lock-support-mode)
       (fast-lock-mode (fast-lock-mode t))
       (lazy-lock-mode (lazy-lock-mode t))
       (jit-lock-mode
        ;; Prepare for jit-lock
        (remove-hook 'after-change-functions
                     'font-lock-after-change-function t)
        (set (make-local-variable 'font-lock-fontify-buffer-function)
             'jit-lock-refontify)
        ;; Don't fontify eagerly (and don't abort if the buffer is large).
        (set (make-local-variable 'font-lock-fontified) t)
        ;; Use jit-lock.
        (jit-lock-register 'font-lock-fontify-region
                           (not font-lock-keywords-only))
        ;; Tell jit-lock how we extend the region to refontify.
        (add-hook 'jit-lock-after-change-extend-region-functions
                  'font-lock-extend-jit-lock-region-after-change
                  nil t))
       (lisp-jit-lock-mode
        ;; Prepare for jit-lock
        (remove-hook 'after-change-functions
                     'font-lock-after-change-function t)
        (set (make-local-variable 'font-lock-fontify-buffer-function)
             'lisp-jit-lock-refontify)
        ;; Don't fontify eagerly (and don't abort if the buffer is large).
        (set (make-local-variable 'font-lock-fontified) t)
        ;; Use jit-lock.
        (lisp-jit-lock-register 'font-lock-fontify-region
                                (not font-lock-keywords-only))
        ;; Tell jit-lock how we extend the region to refontify.
        (add-hook 'lisp-jit-lock-after-change-extend-region-functions
                  'font-lock-extend-jit-lock-region-after-change
                  nil t))))))


(provide 'lisp-jit-lock)

;;; jit-lock.el ends here
