;;; json-error.el --- JSON syntax error highlighting

;; Author:  Peter Sanford (psanford@petersdanceparty.com)
;; Keywords:  json

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This is a json minor mode mode that supports full json parsing. With
;; a full parser we are able to provide accurate syntax-error checking.

;;; Code:

(defgroup json-error-mode nil
  "JSON editing mode"
  :group 'languages)

(defface json-error-error-face
  `((t (:inherit error)))
  "Face for JSON errors."
  :group 'json-error-mode)

;; json-error-mode state
(make-local-variable 'json-error-mode-parsing) ; in the middle of a parse
(make-local-variable 'json-error-mode-buffer-dirty-p)
(make-local-variable 'json-error-mode-parse-timer)
(make-local-variable 'json-error-parsed-errors)
(defvar json-error-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.")

(define-minor-mode json-error-mode
  "Minor mode to add error highlighting to json mode"
  :init-value nil

  (if json-error-mode
      (json-error-mode-enter)
    (json-error-mode-exit)))

(defun json-error-mode-enter ()
  (setq json-error-mode-parsing nil)
  (setq json-error-mode-buffer-dirty-p t)
  (setq json-error-mode-parse-timer nil)
  (setq json-error-parsed-errors nil)

  (add-hook 'change-major-mode-hook #'json-error-mode-exit nil t)
  (add-hook 'after-change-functions #'json-error-mode-edit nil t)

  (json-error-reparse))

(defun json-error-mode-exit ()
  (interactive)
  (json-error-remove-overlays)
  (remove-hook 'after-change-functions #'json-error-mode-edit t)
  (remove-hook 'change-major-mode-hook #'json-error-mode-exit t))

(defun json-error-mode-reset-timer ()
  (if json-error-mode-parse-timer
      (cancel-timer json-error-mode-parse-timer))
  (setq json-error-mode-parsing nil)
  (setq json-error-mode-parse-timer
        (run-with-idle-timer json-error-idle-timer-delay nil #'json-error-hook-timer (current-buffer))))

(defun json-error-hook-timer (buf)
  (json-error-reparse buf))

(defun json-error-mode-edit (beg end len)
  "Schedule a new parse after buffer is edited."
  (setq json-error-mode-buffer-dirty-p t)
  (json-error-mode-reset-timer))


(defun json-error-reparse (&optional buffer)
  "Re-parse current buffer after user finishes some data entry.
If we get any user input while parsing, including cursor motion,
we discard the parse and reschedule it."
  (with-current-buffer (or buffer (current-buffer))
    (let (interrupted-p)
      (unless json-error-mode-parsing
        (setq json-error-mode-parsing t)
        (unwind-protect
            (when json-error-mode-buffer-dirty-p
              (with-silent-modifications
                (setq json-error-mode-buffer-dirty-p nil)
                (setq json-error-parsed-errors '())
                (json-error-remove-overlays)
                (setq interrupted-p
                      (catch 'interrupted
                        (json-error-parse-buffer)
                        (json-error-mode-show-errors)
                        nil))
                (when interrupted-p
                  ;; unfinished parse => try again
                  (setq json-error-mode-buffer-dirty-p t)
                  (json-error-mode-reset-timer))))
          ;; finally
          (setq json-error-mode-parsing nil)
          (unless interrupted-p
            (setq json-error-mode-parse-timer nil)))))))

(defun json-error-clear-face (beg end)
  (remove-text-properties beg end '(help-echo nil
                                    point-entered nil)))


(defun json-error-mode-show-errors ()
  "Highlight syntax errors."
  (json-error-clear-face (point-min) (point-max))
  (dolist (e json-error-parsed-errors)
    (let* ((msg (first e))
           (pos (second e))
           (beg (max (point-min) (min (- pos 1) (point-max))))
           (end (point-max))
           (ovl (make-overlay beg end)))

      (overlay-put ovl 'font-lock-face 'json-error-error-face)
      (overlay-put ovl 'json-error-error t)
      (put-text-property beg end 'help-echo msg)
      (put-text-property beg end 'point-entered #'json-error-echo-error))))

(defun json-error-remove-overlays ()
  "Remove overlays from buffer that have a `json-error-error' property."
  (let ((beg (point-min))
        (end (point-max)))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (overlay-get o 'json-error-error)
          (delete-overlay o))))))

(defun json-error-echo-error (old-point new-point)
  (let ((msg (get-text-property new-point 'help-echo)))
    (if msg
        (message msg))))

(defun json-error-parse-buffer ()
  (json-error-init-scanner)
  (let (c state)
    (catch 'done
      (while t
        (setq c (json-error-next-char))
        (when (eq c json-error-EOF-CHAR)
          (throw 'done nil))
        (setq state (funcall json-error-step c))
        (when (>= state json-error-scan-skip-space)
          (when (= state json-error-scan-error)
            (push (list json-error-error json-error-cursor json-error-lineno json-error-line-offset)
                  json-error-parsed-errors)
            (throw 'done nil)))))
    ))

;; Internal parser state:

(defvar json-error-EOF-CHAR -1)

;; Continue.
(defvar json-error-scan-continue 0)
(defvar json-error-scan-begin-literal 1)
(defvar json-error-scan-begin-object 2)
(defvar json-error-scan-object-key 3)
(defvar json-error-scan-object-value 4)
(defvar json-error-scan-end-object 5)
(defvar json-error-scan-begin-array 6)
(defvar json-error-scan-array-value 7)
(defvar json-error-scan-end-array 8)
(defvar json-error-scan-skip-space 9)
;; Stop.
(defvar json-error-scan-end 10)
(defvar json-error-scan-error 11)

;; parser state
(make-local-variable 'json-error-cursor) ; current position
(make-local-variable 'json-error-step) ; next parse function
(make-local-variable 'json-error-parse-state) ; Stack of what we are in the middle of
(make-local-variable 'json-error-error) ; error that happened, if any
(make-local-variable 'json-error-lineno) ; line number
(make-local-variable 'json-error-line-offset) ; char offset in line


(defun json-error-init-scanner (&optional buf)
  (save-excursion
    (when buf (set-buffer buf))
    (setq
     json-error-cursor (point-min)
     json-error-step 'json-error-state-begin-value
     json-error-parse-state '()
     json-error-error nil
     json-error-lineno 1
     json-error-line-offset 0)))

(defun json-error-next-char ()
  (let (c)
    (if (>= json-error-cursor (point-max))
        (setq c json-error-EOF-CHAR)
      (setq c (char-after json-error-cursor))
      (when (= c ?\n)
        (setq json-error-lineno (+ 1 json-error-lineno))
        (setq json-error-line-offset -1))
      (setq json-error-cursor (+ 1 json-error-cursor))
      (setq json-error-line-offset (+ 1 json-error-line-offset)))
    c))

(defun json-error-state-error (c)
  "state after reaching a syntax error"
  json-error-scan-error)

(defun json-error-set-error (c context)
  "record error and switch to error state"
  (setq json-error-step 'json-error-state-error
        json-error-error (format "invalid character '%c' %s" c context))
  json-error-scan-error)

(defsubst json-error-is-space (c)
  "returns t if character is whitespace"
  (case c
    (?\s t)
    (?\t t)
    (?\r t)
    (?\n t)
    (t nil)))

(defun json-error-state-begin-value-or-empty (c)
  "state after reading `[`"
  (cond
   ((json-error-is-space c)
    json-error-scan-skip-space)
   ((= c ?\])
    (json-error-state-end-value c))
   (t
    (json-error-state-begin-value c))))

(defun json-error-state-begin-value (c)
  "State at the beginning of the input."
  (case c
    (?\s json-error-scan-skip-space)
    (?\t json-error-scan-skip-space)
    (?\r json-error-scan-skip-space)
    (?\n json-error-scan-skip-space)
    (?{  (progn
           (setq json-error-step 'json-error-state-begin-string-or-empty)
           (push 'json-error-parse-object-key json-error-parse-state)
           json-error-scan-begin-object))
    (?\[ (progn
           (setq json-error-step 'json-error-state-begin-value-or-empty)
           (push 'json-error-parse-array-value json-error-parse-state)
           json-error-scan-begin-array))
    (?\" (progn
           (setq json-error-step 'json-error-state-in-string)
           json-error-scan-begin-literal))
    (?-  (progn
           (setq json-error-step 'json-error-state-neg)
           json-error-scan-begin-literal))
    (?0  (progn                         ; beginning of 0.123
           (setq json-error-step 'json-error-state-0)
           json-error-scan-begin-literal))
    (?t  (progn                         ; beginning of true
           (setq json-error-step 'json-error-state-t)
           json-error-scan-begin-literal))
    (?f  (progn                         ; beginning of false
           (setq json-error-step 'json-error-state-f)
           json-error-scan-begin-literal))
    (?n  (progn                         ; beginning of null
           (setq json-error-step 'json-error-state-n)
           json-error-scan-begin-literal))
    (t
     (if (and (<= ?1 c) (<= c ?9))
         (progn                         ; beginning of 1234.5
           (setq json-error-step 'json-error-state-1)
           json-error-scan-begin-literal)
                                        ; else error
       (json-error-set-error c "looking for beginning of value")))))


(defun json-error-state-begin-string-or-empty (c)
  "state after reading ?{"
  (cond
   ((json-error-is-space c)
    json-error-scan-skip-space)
   ((= c ?})
    (pop json-error-parse-state)
    (push 'json-error-parse-object-value json-error-parse-state)
    (json-error-state-end-value c))
   (t
    (json-error-state-begin-string c))))

(defun json-error-state-begin-string (c)
  "state after reading `{\"key\": value,`"
  (cond
   ((json-error-is-space c) json-error-scan-skip-space)
   ((= c ?\")
    (setq json-error-step 'json-error-state-in-string)
    json-error-scan-begin-literal)
   (t (json-error-set-error c "looking for beginning of object key string"))))


(defun json-error-state-end-value (c)
  "state after completing a value, such as after reading '{}' or 'true'"
  (catch 'return
    (let ((ps (first json-error-parse-state)))
      (cond
       ((= 0 (length json-error-parse-state))
        ;; completed top-level before the current char
        (setq json-error-step 'json-error-state-end-top)
        (throw 'return (json-error-state-end-top c)))
       ((json-error-is-space c)
        (setq json-error-step 'json-error-state-end-value)
        (throw 'return json-error-scan-skip-space))
       ((eq ps 'json-error-parse-object-key)
        (when (= c ?:)
          (pop json-error-parse-state)
          (push 'json-error-parse-object-value json-error-parse-state)
          (setq json-error-step 'json-error-state-begin-value)
          (throw 'return json-error-scan-object-key))
        (throw 'return (json-error-set-error c "after object key")))
       ((eq ps 'json-error-parse-object-value)
        (when (= c ?,)
          (pop json-error-parse-state)
          (push 'json-error-parse-object-key json-error-parse-state)
          (setq json-error-step 'json-error-state-begin-string)
          (throw 'return json-error-scan-object-value))
        (when (= c ?})
          (pop json-error-parse-state)
          (if (= 0 (length json-error-parse-state))
              (setq json-error-step 'json-error-state-end-top)
            (setq json-error-step 'json-error-state-end-value))
          (throw 'return json-error-scan-end-object))
        (throw 'return (json-error-set-error c "after object key:value pair")))
       ((eq ps 'json-error-parse-array-value)
        (when (= c ?,)
          (setq json-error-step 'json-error-state-begin-value)
          (throw 'return json-error-scan-array-value))
        (when (= c ?\])
          (pop json-error-parse-state)
          (if (= 0 (length json-error-parse-state))
              (setq json-error-step 'json-error-state-end-top)
            (setq json-error-step 'json-error-state-end-value))
          (throw 'return json-error-scan-end-array))
        (throw 'return (json-error-set-error c "after array element")))
       (t (throw 'return (json-error-set-error c "")))))))

(defun json-error-state-end-top (c)
  "state after finishing the top-level value such as `{}`.
   Only space characters should be seen now"
  (if (json-error-is-space c)
      json-error-scan-end
    (json-error-set-error c "after top-level value")))

(defun json-error-state-in-string (c)
  (cond
   ((= c ?\")
    (setq json-error-step 'json-error-state-end-value)
    json-error-scan-continue)
   ((= c ?\\)
    (setq json-error-step 'json-error-state-in-string-esc)
    json-error-scan-continue)
   ((< c #x20)
    (json-error-set-error c "in string literal"))
   (t json-error-scan-continue)))

(defun json-error-state-in-string-esc (c)
  (cond
   ((or (= c ?b)
        (= c ?f)
        (= c ?n)
        (= c ?r)
        (= c ?t)
        (= c ?\\)
        (= c ?/)
        (= c ?\"))
    (setq json-error-step 'json-error-state-in-string)
    json-error-scan-continue)
   ((= c ?u)
    (setq json-error-step 'json-error-state-in-string-esc-u)
    json-error-scan-continue)
   (t
    (json-error-set-error c "in string escape code"))))

(defun json-error-state-in-string-esc-u (c)
  (cond
   ((or
     (and (<= ?0 c) (<= c ?9))
     (and (<= ?a c) (<= c ?f))
     (and (<= ?A c) (<= c ?F)))
    (setq json-error-step 'json-error-state-in-string-esc-u1)
    json-error-scan-continue)
   (t
    (json-error-set-error c "in \\u hexadecimal character escape"))))

(defun json-error-state-in-string-esc-u1 (c)
  (cond
   ((or
     (and (<= ?0 c) (<= c ?9))
     (and (<= ?a c) (<= c ?f))
     (and (<= ?A c) (<= c ?F)))
    (setq json-error-step 'json-error-state-in-string-esc-u12)
    json-error-scan-continue)
   (t
    (json-error-set-error c "in \\u hexadecimal character escape"))))

(defun json-error-state-in-string-esc-u12 (c)
  (cond
   ((or
     (and (<= ?0 c) (<= c ?9))
     (and (<= ?a c) (<= c ?f))
     (and (<= ?A c) (<= c ?F)))
    (setq json-error-step 'json-error-state-in-string-esc-u123)
    json-error-scan-continue)
   (t
    (json-error-set-error c "in \\u hexadecimal character escape"))))

(defun json-error-state-in-string-esc-u123 (c)
  (cond
   ((or
     (and (<= ?0 c) (<= c ?9))
     (and (<= ?a c) (<= c ?f))
     (and (<= ?A c) (<= c ?F)))
    (setq json-error-step 'json-error-state-in-string)
    json-error-scan-continue)
   (t
    (json-error-set-error c "in \\u hexadecimal character escape"))))

(defun json-error-state-neg (c)
  "after reading - during a number"
  (cond
   ((= c ?0)
    (setq json-error-step 'json-error-state-0)
    json-error-scan-continue)
   ((and (<= ?1 c) (<= c ?9))
    (setq json-error-step 'json-error-state-1)
    json-error-scan-continue)
   (t
    (json-error-set-error c "in numeric literal"))))

(defun json-error-state-0 (c)
  "after reading `0' during a number"
  (cond
   ((= c ?.)
    (setq json-error-step 'json-error-state-dot)
    json-error-scan-continue)
   ((or (= c ?e) (= c ?E))
    (setq json-error-step 'json-error-state-E)
    json-error-scan-continue)
   (t (json-error-state-end-value c))))

(defun json-error-state-1 (c)
  "state-1 is the state after reading a non-zero interger during a number,
such as after reading `1` or `100` but not `0`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-error-step 'json-error-state-1)
    json-error-scan-continue)
   (t (json-error-state-0 c))))

(defun json-error-state-dot (c)
  "state afeter reading the integer and decimal point in a number
such as after reading `1.`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-error-step 'json-error-state-dot-0)
    json-error-scan-continue)
   (t (json-error-set-error c "after decimal point in numeric literal"))))

(defun json-error-state-dot-0 (c)
  "stae after reading an integer, decimal point, and subsequent digits
of a number, such as after reading `3.14`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-error-step 'json-error-state-dot-0)
    json-error-scan-continue)
   ((or (= c ?e) (= c ?E))
    (setq json-error-step 'json-error-state-E)
    json-error-scan-continue)
   (t (json-error-state-end-value c))))

(defun json-error-state-E (c)
  "state after reading the mantissa and e in a number,
such as after reading `314e` or `0.314e`"
  (cond
   ((= c ?+)
    (setq json-error-step 'json-error-state-E-sign)
    json-error-scan-continue)
   ((= c ?-)
    (setq json-error-step 'json-error-state-E-sign)
    json-error-scan-continue)
   (t (json-error-state-E-sign c))))

(defun json-error-state-E-sign (c)
  "state after reading the mantissa, e, and sign in a number,
such as after reading `314e-` or `0.314e+`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-error-step 'json-error-state-E-0)
    json-error-scan-continue)
   (t (json-error-set-error c "in exponent of numeric literal"))))

(defun json-error-state-E-0 (c)
  "state after reading mantissa, e, optional sign, and at least
one digit of the exponent in a number, such as `314e-2`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-error-step 'json-error-state-E-0)
    json-error-scan-continue)
   (t (json-error-state-end-value c))))

(defun json-error-state-t (c)
  "state after reading `t`"
  (cond
   ((= c ?r)
    (setq json-error-step 'json-error-state-tr)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal true (expecting 'r')"))))

(defun json-error-state-tr (c)
  "state after reading `tr`"
  (cond
   ((= c ?u)
    (setq json-error-step 'json-error-state-tru)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal true (expecting 'u')"))))

(defun json-error-state-tru (c)
  "state after reading `tru`"
  (cond
   ((= c ?e)
    (setq json-error-step 'json-error-state-end-value)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal true (expecting 'e')"))))

(defun json-error-state-f (c)
  "state after reading `f`"
  (cond
   ((= c ?a)
    (setq json-error-step 'json-error-state-fa)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal false (expecting 'a')"))))

(defun json-error-state-fa (c)
  "state after reading `fa`"
  (cond
   ((= c ?l)
    (setq json-error-step 'json-error-state-fal)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal false (expecting 'l')"))))

(defun json-error-state-fal (c)
  "state after reading `fal`"
  (cond
   ((= c ?s)
    (setq json-error-step 'json-error-state-fals)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal false (expecting 's')"))))

(defun json-error-state-fals (c)
  "state after reading `fals`"
  (cond
   ((= c ?e)
    (setq json-error-step 'json-error-state-end-value)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal false (expecting 'e')"))))

(defun json-error-state-n (c)
  "state after reading `n`"
  (cond
   ((= c ?u)
    (setq json-error-step 'json-error-state-nu)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal null (expecting 'u')"))))

(defun json-error-state-nu (c)
  "state after reading `nu`"
  (cond
   ((= c ?l)
    (setq json-error-step 'json-error-state-nul)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal null (expecting 'l')"))))

(defun json-error-state-nul (c)
  "state after reading `nu`"
  (cond
   ((= c ?l)
    (setq json-error-step 'json-error-state-end-value)
    json-error-scan-continue)
   (t (json-error-set-error c "in literal null (expecting 'l')"))))

(provide 'json-error)

;;; json-error.el ends here
