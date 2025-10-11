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

(defcustom json-error-highlighting-style 'context
  "Style of error highlighting.
`following' - Highlight everything from the error position to end of buffer.
`context' - Highlight just the line containing the error."
  :type '(choice (const :tag "Following (highlight from error to EOF)" following)
                 (const :tag "Context (highlight error line)" context))
  :group 'json-error-mode)

;; json-error-mode state
(defvar-local json-error-mode-parsing nil) ; in the middle of a parse
(defvar-local json-error-mode-buffer-dirty-p nil)
(defvar-local json-error-mode-parse-timer nil)
(defvar-local json-error-parsed-errors nil)
(defvar json-error-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.")

;;;###autoload
(define-minor-mode json-error-mode
  "Minor mode to add error highlighting to json mode"
  :init-value nil
  (if json-error-mode
      (json-error-mode-enter)
    (json-error-mode-exit)))

(defun json-error-mode-enter ()
  (setq json-error-mode-parsing nil
        json-error-mode-buffer-dirty-p t
        json-error-mode-parse-timer nil
        json-error-parsed-errors nil)

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
    (let* ((msg (car e)) ; first
           (pos (cadr e)) ; second
           (error-point (max (point-min) (min pos (point-max))))
           beg end)

      (cond
       ;; Following style: highlight from error to end of buffer
       ((eq json-error-highlighting-style 'following)
        (setq beg error-point)
        (setq end (point-max)))

       ;; Context style: highlight just the error line
       ((eq json-error-highlighting-style 'context)
        (let ((highlight-point error-point))
          ;; Determine which line to highlight based on error type
          (cond
           ;; For string errors (newline, premature end), highlight the line containing the error
           ;; Check this first since "unexpected newline" contains "expected"
           ((or (string-match "newline" msg)
                (string-match "premature end" msg))
            ;; For newline errors, the error is at the newline character
            ;; We want to highlight the line containing the unterminated string (before the newline)
            (if (and (string-match "newline" msg)
                     (= (char-after error-point) ?\n))
                ;; The error is at a newline, highlight the previous line
                (setq highlight-point (1- error-point))
              ;; For other string errors, use the error position directly
              (setq highlight-point error-point)))

           ;; For comma-related "expected" errors at the beginning of a line, highlight previous line
           ;; But not for colon errors - those should highlight the current line
           ((and (string-match "expected" msg)
                 (not (string-match "':'" msg))  ; Don't treat missing colons as previous-line errors
                 (save-excursion
                   (goto-char error-point)
                   (beginning-of-line)
                   ;; Check if we're at the beginning of a line with content
                   ;; Include numeric values, true, false, null for array elements
                   (looking-at "\\s-*[\"'{\\[0-9tfn]")))
            (setq highlight-point
                  (save-excursion
                    (goto-char error-point)
                    (forward-line -1)
                    (point)))))

          ;; Find the beginning and end of the line to highlight
          (setq beg (save-excursion
                      (goto-char highlight-point)
                      (beginning-of-line)
                      (point)))
          (setq end (save-excursion
                      (goto-char highlight-point)
                      (end-of-line)
                      (point))))))

      (let ((ovl (make-overlay beg end)))
        (overlay-put ovl 'font-lock-face 'json-error-error-face)
        (overlay-put ovl 'json-error-error t)
        (put-text-property beg end 'help-echo msg)
        (put-text-property beg end 'point-entered #'json-error-echo-error)))))

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
  ;; Use the builtin json-parse-buffer for fast JSON validation
  (unless (fboundp 'json-parse-buffer)
    (error "json-parse-buffer is not available. This mode requires Emacs 27.1 or later."))

  (condition-case err
      (save-excursion
        (goto-char (point-min))
        (json-parse-buffer)
        ;; JSON is valid, no errors
        nil)
    (error
     ;; JSON is invalid, extract error position
     (let* ((error-data (cdr err))
            (error-msg (car error-data))
            (error-pos (when (>= (length error-data) 5)
                         (1+ (nth 4 error-data))))) ; Convert 0-based to 1-based
       (when error-pos
         (push (list error-msg error-pos nil nil)
               json-error-parsed-errors))))))

(provide 'json-error)

;;; json-error.el ends here
