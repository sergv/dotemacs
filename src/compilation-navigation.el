;; compilation-navigation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 25 November 2019
;; Description:

(require 'common)
(require 'compile)

(cl-defstruct (compilation-error
               (:conc-name compilation-error/))
  compilation-root-directory
  filename
  line-number
  column-number)

;;;###autoload
(defun compilation/get-selected-error ()
  "Return filename, line and column for error or warning on current line
\(i.e. the selected one), depending on `compilation-error-regexp-alist'."
  (save-excursion
    (save-match-data
      (when-let (err-entry (find-if (lambda (entry)
                                      (save-excursion
                                        (let ((regexp (car entry)))
                                          (when (< 0 (length regexp))
                                            (or (looking-at regexp)
                                                (progn
                                                  (beginning-of-line)
                                                  (looking-at regexp)))))))
                                    (-map (lambda (entry)
                                            (if (symbolp entry)
                                                (cdr-safe
                                                 (assq
                                                  entry
                                                  compilation-error-regexp-alist-alist))
                                              entry))
                                          compilation-error-regexp-alist)))
        (compilation/parse-matched-error-entry err-entry)))))


(defun compilation/parse-matched-error-entry (entry)
  "Parse ENTRY and return `compilation-error' structure for
previously matched error message.

ENTRY should be of format used by `compilation-error-regexp-alist'."
  (let* ((file-group (cadr entry))
         (strip-cons (lambda (x)
                       (if (consp x)
                         (car x)
                         x)))
         (line-group
          (funcall strip-cons (cadr-safe (cdr entry))))
         (column-group
          (funcall strip-cons (cadr-safe (cdr-safe (cdr entry))))))
    (make-compilation-error
     :compilation-root-directory default-directory
     :filename
     (normalise-file-name
      (trim-whitespace
       (match-string-no-properties file-group)))
     :line-number
     (when (and line-group
                ;; it turns out that someone may put lambdas here,
                ;; e.g. grep...
                (integerp line-group))
       (awhen (match-string-no-properties line-group)
         (string->number it)))
     :column-number
     (when (and column-group
                ;; it turns out that someone may put lambdas here,
                ;; e.g. grep...
                (integerp column-group))
       (awhen (match-string-no-properties column-group)
         (- (string->number it)
            compilation-first-column))))))



(defun compilation/find-buffer (filename &optional root)
  "Get buffer that corresponds to FILENAME, which may be neither full nor
relative path. In case it's neither, the filename with suffix equal to FILENAME
will searched for."
  (cl-assert (not (= 0 (length filename))))
  (aif (find-if (lambda (buf)
                  (string-suffix-p filename (buffer-file-name buf)))
                (visible-buffers))
      it
    (let ((resolved-filename (resolve-to-abs-path filename root)))
      (aif (get-file-buffer resolved-filename)
          it
        (find-file-noselect resolved-filename)))))

(defun compilation/jump-to-error (err &optional other-window)
  "Jump to source of compilation error. ERR should be structure describing
error location - value of compilation-error structure."
  (aif (compilation/find-buffer
        (compilation-error/filename err)
        (compilation-error/compilation-root-directory err))
    (funcall (if other-window
               #'switch-to-buffer-other-window
               #'switch-to-buffer)
             it)
    (error "Could not find buffer for file %s" (compilation-error/filename err)))
  (vim:save-position)
  (goto-line-dumb (compilation-error/line-number err))
  (awhen (compilation-error/column-number err)
    (move-to-character-column it)))

(defun compilation/goto-error ()
  "Jump to location of error or warning (file, line and column) in current window."
  (interactive)
  (when-let (err (compilation/get-selected-error))
    (compilation/jump-to-error err nil)))

(defun compilation/goto-error-other-window ()
  "Jump to location of error or warning (file, line and column) in other window."
  (interactive)
  (when-let (err (compilation/get-selected-error))
    (compilation/jump-to-error err t)))





(defun compilation-navigation--use-selected-error-or-jump-to-next (win buf jump-to-next-err-func)
  "Either return error currently selected in the compilation buffer BUF, if
point is not located on it, or return the next error if current position argees
with the position of the selected error."
  (let ((curr-buf (current-buffer))
        (line (line-number-at-pos)))
    (with-selected-window win
      (with-current-buffer buf
        (if-let ((selected-err (compilation/get-selected-error)))
            (if (and
                 (eq (compilation/find-buffer
                      (compilation-error/filename selected-err)
                      (compilation-error/compilation-root-directory selected-err))
                     curr-buf)
                 (equal (compilation-error/line-number selected-err)
                        line))
                ;; If we're already on the selected error then jump to next error.
                (progn
                  (funcall jump-to-next-err-func)
                  (compilation/get-selected-error))
              selected-err)
          (progn
            (funcall jump-to-next-err-func)
            (compilation/get-selected-error)))))))

(defun compilation-navigation--go-navigate-errors (buf jump-to-next-err-func fallback)
  "Navigate errors in compilation buffer BUF."
  (cl-assert (bufferp buf))
  (if (buffer-live-p buf)
      (let ((win (get-buffer-window buf
                                    t ;; all-frames
                                    )))
        (if (and win
                 (window-live-p win))
            (if-let (err (compilation-navigation--use-selected-error-or-jump-to-next
                          win
                          buf
                          jump-to-next-err-func))
                (compilation/jump-to-error err nil)
              (funcall fallback))
          (funcall fallback)))
    (funcall fallback)))

;;;###autoload
(defun compilation-navigation-next-error-in-buffer-other-window (buf)
  "Select next error in compilation buffer BUF and jump to
it's position in current window."
  (compilation-navigation--go-navigate-errors
   buf
   #'compilation-jump-to-next-error
   #'flycheck-next-error))

;;;###autoload
(defun compilation-navigation-prev-error-in-buffer-other-window (buf)
  "Select previous error in compilation buffer BUF and jump to
it's position in current window."
  (compilation-navigation--go-navigate-errors
   buf
   #'compilation-jump-to-prev-error
   #'flycheck-previous-error))

;; Local Variables:
;; End:

(provide 'compilation-navigation)

;; compilation-navigation.el ends here
