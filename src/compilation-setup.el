;; compilation-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 29 February 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'compile)

(setf compilation-always-kill t)

(defvar-local *compilation-jump-error-regexp*
  (rxx ((delim (or "/"
                   "\\"))
        (filename (+ (not (any ?/ ?\\ ?\n ?\t)))))
    bol
    (group (*? (? (or "."
                      ".."))
               delim
               filename))
    (? delim)
    filename
    ":"
    (group (+ (any (?0 . ?9))))
    ":"
    (group (+ (any (?0 . ?9))))
    ":")
  "Regexp which is used by `compilation-jump-to-next-error'
and `compilation-jump-to-prev-error' to detect errors
in compilation or related buffers")

(define-circular-jumps
    compilation-jump-to-next-error
    compilation-jump-to-prev-error
  *compilation-jump-error-regexp*
  (unless (or (compilation-buffer-p (current-buffer))
              (eq? major-mode 'ghc-check-mode))
    (error "Not in a compilation buffer")))

;;; compilation info

(defparameter *compile-caller-info* nil
  "Alist containing information about buffer, major mode etc.
from where current compile command was invoked. Should be cleared
up by functions in compilation-finish-functions.")

(defadvice compilation-start (before
                              compilation-start-store-info
                              activate
                              compile)
  "Record information about caller of compile command into
`*compile-caller-info*'"
  (setf *compile-caller-info* `((mode . ,major-mode)
                                (compile-command . ,compile-command)
                                (buffer . ,(current-buffer)))))


(defun compilation/parse-matched-error-entry (entry)
  "Parse ENTRY and return (<filename> [<line>] [<column>]) of previously matched
error message. <line> and <column> are nullable.
ENTRY should be of format used by `compilation-error-regexp-alist'."
  (let* ((file-group (cadr entry))
         (strip-cons (lambda (x)
                       (if (cons? x)
                         (car x)
                         x)))
         (line-group
          (funcall strip-cons (car-safe (cdr-safe (cdr entry)))))
         (column-group
          (funcall strip-cons (car-safe (cdr-safe (cdr-safe (cdr entry)))))))
    (values (normalize-file-name (match-string-no-properties file-group))
            (when (and (not (null? line-group))
                       ;; it turns out that someone may put lambdas here,
                       ;; e.g. grep...
                       (integer? line-group))
              (string->number (match-string-no-properties line-group)))
            (when (and (not (null? column-group))
                       ;; it turns out that someone may put lambdas here,
                       ;; e.g. grep...
                       (integer? column-group))
              (- (string->number (match-string-no-properties column-group))
                 compilation-first-column)))))

(defun compilation/get-selected-error ()
  "Return filename, line and column for error or warning on current line
\(i.e. the selected one), depending on `compilation-error-regexp-alist'."
  (save-excursion
    (save-match-data
      (when-let (entry (find-if (lambda (alist-entry)
                                  (save-excursion
                                    (let ((regexp (car alist-entry)))
                                      (when (< 0 (length regexp))
                                        ;; a bit hacky beginning-of-line call
                                        (when (char=? (aref regexp 0) ?^)
                                          (beginning-of-line))
                                        (looking-at regexp)))))
                                ;; (comp #'looking-at #'car)
                                compilation-error-regexp-alist))
        (compilation/parse-matched-error-entry entry)))))

(defun compilation/find-buffer (filename)
  "Get buffer that corresponds to FILENAME, which may be neither full nor relative
path, in which case filename with suffix equal to FILENAME will be tried."
  (assert (not (= 0 (length filename))))
  (aif (find-if (lambda (buf)
                  (string-suffix? filename (buffer-file-name buf)))
                (visible-buffers))
    it
    (when (file-exists? filename)
      (cond ((get-file-buffer filename)
             (get-file-buffer filename))
            (t
             (find-file-noselect filename))))))

(defun compilation/jump-to-error (err &optional other-window)
  "Jump to source of compilation error. ERR should be structure describing
error location - list of (filename line column)."
  (destructuring-bind (filename line column) err
    (aif (compilation/find-buffer filename)
      (funcall (if other-window
                 #'switch-to-buffer-other-window
                 #'switch-to-buffer)
               it)
      (error "File %s not found" filename))
    (vim:save-position)
    (goto-line line)
    (when column
      (move-to-column column))))

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

(eval-after-load "compile"
  '(progn
     (def-keys-for-map compilation-mode-map
       +vi-keys+
       +vim-special-keys+
       +vi-search-keys+
       +vim-word-motion-keys+
       ("<up>"     compilation-jump-to-prev-error)
       ("<down>"   compilation-jump-to-next-error)
       ("t"        compilation-jump-to-prev-error)
       ("h"        compilation-jump-to-next-error)
       ("M-p"      nil)
       ("q"        remove-buffer)
       ("C-c C-c"  kill-compilation)

       ("C-v"      set-mark-command)
       ("C-y"      copy-region-as-kill)
       ("v"        set-mark-command)
       ("y"        copy-region-as-kill)

       ("<f9>"     recompile)
       ("`"        recompile)
       ("SPC"      compile-goto-error))))

(provide 'compilation-setup)

;; Local Variables:
;; End:

;; compilation-setup.el ends here
