;; compilation-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 29 February 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'common)
(require 'compile)

(setf compilation-always-kill t
      ;; don't ask - just save
      compilation-ask-about-save nil)

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
  :init
  (unless (compilation-buffer-p (current-buffer))
    (error "Not in a compilation buffer")))

;;; compilation info

(cl-defstruct (compilation-caller-info
               (:conc-name compilation-caller-info/))
  mode   ;; Mode from which compilation was started.
  compile-command
  buffer ;; Buffer from where compilation was started.
  )

(defparameter *compile-caller-info* nil
  "Structure containing information about buffer, major mode etc.
from where current compile command was invoked. Should be cleared
up by functions in compilation-finish-functions.")

(defadvice compilation-start (before
                              compilation-start-store-info
                              activate
                              compile)
  "Record information about caller of compile command into
`*compile-caller-info*'"
  (setf *compile-caller-info*
        (make-compilation-caller-info
         :mode major-mode
         :compile-command compile-command
         :buffer (current-buffer))))

(cl-defstruct (compilation-error
               (:conc-name compilation-error/))
  compilation-root-directory
  filename
  line-number
  column-number)

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
     (normalize-file-name
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

(defun compilation/find-buffer (filename root)
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
error location - list of (filename line column)."
  (aif (compilation/find-buffer
        (compilation-error/filename err)
        (compilation-error/compilation-root-directory err))
    (funcall (if other-window
               #'switch-to-buffer-other-window
               #'switch-to-buffer)
             it)
    (error "Buffer for file %s not found" (compilation-error/filename err)))
  (vim:save-position)
  (goto-line1 (compilation-error/line-number err))
  (awhen (compilation-error/column-number err)
    (move-to-column it)))

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
       +vim-search-keys+
       +vim-mock:word-motion-keys+
       ("<up>"     compilation-jump-to-prev-error)
       ("<down>"   compilation-jump-to-next-error)
       ("t"        compilation-jump-to-prev-error)
       ("h"        compilation-jump-to-next-error)
       ("M-p"      nil)
       ("q"        remove-buffer)
       ("C-c C-c"  kill-compilation)
       ("m"        vim-mock:motion-jump-item)
       ("0"        vim-mock:motion-beginning-of-line-or-digit-argument)
       ("^"        vim-mock:motion-first-non-blank)
       ("$"        vim-mock:motion-end-of-line)

       ("C-v"      set-mark-command)
       ("C-y"      copy-region-as-kill)
       ("v"        set-mark-command)
       ("y"        copy-region-as-kill)

       (("C-m" "<f9>") recompile)
       ("SPC"          compile-goto-error))))

(vim:defcmd vim:recompile (nonrepeatable)
  (recompile))

(provide 'compilation-setup)

;; Local Variables:
;; End:

;; compilation-setup.el ends here
