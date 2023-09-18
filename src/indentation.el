;; indentation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 April 2017
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'current-column-fixed)

;;;###autoload
(defun setup-indent-size (width)
  (declare (pure nil) (side-effect-free nil))
  (setq-local vim-shift-width width
              standard-indent width
              ;; Affects only tab display.
              tab-width width
              tab-always-indent t))

(defvar *mode-indent-functions-table*
  (make-hash-table :test #'eq)
  "Hash table of (major-mode . function) pairs, where functions
should take no arguments and destructively indent current buffer.")

(defun indent-to! (col)
  "Indent current line to exactly COL'th column with spaces."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (delete-region (line-beginning-position) (point))
    (insert-char ?\s col)))

;;;###autoload
(defun reindent-file-noninteractive (filename)
  "Load FILENAME contents, try to infer mode for it, reindent according
to mode and write new contents back to FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((buffer-file-name filename))
      (normal-mode)
      (vim:indent:wrapper)
      (write-region (point-min) (point-max) filename))))

;;;###autoload
(defun indent-relative-backwards ()
  "Indent backwards (inefficiently). Dual to `indent-relative'."
  (interactive)
  (let ((current-point (point))
        (prev-indent 0))
    (goto-char (line-beginning-position))
    (save-excursion
      (while (< (point) current-point)
        (setf prev-indent (current-column-fixed-uncached))
        (indent-relative)))
    (move-to-column prev-indent)))

;;;###autoload
(defun indent-whole-buffer ()
  "Indent whole buffer with `indent-region'."
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun indent-relative-forward ()
  "Indent forwards similarly to `indent-relative'."
  (interactive)
  (indent-relative+ t))

;;;###autoload
(defun indent-relative-backward ()
  "Indent backwards similarly to `indent-relative'."
  (interactive)
  (indent-relative+ nil))

;; inspired by indent-relative for v24.3.1
;;;###autoload
(defun indent-relative+ (forward?)
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace.
The following line shows the indentation points in this line.
    ^         ^    ^     ^   ^           ^      ^  ^    ^
If the previous nonblank line has no indent points beyond the
column point starts at, `tab-to-tab-stop' is done instead, unless
this command is invoked with a numeric argument, in which case it
does nothing.

See also `indent-relative-maybe'."
  (save-match-data
    (let ((start-column (current-column-fixed))
          indent)
      (with-disabled-undo
       (with-inhibited-modification-hooks
        (with-inhibited-redisplay
          (with-expanded-invisible-overlays
              (max (save-excursion (haskell-move-to-topmost-start)
                                   (point))
                   (point-min))
              (point)
            (save-restriction
              (save-excursion
                (widen)
                (forward-line -1)
                (beginning-of-line)
                (while (and (not (bobp))
                            (or (haskell-on-blank-line-p)
                                (if (or forward?
                                        ;; If we start at column 0
                                        ;; then we do not want to iterate
                                        ;; all the way to the beginning of
                                        ;; the buffer.
                                        (= 0 start-column))
                                    (< start-column (indentation-size))
                                  ;; If we're looking backward then
                                  ;; we'd like to use a lite with less
                                  ;; indent than we currently have as
                                  ;; a reference.
                                  (<= start-column (indentation-size)))))
                  (forward-line -1))
                (let ((reference-line-indent (indentation-size)))
                  (let ((end (if forward?
                                 (min (+ (line-end-position) 1)
                                      (point-max))
                               (line-beginning-position))))
                    (move-to-column start-column)
                    ;; Is start-column inside a tab on this line?
                    (when (> (current-column-fixed-uncached) start-column)
                      (backward-char 1))
                    (if forward?
                        (if (= reference-line-indent start-column)
                            ;; Add extra one tab stop after 0th column.
                            (move-to-column (indent-next-tab-stop start-column))
                          (progn
                            (skip-chars-forward "^ \t" end)
                            (skip-chars-forward " \t" end)))
                      (progn
                        (skip-chars-backward " \t" end)
                        (skip-chars-backward "^ \t" end)
                        (let ((col (current-column-fixed-uncached)))
                          (when (= reference-line-indent col)
                            (let ((indent-after-one-tabstop (indent-next-tab-stop col)))
                              (when (/= indent-after-one-tabstop start-column)
                                (move-to-column indent-after-one-tabstop)))))))
                    (when (or (not forward?)
                              (/= (point) end))
                      (setf indent (current-column-fixed-uncached)))))))))))
      (cond
        (indent
         (indent-to! indent)
         (move-to-column indent))
        (forward?
         (tab-to-tab-stop))
        (t
         (tab-to-tab-stop-backward))))))

;;;###autoload
(defun tab-to-tab-stop-backward ()
  "Like `tab-to-tab-stop' but backwards."
  (interactive)
  (let ((nexttab (indent-next-tab-stop (current-column-fixed) t)))
    (delete-horizontal-space t)
    (indent-to nexttab)))

;;;; Navigation over indentation levels

(defun indent-on-blank-line-p ()
  "Assumes point is at 0th column."
  (save-excursion
    (skip-indentation-forward)
    (let ((c (char-after)))
      ;; Check that we’re at line end.
      (or (eq c ?\r)
          (eq c ?\n)))))

(defun indent-back-up-indent-level (on-blank-line-p)
  "Move up to lesser indentation level, skipping empty lines.

Returns t if point moved."
  (cl-assert (functionp on-blank-line-p))
  (let ((start-indent (indentation-size))
        (start-col (current-column-fixed)))
    (cond
      ((< start-indent start-col)
       (skip-to-indentation)
       t)
      ;; Do not move past 0th column in order to not skip to the
      ;; beginning of file.
      ((/= 0 start-indent)
       ;;(= start-col start-indent)
       (while (and (not (bobp))
                   (<= start-indent (indentation-size)))
         (forward-line -1)
         (while (funcall on-blank-line-p)
           (forward-line -1)))
       (skip-to-indentation)
       t)
      (t
       nil))))

(defun indent-backward-up-indentation-or-sexp (on-blank-line-p)
  "Alternative ‘paredit-backward-up’ that considers both
sexps and indentation levels."
  (let* ((start (point))
         (via-indentation
          (with-demoted-errors
              (save-excursion
                (indent-back-up-indent-level on-blank-line-p)
                (let ((p (point)))
                  (when (/= p start)
                    p)))))
         (via-parens
          (when (/= 0 (syntax-ppss-depth (syntax-ppss start)))
            (with-demoted-errors
                (save-excursion
                  (paredit-backward-up)
                  (let ((p (point)))
                    (when (/= p start)
                      p)))))))
    (if (and via-indentation
             via-parens)
        (goto-char (max via-indentation via-parens))
      (goto-char (or via-indentation
                     via-parens
                     (error "Both indentation-based and sexp-based navigations failed"))))))

(provide 'indentation)

;; Local Variables:
;; End:

;; indentation.el ends here
