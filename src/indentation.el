;; indentation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 April 2017
;; Description:

(defconst +buffer-indent-temporary-filename+
  (concat temporary-file-directory "/indent-buffer.tmp")
  "Path to temporary file reserved for buffer indentation puproses.
See also `*mode-indent-functions-table*'.")

(defparameter *mode-indent-functions-table*
  (make-hash-table :test #'eq)
  "Hash table of (major-mode . function) pairs, where functions
should take no arguments and destructively indent current buffer.
See also `+buffer-indent-temporary-filename+'.")

(defun indent-to! (col)
  "Indent current line to exactly COL'th column with spaces."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (delete-region (line-beginning-position) (point))
    (dotimes (i col)
      (insert ?\s))))

;;;###autoload
(defun util/reindent-file (filename)
  "Load FILENAME contents, try to infer mode for it, reindent according
to mode and write new contents back to FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((buffer-file-name filename))
      (normal-mode)
      (when (memq major-mode '(fundamental-mode
                               text-mode))
        (error "cannot reliably infer mode for file %s" filename))
      (vim:indent)
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
        (setf prev-indent (current-column))
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
    (let ((start-column (current-column))
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
                            (or (looking-at-p haskell-regexen/preprocessor-or-empty-line)
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
                    (when (> (current-column) start-column)
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
                        (when (= reference-line-indent (current-column))
                          (let ((indent-after-one-tabstop
                                 (indent-next-tab-stop (current-column))))
                            (when (/= indent-after-one-tabstop start-column)
                              (move-to-column indent-after-one-tabstop))))))
                    (when (or (not forward?)
                              (/= (point) end))
                      (setf indent (current-column)))))))))))
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
  (let ((nexttab (indent-next-tab-stop (current-column) t)))
    (delete-horizontal-space t)
    (indent-to nexttab)))

;; Indentation of c-style languages via AStyle command-line utility.

(defparameter c-indentation-indent-styles-alist
  '(("c-linux"
     "--style=linux"
     "--indent=spaces=8"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux")
    ("c-standard4"
     "--style=linux"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux")
    ("c-standard2"
     "--style=linux"
     "--indent=spaces=2"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux")
    ("c-gnu2"
     "--style=gnu"
     "--indent=spaces=2"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux")

    ("java-standard"
     "--style=java"
     "--align-pointer=middle"
     "--formatted"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--add-brackets"
     "--convert-tabs"
     "--mode=java"
     "--suffix=none"
     "--lineend=linux"
     "--indent-namespaces" ;; standard java indents namespaces
     )
    ("java-clojure"
     "--style=java"
     ;; "--style=break"
     "--align-pointer=middle"
     "--formatted"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--add-brackets"
     "--convert-tabs"
     "--mode=java"
     "--suffix=none"
     "--lineend=linux")))

(defparameter c-indentation-indent-styles
  (alist->hash-table
   (remq nil
         c-indentation-indent-styles-alist)))

(defparameter c-indentation-indent-style (caar c-indentation-indent-styles-alist)
  "Default indent style to use.")

(defparameter c-indentation-style-history nil)

;;;###autoload
(defun c-indentation-indent-buffer (&optional change-indent-style)
  (interactive "p")
  (when change-indent-style
    (setf c-indentation-indent-style
          (completing-read "Choose style: "
                           (hash-table-keys c-indentation-indent-styles)
                           nil
                           t ;; require match
                           nil
                           c-indentation-style-history ;; history
                           )))
  (save-current-line-column
   (let ((file +buffer-indent-temporary-filename+)
         (p (point))
         (indent-options (gethash c-indentation-indent-style
                                  c-indentation-indent-styles)))
     (unless indent-options
       (error "No options for indent style %s" c-indentation-indent-style))
     (write-region (point-min) (point-max) file)
     (erase-buffer)
     (shell-command
      (join-lines (append (list (platform-dependent-executable
                                 (concat +execs-path+ "/astyle.custom")))
                          indent-options
                          (list (format "<%s" file)))
                  " ")
      (current-buffer))
     (goto-char p))))

(dolist (mode '(c-mode java-mode))
  (puthash mode
           #'c-indentation-indent-buffer
           *mode-indent-functions-table*))

(defun c-indentation-indent-file (filename &optional style)
  "Indent FILENAME according to STYLE by running astyle on it."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((c-indentation-indent-style (or style
                                          c-indentation-indent-style)))
      (c-indentation-indent-buffer)
      (write-region (point-min) (point-max) filename))))

(provide 'indentation)

;; Local Variables:
;; End:

;; indentation.el ends here
