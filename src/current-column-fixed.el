;; current-column-fixed.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  6 February 2022
;; Description:

(eval-when-compile
  (require 'el-patch))

(defvar current-column-fixed--last-point nil)
(defvar current-column-fixed--last-modified nil)
(defvar current-column-fixed--last-result nil)

;;;###autoload
(defun current-column-fixed ()
  "Similar to ‘current-column’ but doesn’t get confused by ‘prettify-symbols-mode’s fiddling
with the current buffer. Pretty ligatures may change perceived width of lines and also affect
column numbers for elisp that executes. This function is immune to that deficiency.

HOWEVER this function doesn’t deal all that well with tabs -
their width is always counted as 1, same as for space."
  (let ((tick (buffer-modified-tick))
        (pt (point)))
    (if (and (eq tick current-column-fixed--last-modified)
             (eq pt current-column-fixed--last-point))
        current-column-fixed--last-result
      (let ((ret (- pt (point-at-bol))))
        (setf current-column-fixed--last-modified tick
              current-column-fixed--last-point pt
              current-column-fixed--last-result ret)
        ret))))

(defun current-column-fixed-uncached ()
  (- (point) (point-at-bol)))

;;;###autoload
(el-patch-feature smie)

(el-patch-defun smie-indent--current-column ()
  "Like `current-column', but if there's a comment before us, use that."
  ;; This is used, so that when we align elements, we don't get
  ;;    toto = { /* foo, */ a,
  ;;                        b }
  ;; but
  ;;    toto = { /* foo, */ a,
  ;;             b }
  (let ((pos (point))
        (lbp (line-beginning-position)))
    (save-excursion
      (unless (and (forward-comment -1) (>= (point) lbp))
        (goto-char pos))
      (el-patch-swap
        (current-column)
        current-column-fixed-uncached))))

(provide 'current-column-fixed)

;; Local Variables:
;; End:

;; current-column-fixed.el ends here
