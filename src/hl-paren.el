;;; hl-paren.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  5 November 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'common)

(defconst hl-paren-parentheses (string-to-list "(){}[]"))
(defconst hl-paren-open-parentheses (string-to-list "({["))

(defface hl-paren-selection-face '((t (:underline "#d33682")))
  "Face to highlight parentheses.")

;; Cons with overlays for current and its' corresponding paren.
(defvar-local hl-paren-overlay nil)

(defsubst hl-paren-move-overlay-to (overlay pos)
  (move-overlay overlay pos (1+ pos)))

(defun hl-paren-make-overlay (pos)
  (let* ((x (make-overlay pos (1+ pos))))
    (overlay-put x 'face 'hl-paren-selection-face)
    x))

(defsubst hl-paren-optionally-delete-overlay (overlay)
  (when overlay
    (delete-overlay overlay)))

(defsubst hl-paren-cleanup-overlays ()
  "Remove any currently active overlays."
  (hl-paren-optionally-delete-overlay (car-safe hl-paren-overlay))
  (hl-paren-optionally-delete-overlay (cdr-safe hl-paren-overlay)))

(defun hl-paren-highlight-matching-paren-at-point ()
  "Highlight paren that is matching for symbol at point.
Turn off highlighting if character at point is not parentheses."
  (interactive)
  (if (and (not (eobp))
           (memq (char-after)
                 hl-paren-parentheses))
      (let ((matching-pos (save-excursion
                            (vim:motion-jump-item)
                            (point))))
        (if hl-paren-overlay
            (progn
              ;; move overlays if they already exist
              (hl-paren-move-overlay-to (car hl-paren-overlay) (point))
              (hl-paren-move-overlay-to (cdr hl-paren-overlay) matching-pos))
          (progn
            ;; re-create overlays
            (hl-paren-cleanup-overlays)
            (setf hl-paren-overlay
                  (cons (hl-paren-make-overlay (point))
                        (hl-paren-make-overlay matching-pos))))))
    (hl-paren-cleanup-overlays)))

(defun hl-paren-do-highlight ()
  "Refresh highlighting if last command was a move one."
  (interactive)
  (if (and (symbolp this-command)
           (memq this-command
                 '(autopair-newline
                   paredit-newline
                   newline
                   newline-and-indent)))
    (hl-paren-cleanup-overlays)
    (condition-case nil
        (hl-paren-highlight-matching-paren-at-point)
      ;; do not let errors interrupt normal workflow
      (error (hl-paren-cleanup-overlays)))))

(defun setup-hl-paren ()
  (add-hook 'post-command-hook #'hl-paren-do-highlight nil t))

(provide 'hl-paren)

;; Local Variables:
;; End:

;;; hl-paren.el ends here
