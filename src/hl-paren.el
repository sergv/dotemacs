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

;; (defvar-buffer-local hl-paren-overlay nil)

;; overlays for current and its' corresponding paren
(defvar-buffer-local hl-paren-current-overlay nil)
(defvar-buffer-local hl-paren-corresponding-overlay nil)

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
  (hl-paren-optionally-delete-overlay
   hl-paren-current-overlay)
  (hl-paren-optionally-delete-overlay
   hl-paren-corresponding-overlay)

  (setq hl-paren-current-overlay nil
        hl-paren-corresponding-overlay nil))

(defun hl-paren-highlight-matching-paren-at-point ()
  "Highlight paren that is matching for symbol at point.
Turn off highlighting if character at point is not parentheses."
  (interactive)
  (if (and (not (eobp))
           (member (char-after)
                   hl-paren-parentheses))
    (let ((matching-pos (save-excursion
                         (vim:motion-jump-item)
                         (point))))
      (if (and hl-paren-current-overlay
               hl-paren-corresponding-overlay)
        ;; move overlays if they already exist
        (progn
          (hl-paren-move-overlay-to
           hl-paren-current-overlay (point))

          (hl-paren-move-overlay-to
           hl-paren-corresponding-overlay matching-pos))

        ;; re-create overlays
        (progn
          (hl-paren-cleanup-overlays)

          (setf hl-paren-current-overlay
                (hl-paren-make-overlay (point))
                hl-paren-corresponding-overlay
                (hl-paren-make-overlay matching-pos)))))

    (hl-paren-cleanup-overlays)))

(defun hl-paren-do-highlight ()
  "Refresh highlighting if last command was a move one."
  (interactive)
  (if (and (symbolp this-command)
           (memq this-command
                 '(autopair-newline)))
    (hl-paren-cleanup-overlays)
    (condition-case nil
        (hl-paren-highlight-matching-paren-at-point)
      ;; do not let errors interrupt normal workflow
      (error (hl-paren-cleanup-overlays)))))

(add-hook 'post-command-hook #'hl-paren-do-highlight)

(provide 'hl-paren)

;; Local Variables:
;; End:

;;; hl-paren.el ends here
