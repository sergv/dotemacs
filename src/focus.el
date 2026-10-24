;; focus.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 24 October 2026
;; Description:

(eval-when-compile
  (require 'nanothunk))

(defvar display-fill-column-indicator-column)

(require 'nanothunk)

(defvar focus--padding-buffer-thunk
  (nanothunk-delay
   (let ((buf (get-buffer-create " *focus-padding*")))
     (with-current-buffer buf
       (text-mode)
       (setq-local cursor-type nil
                   ;; No mode line
                   mode-line-format nil
                   ;; Dummy empty mode line
                   ;; mode-line-format t
                   )
       (read-only-mode t))
     buf)))

(defvar focus-mode nil)

(defvar focus-mode--saved-display-fill-column-indicator nil
  "Value of ‘display-fill-column-indicator’ before ‘focus-mode’ was enabled.")

;;;###autoload
(defun toggle-focus-mode ()
  "Center text in current buffer."
  (interactive)
  (let ((window (selected-window))
        (single-window? (eq 1 (length (window-list))))
        (left-padding-window nil)
        (contents-window nil)
        (right-padding-window nil)
        (padding-buf (nanothunk-force focus--padding-buffer-thunk)))
    (if (or single-window?
            (not focus-mode))
        (progn
          (setq-local focus-mode t)
          (unless single-window?
            (delete-other-windows))
          (let* ((total-width (window-width window))
                 (padding-width (/ total-width 4)))
            (setf left-padding-window window
                  contents-window (split-window-right padding-width window)
                  right-padding-window (split-window-right (- padding-width) contents-window))
            (set-window-buffer left-padding-window padding-buf)
            (set-window-buffer right-padding-window padding-buf)
            (window-preserve-size left-padding-window t t)
            (window-preserve-size right-padding-window t t)
            (set-window-parameter left-padding-window 'focus-padding t)
            (set-window-parameter right-padding-window 'focus-padding t)
            (select-window contents-window)
            (setq-local focus-mode--saved-display-fill-column-indicator display-fill-column-indicator
                        display-fill-column-indicator nil)))
      (progn
        (setq-local focus-mode nil)
        (let ((windows (window-list))
              (padding-windows nil))
          (dolist (w (window-list))
            (when (and (eq (window-buffer w) padding-buf)
                       (window-parameter w 'focus-padding))
              (push w padding-windows)))
          (mapc #'delete-window padding-windows)
          (setq-local display-fill-column-indicator focus-mode--saved-display-fill-column-indicator))))))

(provide 'focus)

;; Local Variables:
;; End:

;; focus.el ends here
