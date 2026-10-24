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

(defvar focus--fringe-indicators-sans-continuation
  (nanothunk-delay
   (let ((tmp (copy-sequence fringe-indicator-alist)))
     (setf (cdr (assoc 'continuation tmp))
           (list nil nil))
     tmp))
  "Lazy-initialized")

(defun focus--centered-window-fringes (&optional window)
  (let* ((window-width (window-pixel-width window))
         (pixel (frame-char-width (window-frame window)))
         (text-width (or display-fill-column-indicator-column
                         (current-fill-column)))
         (half-in-pixels (- (/ window-width 2) (/ (* pixel text-width) 2)))
         (half-in-chars (- (/ (window-width window) 2) (/ text-width 2))))
    (cons half-in-pixels half-in-chars)))

(defun focus--center-text (&optional window)
  (let* ((centered-fringes (focus--centered-window-fringes window))
         (pixel-width (car centered-fringes))
         (char-width (cdr centered-fringes)))
    (set-window-fringes window
                        pixel-width
                        nil ;; pixel-width
                        )
    char-width))

;;;###autoload
(defun focus-mode-spacing ()
  (let ((w (selected-window)))
    (when (window-parameter w 'focus-mode)
      (window-parameter w 'focus-mode-spacing))))

;;;###autoload
(defun toggle-focus-mode ()
  "Center text in current buffer."
  (interactive)
  (let* ((window (selected-window))
         (fringes (window-fringes window)))
    (if (and (eq (car fringes) 0)
             (eq (cadr fringes) 0))
        (progn
          (setq-local fringe-indicator-alist
                      (nanothunk-force focus--fringe-indicators-sans-continuation))
          (let ((width (focus--center-text)))
            (set-window-parameter window 'focus-mode t)
            (set-window-parameter window 'focus-mode-spacing (make-string width ?\s))
            (force-mode-line-update)))
      (progn
        (set-window-parameter window 'focus-mode nil)
        (set-window-parameter window 'focus-mode-spacing nil)
        (kill-local-variable 'fringe-indicator-alist)
        (set-window-fringes window 0)))))

(provide 'focus)

;; Local Variables:
;; End:

;; focus.el ends here
