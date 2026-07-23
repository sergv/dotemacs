;; semnav.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 April 2026
;; Description:
;;
;; SEMantic NAVigation.

(defsubst parse-partial-sexp--inside-string? (state)
  (nth 3 state))

(defsubst parse-partial-sexp--inside-comment? (state)
  (nth 4 state))

(defsubst parse-partial-sexp--comment-or-string-start (state)
  (nth 8 state))

(defvar point-inside-string?-override nil)
(defvar point-inside-comment?-override nil)
(defvar point-inside-string-or-comment?-override nil)
(defvar point-not-inside-string-or-comment?-override nil)
(defvar semnav-bounds-of-string-at-override #'semnav-bounds-of-string-at--default)

(defun point-inside-string? (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (if point-inside-string?-override
      (funcall point-inside-string?-override pos)
    (point-inside-string?--default pos)))

(defun point-inside-comment? (&optional pos)
  "Return non-nil if point is positioned inside a comment."
  (declare (pure nil) (side-effect-free t))
  (if point-inside-comment?-override
      (funcall point-inside-comment?-override pos)
    (point-inside-comment?--default pos)))

(defun point-inside-string-or-comment? (&optional pos)
  "Return t if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (if point-inside-string-or-comment?-override
      (funcall point-inside-string-or-comment?-override pos)
    (point-inside-string-or-comment?--default pos)))

(defsubst point-not-inside-string-or-comment? (&optional pos)
  (declare (pure nil) (side-effect-free t))
  (if point-not-inside-string-or-comment?-override
      (funcall point-not-inside-string-or-comment?-override pos)
    (point-not-inside-string-or-comment?--default pos)))

(defun point-inside-string?--default (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      (parse-partial-sexp--inside-string? state))))

(defun point-inside-comment?--default (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      (parse-partial-sexp--inside-comment? state))))

(defun point-inside-string-or-comment?--default (&optional pos)
  "Return t if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      ;; Should be the same as (nth 8 state) (aka
      ;; ‘parse-partial-sexp--comment-or-string-start’) but company
      ;; did it this way and it seems reasonable to take everything
      ;; ‘parse-partial-sexp’ has to offer into account.
      (or (car (setq state (nthcdr 3 state)))
          (car (setq state (cdr state)))
          (nth 3 state)))))

(defsubst point-not-inside-string-or-comment?--default (&optional pos)
  (declare (pure nil) (side-effect-free t))
  (not (point-inside-string-or-comment? pos)))

(defun semnav-bounds-of-string-at--default (pos)
  (save-excursion
    (goto-char pos)
    (bounds-of-thing-at-point 'string)))

(defun semnav-bounds-of-string-at (pos)
  (if semnav-bounds-of-string-at-override
      (funcall semnav-bounds-of-string-at-override pos)
    (semnav-bounds-of-string-at--default pos)))

(provide 'semnav)

;; Local Variables:
;; End:

;; semnav.el ends here
