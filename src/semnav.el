;; semnav.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 April 2026
;; Description:
;;
;; SEMantic NAVigation.

(defsubst parse-partial-sexp--inside-comment? (state)
  (elt state 4))

(defsubst parse-partial-sexp--comment-or-string-start (state)
  (elt state 8))

(defvar point-inside-string?-impl #'point-inside-string?--default)
(defvar point-inside-comment?-impl #'point-inside-comment?--default)
(defvar point-inside-string-or-comment?-impl #'point-inside-string-or-comment?--default)
(defvar point-not-inside-string-or-comment?-impl #'point-not-inside-string-or-comment?--default)

(defun point-inside-string? (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (funcall point-inside-string?-impl pos))

(defun point-inside-comment? (&optional pos)
  "Return non-nil if point is positioned inside a comment."
  (declare (pure nil) (side-effect-free t))
  (funcall point-inside-comment?-impl pos))

(defun point-inside-string-or-comment? (&optional pos)
  "Return t if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (funcall point-inside-string-or-comment?-impl pos))

(defsubst point-not-inside-string-or-comment? (&optional pos)
  (declare (pure nil) (side-effect-free t))
  (funcall point-not-inside-string-or-comment?-impl pos))

(defun point-inside-string?--default (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      (elt state 3))))

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
      (parse-partial-sexp--comment-or-string-start state))))

(defsubst point-not-inside-string-or-comment?--default (&optional pos)
  (declare (pure nil) (side-effect-free t))
  (not (point-inside-string-or-comment? pos)))

(provide 'semnav)

;; Local Variables:
;; End:

;; semnav.el ends here
