;;; shm-in.el --- Are we in some thing.

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun shm-in-comment ()
  "Are we currently in a comment?"
  (save-excursion
    (when (and (= (line-end-position)
                  (point))
               (/= (line-beginning-position) (point)))
      (forward-char -1))
    (and (or (let* ((state (parse-partial-sexp (line-beginning-position)
                                               (point)))
                    (inside-comment? (elt state 4)))
               inside-comment?)
             (eq 'font-lock-comment-delimiter-face
                 (get-text-property (point) 'face))
             (eq 'font-lock-doc-face
                 (get-text-property (point) 'face))
             (eq 'font-lock-comment-face
                 (get-text-property (point) 'face))
             (save-excursion (goto-char (line-beginning-position))
                             (looking-at-p "^\-\- ")))
         ;; Pragmas {-# SPECIALIZE .. #-} etc are not to be treated as
         ;; comments, even though they are highlighted as such
         (not (save-excursion (goto-char (line-beginning-position))
                              (looking-at-p "{-# "))))))

(defun shm-in-string ()
  "Are we in a string?"
  (let* ((state (parse-partial-sexp (line-beginning-position)
                                    (point)))
         (inside-string? (elt state 3)))
    (or inside-string?
        (and (eq 'font-lock-string-face
                 (get-text-property (point) 'face))
             (if (char-equal (char-after) ?\")
               (eq 'font-lock-string-face
                   (get-text-property (- (point) 1) 'face))
               t)))))

(defun shm-in-char ()
  "Are we in a char literal?"
  (save-excursion
    (and (looking-at-p "'")
         (looking-back "'"))))

(defun shm-in-string-or-comment ()
  "Are we in string or comment?"
  (let* ((state (parse-partial-sexp (line-beginning-position)
                                    (point)))
         (inside-string? (elt state 3))
         (inside-comment? (elt state 4)))
    (or inside-comment?
        inside-string?
        (and (eq 'font-lock-string-face
                 (get-text-property (point) 'face))
             (if (char-equal (char-after) ?\")
               (eq 'font-lock-string-face
                   (get-text-property (- (point) 1) 'face))
               t)))))


(defun shm-literal-insertion ()
  "Should a node have literal insertion?"
  (or (shm-in-string-or-comment)
      (shm-in-char)
      (and (memq (char-before) '(?\' ?\\))
           (char-equal (char-after) ?\'))
      (and (memq (char-before) '(?\" ?\\))
           (char-equal (char-after) ?\"))))

(provide 'shm-in)
