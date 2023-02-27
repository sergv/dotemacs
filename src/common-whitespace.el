;; common-whitespace.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 26 January 2022
;; Description:

(defsubst whitespace-char? (char)
  (or (eq char ?\s)
      (eq char ?\n)
      (eq char ?\r)
      (eq char ?\t)))

(defsubst extended-whitespace-char? (char)
  (or (whitespace-char? char)
      (eq char ?\f)
      (eq char ?\v)))

(defun string-contains-only-whitespace? (str)
  (let ((i 0)
        (end (length str))
        (continue t))
    (while (and continue
                (< i end))
      (setq continue
            (extended-whitespace-char? (aref str i)))
      (cl-incf i))
    continue))

(defun trim-whitespace (str)
  "Trim leading and tailing whitespace from STR."
  (cl-assert (stringp str))
  (let ((i 0)
        (j (1- (length str))))
    (while (and (<= i j)
                (extended-whitespace-char? (aref str i)))
      (cl-incf i))
    (while (and (<= i j)
                (extended-whitespace-char? (aref str j)))
      (cl-decf j))
    (substring str i (1+ j))))

(defun remove-whitespace (str)
  "Remove all occurences of various whitespace characters from string."
  (cl-assert (stringp str))
  (replace-regexp-in-string "[ \t\v\f\r\n]+" "" str))

(defun trim-whitespace-left (str)
  "Trim leading whitespace from STR."
  (cl-assert (stringp str))
  (let ((i 0)
        (end (length str)))
    (while (and (< i end)
                (extended-whitespace-char? (aref str i)))
      (cl-incf i))
    (substring str i end)))

(defun trim-whitespace-right (str)
  "Trim trailing whitespace from STR."
  (cl-assert (stringp str))
  (let ((j (1- (length str))))
    (while (and (<= 0 j)
                (extended-whitespace-char? (aref str j)))
      (cl-decf j))
    (substring str 0 (1+ j))))

(defun delete-whitespace-forward ()
  "Delete whitespaces forward until non-whitespace
character found"
  (interactive "*")
  (let ((start (point)))
    (while (and (not (eobp))
                (whitespace-char? (char-after))
                (not (get-char-property (1+ (point)) 'read-only)))
      (forward-char 1))
    (delete-region start (point))))

(defun delete-whitespace-backward ()
  "Delete whitespaces backward until non-whitespace
character found. Returns t if any whitespace was actually deleted."
  (interactive "*")
  (let ((any-deleted? nil)
        (start (point)))
    (while (and (not (bobp))
                (whitespace-char? (char-before))
                (not (get-char-property (1- (point)) 'read-only)))
      (forward-char -1)
      (setf any-deleted? t))
    (when any-deleted?
      (delete-region (point) start))
    any-deleted?))

(defun skip-whitespace-forward ()
  (skip-chars-forward " \t\v\f\r\n"))

(defun skip-whitespace-backward ()
  (skip-chars-backward " \t\v\f\r\n"))

(provide 'common-whitespace)

;; Local Variables:
;; End:

;; common-whitespace.el ends here
