;; s-extras.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  6 February 2022
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'macro-util))

(require 'common-small)

(defun s-extras-count-chars-in-string (c str)
    "Function to count number of times character C occurs in string STR.

Implementation is very straightforward and because of that fast and reliable."
    (cl-assert (characterp c))
    (cl-assert (stringp str))
    (let ((res (comp-hint-fixnum 0))
          (i (comp-hint-fixnum 0))
          (len (length str)))
      (while (< i len)
        (when (eq c (aref str i))
          (setq res (1+ (comp-hint-fixnum res))))
        (cl-incf i))
      res))

(defun s-extras-replace-char (old new str)
  "Replace all OLD characters with NEW in the STR string and return new string.

Works with both unibyte and multibyte characters."
  (cl-assert (stringp str))
  (cl-assert (characterp old))
  (cl-assert (characterp new))
  (let* ((strs (cons nil nil))
         (tmp strs)
         (limit (comp-hint-fixnum (length str)))
         (i (comp-hint-fixnum 0))
         (prev (comp-hint-fixnum 0))
         (new-str (char-to-string new)))
    (while (< (comp-hint-fixnum i)
              (comp-hint-fixnum limit))
      (if (eq (aref str (comp-hint-fixnum i))
                (comp-hint-fixnum old))
          (progn
            (if (eq (comp-hint-fixnum prev) (comp-hint-fixnum i))
                (setf tmp (setf (cdr tmp) (cons new-str nil)))
              (let ((end (cons new-str nil)))
                (setf (cdr tmp) (cons (substring-no-properties str
                                                               (comp-hint-fixnum prev)
                                                               (comp-hint-fixnum i))
                                      end)
                      tmp end)))
            (setf i (+ (comp-hint-fixnum i) 1)
                  prev (comp-hint-fixnum i)))
        (setf i (+ (comp-hint-fixnum i) 1))))
    (message "prev = %s, i = %s"
             (pp-to-string prev)
             (pp-to-string i))
    (unless (eq (comp-hint-fixnum prev) (comp-hint-fixnum i))
      (setf (cdr tmp)
            (cons (substring-no-properties str
                                           (comp-hint-fixnum prev)
                                           (comp-hint-fixnum i))
                  nil)))
    (apply #'concat (cdr strs))))

(defun s-extras--strip-terminal-save-restore-cursor-escape-sequences! (str)
  (let* ((i 0)
         (j 0)
         (len (length str))
         (dest str))
    (while (< i len)
      (let ((c (aref str i)))
        (if (eq c 27)
            (let ((k (+ i 1)))
              (when (< k len)
                (let ((c2 (aref str k)))
                  (if (or (eq c2 ?7)
                          (eq c2 ?8))
                      (progn
                        (cl-incf i 2))
                    (progn
                      (setf (aref dest j) c)
                      (cl-incf i)
                      (cl-incf j))))))
          (progn
            (setf (aref dest j) c)
            (cl-incf i)
            (cl-incf j)))))
    (substring dest nil j)))

(defun s-extras--strip-terminal-save-restore-cursor-escape-sequences (str)
  (let* ((i 0)
         (j 0)
         (len (length str))
         (dest (make-string len 0)))
    (while (< i len)
      (let ((c (aref str i)))
        (if (eq c 27)
            (let ((k (+ i 1)))
              (when (< k len)
                (let ((c2 (aref str k)))
                  (if (or (eq c2 ?7)
                          (eq c2 ?8))
                      (progn
                        (cl-incf i 2))
                    (progn
                      (setf (aref dest j) c)
                      (cl-incf i)
                      (cl-incf j))))))
          (progn
            (setf (aref dest j) c)
            (cl-incf i)
            (cl-incf j)))))
    (substring dest nil j)))

(defun s-extras-expand-escape-sequences (str)
  "Expand escape sequences within STR:

\\\\ -> <backspace>
\\n  -> <newline>
\\r  -> <carriage-return>
\\t  -> <tab>

All other backslashes are preserved as is."
  (cl-assert (stringp str))
  (let* ((strs (cons nil nil))
         (tmp strs)
         (limit (comp-hint-fixnum (length str)))
         (i (comp-hint-fixnum 0))
         (prev (comp-hint-fixnum 0))
         (escapes
          '((?n  . "\n")
            (?r  . "\r")
            (?t  . "\t")
            (?\\ . "\\"))))
    (while (< (comp-hint-fixnum i) (comp-hint-fixnum limit))
      (let ((c (aref str (comp-hint-fixnum i)))
            (j (comp-hint-fixnum (+ (comp-hint-fixnum i) 1))))
        (if-let* ((_ (and (eq (comp-hint-fixnum c) ?\\)
                          (< (comp-hint-fixnum j) (comp-hint-fixnum limit))))
                  (entry (assq (aref str (comp-hint-fixnum j)) escapes)))
            (progn
              (if (eq (comp-hint-fixnum prev) (comp-hint-fixnum i))
                  (setf tmp (setf (cdr tmp) (cons (cdr entry) nil)))
                (let ((end (cons (cdr entry) nil)))
                  (setf (cdr tmp) (cons (substring-no-properties str
                                                                 (comp-hint-fixnum prev)
                                                                 (comp-hint-fixnum i))
                                        end)
                        tmp end)))
              (setf i (+ (comp-hint-fixnum j) 1)
                    prev (comp-hint-fixnum i)))
          (setf i (comp-hint-fixnum j)))))
    (unless (eq (comp-hint-fixnum prev) (comp-hint-fixnum i))
      (setf (cdr tmp)
            (cons (substring-no-properties str
                                           (comp-hint-fixnum prev)
                                           (comp-hint-fixnum i))
                  nil)))
    (apply #'concat (cdr strs))))

(provide 's-extras)

;; Local Variables:
;; End:

;; s-extras.el ends here
