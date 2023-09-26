;; windows-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C)
;;
;; Author:  <serg.foo@gmail.com>
;; Created: 18 Декабрь 2021
;; Description:

(require 'subr-x)

(defun msys-directory-name-to-emacs (dir)
  "Return a canonical directory for comparison purposes.
Such a directory is all lowercase, has forward-slashes as delimiters,
and ends with a forward slash."
  (when dir
    (file-name-as-directory
     (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:/"
                               (downcase
                                (subst-char-in-string ?\\ ?/ dir))))))

(defun cygwin-directory-name-to-emacs (dir)
  "Return a canonical directory for comparison purposes.
Such a directory is all lowercase, has forward-slashes as delimiters,
and ends with a forward slash."
  (when dir
    (file-name-as-directory
     (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:/"
                               (string-remove-prefix "/cygdrive"
                                                     (downcase
                                                      (subst-char-in-string ?\\ ?/ dir)))))))

(defun msys-or-cygwin-file-name-to-emacs (x)
  (let ((case-fold-search t))
    (cond
      ((string-match-p "^/cygdrive/[a-z]/" x)
       (cygwin-directory-name-to-emacs x))
      ((string-match-p "^/[a-z]/" x)
       (msys-directory-name-to-emacs x))
      (t
       x))))

(defun msys-file-name-handler (func &rest args)
  "Make Emacs transparently handle MSYS-style paths when put in
‘file-name-handler-alist’."
  (apply func (mapcar #'msys-directory-name-to-emacs args)))

(add-to-list 'file-name-handler-alist
             (cons "^/[a-z]/" #'msys-file-name-handler))

(provide 'windows-setup)

;; Local Variables:
;; End:

;; windows-setup.el ends here
