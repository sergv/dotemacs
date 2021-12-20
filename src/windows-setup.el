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
  (file-name-as-directory
   (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:"
                             (downcase
                              (subst-char-in-string ?\\ ?/ dir)))))

(defun cygwin-directory-name-to-emacs (dir)
  "Return a canonical directory for comparison purposes.
Such a directory is all lowercase, has forward-slashes as delimiters,
and ends with a forward slash."
  (file-name-as-directory
   (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:"
                             (string-remove-prefix "/cygdrive"
                                                   (downcase
                                                    (subst-char-in-string ?\\ ?/ dir))))))

(provide 'windows-setup)

;; Local Variables:
;; End:

;; windows-setup.el ends here
