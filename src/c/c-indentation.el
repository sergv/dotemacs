;; c-indentation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 22 July 2013
;; Description:

(require 'common)

(defvar c-indentation-indent-styles
  (alist->hash-table
   '(("c"
      "--style=linux"
      "--indent=spaces=8"
      "--brackets=linux"
      "--pad-oper"
      "--pad-header"
      "--unpad-paren"
      "--keep-one-line-statements"
      "--keep-one-line-blocks"
      "--convert-tabs"
      "--align-pointer=name"
      "--mode=c"
      "--suffix=none"
      "--lineend=linux")
     ("sophia"
      "--style=bsd"          ;; -A2
      "--brackets=break"     ;; -b
      "--align-pointer=name" ;; -k2
      "--formatted"          ;; -Q
      "--indent=spaces=4"
      "--pad-oper"
      "--pad-header"
      "--unpad-paren"
      "--indent-namespaces"
      "--keep-one-line-statements"
      "--keep-one-line-blocks"
      "--add-brackets"
      "--convert-tabs"
      "--mode=c"
      "--suffix=none"
      "--lineend=linux"
      )
     ("java"
      "--style=java"           ;; -A2
      "--brackets=break"       ;; -b
      "--align-pointer=middle" ;; -k2
      "--formatted"            ;; -Q
      "--indent=spaces=4"
      "--pad-oper"
      "--pad-header"
      "--unpad-paren"
      "--keep-one-line-statements"
      "--keep-one-line-blocks"
      "--add-brackets"
      "--convert-tabs"
      "--mode=c"
      "--suffix=none"
      "--lineend=linux"))))

(defvar c-indentation-indent-style "c"
  "Indent style to use.")

(defvar c-indentation-style-history nil)


(defun c-indentation-indent-buffer (&optional change-indent-style)
  (interactive "p")
  (when change-indent-style
    (setf c-indentation-indent-style
          (completing-read "Choose style:"
                           (map #'car
                                (hash-table->alist c-indentation-indent-styles))
                           nil
                           t ;; require match
                           nil
                           c-indentation-style-history ;; history
                           )))
  (save-current-line-column
   (let ((file +buffer-indent-temporary-filename+)
         (p (point))
         (indent-options (gethash c-indentation-indent-style
                                  c-indentation-indent-styles)))
     (unless indent-options
       (error "No options for indent style %s" c-indentation-indent-style))
     (write-region (point-min) (point-max) file)
     (erase-buffer)
     (shell-command
      (join-lines (append (list (concat +execs-path+ "/astyle.custom"))
                          indent-options
                          (list (format "<%s" file)))
                  " ")
      (current-buffer))
     (goto-char p))))

(provide 'c-indentation)

;; Local Variables:
;; End:

;; c-indentation.el ends here
