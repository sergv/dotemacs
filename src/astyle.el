;; astyle.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 September 2021
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'set-up-paths)

;; Indentation of c-style languages via AStyle command-line utility.

(defvar astyle-indent--styles-alist
  '(("c-standard4"
     "--style=linux"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux"
     "--add-brackets")
    ("c-standard2"
     "--style=linux"
     "--indent=spaces=2"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux"
     "--add-brackets")
    ("c-gnu2"
     "--style=gnu"
     "--indent=spaces=2"
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

    ("java-standard"
     "--style=java"
     "--align-pointer=middle"
     "--formatted"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--add-brackets"
     "--convert-tabs"
     "--mode=java"
     "--suffix=none"
     "--lineend=linux"
     "--indent-namespaces" ;; standard java indents namespaces
     )
    ("java-clojure"
     "--style=java"
     ;; "--style=break"
     "--align-pointer=middle"
     "--formatted"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--add-brackets"
     "--convert-tabs"
     "--mode=java"
     "--suffix=none"
     "--lineend=linux")))

(defvar astyle-indent--styles
  (alist->hash-table
   (remq nil
         astyle-indent--styles-alist)))

(defvar astyle-indent-style (caar astyle-indent--styles-alist)
  "Default indent style to use.")

(defvar astyle-indent-style-history nil)

;;;###autoload
(defun astyle-format-buffer (&optional change-indent-style)
  (interactive "p")
  (when change-indent-style
    (setf astyle-indent-style
          (completing-read "Choose style: "
                           (hash-table-keys astyle-indent--styles)
                           nil
                           t ;; require match
                           nil
                           'astyle-indent-style-history ;; history
                           )))
  (with-temporary-file file "astyle-indent" nil nil
    (let ((p (point))
          (indent-options (gethash astyle-indent-style
                                   astyle-indent--styles)))
      (unless indent-options
        (error "No options for indent style %s" astyle-indent-style))
      (write-region (point-min) (point-max) file)
      (erase-buffer)
      (shell-command
       (join-lines (append (list (or (platform-dependent-executable
                                      (concat +execs-path+ "/astyle.custom"))
                                     "astyle"))
                           indent-options
                           (list (format "<%s" file)))
                   " ")
       (current-buffer))
      (goto-char p))))

(provide 'astyle)

;; Local Variables:
;; End:

;; astyle.el ends here
