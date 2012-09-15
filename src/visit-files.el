;; visit-files.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 15 July 2012
;; Description:

(defvar *visit-files-extension-aliases*
  `(("all"   . (".*"))
    ("el"    . ("el"))
    ("ch"    . ("[CcHh]"))
    ("c"     . ("c"))
    ("h"     . ("h"))
    ("ch"    . ("c" "h"))
    ("hh"    . ("hxx" "hpp" "[Hh]" "HH" "h\\+\\+"))
    ("cc"    . ("cc" "cxx" "cpp" "C" "CC" "c\\+\\+"))
    ("cchh"  . ("[Cc][Cc]" "[CcHh]xx" "[CcHh]pp" "[CHh]" "[Hh][Hh]" "[CcHh]\\+\\+"))
    ("l"     . ,+common-lisp-file-extensions+)
    ("cl"    . ,+common-lisp-file-extensions+)
    ("lsp"   . ,+common-lisp-file-extensions+)
    ("lisp"  . ,+common-lisp-file-extensions+)
    ("clisp" . ,+common-lisp-file-extensions+)
    ("scm"   . ,+scheme-file-extensions+)
    ("m"     . ("[Mm]akefile.*"))
    ("make"  . ("[Mm]akefile.*"))
    ("tex"   . ("tex"))
    ("texi"  . ("texi"))
    ("asm"   . ("[sS]"))
    ("hs"    . ("hs" "hsc" "lhs"))
    ("py"    . ("py" "pyx" "pxd" "pxi"))))

(defun visit-files ()
  (interactive)
  (let* ((group (completing-read-vanilla
                 "File extension alias or extension regexp: "
                 (mapcar #'car *visit-files-extension-aliases*)
                 nil ;; predicate
                 nil ;; require match
                 ))
         (extension-re
           (let ((extensions
                   (assoc group *visit-files-extension-aliases*)))
             (if extensions
               (mapconcat (lambda (x) (concat "\\(?:" x "\\)"))
                          (cdr extensions)
                          "\\|")
               group))))
    (directory-files "."
                     nil
                     (concat "\\."
                             "\\(?:"
                             extension-re
                             "\\)\\'"
                             "\\|"
                             "\\`\\(?:"
                             extension-re
                             "\\)\\'")
                     nil)))


(provide 'visit-files)

;; Local Variables:
;; End:

;; visit-files.el ends here
