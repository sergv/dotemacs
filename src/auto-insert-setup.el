(require 'custom)
(require 'common)

(setf auto-insert-directory (path-concat +prog-data-path+ "auto-insert")
      auto-insert 'other
      auto-insert-query nil)


(setf auto-insert-alist
      '(("\\.sh$"      . ["insert.sh"    auto-insert-update])
        ("\\.el$"      . ["insert.el"    auto-insert-update])
        ("\\.hs$"      . ["insert.hs"    auto-insert-update])
        ("\\.awk$"     . ["insert.awk"   auto-insert-update])
        ("\\.tex$"     . ["insert.tex"   auto-insert-update])
        ("\\.snip$"    . ["insert.snip"  auto-insert-update])
        ("\\.h$"       . ["insert.h"     auto-insert-update])
        ("\\.x?html?$" . ["insert.xhtml" auto-insert-update])
        ("\\.py$"      . ["insert.py"    auto-insert-update])
        ("\\.org$"     . ["insert.org"   auto-insert-update])
        ("\\.scm$"     . ["insert.scm"   auto-insert-update])
        ("\\.\\(?:l\\|cl\\|asd\\|lsp\\|lisp\\|clisp\\)$"
         . ["insert.lisp" auto-insert-update])))


(defvar auto-insert-fields
  (list
   (list "filename" #'(lambda ()
                        (file-name-nondirectory
                         (buffer-file-name))))
   (list "filename no ext" #'(lambda ()
                               (file-name-sans-extension
                                (file-name-nondirectory
                                 (buffer-file-name)))))
   (list "filename no ext uppercase" #'(lambda ()
                                         (upcase
                                          (file-name-sans-extension
                                           (file-name-nondirectory
                                            (buffer-file-name))))))
   (list "date"      (apply-partially #'format-time-string
                                      "%A, %e %B %Y"))
   (list "date year" (apply-partially #'format-time-string
                                      "%Y"))
   (list "author" (lambda () "Sergey Vinokurov"))
   (list "email" (lambda () "serg-foo@gmail.com"))
   (list "empty" (lambda () "")))
  "Alist of form (string function), used by `util:auto-insert-update'.
When auto-insert file template contains entry of form ${HELLO} then
`util:auto-insert-update' will replace it with return value
function than has correspondence to HELLO in this alist.")


(defun auto-insert-update ()
  "Function that substitutes actual data into auto-insert
template files, data description may be found in
`auto-insert-fields' alist."
  (save-excursion
   (save-match-data
    (with-disabled-undo
     (with-preserved-buffer-modified-p
      (with-inhibited-modification-hooks
       (mapc #'(lambda (x)
                 (goto-char (point-min))
                 (let ((pattern (concat "\\${" (first x) "}"))
                       (new-data (funcall (second x))))
                   (while (re-search-forward pattern nil t)
                     (replace-match new-data t t))))
             auto-insert-fields)))))))



(add-hook 'find-file-hooks 'auto-insert)

