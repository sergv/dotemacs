;; auto-insert-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)

(defvar auto-insert-fields
  (list
   (list "license type" (lambda ()
                          "Apache-2.0"))
   (list "license spdx" (lambda ()
                          "Apache-2.0"))
   (list "filename" (lambda ()
                      (file-name-nondirectory
                       buffer-file-name)))
   (list "filename no ext" (lambda ()
                             (file-name-sans-extension
                              (file-name-nondirectory
                               buffer-file-name))))
   (list "filename no ext uppercase" (lambda ()
                                       (upcase
                                        (file-name-sans-extension
                                         (file-name-nondirectory
                                          buffer-file-name)))))
   (list "date"      (apply-partially #'format-time-string
                                      "%e %B %Y"))
   (list "date year" (apply-partially #'format-time-string
                                      "%Y"))
   (list "author" (lambda () user-full-name))
   (list "email" (lambda () user-mail-address))
   (list "clojure-path-to-ns"
         (lambda ()
           (save-match-data
             (let ((name buffer-file-name))
               (if (string-match "^.*/src\\(?:/clojure\\)?/\\(.*\\)\\.clj\\'" name)
                   (replace-regexp-in-string "/"
                                             "."
                                             (match-string-no-properties 1 name))
                 "")))))
   (list "haskell path to module name"
         (lambda ()
           (let* ((root
                   (locate-dominating-file
                    (file-name-directory buffer-file-name)
                    (lambda (dir)
                      (or (not (string-match-p "[A-Z][a-zA-Z0-9_']*"
                                               (file-name-nondirectory
                                                (strip-trailing-slash dir))))
                          (-any-p (lambda (path)
                                    (or (and (file-regular-p path)
                                             (string-match-p
                                              (rx (or (seq (+ not-newline)
                                                           ".cabal")
                                                      "Setup.hs"
                                                      "Setup.lhs")
                                                  eos)
                                              path))
                                        (and (file-directory-p path)
                                             (or (string= "src" path)
                                                 (member path
                                                         +version-control-directories+)
                                                 ;; this is somewhat vacuous
                                                 ;; (not (string-match-p
                                                 ;;       "[A-Z][a-zA-Z]*"
                                                 ;;       path))
                                                 ))))
                                  (directory-files dir
                                                   t
                                                   directory-files-no-dot-files-regexp))))))
                  (raw-name (strip-string-prefix (strip-trailing-slash
                                                  (expand-file-name root))
                                                 (file-name-sans-extension
                                                  (expand-file-name
                                                   buffer-file-name))
                                                 :starting-at 1)))
             (replace-regexp-in-string "/" "." raw-name))))
   (list "empty" (lambda () "")))
  "Alist of form (string function), used by `util:auto-insert-update'.
When auto-insert file template contains entry of form ${HELLO} then
`util:auto-insert-update' will replace it with return value
function than has correspondence to HELLO in this alist.")

;;;###autoload
(defun auto-insert-update ()
  "Function that substitutes actual data into auto-insert
template files, data description may be found in
`auto-insert-fields' alist."
  (save-excursion
    (save-match-data
      (with-disabled-undo
       (with-preserved-buffer-modified-p
        (with-inhibited-modification-hooks
         (mapc (lambda (x)
                 (goto-char (point-min))
                 (let ((pattern (concat "\\${" (first x) "}"))
                       (new-data (funcall (second x))))
                   (while (re-search-forward pattern nil t)
                     (replace-match new-data t t))))
               auto-insert-fields)))))))

;;;###autoload
(add-hook 'find-file-hook #'auto-insert)

(provide 'auto-insert-setup)

;; Local Variables:
;; End:

;; auto-insert-setup.el ends here
