;; auto-insert-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'common)

;;;###autoload
(setf auto-insert-directory (concat +prog-data-path+ "/auto-insert")
      auto-insert 'other
      auto-insert-query nil
      auto-insert-alist
      (mapcar (lambda (spec)
                (let ((actions (cdr spec)))
                  (cons (car spec)
                        (list->vector
                         (-map (lambda (action)
                                 (or
                                  (when (and (string? action)
                                             (platform-use? 'work))
                                    (let ((work-specific-insert-file
                                           (concat (file-name-sans-extension action)
                                                   ".work"
                                                   "."
                                                   (file-name-extension action))))
                                      (when (file-exists?
                                             (concat auto-insert-directory
                                                     "/"
                                                     work-specific-insert-file))
                                        work-specific-insert-file)))
                                  action))
                               actions)))))
              '(("\\.sh$"      . ["insert.sh"    auto-insert-update])
                ("\\.clj$"     . ["insert.clj"   auto-insert-update])
                ("\\.el$"      . ["insert.el"    auto-insert-update])
                ("\\.hs$"      . ["insert.hs"    auto-insert-update])
                ("\\.cabal$"   . ["insert.cabal" auto-insert-update])
                ("\\.chs$"     . ["insert.chs"   auto-insert-update])
                ("\\.awk$"     . ["insert.awk"   auto-insert-update])
                ("\\.tex$"     . ["insert.tex"   auto-insert-update])
                ("\\.snip$"    . ["insert.snip"  auto-insert-update])
                ("\\.h$"       . ["insert.h"     auto-insert-update])
                ("\\.x?html?$" . ["insert.xhtml" auto-insert-update])
                ("\\.py$"      . ["insert.py"    auto-insert-update])
                ("\\.org$"     . ["insert.org"   auto-insert-update])
                ("\\.eproj-info$" . ["insert.eproj-info" auto-insert-update])

                ("AndroidManifest.xml$"      . ["insert-android-manifest.xml" auto-insert-update])
                ("/res/drawable.*/.*\\.xml$" . ["insert-android-drawable.xml" auto-insert-update])
                ("/res/layout.*/.*\\.xml$"   . ["insert-android-layout.xml"   auto-insert-update])
                ("/res/menu.*/.*\\.xml$"     . ["insert-android-menu.xml"     auto-insert-update])
                ("/res/values.*/.*\\.xml$"   . ["insert-android-values.xml"   auto-insert-update])

                ("\\.scm$"     . ["insert.scm"   auto-insert-update])
                ("\\.\\(?:l\\|cl\\|asd\\|lsp\\|lisp\\|clisp\\)$"
                 . ["insert.lisp" auto-insert-update]))))

(defparameter auto-insert-fields
  (list
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
   (list "date"      (partial #'format-time-string
                              "%A, %e %B %Y"))
   (list "date year" (partial #'format-time-string
                              "%Y"))
   (list "author" (lambda () user-full-name))
   (list "email" (lambda () user-mail-address))
   (list "clojure-path-to-ns"
         (lambda ()
           (save-match-data
             (let ((name buffer-file-name))
               (if (string-match "^.*/src\\(?:/clojure\\)?/\\(.*\\)\\.clj$" name)
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
                      (or (not (string-match-pure? "[A-Z][a-zA-Z0-9_']*"
                                                   (file-name-nondirectory
                                                    (strip-trailing-slash dir))))
                          (-any? (lambda (path)
                                   (or (and (file-regular? path)
                                            (string-match-pure?
                                             (rx (or (seq (+ not-newline)
                                                          ".cabal")
                                                     "Setup.hs"
                                                     "Setup.lhs")
                                                 eos)
                                             path))
                                       (and (file-directory? path)
                                            (or (string= "src" path)
                                                (member path
                                                        *version-control-directories*)
                                                ;; this is somewhat vacuous
                                                ;; (not (string-match-pure?
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
(add-hook 'find-file-hooks 'auto-insert)

(provide 'auto-insert-setup)

;; Local Variables:
;; End:

;; auto-insert-setup.el ends here
