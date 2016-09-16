;; recompile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:

(eval-when-compile (require 'cl))

(defvar *ignored-files*
  (let ((tbl (make-hash-table :test #'equal)))
    (dolist (x '("org-jira.el"
                 "recompile.el"
                 "sexpy-highlight.el"
                 "sexpy-highlight-old.el"
                 "lisp-jit-lock.el"
                 "pycomplete.el"
                 "query-in-buffer.el"
                 "repl.el"
                 "prv-xemacs.el" ;; this one is really dangerous
                 "ob-oz.el"
                 "dbl.el"
                 "setup-imaxima-imath.el"
                 "common-lisp-setup.el"))
      (puthash x t tbl))
    tbl))

(defun recompile-main (emacs-dir)
  ;; (dolist (file files-to-recompile)
  ;;   (load-library file))
  (assert emacs-dir)
  (setf emacs-dir (expand-file-name emacs-dir))
  (let* ((init-file
          (find-if #'file-exists-p
                   (mapcan (lambda (x) (list (concat emacs-dir "/src/" x)
                                        (concat "~" x)))
                           '(".emacs"))))
         (disable-hooks
          (lambda ()
            (message "[recompile.el] disabling unsafe hooks")
            (mapc (lambda (func)
                    (remove-hook 'kill-emacs-hook func)
                    (remove-hook 'kill-emacs-hook func t))
                  '(icicle-command-abbrev-save
                    emms-cache-save
                    smex-save-to-file
                    doc-view-save-pages-on-kill
                    save-place-kill-emacs-hook
                    backup-all-buffers
                    persistent-store-flush-database)))))

    ;; load init file to get path detection from set-up-paths.el
    (load-library init-file)
    (funcall disable-hooks)
    (let* ((dirs
            (progn
              (message "[recompile.el] collecting *.el files")
              (append
               (find-elisp-dirs (concat emacs-dir "/src"))
               (find-elisp-dirs (concat emacs-dir "/third-party")
                                set-up-paths--ignored-third-party-el-dirs-re))))
           (extra-files (list init-file))
           (files-to-recompile
            (remove-if
             (lambda (x)
               (let ((fname (file-name-nondirectory x)))
                 (or (gethash fname *ignored-files* nil)
                     ;; (string-match-pure? "^ob-.*\\.el$" fname)
                     (string-match-pure? "^\\..*el$" fname))))
             (append
              extra-files
              (mapcan (lambda (dir)
                        (directory-files dir
                                         t ;; produce full names
                                         "^.*\\.el\\'"
                                         nil ;; do sort
                                         ))
                      dirs))))
           ;; (byte-compile-warning-types
           ;;  '(redefine callargs free-vars unresolved obsolete noruntime
           ;;             interactive-only make-local mapcar
           ;;             constants suspicious lexical))
           )
      (message "[recompile.el] recompiling files")
      (dolist (file files-to-recompile)
        (byte-compile-file file)))
    (message "[recompile.el] done")
    (funcall disable-hooks)))


;; Local Variables:
;; End:

;; recompile.el ends here
