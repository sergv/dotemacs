;; recompile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:

(eval-when-compile (require 'cl))

(defvar *ignored-files*
  '("org-jira.el"
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

(defun recompile-main (emacs-dir)
  ;; (dolist (file files-to-recompile)
  ;;   (load-library file))
  (assert emacs-dir)
  (setf emacs-dir (expand-file-name emacs-dir))
  (let* ((dotemacs-init-file
          (find-if #'file-exists-p
                   (mapcan (lambda (x) (list (concat emacs-dir "/" x)
                                             (concat emacs-dir "/../" x)
                                             (concat "~" x)))
                           '(".emacs"))))

         (detach-hooks
          (lambda ()
            (mapc (lambda (func)
                    (remove-hook 'kill-emacs-hook func)
                    (remove-hook 'kill-emacs-hook func t))
                  '(icicle-command-abbrev-save
                    emms-cache-save
                    smex-save-to-file
                    doc-view-save-pages-on-kill
                    save-place-kill-emacs-hook
                    backup-all-buffers
                    persistent-store-flush-database))))
         (dirs (mapcar (lambda (x) (concat emacs-dir "/" x))
                       '("third-party"
                         "src"
                         "standalone")))
         (more-files
          (list dotemacs-init-file)))

    ;; load init file to get find-rec
    (load-library dotemacs-init-file)
    (funcall detach-hooks)
    (let ((files-to-recompile
           (remove-if
            (lambda (x)
              (member* (file-name-nondirectory x) *ignored-files*
                       :test #'string=))
            (append
             more-files
             (mapcan (lambda (dir)
                       (find-rec dir
                                 :filep
                                 (lambda (x)
                                   (and (string-match-pure? "\\.el$" x)
                                        (not (string-match-pure? "^ob-.*\\.el$"
                                                                 (file-name-nondirectory x)))
                                        (not (string-match-pure? "^\\..*el$"
                                                                 (file-name-nondirectory x)))))))
                     dirs))))
          ;; (byte-compile-warning-types
          ;;  '(redefine callargs free-vars unresolved obsolete noruntime
          ;;             interactive-only make-local mapcar
          ;;             constants suspicious lexical))
          )
      (dolist (file files-to-recompile)
        (byte-compile-file file)))
    (funcall detach-hooks)))


;; Local Variables:
;; End:

;; recompile.el ends here
