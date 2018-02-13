;; recompile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:

(eval-when-compile (require 'cl))

(defconst +ignored-files-re+
  (rx bol
      (or "third-party/yafolding.el/features/support/env.el")
      eol))

(defun recompile-main (emacs-dir)
  ;; (dolist (file files-to-recompile)
  ;;   (load-library file))
  (cl-assert emacs-dir)
  (cl-proclaim '(optimize (speed 3) (safety 0)))
  (message "cl--optimize-speed = %s"
           (pp-to-string cl--optimize-speed))
  (message "cl--optimize-safety = %s"
           (pp-to-string cl--optimize-safety))
  (setf emacs-dir (expand-file-name (directory-file-name emacs-dir)))
  (let* ((init-file
          (find-if #'file-exists-p
                   (mapcan (lambda (x) (list (concat emacs-dir "/src/" x)
                                        (concat "~/" x)))
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
               (let ((fname (file-name-nondirectory x))
                     (rel-name (file-relative-name x emacs-dir)))
                 (or (string-match-p +ignored-files-re+ rel-name)
                     ;; (string-match-p "^ob-.*\\.el$" fname)
                     (string-match-p "^\\..*el$" fname))))
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
