;; recompile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:

(eval-when-compile (require 'cl))

(defvar *emacs-dir*
  (find-if (lambda (p) (and (file-accessible-directory-p p)))
           (list
            (expand-file-name "~/emacs")
            (expand-file-name "~/.emacs.d"))))

(defvar *dotemacs-init-file*
  (find-if #'file-exists-p
           (mapcan (lambda (x) (list (concat *emacs-dir* "/" x)
                                (concat *emacs-dir* "/../" x)))
                   '(".emacs"))))

(load-library *dotemacs-init-file*)

(defun detach-hooks ()
  (mapc (lambda (func)
          (remove-hook 'kill-emacs-hook func)
          (remove-hook 'kill-emacs-hook func t))
        '(icicle-command-abbrev-save
          emms-cache-save
          smex-save-to-file
          doc-view-save-pages-on-kill
          save-place-kill-emacs-hook
          backup-all-buffers
          persistent-store-flush-database)))
(detach-hooks)


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

(defvar *dirs* (mapcar (lambda (x) (concat *emacs-dir* "/" x))
                       '("third-party"
                         "src"
                         "standalone")))

(defvar *more-files*
  (list *dotemacs-init-file*))

(defvar *files*
  (remove-if
   (lambda (x)
     (member* (file-name-nondirectory x) *ignored-files*
              :test #'string=))
   (append
    *more-files*
    (loop
      for dir in *dirs*
      appending
      (find-rec dir
                :filep
                (lambda (x)
                  (and (string-match-pure? "\\.el$" x)
                       (not (string-match-pure? "^ob-.*\\.el$"
                                                (file-name-nondirectory x)))
                       (not (string-match-pure? "^\\..*el$"
                                                (file-name-nondirectory x)))))
                :do-not-visitp
                (lambda (x)
                  (string-match-pure? "nxhtml\\|org-7\\.8\\.11" x)))))))

(defun recompile-main ()
  ;; (dolist (file *files*)
  ;;   (load-library file))

  (let (;; (byte-compile-warning-types
        ;;  '(redefine callargs free-vars unresolved obsolete noruntime
        ;;             interactive-only make-local mapcar
        ;;             constants suspicious lexical))
        )
    (dolist (file *files*)
      (byte-compile-file file)))

  (detach-hooks))


;; Local Variables:
;; End:

;; recompile.el ends here
