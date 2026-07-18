;; -*- mode: emacs-lisp; no-byte-compile: t; lexical-binding: t; -*-

;; Added by Package.el. Keep commented, don’t delete.
;; (package-initialize)

;; 1e4   recursion depth is quite safe since:
;; 1.6e4 still works
;; 2e4   makes emacs crash
(setq max-lisp-eval-depth 1000
      ;; Handle all unwind-protects and other resources in deep recursion
      max-specpdl-size    5000)

;; speeds up startup time considerably, worth to use
(setq gc-cons-threshold (* 4 1024 1024)
      gc-cons-percentage 0.001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Customizations done by emacs customize routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables)

(provide 'custom-variables-defined)

(defvar compile--in-progress)

(unless (featurep 'start)
  (let* ((emacs-root (getenv "EMACS_ROOT"))
         (emacs-dir
          (if emacs-root
              (if (file-directory-p emacs-root)
                  emacs-root
                (error "EMACS_ROOT points to non-existing directory: ‘%s’" emacs-root))
            (let ((default-emacs-dir (directory-file-name (expand-file-name "~/.emacs.d"))))
              (if (file-directory-p default-emacs-dir)
                  (push default-emacs-dir load-path)
                (error "EMACS_ROOT not defined and default at ‘%s’ doesn’t exist"
                       default-emacs-dir)))))
         (emacs-compiled-dir
          (let ((env (getenv "EMACS_COMPILED_ROOT")))
            (if env
                (progn
                  (if (file-directory-p env)
                      env
                    (error "EMACS_COMPILED_ROOT points to non-existing directory: ‘%s’" env)))
              emacs-dir))))
    ;; These must be present at all times.
    (dolist (root-and-dirs
             (list (progn ;; when (bound-and-true-p compile--in-progress)
                     (list emacs-dir
                           "src"
                           "src/custom"))
                   (list emacs-compiled-dir
                         "lib")))
      (when root-and-dirs
        (let ((root (car root-and-dirs)))
          (dolist (dir (cdr root-and-dirs))
            (let ((src-dir (directory-file-name (concat root "/" dir))))
              (unless (file-directory-p src-dir)
                (error "Important source directory does not exist: %s" src-dir))
              (push src-dir load-path))))))

    ;; These may be missing in fresh build. Add these after adding
    ;; src/ with .el/.el.gz files so that compiled files will be
    ;; picked up first.
    (dolist (dir (list "compiled"
                       (unless (equal (getenv "EMACS_SKIP_ELC") "1")
                         "compiled/elc")))
      (when dir
        (push (directory-file-name (concat emacs-compiled-dir "/" dir)) load-path)))

    ;; (let ((compiled-dir (concat emacs-dir "/compiled")))
    ;;   (unless (equal (car native-comp-eln-load-path) compiled-dir)
    ;;     (startup-redirect-eln-cache compiled-dir)))
    ;; Sometimes there are extra entries, remove them all except the last one.
    (when (boundp 'native-comp-eln-load-path)
      (while (cdr native-comp-eln-load-path)
        (setf native-comp-eln-load-path (cdr native-comp-eln-load-path)))
      (push (directory-file-name (concat emacs-compiled-dir "/compiled/eln")) native-comp-eln-load-path)))

  ;; todo: figure out why this loads non-compiled files in addition to compiled ones.
  (load-library "start")
  ;; (if (bound-and-true-p compile--in-progress)
  ;;     (load-library "start")
  ;;   (let ((load-file-rep-suffixes '(""))
  ;;         (load-suffixes '(".so" ".elc")))
  ;;     (load-library "start")))
  )
