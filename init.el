;; -*- mode: emacs-lisp; no-byte-compile: t; lexical-binding: t; -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(eval-when-compile
  (require 'cl))

(require 'cl)

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

;; Emacs uses following environment variables for configuration:
;; 1. EMACS_ROOT - path to .emacs.d directory.
;;
;; 2. EMACS_ENV_DEFS - paths to .bash_env file - shell script that sets
;; up environment variables on the system for current user.
;;
;; 3. BASHRC_ENV_LOADED - whether ~/.bash_env was already loaded.
;;
;; 4. EMACS_COMPILED_ROOT
(unless (featurep 'start)

  (let* ((emacs-root (getenv "EMACS_ROOT"))
         (emacs-dir
          (if emacs-root
              (if (file-directory-p emacs-root)
                  emacs-root
                (error "EMACS_ROOT points to non-existing directory: ‘%s’" emacs-root))
            (let ((default-emacs-dir (expand-file-name "~/.emacs.d")))
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
              emacs-root))))
    ;; These may be missing in fresh build.
    (dolist (dir '("compiled"
                   "compiled/elc"))
      (push (concat emacs-compiled-dir "/" dir) load-path))

    ;; These must be present at all times.
    (dolist (root-and-dirs
             (list (list emacs-dir
                         "src"
                         "src/custom")
                   (list emacs-compiled-dir
                         "lib")))
      (let ((root (car root-and-dirs)))
        (dolist (dir (cdr root-and-dirs))
          (let ((src-dir (concat root "/" dir)))
            (unless (file-directory-p src-dir)
              (error "Important source directory does not exist: %s" src-dir))
            (push src-dir load-path)))))

    ;; (let ((compiled-dir (concat emacs-dir "/compiled")))
    ;;   (unless (equal (car native-comp-eln-load-path) compiled-dir)
    ;;     (startup-redirect-eln-cache compiled-dir)))
    ;; Sometimes there are extra entries, remove them all except the last one.
    (when (boundp 'native-comp-eln-load-path)
      (while (cdr native-comp-eln-load-path)
        (setf native-comp-eln-load-path (cdr native-comp-eln-load-path)))
      (push (concat emacs-compiled-dir "/compiled/eln") native-comp-eln-load-path)))

  (load-library "start"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Custom function declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Customizations done by emacs customize routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
