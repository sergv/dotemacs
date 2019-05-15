;; -*- mode: emacs-lisp; no-byte-compile: t; lexical-binding: t; -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(eval-when-compile (require 'cl-lib))

(require 'cl)

;; 1e4   recursion depth is quite safe since:
;; 1.6e4 still works
;; 2e4   makes emacs crash
(setq max-lisp-eval-depth 1000
      ;; Handle all unwind-protects and other resources in deep recursion
      max-specpdl-size    5000)

;; speeds up startup time considerably, worth to use
(setq gc-cons-threshold (* 1 1024 1024)
      gc-cons-percentage 0.01)

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
(unless (featurep 'start)
  (let ((emacs-root (getenv "EMACS_ROOT")))
    (if emacs-root
        (progn
          (cl-assert (file-directory-p emacs-root))
          (let ((src-dir (concat emacs-root "/src")))
            (cl-assert (file-directory-p src-dir))
            (add-to-list 'load-path src-dir)))
      (error "EMACS_ROOT not defined")))
  (load-library "start"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Custom function declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Customizations done by emacs customize routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
