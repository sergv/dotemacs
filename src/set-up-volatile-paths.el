;; set-up-volatile-paths.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 13 August 2026
;; Description:
;; Set up paths that may change between different Emacs processes.

(eval-when-compile
  (require 'cl-lib))

(defconst +emacs-writable-config-path+
  (let ((writable-root (getenv "EMACS_WRITABLE_ROOT")))
    (if writable-root
        (progn
          (when (not (file-directory-p writable-root))
            (error "Path pointed to by EMACS_WRITABLE_ROOT does not exsit: ‘%s’"
                   writable-root))
          writable-root)
      +emacs-config-path+))
  "Path to root for my emacs configuration for writing things.

Usually ~/.emacs.d

By default points to the same destination as ‘+emacs-config-path+’.")

(defconst +resources-path+
  (concat +emacs-config-path+ "/resources")
  "Path to directory with resource files like snippets or templates.")

(defconst +prog-data-path+
  (concat +emacs-writable-config-path+ "/prog-data")
  "Path to directory for storing persintest data like backups.")

(defconst +execs-path+ (concat +emacs-config-path+ "/execs")
  "Path to directory with programs executables files.")

(provide 'set-up-volatile-paths)

;; Local Variables:
;; End:

;; set-up-volatile-paths.el ends here
