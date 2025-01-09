;; set-up-paths.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  6 November 2012
;; Description:

(eval-when-compile
  (require 'cl))

(require 'set-up-platform)
(require 'set-up-environment-variables)

;;;; Paths

(defconst +emacs-config-path+ (getenv "EMACS_ROOT")
  "Path to root for my emacs configuration.")

(when (or (null +emacs-config-path+)
          (not (stringp +emacs-config-path+))
          (not (file-directory-p +emacs-config-path+)))
  (error "No accessible directory found for +emacs-config-path+"))

(defconst +resources-path+
  (concat +emacs-config-path+ "/resources")
  "Path to directory with resource files like snippets or templates.")

(defconst +prog-data-path+
  (concat +emacs-config-path+ "/prog-data")
  "Path to directory for storing persintest data like backups.")

(defconst +execs-path+ (concat +emacs-config-path+ "/execs")
  "Path to directory with programs executables files.")

(defconst +tmp-global-path+ (fold-platform-os-type (if (getenv "IN_NIX_SHELL")
                                                       "/tmp"
                                                     temporary-file-directory)
                                                   temporary-file-directory)
  "Path to temporary files that are visible across different emacs instances.")

(provide 'set-up-paths)

;; Local Variables:
;; End:

;; set-up-paths.el ends here
