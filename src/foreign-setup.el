;; foreign-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 June 2018
;; Description:

(eval-when-compile
  (require 'set-up-platform))

(defvar dumping?)

(defvar use-foreign-libraries? t
  "Whether to use foreign extensions to Emacs.")

(when (and use-foreign-libraries?
           ;; Ideally we don’t even want to load *.el files when compiling.
           ;; Definitely don’t want foreign modules during elisp compilation -
           ;; compilation should be completable through
           ;; emacs --batch -f batch-byte-compile
           ;; We’re not there yet but foreign modules severely impede that goal.
           (not (bound-and-true-p byte-compile-current-file))
           (not (bound-and-true-p dumping?)))
  (load (fold-platform-os-type "libemacs-native" "emacs-native") nil noninteractive))

(defun foregin-setup-resolve-emacs-native-helper-path ()
  "Return path to ‘emacs-native-helper’ to use or nil if it’s not available."
  (executable-find "emacs-native-helper"))

(defun foregin-setup--format-globs-args-for-emacs-native-hepler
    (globs-to-find
     ignored-extensions-globs
     ignored-absolute-files-globs
     ignored-absolute-dirs-globs
     ignored-directories
     ignored-directory-prefixes)
  (let ((ignored-files-basename-globs ignored-extensions-globs)
        (ignored-files-abs-globs ignored-absolute-files-globs)
        (ignored-dirs-basename-globs (append ignored-directories
                                             (--map (concat it "*") ignored-directory-prefixes)))
        (ignored-dirs-abs-globs ignored-absolute-dirs-globs))
    (nconc
     (mapcan (lambda (glob)
               (list "-g" glob))
             globs-to-find)
     (mapcan (lambda (glob)
               (list "--ifbg" glob))
             ignored-files-basename-globs)
     (mapcan (lambda (glob)
               (list "--ifag" glob))
             ignored-files-abs-globs)
     (mapcan (lambda (glob)
               (list "--idbg" glob))
             ignored-dirs-basename-globs)
     (mapcan (lambda (glob)
               (list "--idag" glob))
             ignored-dirs-abs-globs))))

(provide 'foreign-setup)

;; Local Variables:
;; End:

;; foreign-setup.el ends here
