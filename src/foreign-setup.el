;; foreign-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 June 2018
;; Description:

(eval-when-compile
  (require 'set-up-platform))

(require 'dump-init)

(defcustom use-foreign-libraries? t
  "Whether to use foreign extensions to Emacs."
  :type 'boolean
  :group 'common)

(when (and use-foreign-libraries?
           ;; Ideally we don’t even want to load *.el files when compiling.
           ;; Definitely don’t want foreign modules during elisp compilation -
           ;; compilation should be completable through
           ;; emacs --batch -f batch-byte-compile
           ;; We’re not there yet but foreign modules severely impede that goal.
           (not (bound-and-true-p byte-compile-current-file))
           (not dumping))
  (load (fold-platform-os-type "libemacs-native" "emacs-native")))

(provide 'foreign-setup)

;; Local Variables:
;; End:

;; foreign-setup.el ends here
