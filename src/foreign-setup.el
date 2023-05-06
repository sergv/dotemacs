;; foreign-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 June 2018
;; Description:

(eval-when-compile
  (require 'set-up-platform))

(defvar dumping)

(defcustom use-foreign-libraries? t
  "Whether to use foreign extensions to Emacs."
  :type 'boolean
  :group 'common)

(when (and use-foreign-libraries?
           (not dumping))
  (load (fold-platform-os-type "libemacs-native" "emacs-native")))

(provide 'foreign-setup)

;; Local Variables:
;; End:

;; foreign-setup.el ends here
