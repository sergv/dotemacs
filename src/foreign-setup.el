;; foreign-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 June 2018
;; Description:

(defcustom use-foreign-libraries? t
  "Whether to use foreign extensions to Emacs."
  :type 'boolean
  :group 'common)

(when use-foreign-libraries?
  (load (fold-platform-os-type "libemacs_native_rs" "emacs_native_rs")))

(provide 'foreign-setup)

;; Local Variables:
;; End:

;; foreign-setup.el ends here
