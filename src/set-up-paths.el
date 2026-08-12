;; set-up-paths.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  6 November 2012
;; Description:
;; Set up paths that should be the same between different Emacs processes.

(eval-when-compile
  (require 'cl-lib)
  (require 'set-up-platform))

(defconst +tmp-global-path+
  (fold-platform-os-type
   (if (getenv "IN_NIX_SHELL")
       "/tmp"
     temporary-file-directory)
   temporary-file-directory)
  "Path to temporary files that are visible across different emacs instances.")

(provide 'set-up-paths)

;; Local Variables:
;; End:

;; set-up-paths.el ends here
