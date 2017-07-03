;; windows-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 13 November 2013
;; Description:

(fold-platform-os-type
 (error "This should not load on non-windows platform!")
 nil)

(require 'common)
(require 'cygwin-mount)

(add-to-list 'exec-suffixes ".sh")

(cygwin-mount-activate)

(provide 'windows-setup)

;; Local Variables:
;; End:

;; windows-setup.el ends here
