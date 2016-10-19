;; windows-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 13 November 2013
;; Description:

(cl-assert (platform-os-type? 'windows) "Not on windows platform!")

(require 'common)
(require 'cygwin-mount)

(add-to-list 'exec-suffixes ".sh")

(cygwin-mount-activate)

(provide 'windows-setup)

;; Local Variables:
;; End:

;; windows-setup.el ends here
