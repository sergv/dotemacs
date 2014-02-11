;; windows-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 13 November 2013
;; Description:

(require 'common)

(assert (platform-os-type? 'windows) "Not on windows platform!")

(when (executable-find "mount")
  (require 'cygwin-mount)
  (cygwin-mount-activate))

(add-to-list 'exec-suffixes ".sh")

(provide 'windows-setup)

;; Local Variables:
;; End:

;; windows-setup.el ends here
