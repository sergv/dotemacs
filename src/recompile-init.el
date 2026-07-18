;; recompile-init.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  8 January 2025
;; Description:

(defvar compile--strict-mode t
  "Flag that becomes true during compilation. Intended to enable performing some checks
during compile-time that we don’t want to do at runtime.")

(defvar compile--in-progress t
  "Flag that becomes true during compilation. Intended to let modules distinguish
whether we’re really loading them or loading only to compile and thus e.g. some checks
should be omitted.")

(provide 'recompile-init)

;; Local Variables:
;; End:

;; recompile-init.el ends here
