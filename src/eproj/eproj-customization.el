;; eproj-customization.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  2 July 2016
;; Description:

(defgroup eproj
  nil
  "Emacs PROJect management."
  :group 'tools)

(defcustom eproj-symbnav-remember-choices nil
  "Whether to remember choices of previous tags and unconditionally jump to them
when the same navigation ambiguity occurs."
  :type 'boolean
  :group 'eproj)

;;; configurable parameters

(defcustom eproj-verbose-tag-loading t
  "Show progress on project and tag loading"
  :type 'boolean
  :group 'eproj)

(provide 'eproj-customization)

;; Local Variables:
;; End:

;; eproj-customization.el ends here
