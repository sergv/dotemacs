;; persistent-sessions-global-vars.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  3 January 2021
;; Description:

(defvar *sessions-global-variables*
  (alist->hash-table
   '((log-edit-comment-ring . t)
     (read-expression-history . t)))
  "List of global variables to save in session file.")

;;;###autoload
(defun sessions-mark-global-var-for-save (var)
  "Mark variable VAR to be saved via persistent sessions facility."
  (cl-assert (symbolp var))
  (puthash var t *sessions-global-variables*))

(provide 'persistent-sessions-global-vars)

;; Local Variables:
;; End:

;; persistent-sessions-global-vars.el ends here
