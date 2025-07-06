;; indirect-aware-save-buffer.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  7 July 2025
;; Description:

(defvar indirect-aware-after-save-hook nil
  "Special version of ‘after-save-hook’ that runs in indirect buffer and not
in the base buffer that was actually saved.")

;;;###autoload
(defun indirect-aware-save-buffer ()
  "Like ‘save-buffer’ but runs correct after-save-hook for
indirect buffers so that hook functions can observe the original
indirect buffer being saved."
  (interactive)
  (when (basic-save-buffer)
    (run-hooks 'indirect-aware-after-save-hook)))

(provide 'indirect-aware-save-buffer)

;; Local Variables:
;; End:

;; indirect-aware-save-buffer.el ends here
