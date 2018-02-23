;; shm-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 23 February 2018
;; Description:

(defun shm-modeline-set-syntax-check-result (res &optional properties)
  (when (fboundp 'modeline-set-syntax-check-result)
    (modeline-set-syntax-check-result res properties)))

(provide 'shm-utils)

;; Local Variables:
;; End:

;; shm-utils.el ends here
