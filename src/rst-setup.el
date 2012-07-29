;;; rst-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 30 January 2012
;; Keywords:
;; Requirements:
;; Status:

(defun rst-setup ()
  (init-common))

(add-hook 'rst-mode-hook #'rst-setup)

(provide 'rst-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; rst-setup.el ends here
