;; tar-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 12 May 2024
;; Description:

;;;###autoload
(defun tar-mode-setup ()
  (hl-line-mode +1))

;;;###autoload
(add-hook 'tar-mode-hook #'tar-mode-setup)

(provide 'tar-mode-setup)

;; Local Variables:
;; End:

;; tar-mode-setup.el ends here
