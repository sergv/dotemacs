;; messages-buffer-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 January 2022
;; Description:

;;;###autoload
(defun messages-buffer-setup ()
  (hl-line-mode +1))

;;;###autoload
(add-hook 'messages-buffer-mode-hook #'messages-buffer-setup)

(provide 'messages-buffer-setup)

;; Local Variables:
;; End:

;; messages-buffer-setup.el ends here
