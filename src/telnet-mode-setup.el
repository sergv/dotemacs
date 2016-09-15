;; telnet-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 29 September 2012
;; Description:

;;;###autoload
(defun telnet-mode-setup ()
  (init-common :use-yasnippet nil :use-comment nil))

;;;###autoload
(add-hook 'telnet-mode-hook #'telnet-mode-setup)

(provide 'telnet-mode-setup)

;; Local Variables:
;; End:

;; telnet-setup.el ends here
