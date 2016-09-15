;; css-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 April 2015
;; Description:

;;;###autoload
(defun css-setup ()
  (init-common :use-whitespace 'tabs-only))

;;;###autoload
(add-hook 'css-mode-hook #'css-setup)

(provide 'css-setup)

;; Local Variables:
;; End:

;; css-setup.el ends here
