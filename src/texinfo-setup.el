;; texinfo-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  5 February 2012
;; Description:

;;;###autoload
(defun texinfo-setup ()
  (init-common :use-yasnippet nil))

;;;###autoload
(add-hook 'texinfo-mode-hook #'texinfo-setup)

;;;###autoload
(add-hook 'Texinfo-mode-hook #'texinfo-setup)

(provide 'texinfo-setup)

;; Local Variables:
;; End:

;; texinfo-setup.el ends here
