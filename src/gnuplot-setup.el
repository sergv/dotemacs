;; gnuplot-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 16 August 2011
;; Keywords:
;; Requirements:
;; Status:

;; (require 'gnuplot)

;;;###autoload
(setq auto-mode-alist (append '(("\\.gp\\'"  . gnuplot-mode)
                                ("\\.gpi\\'" . gnuplot-mode))
                              auto-mode-alist))

;;;###autoload
(defun gnuplot-setup ()
  (init-common :use-render-formula t))

;;;###autoload
(add-hook 'gnuplot-mode-hook #'gnuplot-setup)

(provide 'gnuplot-setup)

;; Local Variables:
;; End:

;; gnuplot-setup.el ends here
