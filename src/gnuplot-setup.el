;;; gnuplot-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 16 August 2011
;; Keywords:
;; Requirements:
;; Status:

;; (require 'gnuplot)

(autoload 'gnuplot-mode "gnuplot" "Start gnuplot mode.")


(setq auto-mode-alist (append '(("\\.gp\\'"  . gnuplot-mode)
                                ("\\.gpi\\'" . gnuplot-mode))
                              auto-mode-alist))

(defun gnuplot-setup ()
  (init-common)
  (autopair-mode))

(add-hook 'gnuplot-mode-hook #'gnuplot-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; gnuplot-setup.el ends here
