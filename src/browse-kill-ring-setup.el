;; kill-ring-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  1 March 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'macro-util))

(require 'bkr)

;;;###autoload
(defun bkr-mode-setup ()
  (hl-line-mode -1))

;;;###autoload
(add-hook 'bkr-mode-hook #'bkr-mode-setup)

;;;###autoload
(defun browse-kill-ring ()
  (interactive)
  (bkr-start-for-variable 'kill-ring "*Kill Ring*"))

;;;###autoload
(defun browse-eshell-input-history ()
  (interactive)
  (bkr-start-for-variable 'eshell-history-ring "*Eshell History*"))

;;;###autoload
(defun browse-comint-input-history ()
  (interactive)
  (bkr-start-for-variable 'comint-input-ring "*Comint Input History*"))

(provide 'browse-kill-ring-setup)

;; Local Variables:
;; End:

;; kill-ring-setup.el ends here
