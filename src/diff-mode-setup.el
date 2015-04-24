;; diff-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 28 August 2014
;; Description:

(require 'common)

(defun diff-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil))

(add-hook 'diff-mode-hook #'diff-mode-setup)

(provide 'diff-mode-setup)

;; Local Variables:
;; End:

;; diff-mode-setup.el ends here
