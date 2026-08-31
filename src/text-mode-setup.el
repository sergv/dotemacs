;; text-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 25 June 2020
;; Description:

(require 'indentation)
(require 'folding-setup)

;;;###autoload
(defun text-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-render-formula nil
               :use-whitespace t)
  (setq-local hs-allow-nesting t)
  (setup-indent-size 2)
  (setq-local tab-width 8)
  (let (
        ;; trick hideshow into thinknig that comments are available.
        (commend-start "#")
        (commend-end ""))
    (hs-minor-mode-initialize
     :start (rx (or "[" "(" "{"))
     :comment-start-re "#")
    (setup-hideshow-yafolding t nil)))

;;;###autoload
(add-hook 'text-mode-hook #'text-mode-setup)

;;;###autoload
(defun change-log-mode-setup ()
  (text-mode-setup))

;;;###autoload
(add-hook 'change-log-mode-hook #'change-log-mode-setup)

;;;###autoload
(defun outline-mode-setup ()
  (text-mode-setup))

;;;###autoload
(add-hook 'outline-mode-hook #'outline-mode-setup)

(provide 'text-mode-setup)

;; Local Variables:
;; End:

;; text-mode-setup.el ends here
