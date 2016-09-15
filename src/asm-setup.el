;; asm-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 15 November 2012
;; Description:

;;;###autoload
(defun asm-mode-setup ()
  (init-common :use-yasnippet      nil
               :use-comment        t
               :use-fci            t
               :use-render-formula t
               :use-whitespace     'tabs-only)

  ;; do not colorize tabs
  (setq-local whitespace-style (remove 'tabs whitespace-style))
  (def-keys-for-map asm-mode-map
    (";" nil)))

;;;###autoload
(add-hook 'asm-mode-hook #'asm-mode-setup)

(provide 'asm-mode-setup)

;; Local Variables:
;; End:

;; asm-setup.el ends here
