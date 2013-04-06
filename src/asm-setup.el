;; asm-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 15 November 2012
;; Description:

(defun asm-mode-setup ()
  (set (make-variable-buffer-local 'whitespace-style)
       '(face lines-tail
              ;; tabs
              ))
  (init-common :use-yasnippet nil
               :use-comment t
               :use-whitespace t
               :use-render-formula t)

  (def-keys-for-map asm-mode-map
    (";" nil)))

(add-hook 'asm-mode-hook #'asm-mode-setup)

(provide 'asm-mode-setup)

;; Local Variables:
;; End:

;; asm-setup.el ends here
