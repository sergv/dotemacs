;; asm-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 15 November 2012
;; Description:

;; TODO add nasm-mode

(eval-when-compile
  (require 'macro-util)
  (defvar asm-mode-map))

;;;###autoload
(setq auto-mode-alist
      (cons '("\\.\\(?:dump-\\)?asm\\'" . nasm-mode)
            auto-mode-alist))

;;;###autoload
(defun nasm-mode-setup ()
  (init-common :use-yasnippet      nil
               :use-comment        t
               :use-fci            t
               :use-render-formula t
               :use-whitespace     'tabs-only))

;;;###autoload
(add-hook 'nasm-mode-hook #'nasm-mode-setup)

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

(provide 'asm-setup)

;; Local Variables:
;; End:

;; asm-setup.el ends here
