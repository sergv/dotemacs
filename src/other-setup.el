;; other-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 26 September 2012
;; Description:

;;;###autoload
(autoload 'xmodmap-mode "xmodmap-mode" "" t)

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx (or (seq (? ".") "xmodmaprc")
                           ".xmodmap")
                       eos)
                   'xmodmap-mode))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx "config.log" eos)
                   'text-mode))

;;;###autoload
(defun xmodmap-setup ()
  (init-common :use-yasnippet nil))

;;;###autoload
(add-hook 'xmodmap-mode-hook #'xmodmap-setup)

;;;###autoload
(defun Man-setup ()
  (hl-line-mode +1))

;;;###autoload
(add-hook 'Man-mode-hook #'Man-setup)

;;;###autoload
(defun archive-setup ()
  (hl-line-mode +1))

;;;###autoload
(add-hook 'archive-mode-hook #'archive-setup)

;;;###autoload
(defun mesages-buffer-setup ()
  (hl-line-mode +1))

;;;###autoload
(add-hook 'mesages-buffer-mode-hook #'mesages-buffer-setup)

(provide 'other-setup)

;; Local Variables:
;; End:

;; other-setup.el ends here
