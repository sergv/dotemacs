;; other-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 26 September 2012
;; Description:

(autoload 'xmodmap-mode "xmodmap-mode" "" t)
(add-to-list 'auto-mode-alist
             (cons (rx (or (seq (? ".") "xmodmaprc")
                           "\.xmodmap"))
                   'xmodmap-mode))

(defun xmodmap-setup ()
  (init-common :use-yasnippet nil))
(add-hook 'xmodmap-mode-hook #'xmodmap-setup)

;; Local Variables:
;; End:

;; other-setup.el ends here
