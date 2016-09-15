;; image-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  6 September 2012
;; Description:

;;;###autoload
(defun image-mode-setup ()
  (def-keys-for-map image-mode-map
    +vim-special-keys+
    ("d" image-backward-hscroll)
    ("h" image-scroll-down)
    ("t" image-scroll-up)
    ("n" image-forward-hscroll)
    ("r" (lambda () (interactive) (revert-buffer nil t)))

    ("<left>"  image-backward-hscroll)
    ("<down>"  image-scroll-down)
    ("<up>"    image-scroll-up)
    ("<right>" image-forward-hscroll)))

;;;###autoload
(add-hook 'image-mode-hook #'image-mode-setup)

(provide 'image-mode-setup)

;; Local Variables:
;; End:

;; image-mode-setup.el ends here
