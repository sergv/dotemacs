;; doc-view-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 August 2012
;; Description:

(eval-after-load
 "doc-view"
 '(progn
   (def-keys-for-map1 doc-view-mode-map +control-x-prefix+)
   (def-keys-for-map1 doc-view-mode-map +vim-special-keys+)
   ;; don't bind nor vi-keys nor vim's word-motion keys here as the're useless
   ;; when navigating pdfs
   (def-keys-for-map1 doc-view-mode-map
     (("h" image-backward-hscroll)
      ("t" doc-view-next-line-or-next-page)
      ("n" doc-view-previous-line-or-previous-page)
      ("s" image-forward-hscroll)
      ("p" nil)))))

;; (defun doc-view-setup ())
;; (add-hook 'doc-view-mode-hook 'doc-view-setup)

(provide 'doc-view-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;; doc-view-setup.el ends here
