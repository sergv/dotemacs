;; tagged-buflist-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 25 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)

(autoload 'tagged-buflist-show "tagged-buflist"
          "Switch to tagged buffer list." t)

(eval-after-load "tagged-buflist"
  '(progn
     (defun tagged-buflist-cycle-buffers-forward (count)
       "Cycle through buffer list forward selecting next buffer"
       (interactive "p")
       (funcall
        (make-cycle-on-lines-in-region 0 0 forward)
        count))

     (defun tagged-buflist-cycle-buffers-backward (count)
       "Cycle through buffer list backward selecting next buffer"
       (interactive "p")
       (funcall
        (make-cycle-on-lines-in-region 0 0 backward)
        count))

     (def-keys-for-map tagged-buflist-mode-map
       ("C-k"      remove-buffer)
       ("C-S-k"    remove-buffer-and-window)
       ("<escape>" remove-buffer)
       ("t"        tagged-buflist-cycle-buffers-forward)
       ("n"        tagged-buflist-cycle-buffers-backward)
       ("<down>"   tagged-buflist-cycle-buffers-forward)
       ("<up>"     tagged-buflist-cycle-buffers-backward))))

(def-keys-for-map global-map
  ("<XF86Favorites>" tagged-buflist-show)
  ("C-x b"           tagged-buflist-show)
  ("C-b"             icicle-buffer)
  ("M-b"             tagged-buflist-show))

(def-keys-for-map vim:normal-mode-keymap
  (", b" tagged-buflist-show))

(provide 'tagged-buflist-setup)

;; Local Variables:
;; End:

;; tagged-buflist-setup.el ends here
