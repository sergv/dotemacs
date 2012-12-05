;; minimap-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 December 2012
;; Description:

(setf minimap-width-fraction 0.15
      minimap-window-location 'right
      minimap-hide-scroll-bar t
      minimap-hide-fringes t
      minimap-recenter-type 'relative ;; 'free
      )

(defun minimap-enabled? ()
  (not (null? minimap-bufname)))

(defun minimap-toggle ()
  "If there's no minimap, create one; if there's one - kill it."
  (interactive)
  (if (minimap-enabled?)
    (minimap-kill)
    (minimap-create)))


(eval-after-load
 "minimap"
 '((add-hook '&&hdr-outline-view-change-hook #'minimap-sync-overlays)

   (def-keys-for-map minimap-mode-map
     ("<escape>" remove-buffer-and-window)
     ("q"        remove-buffer-and-window))))


(provide 'minimap-setup)

;; Local Variables:
;; End:

;; minimap-setup.el ends here
