;; emms-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  1 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'set-up-paths)
(add-to-list 'load-path (path-concat +emacs-standalone-path+
                                     "emms"
                                     "lisp"))

(require 'emms-auto)
(setf emms-cache-file (concat +prog-data-path+ "/emms/cache")
      emms-directory (concat +prog-data-path+ "/emms"))

(eval-after-load "emms-playlist-mode"
  '(progn
     (require 'emms-setup)
     (emms-standard)
     (emms-default-players)

     (defun emms-setup-mode ()
       (def-keys-for-map emms-playlist-mode-map
         +control-x-prefix+
         +vim-special-keys+
         ("u"        emms-playlist-mode-undo)
         ("t"        next-line)
         ("n"        previous-line)
         ("s"        nil)
         ("h"        nil)
         ("<left>"   nil)
         ("<right>"  nil)
         ("<up>"     previous-line)
         ("<down>"   next-line)
         ("p"        emms-playlist-mode-yank)

         ("SPC"      emms-pause)
         ("k"        emms-playlist-mode-kill-track)
         ("K"        emms-playlist-mode-kill-entire-track)
         ("d"        emms-playlist-mode-kill-track)
         ("D"        emms-playlist-mode-kill-entire-track)))

     (add-hook 'emms-playlist-mode-hook #'emms-setup-mode)))

(provide 'emms-setup)

;; Local Variables:
;; End:

;; emms-setup.el ends here
