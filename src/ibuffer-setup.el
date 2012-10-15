;; ibuffer-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 11 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'ibuffer)


(defalias 'list-buffers 'ibuffer)

(eval-after-load
 "ibuffer"
 '(progn
   (require 'ibuf-ext)
   (require 'buffer-groups)

   (add-hook 'ibuffer-mode-hook
    (lambda ()
      (ibuffer-switch-to-saved-filter-groups "default")))

   (define-ibuffer-filter name-not-matches
    "Toggle current view to buffers with name not matching QUALIFIER."
    (:description "buffer name, no match"
     :reader (read-from-minibuffer "Filter by not matching (regexp): "))
    (not (string-match-pure? qualifier (buffer-name buf))))

   (setf ibuffer-saved-filter-groups
         `(("lisp"
            ,(assoc "lisp"       +buffer-groups+)
            ,(assoc "slime"      +buffer-groups+)
            ,(assoc "emacs lisp" +buffer-groups+)
            ,(assoc "scheme"     +buffer-groups+)
            ,(assoc "org"        +buffer-groups+))

           ("math"
            ,(assoc "haskell"    +buffer-groups+)
            ,(assoc "prolog"     +buffer-groups+)
            ,(assoc "octave"     +buffer-groups+)
            ,(assoc "maxima"     +buffer-groups+)
            ,(assoc "org"        +buffer-groups+))

           ("default"
            ,(assoc "lisp"       +buffer-groups+)
            ,(assoc "slime"      +buffer-groups+)
            ,(assoc "emacs lisp" +buffer-groups+)
            ,(assoc "scheme"     +buffer-groups+)

            ,(assoc "haskell"    +buffer-groups+)
            ,(assoc "prolog"     +buffer-groups+)
            ,(assoc "octave"     +buffer-groups+)
            ,(assoc "maxima"     +buffer-groups+)

            ,(assoc "c/cpp"      +buffer-groups+)
            ,(assoc "python"     +buffer-groups+)
            ,(assoc "cython"     +buffer-groups+)
            ,(assoc "org"        +buffer-groups+)
            ,(assoc "books"      +buffer-groups+)
            ,(assoc "latex"      +buffer-groups+)
            ,(assoc "web"        +buffer-groups+)
            ,(assoc "vc"         +buffer-groups+)
            ,(assoc "other programming" +buffer-groups+)

            ,(assoc "utility"    +buffer-groups+)
            ,(assoc "dired"      +buffer-groups+)
            ,(assoc "other"      +buffer-groups+))
           ("all")))


   (setf ibuffer-never-show-predicates
         (list #'invisible-buffer?))

   (defun ibuffer-mark-using-mode (&optional by-regexp)
     (interactive (list current-prefix-arg))
     (if by-regexp
       (call-interactively #'ibuffer-mark-by-mode-regexp)
       (call-interactively #'ibuffer-mark-by-mode)))

   ;; (def-keys-for-map ibuffer-mode-map +vi-essential-keys+)
   (def-keys-for-map ibuffer-mode-map
     +control-x-prefix+
     +vim-special-keys+
     ;;("q"        remove-buffer)
     ("C-k"      remove-buffer)
     ("C-S-k"    remove-buffer-and-window)

     ("f"        nil)
     ("f m"      ibuffer-filter-by-mode)
     ("f n"      ibuffer-filter-by-name)
     ("f c"      ibuffer-filter-by-content)
     ("f f"      ibuffer-filter-by-filename)
     ("f p"      ibuffer-pop-filter)
     ("f o"      ibuffer-or-filter)
     ("SPC"      ibuffer-filter-disable)
     ("* m"      ibuffer-mark-using-mode)
     ("* M"      ibuffer-mark-modified-buffers)
     ("* d"      ibuffer-mark-dired-buffers)
     ("* n"      ibuffer-mark-by-name-regexp)
     ("* f"      ibuffer-mark-by-file-name-regexp)
     ("U"        ibuffer-unmark-all)
     ("T"        ibuffer-toggle-marks)
     ("K"        ibuffer-do-delete)

     ("G"        ibuffer-switch-to-saved-filter-groups)

     ("C-z"      nil)
     ("/"        ibuffer-jump-to-buffer)

     ("<tab>"           ibuffer-forward-filter-group)
     ("<iso-lefttab>"   ibuffer-forward-filter-group)
     ("S-<tab>"         ibuffer-backward-filter-group)
     ("<S-iso-lefttab>" ibuffer-backward-filter-group)

     ("r"        ibuffer-update)
     ("t"        ibuffer-cycle-buffers-forward)
     ("n"        ibuffer-cycle-buffers-backward)
     ("<escape>" remove-buffer)
     ("<down>"   ibuffer-cycle-buffers-forward)
     ("<up>"     ibuffer-cycle-buffers-backward))))


(provide 'ibuffer-setup)

;; Local Variables:
;; End:

;; ibuffer-setup.el ends here
