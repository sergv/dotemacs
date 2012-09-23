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

   (redefun ibuffer-jump-to-buffer (name)
     "Move point to the buffer whose name is NAME.

If called interactively, prompt for a buffer name and go to the
corresponding line in the Ibuffer buffer.  If said buffer is in a
hidden group filter, open it.

If `ibuffer-jump-offer-only-visible-buffers' is non-nil, only offer
visible buffers in the completion list.  Calling the command with
a prefix argument reverses the meaning of that variable."
     (interactive (list
                   (let ((only-visible ibuffer-jump-offer-only-visible-buffers))
                     (when current-prefix-arg
                       (setq only-visible (not only-visible)))
                     (if only-visible
                       (let ((table (mapcar #'(lambda (x)
                                                (buffer-name (car x)))
                                            (ibuffer-current-state-list))))
                         (when (null table)
                           (error "No buffers!"))
                         (completing-read-vanilla "Jump to buffer: "
                                                  table
                                                  nil
                                                  t))
                       (read-buffer "Jump to buffer: " nil t)))))
     (when (not (string= "" name))
       (let (buf-point)
         ;; Blindly search for our buffer: it is very likely that it is
         ;; not in a hidden filter group.
         (ibuffer-map-lines #'(lambda (buf _marks)
                                (when (string= (buffer-name buf) name)
                                  (setq buf-point (point))
                                  nil))
                            t nil)
         (when (and
                (null buf-point)
                (not (null ibuffer-hidden-filter-groups)))
           ;; We did not find our buffer.  It must be in a hidden filter
           ;; group, so go through all hidden filter groups to find it.
           (catch 'found
             (dolist (group ibuffer-hidden-filter-groups)
               (ibuffer-jump-to-filter-group group)
               (ibuffer-toggle-filter-group)
               (ibuffer-map-lines #'(lambda (buf _marks)
                                      (when (string= (buffer-name buf) name)
                                        (setq buf-point (point))
                                        nil))
                                  t group)
               (if buf-point
                 (throw 'found nil)
                 (ibuffer-toggle-filter-group)))))
         (if (null buf-point)
           ;; Still not found even though we expanded all hidden filter
           ;; groups: that must be because it's hidden by predicate:
           ;; we won't bother trying to display it.
           (error "No buffer with name %s" name)
           (goto-char buf-point)))))

   (redefun ibuffer-switch-to-saved-filter-groups (name)
     "Set this buffer's filter groups to saved version with NAME.
The value from `ibuffer-saved-filter-groups' is used."
     (interactive
      (list
       (if (null ibuffer-saved-filter-groups)
         (error "No saved filters")
         (completing-read-vanilla "Switch to saved filter group: "
                                  ibuffer-saved-filter-groups nil t))))
     (setq ibuffer-filter-groups (cdr (assoc name ibuffer-saved-filter-groups))
           ibuffer-hidden-filter-groups nil)
     (ibuffer-update nil t))

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
