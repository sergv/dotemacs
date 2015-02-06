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

(setf ibuffer-formats '((mark modified read-only " " (name 32 32 :left :elide) " " (mode 16 16 :left :elide) " " filename-and-process)
                        (mark " " (name 32 -1) " " filename-and-process))
      ibuffer-jump-offer-only-visible-buffers t
      ibuffer-show-empty-filter-groups nil
      ibuffer-use-other-window nil)

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

     (define-ibuffer-filter eproj-root
         "Toggle current view to buffers with eproj project to QUALIFIER."
       (:description "eproj root"
                     :reader (read-from-minibuffer "Filter by eproj project root: "))
       (ignore-errors
         (string=? (eproj-project/root (eproj-get-project-for-buf buf))
                   qualifier)))

     (define-ibuffer-filter git-repository-root
         "Toggle current view to buffers with git repository equal to QUALIFIER."
       (:description "git repository root"
                     :reader (read-from-minibuffer "Filter by git repository root: "))
       (with-current-buffer buf
         (git-update-file-repository)
         (when git-repository
           (string=? qualifier git-repository))))

     (define-ibuffer-sorter major-mode-and-buffer-name
       "Sort the buffers by major modes and buffer names.
Ordering is lexicographic."
       (:description "major mode and buffer name")
       (let* ((buf-a (car a))
              (mode-a
               (downcase
                (symbol-name (buffer-local-value 'major-mode buf-a))))
              (buf-b (car b))
              (mode-b (downcase
                       (symbol-name (buffer-local-value 'major-mode buf-b)))))
         (or (string-lessp mode-a mode-b)
             (and (string-equal mode-a mode-b)
                  (string-lessp (buffer-name buf-a)
                                (buffer-name buf-b))))))

     (defun ibuffer-generate-filter-group-by-git-repository-root ()
       "Create ibuffer buffer-group specification based on each buffer's
git repository root"
       (if *have-git?*
         (let ((roots (sort
                       (remove-duplicates
                        (map #'strip-trailing-slash
                             (delq nil
                                   (map (lambda (buf)
                                          (with-current-buffer buf
                                            (git-update-file-repository)
                                            git-repository))
                                        (buffer-list))))
                        :test #'string-equal)
                       #'string<)))
           (map (lambda (repo-root)
                  (cons (format "git:%s" repo-root)
                        `((git-repository-root . ,repo-root))))
                roots))
         (error "No git installed on the system")))

     (defun ibuffer-generate-filter-group-by-eproj ()
       "Create ibuffer buffer-group specification based on each buffer's
git repository root"
       (map (lambda (root)
              (cons (format "eproj:%s" root)
                    `((eproj-root . ,root))))
            (sort
             (map #'eproj-project/root
                  (hash-table-values *eproj-projects*))
             #'string<)))

     ;; make it handle ibuffer-aux-fliter-groups and use case-insensetive completion
     (redefun ibuffer-switch-to-saved-filter-groups (name)
       "Set this buffer's filter groups to saved version with NAME.
The value from `ibuffer-saved-filter-groups' is used."
       (interactive
        (list
         (if (null? ibuffer-saved-filter-groups)
           (error "No saved filters")
           (let ((completion-ignore-case t))
             (icicle-completing-read "Switch to saved filter group: "
                                     (append (mapcar #'car ibuffer-saved-filter-groups)
                                             (mapcar #'car ibuffer-aux-filter-groups))
                                     nil
                                     t)))))
       (aif (cdr-safe (assoc name ibuffer-saved-filter-groups))
         (setq ibuffer-filter-groups it)
         (aif (cdr-safe (assoc name ibuffer-aux-filter-groups))
           (setq ibuffer-filter-groups (if (functionp it)
                                         (funcall it)
                                         it))
           (error "definition for group %s not found" name)))
       (setq ibuffer-hidden-filter-groups nil)
       (ibuffer-update nil t))

     (setf ibuffer-saved-filter-groups
           `(("haskell"
              ,(assoc "haskell"              +buffer-groups+)
              ,(assoc "proof assistants"     +buffer-groups+)
              ,(assoc "org"                  +buffer-groups+)
              ,(assoc "lowlevel programming" +buffer-groups+)
              ,(assoc "other programming"    +buffer-groups+)
              ,(assoc "dired"                +buffer-groups+)
              ,(assoc "utility"              +buffer-groups+))

             ("c/c++"
              ,(assoc "c/c++"                +buffer-groups+)
              ,(assoc "lowlevel programming" +buffer-groups+)
              ,(assoc "other programming"    +buffer-groups+)
              ,(assoc "dired"                +buffer-groups+)
              ,(assoc "utility"              +buffer-groups+))

             ("default"
              ,@+buffer-groups+)))

     (defparameter ibuffer-aux-filter-groups
       `(("git repo" . ,#'ibuffer-generate-filter-group-by-git-repository-root)
         ("eproj" . ,#'ibuffer-generate-filter-group-by-eproj))
       "List of auxiliary filter groups than can have default filter-group format
used by ibuffer or can be functions of no arguments that will be called to
generate actual filter group.")


     (setf ibuffer-never-show-predicates
           (list #'invisible-buffer?))

     (defun ibuffer-mark-using-mode (&optional by-regexp)
       (interactive (list current-prefix-arg))
       (if by-regexp
         (call-interactively #'ibuffer-mark-by-mode-regexp)
         (call-interactively #'ibuffer-mark-by-mode)))

     ;; ignore case when prompting for buffer name
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
                                              (ibuffer-current-state-list)))
                               (completion-ignore-case t))
                           (when (null table)
                             (error "No buffers!"))
                           (icicle-completing-read "Jump to buffer: "
                                                   table nil t))
                         (completing-read-buffer "Jump to buffer: " nil t)))))
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

     (defun ibuffer-cycle-buffers-forward (count)
       "Cycle through buffer list forward selecting next buffer"
       (interactive "p")
       (funcall
        (make-cycle-on-lines-in-region (if ibuffer-filter-groups 2 3) 2 forward)
        count))

     (defun ibuffer-cycle-buffers-backward (count)
       "Cycle through buffer list backward selecting next buffer"
       (interactive "p")
       (funcall
        (make-cycle-on-lines-in-region (if ibuffer-filter-groups 2 3) 2 backward)
        count))

     (defun ibuffer-visit-buffer-other-window (&optional single)
       "Visit the buffer on this line in other window."
       (interactive)
       (let ((buf (ibuffer-current-buffer t)))
         (switch-to-buffer-other-window buf)))

     (def-keys-for-map ibuffer-mode-map
       +vim-special-keys+
       ("C-k"      remove-buffer)
       ("C-S-k"    remove-buffer-and-window)

       ("S"        nil)
       ("S a"      ibuffer-do-sort-by-alphabetic)
       ("S f"      ibuffer-do-sort-by-filename/process)
       ("S m"      ibuffer-do-sort-by-major-mode-and-buffer-name)
       ("S r"      ibuffer-do-sort-by-recency)
       ("S M"      ibuffer-do-sort-by-major-mode)
       ("f"        nil)
       ("f m"      ibuffer-filter-by-mode)
       ("f n"      ibuffer-filter-by-name)
       ("f c"      ibuffer-filter-by-content)
       ("f f"      ibuffer-filter-by-filename)
       ("f g"      ibuffer-filter-by-git-repository-root)
       ("f p"      ibuffer-pop-filter)
       ("f o"      ibuffer-or-filter)
       ("SPC"      ibuffer-visit-buffer-other-window)
       ("* m"      ibuffer-mark-using-mode)
       ("* M"      ibuffer-mark-modified-buffers)
       ("* d"      ibuffer-mark-dired-buffers)
       ("* n"      ibuffer-mark-by-name-regexp)
       ("* f"      ibuffer-mark-by-file-name-regexp)
       ;; to be consistent with dired
       ("* %"      ibuffer-mark-by-file-name-regexp)
       ("U"        ibuffer-unmark-all)
       ("T"        ibuffer-toggle-marks)
       ("K"        ibuffer-do-delete)

       ("G"        ibuffer-switch-to-saved-filter-groups)

       ("C-z"      nil)
       ("v"        nil)
       ("/"        ibuffer-jump-to-buffer)

       ("<tab>"           ibuffer-forward-filter-group)
       ("<iso-lefttab>"   ibuffer-forward-filter-group)
       ("S-<tab>"         ibuffer-backward-filter-group)
       ("<S-iso-lefttab>" ibuffer-backward-filter-group)

       ("'"        ibuffer-backward-filter-group)
       ("E"        nil)
       ("e"        ibuffer-do-eval)
       ("r"        ibuffer-update)
       ("h"        ibuffer-cycle-buffers-forward)
       ("t"        ibuffer-cycle-buffers-backward)
       ("<escape>" remove-buffer)
       ("<down>"   ibuffer-cycle-buffers-forward)
       ("<up>"     ibuffer-cycle-buffers-backward))))


(provide 'ibuffer-setup)

;; Local Variables:
;; End:

;; ibuffer-setup.el ends here
