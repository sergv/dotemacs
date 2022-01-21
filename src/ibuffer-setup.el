;; ibuffer-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 11 January 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'ibuf-macs)
  (require 'macro-util)
  (require 'hydra-setup))

(require 'el-patch)
(require 'ibuffer)

(defalias 'list-buffers 'ibuffer)

(setf ibuffer-formats '((mark modified read-only " " (name 32 32 :left :elide) " " (mode 16 16 :left :elide) " " filename-and-process)
                        (mark " " (name 32 -1) " " filename-and-process))
      ibuffer-jump-offer-only-visible-buffers t
      ibuffer-show-empty-filter-groups nil
      ibuffer-use-other-window nil)

;;;###autoload
(el-patch-feature ibuffer)
;;;###autoload
(el-patch-feature ibuf-ext)

;; make it handle ibuffer-aux-filter-groups and use case-insensetive completion
(el-patch-defun ibuffer-switch-to-saved-filter-groups (name)
  "Set this buffer's filter groups to saved version with NAME.
The value from `ibuffer-saved-filter-groups' is used."
  (interactive
   (list
    (cond ((null ibuffer-saved-filter-groups)
           (error "No saved filters"))
          ;; `ibuffer-saved-filter-groups' is a user variable that defaults
          ;; to nil.  We assume that with one element in this list the user
          ;; knows what she wants.  See bug#12331.
          ((null (cdr ibuffer-saved-filter-groups))
           (caar ibuffer-saved-filter-groups))
          (t
           (el-patch-wrap 2 0
             (let ((completion-ignore-case t))
               ((el-patch-swap completing-read ido-completing-read)
                "Switch to saved filter group: "
                (el-patch-swap
                  ibuffer-saved-filter-groups
                  (append ibuffer-saved-filter-groups
                          ibuffer-aux-filter-groups))
                nil
                t)))))))
  (setq ibuffer-filter-groups (cdr (assoc name ibuffer-saved-filter-groups))
        ibuffer-hidden-filter-groups nil)
  (ibuffer-update nil t))

(defun ibuffer-setup ()
  (hl-line-mode +1)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook #'ibuffer-setup)

(defun ibuffer-generate-filter-group-by-git-repository-root ()
  "Create ibuffer buffer-group specification based on each buffer's
git repository root"
  (if *have-git?*
      (let ((roots (remove-duplicates-sorting
                    (-map #'strip-trailing-slash
                          (delq nil
                                (-map (lambda (buf)
                                        (with-current-buffer buf
                                          (git-update-file-repository)
                                          git-repository))
                                      (buffer-list))))
                    #'string=
                    #'string<)))
        (-map (lambda (repo-root)
                (cons (format "git:%s" repo-root)
                      `((git-repository-root . ,repo-root))))
              roots))
    (error "No git installed on the system")))

(defun ibuffer-generate-filter-group-by-eproj ()
  "Create ibuffer buffer-group specification based on each buffer's
git repository root"
  (-map (lambda (root)
          (cons (format "eproj:%s" root)
                `((eproj-root . ,root))))
        (sort
         (-map #'eproj-project/root
               (hash-table-values *eproj-projects*))
         #'string<)))

(defun ibuffer-mark-using-mode (&optional by-regexp)
    (interactive (list current-prefix-arg))
    (if by-regexp
        (call-interactively #'ibuffer-mark-by-mode-regexp)
      (call-interactively #'ibuffer-mark-by-mode)))

  (defun ibuffer-cycle-buffers-forward (count)
    "Cycle through buffer list forward selecting next buffer"
    (interactive "p")
    (funcall
     (make-cycle-on-lines-in-region (if ibuffer-filter-groups 2 3) 2 t)
     count))

  (defun ibuffer-cycle-buffers-backward (count)
    "Cycle through buffer list backward selecting next buffer"
    (interactive "p")
    (funcall
     (make-cycle-on-lines-in-region (if ibuffer-filter-groups 2 3) 2 nil)
     count))

  (defun ibuffer-visit-buffer-other-window ()
    "Visit the buffer on this line in other window."
    (interactive)
    (let ((buf (ibuffer-current-buffer t)))
      (switch-to-buffer-other-window buf)))

(defhydra-ext hydra-ibuffer-sorting (:exit t :foreign-keys nil :hint nil)
  "
_a_lphabetic
_f_ilename
_m_ajor mode and buffer name
_r_ecency
_M_ajor mode
"
  ("a" ibuffer-do-sort-by-alphabetic)
  ("f" ibuffer-do-sort-by-filename/process)
  ("m" ibuffer-do-sort-by-major-mode-and-buffer-name)
  ("r" ibuffer-do-sort-by-recency)
  ("M" ibuffer-do-sort-by-major-mode))

(defhydra-ext hydra-ibuffer-filter (:exit t :foreign-keys nil :hint nil)
  "
_c_ontent              _p_op filter
_f_ilename             _o_r filter
_g_it repository root
_m_ode
_n_ame
"
  ("m" ibuffer-filter-by-mode)
  ("n" ibuffer-filter-by-name)
  ("c" ibuffer-filter-by-content)
  ("f" ibuffer-filter-by-filename)
  ("g" ibuffer-filter-by-git-repository-root)

  ("p" ibuffer-pop-filter)
  ("o" ibuffer-or-filter))

(defhydra-ext hydra-ibuffer-mark (:exit t :foreign-keys nil :hint nil)
  "
_d_ired buffers     _f_ilename regexp
_m_ode              _%_: filename regexp
_M_odified buffers
_n_name
"
  ("m" ibuffer-mark-using-mode)
  ("M" ibuffer-mark-modified-buffers)
  ("d" ibuffer-mark-dired-buffers)
  ("n" ibuffer-mark-by-name-regexp)
  ("f" ibuffer-mark-by-file-name-regexp)

  ;; to be consistent with dired
  ("%" ibuffer-mark-by-file-name-regexp))

(defhydra-ext hydra-ibuffer (:exit t :foreign-keys nil :hint nil)
  "
_s_ort          _*_: mark
_f_ilter        _k_: unmark forward
_m_ark          _K_: unmark all
_T_oggle marks

_SPC_: visit buffer other window
_,_: mark for delete
"
  ("S"   hydra-ibuffer-sorting/body)
  ("f"   hydra-ibuffer-filter/body)

  ("SPC" ibuffer-visit-buffer-other-window)
  ("*"   hydra-ibuffer-mark/body)
  ("m"   hydra-ibuffer-mark/body)

  ("T"   ibuffer-toggle-marks)

  (","   ibuffer-mark-for-delete)
  ("k"   ibuffer-unmark-forward)
  ("K"   ibuffer-unmark-all))

(defun ibuffer-init ()
  (require 'ibuf-ext)
  (require 'buffer-groups)

  (define-ibuffer-filter name-not-matches
      "Toggle current view to buffers with name not matching QUALIFIER."
    (:description "buffer name, no match"
                  :reader (read-from-minibuffer "Filter by not matching (regexp): "))
    (not (string-match-p qualifier (buffer-name buf))))

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

  (defvar ibuffer-aux-filter-groups
    `(("git repo" . ,#'ibuffer-generate-filter-group-by-git-repository-root)
      ("eproj" . ,#'ibuffer-generate-filter-group-by-eproj))
    "List of auxiliary filter groups than can have default filter-group format
used by ibuffer or can be functions of no arguments that will be called to
generate actual filter group.")

  (setf ibuffer-never-show-predicates (list #'invisible-buffer?))

  (def-keys-for-map ibuffer-mode-map
    +vim-special-keys+
    +vim-search-keys+

    ("-"        hydra-ibuffer/body)

    ("T"        ibuffer-toggle-marks)

    ("d"        nil)
    (","        ibuffer-mark-for-delete)
    ("k"        ibuffer-unmark-forward)
    ("K"        ibuffer-unmark-all)

    ("C-z"      nil)
    ("v"        nil)

    ("<tab>"           ibuffer-forward-filter-group)
    ("<iso-lefttab>"   ibuffer-forward-filter-group)
    ("S-<tab>"         ibuffer-backward-filter-group)
    ("<S-iso-lefttab>" ibuffer-backward-filter-group)

    ("'"          ibuffer-backward-filter-group)
    ("E"          nil)
    ("e"          ibuffer-do-eval)
    (("H" "<f5>") ibuffer-update)
    ("h"          ibuffer-cycle-buffers-forward)
    ("t"          ibuffer-cycle-buffers-backward)
    ("<escape>"   quit-window)
    ("<down>"     ibuffer-cycle-buffers-forward)
    ("<up>"       ibuffer-cycle-buffers-backward)))

(eval-after-load "ibuffer" '(ibuffer-init))

(provide 'ibuffer-setup)

;; Local Variables:
;; End:

;; ibuffer-setup.el ends here
