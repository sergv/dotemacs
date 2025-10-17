;; git-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 April 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'el-patch)
  (require 'set-up-platform)
  (require 'macro-util))

(require 'align-util)
(require 'common)
(require 'common-heavy)
(require 'dash)
(require 'el-patch)
(require 'hydra-setup)
(require 'persistent-store)
(require 'pseudovim)
(require 'magit)
(require 'magit-blame)
(require 'search)
(require 'transient-fixes)
(require 'vim-setup)

;;;###autoload
(el-patch-feature magit)

(setf magit-completing-read-function 'ivy-completing-read)

(dolist (x '(magit-reset-soft magit-reset-hard magit-reset-head magit-reset magit-reset-index))
  (push (cons x nil) ivy-sort-functions-alist))

(defun magit-rebase-interactive-1--preserve-commiter-date
    (old-magit-rebase-interactive-1 commit args message &optional editor delay-edit-confirm noassert confirm)
  (let ((amended-args (if (member "--committer-date-is-author-date" args)
                          args
                        (cons "--committer-date-is-author-date" args))))
    (funcall old-magit-rebase-interactive-1 commit amended-args message editor delay-edit-confirm noassert confirm)))

(advice-add 'magit-rebase-interactive-1 :around #'magit-rebase-interactive-1--preserve-commiter-date)

;;; Magit redefinitions

(el-patch-defun magit-reset-read-branch-or-commit (prompt)
  "Prompt for and return a ref to reset HEAD to.

PROMPT is a format string, where either the current branch name
or \"detached head\" will be substituted for %s."
  ((el-patch-swap
     magit-read-branch-or-commit
     magit-read-branch-or-commit-prompt-for-previous-commit-first)
   (format prompt (or (magit-get-current-branch) "detached head"))))

(defun magit-read-branch-or-commit-prompt-for-previous-commit-first (prompt &optional _secondary-default)
  (or (magit-completing-read prompt (cons "HEAD^" (cons "HEAD" (magit-list-refnames)))
                             nil nil nil 'magit-revision-history
                             nil)
      (user-error "Nothing selected")))

(el-patch-defun magit-rebase-interactive-assert (since &optional delay-edit-confirm rebase-merges)
  (el-patch-remove
    (let* ((commit (magit-rebase--target-commit since))
           (branches (magit-list-publishing-branches commit)))
      (setq magit--rebase-public-edit-confirmed
            (delete (magit-toplevel) magit--rebase-public-edit-confirmed))
      (when (and branches
                 (or (not delay-edit-confirm)
                     ;; The user might have stopped at a published commit
                     ;; merely to add new commits *after* it.  Try not to
                     ;; ask users whether they really want to edit public
                     ;; commits, when they don't actually intend to do so.
                     (not (--all-p (magit-rev-equal it commit) branches))))
        (let ((m1 "Some of these commits have already been published to ")
              (m2 ".\nDo you really want to modify them"))
          (magit-confirm (or magit--rebase-published-symbol 'rebase-published)
            (concat m1 "%s" m2)
            (concat m1 "%d public branches" m2)
            nil branches))
        (push (magit-toplevel) magit--rebase-public-edit-confirmed))))
  (if (and (magit-git-lines "rev-list" "--merges" (concat since "..HEAD"))
           (not rebase-merges))
      (magit-read-char-case "Proceed despite merge in rebase range?  " nil
        (?c "[c]ontinue" since)
        (?s "[s]elect other" nil)
        (?a "[a]bort" (user-error "Quit")))
    since))

(el-patch-defun magit-commit-amend-assert (&optional commit)
  (el-patch-swap
    (when-let ((branches (magit-list-publishing-branches commit)))
      (let ((m1 "This commit has already been published to ")
            (m2 ".\nDo you really want to modify it"))
        (magit-confirm 'amend-published
          (concat m1 "%s" m2)
          (concat m1 "%d public branches" m2)
          nil branches)))
    t))

;;; Speed up magit with some caching

(defvar magit--rev-parse-main-cache
  (let ((tbl (make-hash-table :test #'equal)))
    (puthash "--show-toplevel" (make-hash-table :test #'equal) tbl)
    (puthash "--show-cdup" (make-hash-table :test #'equal) tbl)
    (puthash "--git-dir" (make-hash-table :test #'equal) tbl)
    tbl)
  "Hash table from command name into hash table from `default-directory' to
directory computed by git.")

(defun memoize-rev-parse (fun &rest args)
  (pcase args
    (`(,cmd)
     (aif (gethash cmd magit--rev-parse-main-cache)
         (or (gethash default-directory it)
             (let ((dir (apply fun args)))
               (puthash default-directory dir it)
               dir))
       (apply fun args)))
    (_ (apply fun args))))

(advice-add 'magit-rev-parse :around #'memoize-rev-parse)
(advice-add 'magit-rev-parse-safe :around #'memoize-rev-parse)

;;; gitignore

;;;###autoload
(defun gitignore-setup ()
  (init-common :use-yasnippet nil
               :use-comment   t
               :use-fci       t))

;;;###autoload (autoload 'gitconfig-align-on-equals "git-setup.el" nil t)
(defalign gitconfig-align-on-equals "=")

;;;###autoload
(defun gitconfig-align-generic ()
  (interactive)
  (generic-align-on-equals))

(defhydra-ext hydra-gitconfig-align (:exit t :foreign-keys nil :hint nil)
  "
_a_: generic
_=_: on equals"
  ("a" gitconfig-align-generic)
  ("=" generic-align-on-equals))

(defhydra-derive hydra-gitconfig-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign"
  ("a" hydra-gitconfig-align/body))

;;;###autoload
(defun gitconfig-setup ()
  (init-common :use-yasnippet nil
               :use-comment   t
               :use-fci       t)

  (setq-local indent-tabs-mode nil)

  (def-keys-for-map (vim-normal-mode-local-keymap vim-insert-mode-local-keymap)
    ("<tab>"                               tab-to-tab-stop)
    (("<backtab>" "S-<tab>" "S-<iso-tab>") tab-to-tab-stop-backward))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("g" hydra-gitconfig-vim-visual-g-ext/body)))

;;; magit

;;;###autoload
(defun magit-collect-unstaged-hunk-sections ()
  "Return all staged hunk sections in magit status buffer."
  (save-excursion
    (letrec ((collect (lambda (section)
                        (let ((xs (mapcan collect
                                          (oref section children))))
                          (if (magit-section-match '[hunk file unstaged]
                                                   section)
                              (cons section xs)
                            xs)))))
      ;; Expand to load all hunks.
      (magit-section-show-children magit-root-section)
      (sort (funcall collect magit-root-section)
            (lambda (section-a section-b)
              (< (oref section-a start)
                 (oref section-b start)))))))

;;;###autoload
(defun magit-current-section-is-whitespace-only? ()
  (interactive)
  (let ((hunk (magit-current-section)))
    (message "Current hunk %s whitespace-only"
             (if (and (eq? 'hunk (oref hunk type))
                      (patch-whitespace-only-change?
                       (buffer-substring-no-properties
                        (oref hunk start)
                        (oref hunk end))))
                 "IS"
               "IS NOT"))))

;;;###autoload
(defun magit-stage-non-whitespace-changes ()
  (interactive)
  (magit-stage-matching-changes
   (lambda (hunk)
     (not
      (patch-whitespace-only-change?
       (buffer-substring-no-properties
        (oref hunk start)
        (oref hunk end)))))))

;;;###autoload
(defun magit-stage-matching-changes (pred)
  "Stage all hunk that match predicate PRED."
  (let ((matching-patches
         (-map (lambda (hunk)
                 (buffer-substring-no-properties
                  (oref hunk start)
                  (oref hunk end)))
               (-filter pred
                        (reverse (magit-collect-unstaged-hunk-sections))))))
    (dolist (patch matching-patches)
      (when-let ((sections (magit-collect-unstaged-hunk-sections))
                 (hunk (-find (lambda (section)
                                (string= patch
                                         (buffer-substring-no-properties
                                          (oref section start)
                                          (oref section end))))
                              sections)))
        (magit-apply-hunk hunk "--cached")))))

(defhydra-ext hydra-magit (:exit t :foreign-keys warn :hint nil)
  "
^Command^      ^On commit^       ^Remote^      ^Options^
--------------------------------------------------------
_b_ranch       _a_: cherry-pick  _F_: pull     _D_iff options
_c_ommit       _R_ename          _f_etch       _L_og options
_d_iff         _t_ag             _p_ush
_e_diff        no_T_e            re_M_ote
_l_og          re_v_erse         subm_o_dules
_m_erge
_r_ebase
_x_: reset
_z_: stash

Work with changes:
d_i_scard  _s_tage      _u_nstage
         _S_tage all  _U_nstage all

Diff chunks:
_+_: more context
_-_: less context
_=_: default context
"
  ("b" magit-branch)
  ("c" magit-commit)
  ("d" magit-diff)
  ("e" magit-ediff)
  ("l" magit-log)
  ("m" magit-merge)
  ("r" magit-rebase)
  ("x" magit-reset)
  ("z" magit-stash)

  ("a" magit-cherry-pick)
  ("R" magit-file-rename)
  ("t" magit-tag)
  ("T" magit-notes)
  ("v" magit-reverse)

  ("i" magit-discard)
  ("s" magit-stage)
  ("u" magit-unstage)
  ("S" magit-stage-modified)
  ("U" magit-unstage-all)

  ("f" magit-fetch)
  ("F" magit-pull)
  ("M" magit-remote)
  ("o" magit-submodule)
  ("p" magit-push)

  ("D" magit-diff-refresh)
  ("L" magit-log-refresh)

  ("+" magit-diff-more-context :exit nil)
  ("-" magit-diff-less-context :exit nil)
  ("=" magit-diff-default-context))

(defhydra-derive hydra-magit-j hydra-magit (:exit t :foreign-keys warn :hint nil)
  "
_w_indow
ta_b_s
_j_: default vim hydra
"
  ("w" hydra-window-management/body)
  ("b" hydra-tab-management/body)
  ("j" hydra-vim-normal-read-only-j-ext/body))

(defun magit-bind-common-vimless-mode-keymap (map)
  (def-keys-for-map (magit-unstaged-section-map
                     magit-file-section-map
                     magit-hunk-section-map
                     magit-staged-section-map
                     magit-untracked-section-map)
    (";"   magit-stage)
    ("k"   magit-unstage)
    ("SPC" magit-diff-visit-file-other-window))
  (def-keys-for-map (magit-unstaged-section-map
                     magit-file-section-map
                     magit-hunk-section-map
                     magit-staged-section-map
                     magit-untracked-section-map
                     magit-mode-map
                     map)
    ("u" nil)
    ("s" vim-ex-read-command))
  (def-keys-for-map map
    +vim-special-keys+
    +vim-search-keys+
    ("<down>"          forward-line)
    ("<up>"            backward-line)

    ("e"               magit-ediff)
    ("E"               nil)

    ("h"               magit-section-forward)
    ("t"               magit-section-backward)
    ("T"               magit-tag)
    ("n"               magit-notes)
    ("'"               magit-section-up)

    ("p"               magit-stash)
    (("H" "<f5>")      magit-refresh)
    ("-"               hydra-magit/body)
    ("j"               hydra-magit-j/body)
    ("\\"              magit-discard)
    ("M"               vim:jump-to-prev-saved-position:interactive)
    ("O"               magit-remote)

    ("k"               magit-unstage)
    ("K"               magit-unstage-all)
    ("x"               magit-reset-quickly)
    ("X"               magit-reset)

    ;; ("SPC"             magit-visit-thing-other-window)
    (("TAB" "<tab>")   magit-section-cycle)
    (("S-TAB" "<S-tab>" "S-<tab>" "<S-iso-lefttab>" "S-<iso-lefttab>")
                       magit-section-cycle-global)

    ("C-1"             magit-section-show-level-1-all)
    ("C-2"             magit-section-show-level-2-all)
    ("C-3"             magit-section-show-level-3-all)
    ("C-4"             magit-section-show-level-4-all)

    ;; Unbind "y" since it's too slow.
    ("y"               nil)
    ;; I don't use "Y" so swap it with "y"
    ("Y"               magit-show-refs)))

;;;###autoload
(defun magit-diff-mode-setup ()
  "Setup for diff browsing mode."
  (magit-bind-common-vimless-mode-keymap magit-diff-mode-map))

;;;###autoload
(defun magit-stash-mode-setup ()
  "Setup for stash browsing mode."
  (magit-bind-common-vimless-mode-keymap magit-stash-mode-map))

;;;###autoload
(defun magit-log-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-log-mode-map)
  (def-keys-for-map magit-log-mode-map
    ("h" pseudovim-motion-down)
    ("t" pseudovim-motion-up)))

(defun transient-after-init ()
  (def-keys-for-map transient-base-map
    ("<escape>" transient-quit-one)
    ("q"        transient-quit-all)))

(eval-after-load "transient"
  (transient-after-init))

;;;###autoload
(defun magit-refs-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-refs-mode-map))

;;;###autoload
(defun magit-reflog-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-reflog-mode-map))

;;;###autoload
(defun magit-revision-mode-setup ()
  "Setup for commit browsing mode."
  (display-fill-column-indicator-mode -1)
  ;; Do show continuation lines.
  (setq-local truncate-lines nil)
  (magit-bind-common-vimless-mode-keymap magit-revision-mode-map))

;;;###autoload
(defun magit-status-mode-setup ()
  "`magit-status' switches to window with this mode"
  ;; don't do (init-common) here since it's not so common mode
  (magit-bind-common-vimless-mode-keymap magit-status-mode-map))

;;; git-modes

;;;###autoload
(defun git-commit-mode-setup ()
  "Mode for editing commit message."
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci t
               :enable-backup nil
               :smerge nil)

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap
                     git-commit-mode-map)
    ("C-c C-q" magit-log-edit-cancel-log-message)
    ("<up>"    git-commit-prev-message ;; log-edit-previous-comment
     )
    ("<down>"  git-commit-next-message ;; log-edit-next-comment
     )
    ("M-p"     nil)))

(defhydra-ext hydra-git-rebase (:exit t :foreign-keys warn :hint nil)
  "
_d_elete  _k_: undo  _a_bort
_e_dit               _q_uit
_f_ixup
_p_ick
_r_eword
_s_quash
e_x_ec"
  ("s" git-rebase-squash)
  ("p" git-rebase-pick)
  ("k" git-rebase-undo)
  ("x" git-rebase-exec)
  ("r" git-rebase-reword)
  ("w" git-rebase-reword)
  ("e" git-rebase-edit)
  ("s" git-rebase-squash)
  ("f" git-rebase-fixup)
  ("d" git-rebase-kill-line))

(defhydra-derive hydra-git-rebase-from-vim-normal hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  ""
  ("#" with-editor-finish))

;;;###autoload
(defun git-rebase-mode-setup ()
  (hl-line-mode +1)
  (def-keys-for-map git-rebase-mode-map
    +vi-keys+
    +vim-search-keys+
    +vim-word-motion-keys+
    +vim-special-keys+
    (("C-h" "<down>") git-rebase-move-line-down)
    (("C-t" "<up>")   git-rebase-move-line-up)
    ("SPC"            git-rebase-show-commit)
    ("<return>"       with-editor-finish)

    ("g"              hydra-git-rebase-from-vim-normal/body)
    ("-"              hydra-git-rebase/body)

    ("n"              nil)
    ("q"              with-editor-cancel)
    ("a"              git-rebase-abort)

    ("s"              git-rebase-squash)
    ("p"              git-rebase-pick)
    ("k"              git-rebase-undo)
    ("x"              git-rebase-exec)
    ("r"              git-rebase-reword)
    ("w"              git-rebase-reword)
    ("e"              git-rebase-edit)
    ("s"              git-rebase-squash)
    ("f"              git-rebase-fixup)
    ("d"              git-rebase-kill-line)))

;;;###autoload
(defun git-get-repository-root (path)
  "Return root of git repository PATH is part of or nil if it's not
under git version control."
  (when (file-directory? path)
    (if (file-directory? (concat (strip-trailing-slash path) "/.git"))
        path
      (with-temp-buffer
        (cd path)
        (when (= 0 (call-process "git"
                                 nil
                                 t
                                 nil
                                 "rev-parse"
                                 "--show-toplevel"))
          (let ((fix-cygwin-paths
                 (fold-platform-os-type
                  #'identity
                  (lambda (x)
                    (save-match-data
                      (if (string-match "^/\\([a-zA-Z]\\)\\(.*\\)$" x)
                          (concat (match-string-no-properties 1 x)
                                  ":"
                                  (match-string-no-properties 2 x))
                        x))))))
            (funcall fix-cygwin-paths
                     (strip-trailing-slash
                      (trim-whitespace
                       (buffer-substring-no-properties (point-min)
                                                       (point-max)))))))))))

;;;###autoload
(defun git-add ()
  (interactive)
  (aif buffer-file-name
      (magit-stage-file it)
    (error "Current buffer has no file"))
  ;; (shell-command (concat "git add " (shell-quote-argument buffer-file-name)))
  )

;;;###autoload
(defun git-rm ()
  (interactive)
  (aif buffer-file-name
      (rm-on-file-and-kill-buffer-afterwards
       it
       (buffer-name)
       (lambda (path) (shell-command (concat "git rm -r " (shell-quote-argument path))))
       (lambda (path) (shell-command (concat "git rm " (shell-quote-argument path)))))
    (error "Current buffer has no file")))

(vim-define-keymap blame-mode "blame mode")

;;;###autoload (autoload 'vim-activate-blame-mode "git-setup" nil t)
(vim-define-mode blame "VIM git blame mode\n\nBlame mode keymap:\n\\{vim-blame-mode-keymap}"
  :ident "B"
  ;; :message "-- BLAME --"
  :keymaps '(vim-blame-mode-keymap
             vim-operator-pending-mode-keymap
             vim-motion-mode-keymap
             ;; vim-override-keymap
             )
  :command-function #'vim--normal-mode-command
  :cursor 'hbar)

(defhydra-ext hydra-blame (:exit t :foreign-keys nil :hint nil)
  "
_b_lame        _h_: next chunk
cop_y_ hash    _t_: prev chunk
               _C-h_: next chunk same commit
               _C-t_: prev chunk same commit
"
  ("b"   magit-blame)
  ("y"   magit-blame-copy-hash)

  ("h"   magit-blame-next-chunk)
  ("C-h" magit-blame-next-chunk-same-commit)
  ("t"   magit-blame-previous-chunk)
  ("C-t" magit-blame-previous-chunk-same-commit))

(def-keys-for-map vim-blame-mode-keymap
  +vim-normal-mode-navigation-keys+
  +vim-search-keys+
  +vim-search-extended-keys+
  +vim-special-keys+
  ("b"              magit-blame)
  ("h"              magit-blame-next-chunk)
  ("C-h"            magit-blame-next-chunk-same-commit)
  ("t"              magit-blame-previous-chunk)
  ("C-t"            magit-blame-previous-chunk-same-commit)
  ("y"              magit-blame-copy-hash)

  ("-"              hydra-blame/body)

  ("<return>"       magit-show-commit)

  ("<down>"         vim:motion-down:interactive)
  ("<up>"           vim:motion-up:interactive)
  (("q" "<escape>") vim:blame-quit:interactive))

;;; End

(provide 'git-setup)

;; git-setup.el ends here
