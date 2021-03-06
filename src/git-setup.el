;; git-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 April 2012
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'common)
(require 'common-heavy)
(require 'dash)
(require 'el-patch)
(require 'hydra-setup)
(require 'vim-mock)
(require 'magit)
(require 'magit-blame)
(require 'search)
(require 'vim-setup)

;;;###autoload
(el-patch-feature magit)

(setf magit-completing-read-function 'ivy-completing-read)

(dolist (x '(magit-reset-soft magit-reset-hard magit-reset-head magit-reset magit-reset-index))
  (push (cons x nil) ivy-sort-functions-alist))

;;; Magit redefinitions

(el-patch-defun magit-reset-read-branch-or-commit (prompt)
  "Prompt for and return a ref to reset HEAD to.

PROMPT is a format string, where either the current branch name
or \"detached head\" will be substituted for %s."
  ((el-patch-swap
     magit-read-branch-or-commit
     magit-read-branch-or-commit-prompt-for-previous-commit-first)
   (format prompt (or (magit-get-current-branch) "detached head"))))

(defun magit-read-branch-or-commit-prompt-for-previous-commit-first (prompt &optional secondary-default)
  (or (magit-completing-read prompt (cons "HEAD^" (cons "HEAD" (magit-list-refnames)))
                             nil nil nil 'magit-revision-history
                             nil)
      (user-error "Nothing selected")))

(el-patch-defun magit-rebase-interactive-assert (since &optional delay-edit-confirm)
  (el-patch-swap
    (let* ((commit (if (string-suffix-p "^" since)
                       ;; If SINCE is "REV^", then the user selected
                       ;; "REV", which is the first commit that will
                       ;; be replaced. (from^..to] <=> [from..to].
                       (substring since 0 -1)
                     ;; The "--root" argument is being used.
                     since))
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
            (concat m1 "%i public branches" m2)
            nil branches))
        (push (magit-toplevel) magit--rebase-public-edit-confirmed)))
    nil)
  (if (magit-git-lines "rev-list" "--merges" (concat since "..HEAD"))
      (magit-read-char-case "Proceed despite merge in rebase range?  " nil
        (?c "[c]ontinue" since)
        (?s "[s]elect other" nil)
        (?a "[a]bort" (user-error "Quit")))
    since))

(el-patch-defun magit-commit-amend-assert (&optional commit)
  (el-patch-swap
    (--when-let (magit-list-publishing-branches commit)
      (let ((m1 "This commit has already been published to ")
            (m2 ".\nDo you really want to modify it"))
        (magit-confirm 'amend-published
          (concat m1 "%s" m2)
          (concat m1 "%i public branches" m2)
          nil it)))
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
  (gitconfig-align-on-equals))

(defhydra-ext hydra-gitconfig-align (:exit t :foreign-keys nil :hint nil)
  "
_a_: generic
_=_: on equals"
  ("a" gitconfig-align-generic)
  ("=" gitconfig-align-on-equals))

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
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<tab>"       tab-to-tab-stop)
    ("<backtab>"   tab-to-tab-stop-backward)
    ("S-<tab>"     tab-to-tab-stop-backward)
    ("S-<iso-tab>" tab-to-tab-stop-backward))
  (def-keys-for-map vim:visual-mode-local-keymap
    ("g" hydra-gitconfig-vim-visual-g-ext/body)))

;;; magit

;;;###autoload
(defun magit-collect-unstaged-hunk-sections ()
  "Return all staged hunk sections in magit status buffer."
  (save-excursion
    (letrec ((collect (lambda (section)
                        (let ((xs (-mapcat collect
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
         _s_tage      _u_nstage
d_i_scard  _S_tage all  _U_nstage all

Diff chunks:
_+_: more context
_-_: less context
_=_: default context"
  ("b" magit-branch-popup)
  ("c" magit-commit-popup)
  ("d" magit-diff-popup)
  ("e" magit-ediff-popup)
  ("l" magit-log-popup)
  ("m" magit-merge-popup)
  ("r" magit-rebase-popup)
  ("x" magit-reset-popup)
  ("z" magit-stash-popup)

  ("a" magit-cherry-pick-popup)
  ("R" magit-file-rename)
  ("t" magit-tag-popup)
  ("T" magit-notes-popup)
  ("v" magit-reverse)

  ("i" magit-discard)
  ("s" magit-stage)
  ("u" magit-unstage)
  ("S" magit-stage-modified)
  ("U" magit-unstage-all)

  ("f" magit-fetch-popup)
  ("F" magit-pull-popup)
  ("M" magit-remote-popup)
  ("o" magit-submodule-popup)
  ("p" magit-push-popup)

  ("D" magit-diff-refresh-popup)
  ("L" magit-log-refresh-popup)

  ("+" magit-diff-more-context :exit nil)
  ("-" magit-diff-less-context :exit nil)
  ("=" magit-diff-default-context))

(defhydra-derive hydra-magit-j hydra-magit (:exit t :foreign-keys warn :hint nil)
  "
_w_indow
ta_b_s"
  ("w" hydra-window-management/body)
  ("b" hydra-tab-management/body))

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
    ("s" vim:ex-read-command))
  (def-keys-for-map map
    +vim-special-keys+
    +vim-search-keys+
    ("<down>"          forward-line)
    ("<up>"            backward-line)

    ("e"               magit-ediff-popup)
    ("E"               nil)

    ("h"               magit-section-forward)
    ("t"               magit-section-backward)
    ("T"               magit-tag-popup)
    ("n"               magit-notes-popup)
    ("'"               magit-section-up)

    ("p"               magit-stash-popup)
    (("H" "<f5>")      magit-refresh)
    ("-"               hydra-magit/body)
    ("j"               hydra-magit-j/body)
    ("\\"              magit-discard)
    ("M"               vim:jump-to-prev-saved-position)
    ("O"               magit-remote-popup)

    ("k"               magit-unstage)
    ("K"               magit-unstage-all)
    ("x"               magit-reset-quickly)
    ("X"               magit-reset-popup)

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
    ("Y"               magit-show-refs-popup)))

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
    ("h" vim-mock:motion-down)
    ("t" vim-mock:motion-up)))

;;;###autoload
(defun magit-popup-setup ()
  (def-keys-for-map magit-popup-mode-map
    ("<escape>" magit-popup-quit)
    ("q"        magit-popup-quit)))

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
               :enable-backup nil)

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
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
    +vim-mock:word-motion-keys+
    +vim-special-keys+
    ("C-k"      nil) ;; its kill buffer in global map
    ("<down>"   git-rebase-move-line-down)
    ("<up>"     git-rebase-move-line-up)
    ("C-h"      git-rebase-move-line-down)
    ("C-t"      git-rebase-move-line-up)
    ("SPC"      git-rebase-show-commit)
    ("<return>" with-editor-finish)

    ("g"        hydra-git-rebase-from-vim-normal/body)
    ("-"        hydra-git-rebase/body)

    ("n"        nil)
    ("q"        with-editor-cancel)
    ("a"        git-rebase-abort)

    ("s"        git-rebase-squash)
    ("p"        git-rebase-pick)
    ("k"        git-rebase-undo)
    ("x"        git-rebase-exec)
    ("r"        git-rebase-reword)
    ("w"        git-rebase-reword)
    ("e"        git-rebase-edit)
    ("s"        git-rebase-squash)
    ("f"        git-rebase-fixup)
    ("d"        git-rebase-kill-line)))


(defvar-local git-repository nil
  "Path to root of git repository this buffer's file is member of, if any.")

(defvar *git-get-head-commit-cache* nil
  "Optional cache for `git-get-head-commit-cached'.")

;;;###autoload
(defmacro git-with-temp-head-commit-cache (&rest body)
  "Execute BODY with `*git-get-head-commit-cache*' temporarily bound to
hash-table. Therefore all calls to `git-get-head-commit-cached' in BODY
will be cached and therefore will not be able to see changes to HEAD reference
in any repository that was already queried with `git-get-head-commit-cached'."
  `(let ((*git-get-head-commit-cache* (make-hash-table :test #'equal)))
     ,@body))

;;;###autoload
(defun git-get-head-commit-cached (repo-root)
  "Get head commit as sha-1 string of length 40 for git project at REPO-ROOT.
If *git-get-head-commit-cache* is a hash table then try to find answer there
and put one if nothing was found."
  (let ((get-head-commit
         (lambda (repo-root)
           (with-temp-buffer
             (cd repo-root)
             (call-process "git"
                           nil
                           (current-buffer)
                           nil
                           "log"
                           "-n"
                           "1"
                           "--pretty='%H'"
                           "HEAD")
             (trim-whitespace
              (buffer-substring-no-properties (point-min) (point-max)))))))
    (if (hash-table-p *git-get-head-commit-cache*)
        (aif (gethash repo-root *git-get-head-commit-cache*)
            it
          (let ((commit (funcall get-head-commit repo-root)))
            (puthash repo-root commit *git-get-head-commit-cache*)
            commit))
      (funcall get-head-commit repo-root))))

(defvar *git-get-tracked-files-cache* (make-hash-table :test #'equal)
  "Hash table from repository root (expanded) to hash table of
repository commits (sha-1 strings of length 40) to hash tables that map
expanded filenames in git repository to themselves.")

;;;###autoload
(defun git-get-tracked-files (repo-path)
  "Returns hashtable of tracked files in repository located at REPO-PATH"
  (let ((get-tracked-files
         (lambda (repo-path)
           (with-temp-buffer
             (cd repo-path)
             (call-process "git"
                           nil
                           (current-buffer)
                           nil
                           "ls-files"
                           ;; cached
                           "-c"
                           ;; null-separated
                           "-z")
             (let ((filename-table (make-hash-table :test #'equal))
                   (rough-filenames
                    (split-string (buffer-substring-no-properties (point-min)
                                                                  (point-max))
                                  "\0"
                                  t)))
               (dolist (filename
                        (--map (common/registered-filename (expand-file-name it))
                               rough-filenames))
                 (puthash filename filename filename-table))
               filename-table))))
        (commit (git-get-head-commit-cached repo-path)))
    (if-let (inner-table (gethash repo-path *git-get-tracked-files-cache*))
        (if-let (files-entry (gethash commit inner-table))
            files-entry
          (let ((files (funcall get-tracked-files repo-path)))
            (puthash commit files inner-table)
            files))
      (let* ((files (funcall get-tracked-files repo-path))
             (inner-table (make-hash-table :test #'equal)))
        (puthash commit files inner-table)
        (puthash repo-path inner-table *git-get-tracked-files-cache*)
        files))))

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
                          (concat (match-string 1 x)
                                  ":"
                                  (match-string 2 x))
                        x))))))
            (funcall fix-cygwin-paths
                     (strip-trailing-slash
                      (trim-whitespace
                       (buffer-substring-no-properties (point-min)
                                                       (point-max)))))))))))

;;;###autoload
(defun git-update-file-repository ()
  "Update git-repository for current buffer."
  (when-buffer-has-file
    (when (or (not git-repository)
              (not (string-prefix? (expand-file-name git-repository)
                                   (expand-file-name buffer-file-name))))
      (when-let (repository
                 (git-get-repository-root (file-name-directory buffer-file-name)))
        (let ((tracked-files-table (git-get-tracked-files repository)))
          ;; buffer-file-truename does not contain symbolic links
          ;; as buffer-file-name does
          (when (gethash (expand-file-name buffer-file-truename)
                         tracked-files-table)
            (setf git-repository repository)))))))

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
       (lambda (path) (shell-command (concat "git rm -r " (shell-quote-argument path))))
       (lambda (path) (shell-command (concat "git rm " (shell-quote-argument path)))))
    (error "Current buffer has no file")))

(vim:define-keymap blame-mode "blame mode")

;;;###autoload (autoload 'vim:activate-blame-mode "git-setup" nil t)
(vim:define-mode blame "VIM git blame mode\n\nBlame mode keymap:\n\\{vim:blame-mode-keymap}"
  :ident "B"
  ;; :message "-- BLAME --"
  :keymaps '(vim:blame-mode-keymap
             vim:operator-pending-mode-keymap
             vim:motion-mode-keymap
             ;; vim:override-keymap
             )
  :command-function #'vim:normal-mode-command
  :cursor 'hbar)

(def-keys-for-map vim:blame-mode-keymap
  +vim-normal-mode-navigation-keys+
  +vim-search-keys+
  +vim-search-extended-keys+
  +vim-special-keys+
  ("b"        magit-blame-popup)
  ("h"        magit-blame-next-chunk)
  ("H"        magit-blame-next-chunk-same-commit)
  ("t"        magit-blame-previous-chunk)
  ("T"        magit-blame-previous-chunk-same-commit)
  ("y"        magit-blame-copy-hash)

  ("<return>" magit-show-commit)

  ;; ("C-h"      magit-blame-next-chunk)
  ("<down>"   vim:motion-down)
  ;; ("C-t"      magit-blame-previous-chunk)
  ("<up>"     vim:motion-up)
  ("<escape>" vim:blame-quit)
  ("q"        vim:blame-quit))

(provide 'git-setup)

;; git-setup.el ends here
