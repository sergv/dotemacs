;; git-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 April 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'common-heavy)
(require 'vim-mock)
(require 'magit)
(require 'magit-blame)
(require 'search)
(require 'vim-setup)

;;; gitignore

(defun gitignore-setup ()
  (init-common :use-yasnippet  nil
               :use-comment    t
               :use-fci        t))

(defun gitconfig-setup ()
  (init-common :use-yasnippet  nil
               :use-comment    t
               :use-fci        t)
  (setq-local indent-tabs-mode nil)
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<tab>"       tab-to-tab-stop)
    ("<backtab>"   tab-to-tab-stop-backward)
    ("S-<tab>"     tab-to-tab-stop-backward)
    ("S-<iso-tab>" tab-to-tab-stop-backward)))

;;; magit

(defun magit-collect-unstaged-hunk-sections ()
  "Return all staged hunk sections in magit status buffer."
  (save-excursion
    (letrec ((collect (lambda (section)
                        (let ((xs (-mapcat collect
                                           (magit-section-children section))))
                          (if (magit-section-match '[hunk file unstaged]
                                                   section)
                            (cons section xs)
                            xs)))))
      ;; Expand to load all hunks.
      (magit-section-show-children magit-root-section)
      (sort (funcall collect magit-root-section)
            (lambda (section-a section-b)
              (< (magit-section-start section-a)
                 (magit-section-start section-b)))))))

(defun magit-current-section-is-whitespace-only? ()
  (interactive)
  (let ((hunk (magit-current-section)))
    (message "Current hunk %s whitespace-only"
             (if (and (eq? 'hunk (magit-section-type hunk))
                      (patch-whitespace-only-change?
                       (buffer-substring-no-properties
                        (magit-section-start hunk)
                        (magit-section-end hunk))))
               "IS"
               "IS NOT"))))

(defun magit-stage-non-whitespace-changes ()
  (interactive)
  (magit-stage-matching-changes
   (lambda (hunk)
     (not
      (patch-whitespace-only-change?
       (buffer-substring-no-properties
        (magit-section-start hunk)
        (magit-section-end hunk)))))))

(defun magit-stage-matching-changes (pred)
  "Stage all hunk that match predicate PRED."
  (let ((matching-patches
         (-map (lambda (hunk)
                 (buffer-substring-no-properties
                  (magit-section-start hunk)
                  (magit-section-end hunk)))
               (-filter pred
                        (reverse (magit-collect-unstaged-hunk-sections))))))
    (dolist (patch matching-patches)
      (when-let* (sections
                  (magit-collect-unstaged-hunk-sections)
                  hunk
                  (find-if (lambda (section)
                             (string= patch
                                      (buffer-substring-no-properties
                                       (magit-section-start section)
                                       (magit-section-end section))))
                           sections))
        (magit-apply-hunk hunk "--cached")))))

(defun magit-bind-common-vimless-mode-keymap (map)
  (def-keys-for-map (magit-unstaged-section-map
                     magit-file-section-map
                     magit-hunk-section-map
                     magit-staged-section-map
                     magit-untracked-section-map
                     magit-mode-map
                     map)
    ("u" nil)
    ("s" vim:ex-read-command)
    (";" magit-stage)
    ("k" magit-unstage))
  (def-keys-for-map map
    +vim-special-keys+
    +vim-search-keys+
    ("?"               magit-dispatch-popup) ;; override "?" from vim search
    ("<down>"          forward-line)
    ("<up>"            backward-line)
    ("h"               magit-section-forward)
    ("t"               magit-section-backward)
    ("T"               magit-tag-popup)
    ("n"               magit-notes-popup)
    ("'"               magit-section-up)

    ("p"               magit-stash-popup)
    ("H"               magit-refresh)
    ("<f5>"            magit-refresh)
    ("j"               magit-discard)
    ("\\"              magit-discard)
    ("M"               vim:jump-to-prev-saved-position)
    ("O"               magit-remote-popup)

    ("k"               magit-unstage)
    ("K"               magit-unstage-all)
    ("x"               magit-reset-soft)
    ("X"               magit-reset-popup)

    ;; ("SPC"             magit-visit-thing-other-window)
    ("TAB"             magit-section-cycle)
    ("<tab>"           magit-section-cycle)
    ("S-TAB"           magit-section-cycle-global)
    ("<S-tab>"         magit-section-cycle-global)
    ("S-<tab>"         magit-section-cycle-global)
    ("<S-iso-lefttab>" magit-section-cycle-global)
    ("S-<iso-lefttab>" magit-section-cycle-global)

    ("C-1"             magit-section-show-level-1-all)
    ("C-2"             magit-section-show-level-2-all)
    ("C-3"             magit-section-show-level-3-all)
    ("C-4"             magit-section-show-level-4-all)))

(defun magit-diff-mode-setup ()
  "Setup for diff browsing mode."
  (magit-bind-common-vimless-mode-keymap magit-diff-mode-map))

(defun magit-stash-mode-setup ()
  "Setup for stash browsing mode."
  (magit-bind-common-vimless-mode-keymap magit-stash-mode-map))

(defun magit-log-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-log-mode-map)
  (def-keys-for-map magit-log-mode-map
    ("h" vim-mock:motion-down)
    ("t" vim-mock:motion-up)))

(defun magit-popup-setup ()
  (def-keys-for-map magit-popup-mode-map
    ("<escape>" magit-popup-quit)
    ("q"        magit-popup-quit)))

(defun magit-refs-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-refs-mode-map))

(defun magit-reflog-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-reflog-mode-map))

(defun magit-revision-mode-setup ()
  "Setup for commit browsing mode."
  (fci-mode -1)
  ;; Do show continuation lines.
  (setq-local truncate-lines nil)
  (magit-bind-common-vimless-mode-keymap magit-revision-mode-map))

(defun magit-status-mode-setup ()
  "`magit-status' switches to window with this mode"
  ;; don't do (init-common) here since it's not so common mode
  (magit-bind-common-vimless-mode-keymap magit-status-mode-map))

;;; git-modes

(defun git-commit-mode-setup ()
  "Mode for editing commit message."
  (init-common :use-yasnippet nil :use-comment nil :use-fci t)

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     git-commit-mode-map)
    ("C-c C-q" magit-log-edit-cancel-log-message)
    ("<up>"    git-commit-prev-message ;; log-edit-previous-comment
     )
    ("<down>"  git-commit-next-message ;; log-edit-next-comment
     )
    ("M-p"     nil)))

(defun git-rebase-mode-setup ()
  (def-keys-for-map git-rebase-mode-map
    +vi-keys+
    +vim-mock:word-motion-keys+
    +vim-special-keys+
    ("C-k"      nil) ;; its kill buffer in global map
    ("q"        with-editor-cancel)
    ("a"        git-rebase-abort)
    ("<down>"   git-rebase-move-line-down)
    ("<up>"     git-rebase-move-line-up)
    ("C-h"      git-rebase-move-line-down)
    ("C-t"      git-rebase-move-line-up)
    ("SPC"      git-rebase-show-commit)
    ("g #"      with-editor-finish)
    ("<return>" with-editor-finish)

    ("n"        ignore)
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

(when *have-git?*
  (defvar *git-get-head-commit-cache* nil
    "Optional cache for `git-get-head-commit-cached'.")

  (defmacro git-with-temp-head-commit-cache (&rest body)
    "Execute BODY with `*git-get-head-commit-cache*' temporarily bound to
hash-table. Therefore all calls to `git-get-head-commit-cached' in BODY
will be cached and therefore will not be able to see changes to HEAD reference
in any repository that was already queried with `git-get-head-commit-cached'."
    `(let ((*git-get-head-commit-cache* (make-hash-table :test #'equal)))
       ,@body))

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
                          (-map (comp #'common/registered-filename
                                      #'expand-file-name)
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
            (strip-trailing-slash
             (trim-whitespace
              (buffer-substring-no-properties (point-min)
                                              (point-max)))))))))

  (defun git-update-file-repository ()
    "Update git-repository for current buffer."
    (if-buffer-has-file
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
              (setf git-repository repository))))))))

(defun git-add ()
  (interactive)
  (unless buffer-file-name
    (error "Current buffer has no file"))
  (magit-stage-file buffer-file-name)
  ;; (shell-command (concat "git add " (shell-quote-argument buffer-file-name)))
  )

(vim:define-keymap blame-mode "blame mode")

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
  +vim-interbuffer-navigation-keys+
  +vim-normal-mode-navigation-keys+
  +vim-search-keys+
  +vim-search-extended-keys+
  ("b"        magit-blame-popup)
  ("h"        magit-blame-next-chunk)
  ("H"        magit-blame-next-chunk-same-commit)
  ("t"        magit-blame-previous-chunk)
  ("T"        magit-blame-previous-chunk-same-commit)
  ("y"        magit-blame-copy-hash)

  ;; ("C-h"      magit-blame-next-chunk)
  ("<down>"   vim:motion-down)
  ;; ("C-t"      magit-blame-previous-chunk)
  ("<up>"     vim:motion-up)
  ("<escape>" vim:blame-quit)
  ("q"        vim:blame-quit))

(provide 'git-setup)

;; git-setup.el ends here
