;; mmm-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 November 2017
;; Description:

(require 'common-heavy)
(require 'mmm-auto)

(setf mmm-global-mode 'maybe
      mmm-submode-decoration-level 0
      mmm-parse-when-idle nil
      mmm-idle-timer-delay 1
      mmm-never-modes
      (remove-duplicates-hashing
       (append
        mmm-never-modes
        '(Custom-mode
          bkr-mode
          calendar-mode
          clojure-compilation-mode
          compilation-mode
          completion-list-mode
          debugger-mode
          dired-mode
          doc-view-mode
          ediff-mode
          emms-playlist-mode
          flycheck-error-list-mode
          git-rebase-mode
          grep-mode
          haskell-compilation-mode
          rust-compilation-mode
          haskell-lint-mode
          help-mode
          ibuffer-mode
          ebuf-mode
          image-mode
          isearch-mode
          latex-compilation-mode
          magit-branch-manager-mode
          magit-cherry-mode
          magit-commit-mode
          magit-diff-mode
          magit-key-mode
          magit-log-mode
          magit-log-select-mode
          magit-merge-preview-mode
          magit-mode
          magit-popup-mode
          magit-popup-sequence-mode
          magit-process-mode
          magit-rebase-mode
          magit-reflog-mode
          magit-refs-mode
          magit-revision-mode
          magit-show-branches-mode
          magit-stash-mode
          magit-stashes-mode
          magit-status-mode
          minimap-mode
          occur-mode
          org-agenda-modeselect-mode
          sldb-mode
          slime-fuzzy-completions-mode
          slime-inspector-mode
          slime-repl-mode
          slime-xref-mode
          undo-tree-visualizer-mode))
       #'eq))

;;; bind `mmm-parse-buffer' somewhere

;; Local Variables:
;; End:

(provide 'mmm-setup)

;; mmm-setup.el ends here
