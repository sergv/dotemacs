;; git-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 20 August 2015
;; Description:

(setf magit-completing-read-function
      (lambda (prompt collection &optional predicate require-match initial-input hist def)
        (ido-completing-read
         (if (and def (> (length prompt) 2)
                  (string-equal ": " (substring prompt -2)))
           (format "%s (default %s): " (substring prompt 0 -2) def)
           prompt)
         collection predicate require-match initial-input hist def))
      ;;magit-restore-window-configuration t
      magit-refs-show-commit-count 'all
      magit-commit-ask-to-stage nil
      ;; with-editor-emacsclient-executable nil
      magit-process-log-max 256
      magit-popup-show-common-commands nil
      ;; Show refined diffs for selected hunk>
      magit-diff-refine-hunk t
      ;; Save any modified files before opening `magit-status'.
      magit-save-repository-buffers 'dontask

      magit-blame-heading-format "%.7H %-15a %C %s")

(autoload 'gitignore-mode "gitignore-mode"
          "Major mode for editing .gitignore files"
          t)

(add-to-list 'auto-mode-alist
             (cons (rx (or ".gitignore"
                           ".git/info/exclude")
                       eol)
                   'gitignore-mode))

(autoload 'gitconfig-mode "gitconfig-mode"
          "Major mode to edit .gitconfig files"
          t)

(add-to-list 'auto-mode-alist
             (cons (rx (or ".gitconfig"
                           ".gitmodules"
                           ".git/config")
                       eol)
                   'gitconfig-mode))

(autoload 'gitignore-setup "git-setup")
(add-hook 'gitignore-mode-hook #'gitignore-setup)

(autoload 'gitconfig-setup "git-setup")
(add-hook 'gitconfig-mode-hook #'gitconfig-setup)

(autoload 'magit-collect-unstaged-hunk-sections "git-setup")
(autoload 'magit-current-section-is-whitespace-only? "git-setup")
(autoload 'magit-stage-non-whitespace-changes "git-setup" nil t)
(autoload 'magit-stage-matching-changes "git-setup")
;; (autoload 'magit-visit-item-other-window "git-setup")

(autoload 'magit-diff-mode-setup "git-setup")
(add-hook 'magit-diff-mode-hook #'magit-diff-mode-setup)
(autoload 'magit-log-mode-setup "git-setup")
(add-hook 'magit-log-mode-hook #'magit-log-mode-setup)
(autoload 'magit-popup-setup "git-setup")
(add-hook 'magit-popup-mode-hook #'magit-popup-setup)
(add-hook 'magit-popup-sequence-mode-hook #'magit-popup-setup)
(autoload 'magit-revision-mode-setup "git-setup")
(add-hook 'magit-revision-mode-hook #'magit-revision-mode-setup)
(autoload 'magit-refs-mode-setup "git-setup")
(add-hook 'magit-refs-mode-hook #'magit-refs-mode-setup)
(autoload 'magit-reflog-mode-setup "git-setup")
(add-hook 'magit-reflog-mode-hook #'magit-reflog-mode-setup)
(autoload 'magit-status-mode-setup "git-setup")
(add-hook 'magit-status-mode-hook #'magit-status-mode-setup)

;; git-modes

(setf git-commit-confirm-commit t)

(autoload 'git-commit-mode-setup "git-setup")
(add-hook 'git-commit-mode-hook #'git-commit-mode-setup)

(autoload 'git-rebase-mode-setup "git-setup")
(add-hook 'git-rebase-mode-hook #'git-rebase-mode-setup)

(eval-after-load "magit" '(require 'git-setup))

(autoload 'git-add "git-setup" nil t)


(defparameter *have-git?* (executable-find "git")
  "Becomes t when git executable is accessible")

(when *have-git?*
  (autoload 'git-with-temp-head-commit-cache "git-setup" nil nil 'macro)
  (autoload 'git-get-head-commit-cached "git-setup")
  (autoload 'git-get-tracked-files "git-setup")
  (autoload 'git-get-repository-root "git-setup")
  (autoload 'git-update-file-repository "git-setup"))

(autoload 'magit-show-commit "magit" nil t)
(autoload 'magit-status "magit" nil t)
(autoload 'magit-stage-all "magit" nil t)
(autoload 'magit-unstage-all "magit" nil t)
(autoload 'magit-dired-jump "magit" nil t)
(autoload 'magit-show "magit" nil t)
(autoload 'magit-init "magit" nil t)
(autoload 'magit-merge "magit" nil t)
(autoload 'magit-log "magit" nil t)
(autoload 'magit-mode "magit" nil t)
(autoload 'magit-process-mode "magit" nil t)

(autoload 'magit-blame-mode "magit-blame" nil t)
(autoload 'magit-popup-mode "magit-popup")
(autoload 'magit-popup-sequence-mode "magit-popup")

(provide 'git-autoload)

;; Local Variables:
;; End:

;; git-autoload.el ends here
