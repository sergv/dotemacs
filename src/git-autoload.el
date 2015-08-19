;; git-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 20 August 2015
;; Description:

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/magit"))

(setf magit-completing-read-function
      (lambda (prompt collection &optional predicate require-match initial-input hist def)
        (ido-completing-read
         (if (and def (> (length prompt) 2)
                  (string-equal ": " (substring prompt -2)))
           (format "%s (default %s): " (substring prompt 0 -2) def)
           prompt)
         collection predicate require-match initial-input hist def))
      magit-restore-window-configuration t
      magit-commit-ask-to-stage nil
      with-editor-emacsclient-executable nil
      magit-status-buffer-name-format "*magit: %b*"
      magit-diff-buffer-name-format "*magit-diff*"
      magit-commit-buffer-name-format "*magit-commit*"
      magit-log-buffer-name-format "*magit-log*"
      magit-reflog-buffer-name-format "*magit-reflog*"
      magit-branches-buffer-name-format "*magit-branches*"
      magit-wazzup-buffer-name-format "*magit-wazzup*"
      magit-cherry-buffer-name-format "*magit-cherry*"
      ;; make magit-process buffers invisible by default
      magit-process-buffer-name-format " *magit-process: %b*"
      magit-process-log-max 256
      magit-popup-show-help-section nil
      ;; show refined diffs for selected hunk
      magit-diff-refine-hunk t)

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
(autoload 'magit-cycle-sections-visibility "git-setup")
(autoload 'magit-cycle-top-sections-visibility "git-setup")
(autoload 'magit-visit-item-other-window "git-setup")

(autoload 'magit-mode-setup "git-setup")
(add-hook 'magit-mode-hook #'magit-mode-setup)

(autoload 'magit-status-mode-setup "git-setup")
(add-hook 'magit-status-mode-hook #'magit-status-mode-setup)

(autoload 'magit-log-mode-setup "git-setup")
(add-hook 'magit-log-mode-hook #'magit-log-mode-setup)

(autoload 'magit-log-edit-mode-setup "git-setup")
(add-hook 'magit-log-edit-mode-hook #'magit-log-edit-mode-setup)

(autoload 'magit-commit-mode-setup "git-setup")
(add-hook 'magit-commit-mode-hook #'magit-commit-mode-setup)

(autoload 'magit-diff-mode-setup "git-setup")
(add-hook 'magit-diff-mode-hook #'magit-diff-mode-setup)

(autoload 'magit-show-branches-mode-setup "git-setup")
(add-hook 'magit-show-branches-mode-hook #'magit-show-branches-mode-setup)

(autoload 'magit-branch-manager-mode-setup "git-setup")
(add-hook 'magit-branch-manager-mode-hook #'magit-branch-manager-mode-setup)

(autoload 'magit-reflog-mode-setup "git-setup")
(add-hook 'magit-reflog-mode-hook #'magit-reflog-mode-setup)

(autoload 'magit-popup-setup "git-setup")
(add-hook 'magit-popup-mode-hook #'magit-popup-setup)
(add-hook 'magit-popup-sequence-mode-hook #'magit-popup-setup)


;; git-modes

(setf git-commit-confirm-commit t)

(autoload 'git-commit-mode-setup "git-setup")
(add-hook 'git-commit-mode-hook #'git-commit-mode-setup)

(autoload 'git-rebase-mode-setup "git-setup")
(add-hook 'git-rebase-mode-hook #'git-rebase-mode-setup)

(eval-after-load "magit-key-mode" '(require 'git-setup))

(autoload 'git-add "git-setup" nil t)


(defparameter *have-git?* (executable-find "git")
  "Becomes t when git executable is accessible")

(when *have-git?*
  (autoload 'git-with-temp-head-commit-cache "git-setup" nil nil 'macro)
  (autoload 'git-get-head-commit-cached "git-setup")
  (autoload 'git-get-tracked-files "git-setup")
  (autoload 'git-get-repository-root "git-setup")
  (autoload 'git-update-file-repository "git-setup"))

(provide 'git-autoload)

;; Local Variables:
;; End:

;; git-autoload.el ends here
