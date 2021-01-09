;; git-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 20 August 2015
;; Description:

(setf ;; magit-completing-read-function
      ;; (lambda (prompt collection &optional predicate require-match initial-input hist def)
      ;;   (ivy-completing-read
      ;;    (if (and def (> (length prompt) 2)
      ;;             (string-equal ": " (substring prompt -2)))
      ;;        (format "%s (default %s): " (substring prompt 0 -2) def)
      ;;      prompt)
      ;;    collection predicate require-match initial-input hist def))
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

      magit-blame-heading-format "%.7H %-15a %C %s"
      magit-push-always-verify nil
      magit-revert-buffers 'silent
      magit-log-arguments '("--graph" "--color" "--decorate" "-n256" "--show-signature")
      ;; ;; don't confirm when discarding
      ;; magit-no-confirm '(discard)
      )

(add-to-list 'auto-mode-alist
             (cons (rx (or ".gitignore"
                           ".git/info/exclude")
                       eol)
                   'gitignore-mode))

(add-to-list 'auto-mode-alist
             (cons (rx (or ".gitconfig"
                           ".gitmodules"
                           ".git/config")
                       eol)
                   'gitconfig-mode))

(add-hook 'gitignore-mode-hook #'gitignore-setup)

(add-hook 'gitconfig-mode-hook #'gitconfig-setup)

(add-hook 'magit-diff-mode-hook #'magit-diff-mode-setup)
(add-hook 'magit-stash-mode-hook #'magit-stash-mode-setup)
(add-hook 'magit-log-mode-hook #'magit-log-mode-setup)
(add-hook 'magit-popup-mode-hook #'magit-popup-setup)
(add-hook 'magit-popup-sequence-mode-hook #'magit-popup-setup)
(add-hook 'magit-revision-mode-hook #'magit-revision-mode-setup)
(add-hook 'magit-refs-mode-hook #'magit-refs-mode-setup)
(add-hook 'magit-reflog-mode-hook #'magit-reflog-mode-setup)
(add-hook 'magit-status-mode-hook #'magit-status-mode-setup)

;; git-modes

(setf git-commit-confirm-commit t)

(add-hook 'git-commit-mode-hook #'git-commit-mode-setup)

(add-hook 'git-rebase-mode-hook #'git-rebase-mode-setup)

(eval-after-load "magit" '(require 'git-setup))


(defvar *have-git?* (executable-find "git")
  "Becomes t when git executable is accessible")

(provide 'git-autoload)

;; Local Variables:
;; End:

;; git-autoload.el ends here
