;; git-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 April 2012
;; Description:

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/magit"))

(require 'vim-mock)
(require 'magit)
(require 'magit-blame)
(require 'search)

(setf magit-completing-read-function
      (lambda (prompt collection
               &optional predicate require-match initial-input hist def)
        (completing-read-vanilla
         (if (and def (> (length prompt) 2)
                  (string-equal ": " (substring prompt -2)))
           (format "%s (default %s): " (substring prompt 0 -2) def)
           prompt)
         collection predicate require-match initial-input hist def)))

;;;; gitignore

(autoload 'gitignore-mode "gitignore-mode"
          "Major mode for editing .gitignore files"
          t)

(add-to-list 'auto-mode-alist
             (cons (rx (or ".gitignore"
                           ".git/info/exclude")
                       eol)
                   'gitignore-mode))


(defun gitignore-setup ()
  (init-common :use-yasnippet  nil
               :use-comment    t
               :use-whitespace t))

(add-hook 'gitignore-mode-hook
          'gitignore-setup)

;;;; magit

(defadvice magit-log-edit-cleanup
    (before
     magit-log-edit-cleanup-clean-trailing-whitespace
     activate
     compile)
     (delete-trailing-whitespace+))

(defun magit-mode-setup ()
  (setf truncate-lines nil)

  ;; here we do have vim mode enabled
  ;; (def-keys-for-map magit-mode-map
  ;;   +control-x-prefix+)
  ;; (def-keys-for-map magit-mode-map
  ;;   +vim-special-keys+)
  (def-keys-for-map magit-mode-map
    ("r" magit-refresh)
    ("R" magit-refresh-all)
    ("T" magit-key-mode-popup-tagging)))

(add-hook 'magit-mode-hook #'magit-mode-setup)

(defun magit-bind-common-vimless-mode-keymap (map)
  (def-keys-for-map map
    +control-x-prefix+
    +vim-special-keys+
    +vi-search-keys+
    ("r"      magit-refresh)
    ("R"      magit-refresh-all)

    ("p"      magit-key-mode-popup-stashing)
    ("T"      magit-key-mode-popup-tagging)
    ("<down>" magit-goto-next-section)
    ("<up>"   magit-goto-previous-section)
    ("t"      magit-goto-next-section)
    ("n"      magit-goto-previous-section)))

(defun magit-status-mode-setup ()
  "`magit-status' switches to window with this mode"
  ;; don't do (init-common) here since it's not so common mode
  (magit-bind-common-vimless-mode-keymap magit-status-mode-map))

(add-hook 'magit-status-mode-hook #'magit-status-mode-setup)

(defun magit-log-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-log-mode-map)
  (def-keys-for-map magit-log-mode-map
    ("t" vim-mock:motion-down)
    ("n" vim-mock:motion-up)))

(add-hook 'magit-log-mode-hook #'magit-log-mode-setup)

(defun magit-log-edit-mode-setup ()
  "Mode for editing commit message."
  (init-common :use-yasnippet nil :use-comment nil)

  (def-keys-for-map magit-log-edit-mode-map
    ("C-c C-q" magit-log-edit-cancel-log-message)
    ("<up>"    log-edit-previous-comment)
    ("<down>"  log-edit-next-comment))

  (add-hook 'kill-buffer-hook
            #'magit-log-edit-cancel-log-message
            t ;; append
            t ;; local
            ))

(add-hook 'magit-log-edit-mode-hook #'magit-log-edit-mode-setup)

(defun magit-commit-mode-setup ()
  "Mode for browsing commits."
  (magit-bind-common-vimless-mode-keymap magit-commit-mode-map))

(add-hook 'magit-commit-mode-hook #'magit-commit-mode-setup)

(defun magit-diff-mode-setup ()
  "Mode for browsing diffs."
  (magit-bind-common-vimless-mode-keymap magit-diff-mode-map))

(add-hook 'magit-diff-mode-hook #'magit-diff-mode-setup)


(defun magit-show-branches-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-show-branches-mode-map))

(add-hook 'magit-show-branches-mode-hook #'magit-show-branches-mode-setup)

(defun magit-branch-manager-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-branch-manager-mode-map))

(add-hook 'magit-branch-manager-mode-hook #'magit-branch-manager-mode-setup)

;; this mode has no hook
(eval-after-load
 "magit-key-mode"
 '(progn
   (redefun magit-key-mode-build-keymap (for-group)
     "Construct a normal looking keymap for the key mode to use and
put it in magit-key-mode-key-maps for fast lookup."
     (let* ((options (magit-key-mode-options-for-group for-group))
            (actions (cdr (assoc 'actions options)))
            (switches (cdr (assoc 'switches options)))
            (arguments (cdr (assoc 'arguments options)))
            (map (make-sparse-keymap)))
       (suppress-keymap map 'nodigits)
       ;; ret dwim
       (define-key map (kbd "RET") 'magit-key-mode-exec-at-point)

       ;; all maps should `quit' with `C-g' or `q' or `ESC'
       (define-key map (kbd "ESC") `(lambda ()
                                      (interactive)
                                      (magit-key-mode-command nil)))
       (define-key map (kbd "<escape>") `(lambda ()
                                           (interactive)
                                           (magit-key-mode-command nil)))
       (define-key map (kbd "C-g") `(lambda ()
                                      (interactive)
                                      (magit-key-mode-command nil)))
       (define-key map (kbd "q")   `(lambda ()
                                      (interactive)
                                      (magit-key-mode-command nil)))
       ;; run help
       (define-key map (kbd "?") `(lambda ()
                                    (interactive)
                                    (magit-key-mode-help ',for-group)))

       (flet ((defkey (k action)
                (when (and (lookup-key map (car k))
                           (not (numberp (lookup-key map (car k)))))
                  (message "Warning: overriding binding for `%s' in %S"
                           (car k) for-group)
                  (ding)
                  (sit-for 2))
                (define-key map (car k)
                  `(lambda () (interactive) ,action))))
         (when actions
           (dolist (k actions)
             (defkey k `(magit-key-mode-command ',(nth 2 k)))))
         (when switches
           (dolist (k switches)
             (defkey k `(magit-key-mode-add-option ',for-group ,(nth 2 k)))))
         (when arguments
           (dolist (k arguments)
             (defkey k `(magit-key-mode-add-argument
                         ',for-group ,(nth 2 k) ',(nth 3 k))))))

       (aput 'magit-key-mode-key-maps for-group map)
       map))))



(defvar *have-git?* (executable-find "git")
  "Becomes t when git executable is accessible")

(defvar-local git-repository nil
    "Path to root of git repository this buffer's file is member of, if any.")

(when *have-git?*
  (defun git-get-tracked-files (repo-path)
    "Returns list of tracked files in repository located at REPO-PATH"
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
      (split-string (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    "\0"
                    t)))

  (defun git-get-repository-root (root)
    "Returns root of git repository ROOT is part of or nil if it's not
under git version control."
    (with-temp-buffer
      (cd root)
      (when (= 0 (call-process "git"
                               nil
                               t
                               nil
                               "rev-parse"
                               "--show-toplevel"))
        (string-trim-whitespace
         (buffer-substring-no-properties (point-min)
                                         (point-max))))))

  (defun git-update-file-repository ()
    (if-buffer-has-file
     (when (or (not git-repository)
               (not (string-prefix? (expand-file-name git-repository)
                                    (expand-file-name buffer-file-name))))
       (let* ((filename (buffer-file-name))
              (repository
                (git-get-repository-root (file-name-directory filename))))
         (when repository
           (let* ((tracked-files (git-get-tracked-files repository))
                  (tracked-file? (any? (lambda (path)
                                         (string-suffix? path filename))
                                       tracked-files)))
             (when tracked-file?
               (setf git-repository repository)))))))))


(provide 'git-setup)

;; Local Variables:
;; End:

;; git-setup.el ends here
