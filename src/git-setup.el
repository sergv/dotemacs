;; git-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 April 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/magit"))

(require 'common)
(require 'vim-mock)
(require 'magit)
(require 'magit-blame)
(require 'search)

(setf magit-completing-read-function
      (lambda (prompt collection &optional predicate require-match initial-input hist def)
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

(defun magit-cycle-sections-visibility (section)
  "Cycle visibility of childrens of SECTION in folling manner: if the're in the
same state (e.g. all collapsed or shown) then cycle all to next state, and hide
all otherwise."
  (let* ((children (magit-section-children section))
         (all-equal (= 1
                       (length (remove-duplicates (map #'magit-section-hidden children)
                                                  :test #'equal?)))))
    (save-excursion
      (dolist (child children)
        (goto-char (magit-section-beginning child))
        (if all-equal
          (magit-cycle-section)
          (magit-hide-section))))))

(defun magit-cycle-top-sections-visibility ()
  "Cycle visibility of sections across all buffer."
  (interactive)
  (magit-cycle-sections-visibility magit-top-section))

(defun magit-visit-item-other-window ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'magit-visit-item)))

(defun magit-mode-setup ()
  (setf truncate-lines nil)

  ;; here we do have vim mode enabled
  ;; (def-keys-for-map magit-mode-map
  ;;   +control-x-prefix+)
  ;; (def-keys-for-map magit-mode-map
  ;;   +vim-special-keys+)
  (def-keys-for-map magit-mode-map
    ("C"               magit-checkout)
    ("r"               magit-refresh)
    ("T"               magit-key-mode-popup-tagging)
    ("SPC"             magit-visit-item-other-window)
    ("S-TAB"           magit-cycle-top-sections-visibility)
    ("<S-iso-lefttab>" magit-cycle-top-sections-visibility)
    ("S-<iso-lefttab>" magit-cycle-top-sections-visibility)
    ("C-TAB"           magit-cycle-section)
    ("C-<tab>"         magit-cycle-section)))

(add-hook 'magit-mode-hook #'magit-mode-setup)

(defun magit-bind-common-vimless-mode-keymap (map)
  (def-keys-for-map map
    +control-x-prefix+
    +vim-special-keys+
    +vi-search-keys+
    ("r"      magit-refresh)

    ("p"      magit-key-mode-popup-stashing)
    ("T"      magit-key-mode-popup-tagging)
    ("<down>" magit-goto-next-section)
    ("<up>"   magit-goto-previous-section)
    ("t"      magit-goto-next-section)
    ("n"      magit-goto-previous-section)

    ("SPC"             magit-visit-item-other-window)
    ("S-TAB"           magit-cycle-top-sections-visibility)
    ("<S-iso-lefttab>" magit-cycle-top-sections-visibility)
    ("S-<iso-lefttab>" magit-cycle-top-sections-visibility)
    ("C-TAB"           magit-cycle-section)
    ("C-<tab>"         magit-cycle-section)))

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
    ("<down>"  log-edit-next-comment)
    ("M-p"     nil))

  (add-hook 'kill-buffer-hook
            #'magit-log-edit-cancel-log-message
            t ;; append
            t ;; local
            ))

(add-hook 'magit-log-edit-mode-hook #'magit-log-edit-mode-setup)

(defun magit-commit-mode-setup ()
  "Setup for commit browsing mode."
  (magit-bind-common-vimless-mode-keymap magit-commit-mode-map))

(add-hook 'magit-commit-mode-hook #'magit-commit-mode-setup)

(defun magit-diff-mode-setup ()
  "Setup for diff browsing mode."
  (magit-bind-common-vimless-mode-keymap magit-diff-mode-map))

(add-hook 'magit-diff-mode-hook #'magit-diff-mode-setup)


(defun magit-show-branches-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-show-branches-mode-map))

(add-hook 'magit-show-branches-mode-hook #'magit-show-branches-mode-setup)

(defun magit-branch-manager-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-branch-manager-mode-map))

(add-hook 'magit-branch-manager-mode-hook #'magit-branch-manager-mode-setup)

(defun magit-reflog-mode-setup ()
  (magit-bind-common-vimless-mode-keymap magit-reflog-mode-map))

(add-hook 'magit-reflog-mode-hook #'magit-reflog-mode-setup)



;; this mode has no hook
(eval-after-load
    "magit-key-mode"
  '(progn
     ;; add quitting with <escape>
     (redefun magit-key-mode-build-keymap (for-group)
       "Construct a normal looking keymap for the key mode to use.
Put it in `magit-key-mode-key-maps' for fast lookup."
       (let* ((options (magit-key-mode-options-for-group for-group))
              (actions (cdr (assoc 'actions options)))
              (switches (cdr (assoc 'switches options)))
              (arguments (cdr (assoc 'arguments options)))
              (map (make-sparse-keymap)))
         (suppress-keymap map 'nodigits)
         ;; ret dwim
         (define-key map (kbd "RET") 'magit-key-mode-exec-at-point)
         ;; tab jumps to the next "button"
         (define-key map (kbd "TAB") 'magit-key-mode-jump-to-next-exec)

         ;; all maps should `quit' with `C-g' or `q'
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

         (let ((defkey (lambda (k action)
                         (when (and (lookup-key map (car k))
                                    (not (numberp (lookup-key map (car k)))))
                           (message "Warning: overriding binding for `%s' in %S"
                                    (car k) for-group)
                           (ding)
                           (sit-for 2))
                         (define-key map (car k)
                           `(lambda () (interactive) ,action)))))
           (dolist (k actions)
             (funcall defkey k `(magit-key-mode-command ',(nth 2 k))))
           (dolist (k switches)
             (funcall defkey k `(magit-key-mode-add-option ',for-group ,(nth 2 k))))
           (dolist (k arguments)
             (funcall defkey k `(magit-key-mode-add-argument
                                 ',for-group ,(nth 2 k) ',(nth 3 k)))))

         (push (cons for-group map) magit-key-mode-key-maps)
         map))))



(defvar *have-git?* (executable-find "git")
  "Becomes t when git executable is accessible")

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
               (string-trim-whitespace
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
                          (map (comp #'common/registered-filename
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
      (with-temp-buffer
        (cd path)
        (when (= 0 (call-process "git"
                                 nil
                                 t
                                 nil
                                 "rev-parse"
                                 "--show-toplevel"))
          (strip-trailing-slash
           (string-trim-whitespace
            (buffer-substring-no-properties (point-min)
                                            (point-max))))))))

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


(provide 'git-setup)

;; Local Variables:
;; End:

;; git-setup.el ends here
