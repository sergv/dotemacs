;;; magit-commit.el --- Create Git commits  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements commands for creating Git commits.  These
;; commands just initiate the commit, support for writing the commit
;; messages is implemented in `git-commit.el'.

;;; Code:

(require 'magit)
(require 'magit-sequence)

;;; Options

(defcustom magit-commit-ask-to-stage 'verbose
  "Whether to ask to stage everything when committing and nothing is staged."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type '(choice (const :tag "Ask" t)
                 (const :tag "Ask showing diff" verbose)
                 (const :tag "Stage without confirmation" stage)
                 (const :tag "Don't ask" nil)))

(defcustom magit-commit-show-diff t
  "Whether the relevant diff is automatically shown when committing."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-reword-override-date t
  "Whether using `magit-commit-reword' changes the committer date."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-squash-confirm t
  "Whether the commit targeted by squash and fixup has to be confirmed.
When non-nil then the commit at point (if any) is used as default
choice, otherwise it has to be confirmed.  This option only
affects `magit-commit-squash' and `magit-commit-fixup'.  The
\"instant\" variants always require confirmation because making
an error while using those is harder to recover from."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-diff-inhibit-same-window nil
  "Whether to inhibit use of same window when showing diff while committing.

When writing a commit, then a diff of the changes to be committed
is automatically shown.  The idea is that the diff is shown in a
different window of the same frame and for most users that just
works.  In other words most users can completely ignore this
option because its value doesn't make a difference for them.

However for users who configured Emacs to never create a new
window even when the package explicitly tries to do so, then
displaying two new buffers necessarily means that the first is
immediately replaced by the second.  In our case the message
buffer is immediately replaced by the diff buffer, which is of
course highly undesirable.

A workaround is to suppress this user configuration in this
particular case.  Users have to explicitly opt-in by toggling
this option.  We cannot enable the workaround unconditionally
because that again causes issues for other users: if the frame
is too tiny or the relevant settings too aggressive, then the
diff buffer would end up being displayed in a new frame.

Also see https://github.com/magit/magit/issues/4132."
  :package-version '(magit . "3.3.0")
  :group 'magit-commands
  :type 'boolean)

(defvar magit-common-git-post-commit-functions nil
  "Hook run by Git hooks `post-commit', `post-merge' and `post-rewrite'.

There is not a single Git hook, which is called after a commit is
created; to achieve that, all of these Git hooks have to be used.

Git hooks are documented in the githooks(5) manpage.  This Lisp hook
is only run if `magit-run-hooks-from-githooks' is non-nil, and Magit
runs Git asynchronously and on the local machine.  The hook functions
are called with the same arguments as the Git hook; see the mentioned
manpage for details.")

;;; Menu

;;;###autoload(autoload 'magit-commit "magit-commit" nil t)
(transient-define-prefix magit-commit ()
  "Create a new commit or replace an existing commit."
  :info-manual "(magit)Initiating a Commit"
  :man-page "git-commit"
  :value '()
  ["Arguments"
   ("-a" "Stage all modified and deleted files"   ("-a" "--all"))
   ("-e" "Allow empty commit"                     "--allow-empty")
   ("-n" "Disable hooks"                          ("-n" "--no-verify"))
   ("-R" "Claim authorship and reset author date" "--reset-author")
   (magit:--author :description "Override the author")
   (magit-commit:--date :level 7)
   (magit:--gpg-sign :level 5)
   (magit:--signoff)
   (magit-commit:--reuse-message)
   (magit-commit:--reedit-message)]
  [["Create"
    ("c" "Commit"         magit-commit-create)]
   ["Edit HEAD"
    ("a" "Amend"          magit-commit-amend)
    ("w" "Reword"         magit-commit-reword)]
   ["Edit and rebase"
    ("F" "Instant fixup"  magit-commit-instant-fixup)
    ""
    ""
    ("R" "Reword past"    magit-rebase-reword-commit :level 0)]]
  (interactive)
  (if-let ((buffer (magit-commit-message-buffer)))
      (switch-to-buffer buffer)
    (transient-setup 'magit-commit)))

(defun magit-commit-arguments nil
  (transient-args 'magit-commit))

(transient-define-argument magit-commit:--date ()
  :description "Override the author date"
  :class 'transient-option
  :shortarg "-D"
  :argument "--date="
  :reader #'transient-read-date)

(transient-define-argument magit-commit:--reuse-message ()
  :description "Reuse commit message"
  :class 'transient-option
  :shortarg "-C"
  :argument "--reuse-message="
  :reader #'magit-read-reuse-message
  :history-key 'magit-revision-history)

(transient-define-argument magit-commit:--reedit-message ()
  :description "Reedit commit message"
  :class 'transient-option
  :shortarg "-c"
  :argument "--reedit-message="
  :reader #'magit-read-reuse-message
  :history-key 'magit-revision-history)

(defun magit-read-reuse-message (prompt &optional default history)
  (if current-prefix-arg
      (let (rev)
        (magit-log-select
          (lambda (r) (setq rev r) (exit-recursive-edit))
          "Type %p on a commit to reuse its message")
        (recursive-edit)
        rev)
    (magit-completing-read prompt (magit-list-refnames)
                           nil nil nil history
                           (or default
                               (magit-commit-at-point)
                               (and (magit-rev-verify "ORIG_HEAD")
                                    "ORIG_HEAD")))))

;;; Commands
;;;; Create

;;;###autoload
(defun magit-commit-create (&optional args)
  "Create a new commit."
  (interactive (list (magit-commit-arguments)))
  (cond ((member "--all" args)
         (setq this-command 'magit-commit--all))
        ((member "--allow-empty" args)
         (setq this-command 'magit-commit--allow-empty)))
  (when (setq args (magit-commit-assert args))
    (let ((default-directory (magit-toplevel)))
      (magit-run-git-with-editor "commit" args))))

;;;; Edit HEAD

;;;###autoload
(defun magit-commit-amend (&optional args)
  "Amend staged changes (if any) to the last commit, and edit its message."
  (interactive (list (magit-commit-arguments)))
  (magit-commit-amend-assert)
  (magit-run-git-with-editor "commit" "--amend" args))

;;;###autoload
(defun magit-commit-reword (&optional args override-date)
  "Reword the message of the last commit, without amending its tree.

With a prefix argument do not update the committer date; without an
argument update it.  The option `magit-commit-reword-override-date'
can be used to inverse the meaning of the prefix argument.  Called
non-interactively, the optional OVERRIDE-DATE argument controls this
behavior, and the option is of no relevance."
  (interactive (list (magit-commit-arguments)
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (magit-commit-amend-assert)
  (cl-pushnew "--allow-empty" args :test #'equal)
  (if override-date
      (magit-run-git-with-editor "commit" "--amend" "--only" args)
    (with-environment-variables
        (("GIT_COMMITTER_DATE" (magit-rev-format "%cD")))
      (magit-run-git-with-editor "commit" "--amend" "--only" args))))

;;;; Edit

;;;; Edit and Rebase

;;;###autoload
(defun magit-commit-instant-fixup (&optional commit args)
  "Create a fixup commit, and immediately combine it with its target.

If there is a reachable commit at point, target that.  Otherwise prompt
for a commit.  If `magit-commit-squash-confirm' is non-nil, always make
the user explicitly select a commit, in a buffer dedicated to that task.

Leave the original commit message of the targeted commit untouched.

Like `magit-commit-fixup' but also run a \"--autofixup\" rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--fixup=" commit args nil nil 'rebase))

;;;; Internal

(defun magit-commit-squash-internal
    (option commit &optional args nopatch edit rebase confirmed)
  (when-let ((args (magit-commit-assert args nopatch (not edit))))
    (when (and commit rebase (not (magit-rev-ancestor-p commit "HEAD")))
      (magit-read-char-case
          (format "%s isn't an ancestor of HEAD.  " commit) nil
        (?c "[c]reate without rebasing" (setq rebase nil))
        (?s "[s]elect other"            (setq commit nil))
        (?a "[a]bort"                   (user-error "Quit"))))
    (when commit
      (setq commit (magit-rebase-interactive-assert commit t)))
    (if (and commit
             (or confirmed
                 (not (or rebase
                          current-prefix-arg
                          magit-commit-squash-confirm))))
        (let ((magit-commit-show-diff nil))
          (push (concat option commit) args)
          (push (if edit "--edit" "--no-edit") args)
          (if rebase
              (magit-with-editor
                (magit-call-git
                 "commit" "--no-gpg-sign"
                 (seq-remove (apply-partially #'string-prefix-p "--gpg-sign=")
                             args)))
            (magit-run-git-with-editor "commit" args))
          t) ; The commit was created; used by below lambda.
      (let ((winconf (and magit-commit-show-diff
                          (current-window-configuration))))
        (magit-log-select
          (lambda (commit)
            (when (and (magit-commit-squash-internal option commit args
                                                     nopatch edit rebase t)
                       rebase)
              (magit-commit-amend-assert commit)
              (magit-rebase-interactive-1 commit
                  (list "--autosquash" "--autostash" "--keep-empty")
                "" "true" nil t))
            (when winconf
              (set-window-configuration winconf)))
          (format "Type %%p on a commit to %s into it,"
                  (substring option 2))
          nil nil nil commit))
      (when (and magit-commit-show-diff (not nopatch))
        (let ((magit-display-buffer-noselect t))
          (apply #'magit-diff-staged nil (magit-diff-arguments)))))))

(defun magit-commit-amend-assert (&optional commit)
  (when-let ((branches (magit-list-publishing-branches commit)))
    (let ((m1 "This commit has already been published to ")
          (m2 ".\nDo you really want to modify it"))
      (magit-confirm 'amend-published
        (concat m1 "%s" m2)
        (concat m1 "%d public branches" m2)
        nil branches))))

(defun magit-commit-assert (args &optional nopatch strict)
  (cond
    (nopatch (or args (list "--")))
    ((or (magit-anything-staged-p)
         (and (magit-anything-unstaged-p)
              ;; ^ Everything of nothing is still nothing.
              (member "--all" args))
         (and (not strict)
              ;; ^ For amend variants that don't make sense otherwise.
              (or (member "--amend" args)
                  (member "--allow-empty" args)
                  (member "--reset-author" args)
                  (member "--signoff" args)
                  (transient-arg-value "--author=" args)
                  (transient-arg-value "--date=" args))))
     (or args (list "--")))
    ((and (magit-rebase-in-progress-p)
          (not (magit-anything-unstaged-p))
          (y-or-n-p "Nothing staged.  Continue in-progress rebase? "))
     (setq this-command #'magit-rebase-continue)
     (magit-run-git-sequencer "rebase" "--continue")
     nil)
    ((file-exists-p (expand-file-name "MERGE_MSG" (magit-gitdir)))
     (cond ((magit-anything-unmerged-p)
            (user-error "Unresolved conflicts"))
           ((and (magit-anything-unstaged-p)
                 (not (y-or-n-p
                       "Proceed with merge despite unstaged changes? ")))
            (user-error "Abort"))
           ((or args (list "--")))))
    ((not (magit-anything-unstaged-p))
     (user-error "Nothing staged (or unstaged)"))
    (magit-commit-ask-to-stage
     (when (eq magit-commit-ask-to-stage 'verbose)
       (apply #'magit-diff-unstaged (magit-diff-arguments)))
     (prog1 (when (or (eq magit-commit-ask-to-stage 'stage)
                      (y-or-n-p
                       "Nothing staged.  Commit all uncommitted changes? "))
              (setq this-command 'magit-commit--all)
              (cons "--all" (or args (list "--"))))
       (when (and (eq magit-commit-ask-to-stage 'verbose)
                  (derived-mode-p 'magit-diff-mode))
         (magit-mode-bury-buffer))))
    (t
     (user-error "Nothing staged"))))

;;; Pending Diff

(defun magit-commit-diff ()
  (magit-repository-local-set 'this-commit-command
                              (if (eq this-command 'with-editor-finish)
                                  'magit-commit--rebase
                                last-command))
  (when (and git-commit-mode magit-commit-show-diff)
    (when-let ((diff-buffer
                ;; This signals an error if not inside a Git repository,
                ;; but the user may be visiting COMMIT_EDITMSG using a
                ;; tool other than git, which can be used outside a Git
                ;; repository.  See #5527.
                (ignore-error magit-outside-git-repo
                  (magit-get-mode-buffer 'magit-diff-mode))))
      ;; This window just started displaying the commit message buffer.
      ;; Without unrecording that buffer would immediately be replaced
      ;; with the diff buffer.  See #2632.
      (unrecord-window-buffer nil diff-buffer))
    (message "Diffing changes to be committed (C-g to abort diffing)")
    (let ((inhibit-quit nil))
      (condition-case nil
          (with-demoted-errors "Error showing commit diff: %S"
            (magit-commit-diff--show))
        (quit)))))

(defun magit-commit-diff--args ()
  (let ((rev nil)
        (arg "--cached")
        (command (magit-repository-local-get 'this-commit-command))
        (staged (magit-anything-staged-p))
        (unstaged
         ;; Escape $GIT_DIR because `magit-anything-unstaged-p'
         ;; requires a working tree.
         (magit-with-toplevel
           (magit-anything-unstaged-p)))
        (squash (let ((f (expand-file-name "rebase-merge/rewritten-pending"
                                           (magit-gitdir))))
                  (and (file-exists-p f) (length (magit-file-lines f)))))
        (noalt nil))
    (pcase (list staged unstaged command)
      ((guard (not (magit-commit-p "HEAD^")))
       (setq rev "HEAD")
       (setq arg nil))
      ((and `(,_ ,_ magit-commit--rebase)
            (guard (integerp squash)))
       (setq rev (format "HEAD~%s" squash)))
      (`(,_ ,_ magit-commit-amend)
       (setq rev "HEAD^"))
      (`(nil nil magit-commit--allow-empty)
       (setq rev "HEAD")
       (setq arg nil))
      ((or `(,_ ,_ magit-commit-reword)
           `(nil nil ,_))
       (setq rev "HEAD^..HEAD")
       (setq arg nil))
      (`(,_ t magit-commit--all)
       (setq rev "HEAD")
       (setq arg nil))
      (`(nil t handle-switch-frame)
       ;; Either --all or --allow-empty. Assume it is the former.
       (setq rev "HEAD")
       (setq arg nil)))
    (cond
      ((not
        (and-let*
            ((_(eq this-command 'magit-diff-while-committing))
             (buf (magit-get-mode-buffer 'magit-diff-mode nil 'selected))
             (_(equal rev (buffer-local-value 'magit-buffer-diff-range buf)))
             (_(equal arg (buffer-local-value 'magit-buffer-diff-typearg buf)))))))
      ((eq command 'magit-commit-amend)
       (setq rev nil))
      ((or squash
           (file-exists-p (expand-file-name "rebase-merge/amend" (magit-gitdir))))
       (setq rev "HEAD^"))
      ((setq noalt t)))
    (list rev arg noalt)))

(defun magit-commit-diff--show ()
  (pcase-let ((`(,rev ,arg ,noalt) (magit-commit-diff--args)))
    (if noalt
        (message "No alternative diff while committing")
      (let ((magit-inhibit-save-previous-winconf 'unset)
            (magit-display-buffer-noselect t)
            (display-buffer-overriding-action
             display-buffer-overriding-action))
        (when magit-commit-diff-inhibit-same-window
          (setq display-buffer-overriding-action
                '(nil (inhibit-same-window . t))))
        (magit-diff-setup-buffer rev arg (car (magit-diff-arguments)) nil
                                 (cond ((equal rev "HEAD") 'staged)
                                       ((equal rev "HEAD^..HEAD") 'committed)
                                       ('undefined)))))))

(add-hook 'server-switch-hook #'magit-commit-diff)
(add-hook 'with-editor-filter-visit-hook #'magit-commit-diff)

(add-to-list 'with-editor-server-window-alist
             (cons git-commit-filename-regexp #'switch-to-buffer))

(defun magit-commit--reset-command ()
  (magit-repository-local-delete 'this-commit-command))

;;; Message Utilities

(defun magit-commit-message-buffer ()
  (let* ((find-file-visit-truename t) ; git uses truename of COMMIT_EDITMSG
         (topdir (magit-toplevel)))
    (seq-find (##equal topdir (with-current-buffer %
                                (and git-commit-mode (magit-toplevel))))
              (append (buffer-list (selected-frame))
                      (buffer-list)))))

(defvar magit-commit-add-log-insert-function #'magit-commit-add-log-insert
  "Used by `magit-commit-add-log' to insert a single entry.")

(defun magit-commit-add-log ()
  "Add a stub for the current change into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `magit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (pcase-let* ((hunk (and (magit-section-match 'hunk)
                          (magit-current-section)))
               (log  (magit-commit-message-buffer))
               (`(,buf ,pos) (magit-diff-visit-file--noselect)))
    (unless log
      (unless (magit-commit-assert nil)
        (user-error "Abort"))
      (magit-commit-create)
      (while (not (setq log (magit-commit-message-buffer)))
        (sit-for 0.01)))
    (magit--with-temp-position buf pos
      (funcall magit-commit-add-log-insert-function log
               (magit-file-relative-name)
               (and hunk (add-log-current-defun))))))

(defun magit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (undo-boundary)
    (goto-char (point-max))
    (git-commit--goto-insert-position nil)
    (while (re-search-backward (git-commit--trailer-regexp) nil t))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (cond ((re-search-backward (format "* %s\\(?: (\\([^)]+\\))\\)?: " file)
                                 nil t)
             (when (equal (match-str 1) defun)
               (setq defun nil))
             (re-search-forward ": "))
            (t
             (when (re-search-backward "^[\\*(].+\n" nil t)
               (goto-char (match-end 0)))
             (while (re-search-forward "^[^\\*\n].*\n" nil t))
             (if defun
                 (progn (insert (format "* %s (%s): \n" file defun))
                        (setq defun nil))
               (insert (format "* %s: \n" file)))
             (backward-char)
             (unless (looking-at "\n[\n\\']")
               (insert ?\n)
               (backward-char))))
      (when defun
        (forward-line)
        (let ((limit (save-excursion
                       (and (re-search-forward "^\\*" nil t)
                            (point)))))
          (unless (or (looking-back (format "(%s): " defun) (pos-bol))
                      (re-search-forward (format "^(%s): " defun) limit t))
            (while (re-search-forward "^[^\\*\n].*\n" limit t))
            (insert (format "(%s): \n" defun))
            (backward-char)))))))

;;; Post Hook

(defcustom magit-post-commit-hook nil
  "Hook run after creating a commit without the user editing a message.

This hook is run by `magit-refresh' if `this-command' is a member
of `magit-post-commit-hook-commands'.  This only includes commands
named `magit-commit-*' that do *not* require that the user edits
the commit message in a buffer and then finishes by pressing
\\<with-editor-mode-map>\\[with-editor-finish].

Also see `git-commit-post-finish-hook'."
  :package-version '(magit . "2.90.0")
  :group 'magit-commands
  :type 'hook)

(defvar magit-post-commit-hook-commands
  (list #'magit-commit-instant-fixup))

;;;###autoload
(defun magit-run-post-commit-hook ()
  (when (and (not this-command)
             (memq last-command magit-post-commit-hook-commands))
    (run-hooks 'magit-post-commit-hook)))

;;; _
(provide 'magit-commit)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("thread$"      . "cond-let--thread$")
;;   ("when$"        . "cond-let--when$")
;;   ("and-let*"     . "cond-let--and-let*")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let*"      . "cond-let--if-let*")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let*"    . "cond-let--when-let*")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let*"   . "cond-let--while-let*")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; magit-commit.el ends here
