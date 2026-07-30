;;; magit-merge.el --- Merge functionality  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2025 The Magit Project Contributors

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

;; This library implements merge commands.

;;; Code:

(require 'magit)
(require 'magit-diff)
(require 'magit-log)

(declare-function magit-git-push "magit-push" (branch target args))

;;; Commands

;;;###autoload (autoload 'magit-merge "magit" nil t)
(transient-define-prefix magit-merge ()
  "Merge branches."
  :man-page "git-merge"
  :incompatible '(("--ff-only" "--no-ff"))
  ["Arguments"
   :if-not magit-merge-in-progress-p
   ("-f" "Fast-forward only" "--ff-only")
   ("-n" "No fast-forward"   "--no-ff")
   (magit-merge:--strategy)
   (5 magit-merge:--strategy-option)
   (5 "-b" "Ignore changes in amount of whitespace" "-Xignore-space-change")
   (5 "-w" "Ignore whitespace when comparing lines" "-Xignore-all-space")
   (5 magit-diff:--diff-algorithm :argument "-Xdiff-algorithm=")
   (magit:--gpg-sign)
   (magit:--signoff)]
  ["Actions"
   :if-not magit-merge-in-progress-p
   [("m" "Merge"                  magit-merge-plain)
    ("e" "Merge and edit message" magit-merge-editmsg)
    ("n" "Merge but don't commit" magit-merge-nocommit)]
   [("p" "Preview merge"          magit-merge-preview)
    ""
    ("s" "Squash merge"           magit-merge-squash)]]
  ["Actions"
   :if magit-merge-in-progress-p
   ("m" "Commit merge" magit-commit-create)
   ("a" "Abort merge"  magit-merge-abort)])

(defun magit-merge-arguments ()
  (transient-args 'magit-merge))

(transient-define-argument magit-merge:--strategy ()
  :description "Strategy"
  :class 'transient-option
  ;; key for merge and rebase: "-s"
  ;; key for cherry-pick and revert: "=s"
  ;; shortarg for merge and rebase: "-s"
  ;; shortarg for cherry-pick and revert: none
  :key "-s"
  :argument "--strategy="
  :choices '("resolve" "recursive" "octopus" "ours" "subtree"))

(transient-define-argument magit-merge:--strategy-option ()
  :description "Strategy Option"
  :class 'transient-option
  :key "-X"
  :argument "--strategy-option="
  :choices '("ours" "theirs" "patience"))

;;;###autoload
(defun magit-merge-plain (rev &optional args nocommit)
  "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)
                     current-prefix-arg))
  (magit-merge-assert)
  (magit-run-git-async "merge" (if nocommit "--no-commit" "--no-edit") args rev))

;;;###autoload
(defun magit-merge-editmsg (rev &optional args)
  "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.
\n(git merge --edit --no-ff [ARGS] REV)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)))
  (magit-merge-assert)
  (cl-pushnew "--no-ff" args :test #'equal)
  (apply #'magit-run-git-with-editor "merge" "--edit"
         (append (delete "--ff-only" args)
                 (list rev))))

;;;###autoload
(defun magit-merge-nocommit (rev &optional args)
  "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.
\n(git merge --no-commit --no-ff [ARGS] REV)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)))
  (magit-merge-assert)
  (cl-pushnew "--no-ff" args :test #'equal)
  (magit-run-git-async "merge" "--no-commit" args rev))

;;;###autoload
(defun magit-merge-squash (rev)
  "Squash commit REV into the current branch; don't create a commit.
\n(git merge --squash REV)"
  (interactive (list (magit-read-other-branch-or-commit "Squash")))
  (magit-merge-assert)
  (magit-run-git-async "merge" "--squash" rev))

;;;###autoload
(defun magit-merge-preview (rev)
  "Preview result of merging REV into the current branch."
  (interactive (list (magit-read-other-branch-or-commit "Preview merge")))
  (magit-merge-preview-setup-buffer rev))

;;;###autoload
(defun magit-merge-abort ()
  "Abort the current merge operation.
\n(git merge --abort)"
  (interactive)
  (unless (file-exists-p (expand-file-name "MERGE_HEAD" (magit-gitdir)))
    (user-error "No merge in progress"))
  (magit-confirm 'abort-merge)
  (magit-run-git-async "merge" "--abort"))

(defun magit-checkout-stage (file arg)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((file (magit-completing-read "Checkout file"
                                      (magit-tracked-files) nil 'any nil
                                      'magit-read-file-hist
                                      (magit-current-file))))
     (cond ((member file (magit-unmerged-files))
            (list file (magit-checkout-read-stage file)))
           ((yes-or-no-p (format "Restore conflicts in %s? " file))
            (list file "--merge"))
           ((user-error "Quit")))))
  (pcase (cons arg (cddr (car (magit-file-status file))))
    ((or `("--ours"   ?D ,_)
         '("--ours"   ?U ?A)
         `("--theirs" ,_ ?D)
         '("--theirs" ?A ?U))
     (magit-run-git "rm" "--" file))
    (_ (if (equal arg "--merge")
           ;; This fails if the file was deleted on one
           ;; side.  And we cannot do anything about it.
           (magit-run-git "checkout" "--merge" "--" file)
         (magit-call-git "checkout" arg "--" file)
         ;; Do not automatically stage file after we'we picked "ours"
         ;; or "theirs" version because after it's staged there's no
         ;; easy way to redo the choice. And this choice is basically
         ;; a guess, so it needs to be redone pretty regularly.
         ;; (magit-run-git "add" "-u" "--" file)
         (message "Picked %s. Do manual stage if result looks OK" arg)))))

;;; Utilities

(defun magit-merge-in-progress-p ()
  (file-exists-p (expand-file-name "MERGE_HEAD" (magit-gitdir))))

(defun magit--merge-range (&optional head)
  (unless head
    (setq head (magit-get-shortname
                (car (magit-file-lines
                      (expand-file-name "MERGE_HEAD" (magit-gitdir)))))))
  (and head
       (concat (magit-git-string "merge-base" "--octopus" "HEAD" head)
               ".." head)))

(defun magit-merge-assert ()
  (or (not (magit-anything-modified-p t))
      (magit-confirm 'merge-dirty
        "Merging with dirty worktree is risky.  Continue")))

(defun magit-checkout-read-stage (file)
  (magit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c (if magit-verbose-messages "restore [c]onflict" "[c]onflict")
        "--merge")))

;;; Sections

(defun magit-insert-merge-log ()
  "Insert section for the on-going merge.
Display the heads that are being merged.
If no merge is in progress, do nothing."
  (when (magit-merge-in-progress-p)
    (let* ((heads (mapcar #'magit-get-shortname
                          (magit-file-lines
                           (expand-file-name "MERGE_HEAD" (magit-gitdir)))))
           (range (magit--merge-range (car heads))))
      (magit-insert-section (unmerged range)
        (magit-insert-heading
          (format "Merging %s:" (string-join heads ", ")))
        (magit--insert-log nil
          range
          (let ((args magit-buffer-log-args))
            (unless (member "--decorate=full" magit-buffer-log-args)
              (setf args (cons "--decorate=full"
                               (append magit--log-refs-to-decorate
                                       args))))
            args))))))

;;; _
(provide 'magit-merge)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("and>"         . "cond-let--and>")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; magit-merge.el ends here
