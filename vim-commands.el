;;; vim-commands.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; Description:

;; In general there are two types of commands: those operating on a
;; motion and those not taking a motion.  Examples of the first one
;; are the vim-commands c, d, y, =, examples of the second one are dd,
;; D, p, x.
;;
;; A function implementing a motion should take two or three arguments:
;;
;;  - a count
;;  - a motion of type vim:motion
;;  - an (optional) argument character
;;
;; If the operation does not require a motion, the second parameter is
;; usually nil.  If the operation takes a motion, the cound parameter
;; should usually be ignored since the count has already been regarded
;; by the motion itself (the motion function got (command-count *
;; motion-count) as count parameter.
;;
;; An operations based on motions should always respect the motion
;; type, i.e. if the motion is linewise or not.  Motions passed to
;; commands will always be inclusive (and never exlusive).  For
;; example, the command dG has a linewise motion argument and should
;; delete whole lines.
;;
;; Furthermore, each operation should place (point) at the correct
;; position after the operation.

;; TODO:
;;
;;  - vim:yank-line may be wrong at the last line in a buffer
;;  - the position of the cursor after yank works for "p" and "P" but
;;    is wrong for Emacs-commands
;;  - vim:cmd-delete-line should delete newlines correctly (even "\r\n")

(provide 'vim-commands)

(require 'redo)


(defun vim:cmd-insert (count motion)
  (vim:activate-mode vim:insert-mode))

(defun vim:cmd-append (count motion)
  (unless (eolp) (forward-char))
  (vim:activate-mode vim:insert-mode))

(defun vim:cmd-Insert (count motion)
  (beginning-of-line)
  (vim:cmd-insert count motion))

(defun vim:cmd-Append (count motion)
  (end-of-line)
  (vim:cmd-append count motion))



(defun vim:cmd-delete-line (count motion)
  "Deletes the next count lines."
  (vim:cmd-yank-line count motion)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-line (1- (or count 1)))
               (line-end-position))))
    (if (= beg (point-min))
        (if (= end (point-max))
            (erase-buffer)
          (delete-region beg (save-excursion
                               (goto-char end)
                               (forward-line)
                               (line-beginning-position))))
      (delete-region (save-excursion
                       (goto-char beg)
                       (forward-line -1)
                       (line-end-position))
                     end))
    (goto-char beg)
    (goto-char (vim:motion-end (vim:motion-first-non-blank 1)))))


(defun vim:cmd-delete (count motion)
  "Deletes the characters defined by motion."
  (if (eq vim:current-motion-type 'linewise)
      (progn
        (goto-char (vim:motion-begin motion))
        (vim:cmd-delete-line (vim:motion-line-count motion) nil))
    (progn
      (kill-region (vim:motion-begin motion) (min (point-max) (1+ (vim:motion-end motion))))
      (goto-char (vim:motion-begin motion)))))


(defun vim:cmd-change (count motion)
  "Deletes the characters defined by motion and goes to insert mode."
  (if (eq vim:current-motion-type 'linewise)
      (progn
        (goto-char (vim:motion-begin motion))
        (vim:cmd-change-line (vim:motion-line-count motion) nil))
    (progn
      (vim:cmd-delete count motion)
      (if (eolp)
          (vim:cmd-append 1 nil)
        (vim:cmd-insert 1 nil)))))


(defun vim:cmd-change-line (count motion)
  "Deletes count lines and goes to insert mode."
  (let ((pos (line-beginning-position)))
    (vim:cmd-delete-line count motion)
    (if (< (point) pos)
        (progn
          (end-of-line)
          (newline))
      (progn
        (beginning-of-line)
        (newline)
        (forward-line -1)))
    (indent-according-to-mode)
    (if (eolp)
        (vim:cmd-append 1 nil)
      (vim:cmd-insert 1 nil))))


(defun vim:cmd-replace-char (count motion arg)
  "Replaces the next count characters with arg."
  (unless (integerp arg)
    (error "Expected a character."))
  (when (< (- (line-end-position) (point))
           (or count 1))
    (error "Too few characters to end of line."))
  (delete-region (point) (+ (point) (or count 1)))
  (insert-char arg (or count 1))
  (backward-char))


(defun vim:cmd-replace (count motion)
  "Goes to replace-mode."
  (overwrite-mode t)
  (vim:normal-insert 1 nil))


(defun vim:cmd-yank (count motion)
  "Saves the characters in motion into the kill-ring."
  (kill-new (buffer-substring (vim:motion-begin motion) (1+ (vim:motion-end motion)))))
  

(defun vim:cmd-yank-line (count motion)
  "Saves the next count lines into the kill-ring."
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-line (1- (or count 1)))
               (line-end-position))))
    (kill-new (concat (buffer-substring beg end) "\n") nil)))

(defun vim:cmd-paste-before (count motion)
  "Pastes the latest yanked text before the cursor position."
  (unless kill-ring-yank-pointer
    (error "kill-ring empty"))
  
  (let* ((txt (car kill-ring-yank-pointer))
         (linewise (= (elt txt (1- (length txt))) ?\n)))
    (if linewise
        (progn
          (beginning-of-line)
          (save-excursion
            (dotimes (i (or count 1))
              (yank))))
      (progn
        (dotimes (i (or count 1))
          (yank))
        (backward-char)))))


(defun vim:cmd-paste-behind (count motion)
  "Pastes the latest yanked text behind point."
  (unless kill-ring-yank-pointer
    (error "kill-ring empty"))
  
  (let* ((txt (car kill-ring-yank-pointer))
         (linewise (= (elt txt (1- (length txt))) ?\n))
         (last-line (= (line-end-position) (point-max))))
    (if linewise
        (progn
          (if last-line
              (progn
                (end-of-line)
                (newline))
            (forward-line))
          (beginning-of-line)
          (save-excursion
            (dotimes (i (or count 1))
              (yank))
            (when last-line
              ;; remove the last newline
              (let ((del-pos (point)))
                (forward-line -1)
                (end-of-line)
                (delete-region (point) del-pos)))))
      (progn
        (forward-char)
        (dotimes (i (or count 1))
          (yank))
        (backward-char)))))


