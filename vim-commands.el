;;; vim-commands.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; TODO:
;;
;;  - vim:yank-line may be wrong at the last line in a buffer
;;  - the position of the cursor after yank works for "p" and "P" but
;;    is wrong for Emacs-commands

(provide 'vim-commands)

(require 'redo)

(defvar vim:paste-behind nil)

(defun vim:cmd-delete-line (count motion)
  "Deletes the next count lines."
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-line (1- (or count 1)))
               (line-end-position))))
    
    (if (= beg end)
        (kill-region beg (1+ end) (list 'vim:insert-yanked-lines ""))
      (progn
        (kill-region beg end (list 'vim:insert-yanked-lines))
        ;; now we have to remove a new-line
        (cond
         ;; delete the following newline
         ((< end (point-max))
          (delete-char 1))
         ;;  delete the previous newline
         ((> beg (point-min))
          (delete-char -1))
         
         ;; buffer must be empty - do nothing
         (t))))))


(defun vim:cmd-delete (count motion)
  "Deletes the characters defined by motion."
  (kill-region (car motion) (1+ (cdr motion)))
  (goto-char (car motion)))


(defun vim:cmd-change (count motion)
  "Deletes the characters defined by motion and goes to insert mode."
  (vim:cmd-delete count motion)
  (if (eolp)
      (vim:normal-append 1 nil)
    (vim:normal-insert 1 nil)))


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


(defun vim:cmd-yank (count motion)
  "Saves the characters in motion into the kill-ring."
  (kill-new (buffer-substring (car motion) (1+ (cdr motion)))))
  

(defun vim:cmd-yank-line (count motion)
  "Saves the next count lines into the kill-ring."
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-line (1- (or count 1)))
               (line-end-position))))
    (kill-new (buffer-substring beg end) nil
              (list 'vim:insert-yanked-lines))))

(defun vim:cmd-paste-before (count motion)
  "Pastes the latest yanked text before the cursor position."
  (dotimes (i (or count 1))
    (let ((vim:paste-behind nil))
      (yank)))
  (backward-char))

(defun vim:cmd-paste-behind (count motion)
  "Pastes the latest yanked text behind point."
  (dotimes (i (or count 1))
    (let ((vim:paste-behind t))
      (unless (eolp) (forward-char))
      (yank)
      (backward-char))))

(defun vim:insert-yanked-lines (text)
  (if vim:paste-behind
      (progn
        (end-of-line)
        (newline)
        (let ((beg (point)))
          (insert text)
          (remove-text-properties beg (point) '(yank-handler))
          (goto-char beg))
        (setq vim:paste-begin nil))
    (progn
      (beginning-of-line)
      (let ((beg (point)))
        (insert text)
        (remove-text-properties beg (point) '(yank-handler))
        (newline)
        (goto-char beg))))
  (forward-char))


