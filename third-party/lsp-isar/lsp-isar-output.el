;;; lsp-isar-output.el --- Update the state and output buffers -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Mathias Fleury

;; Author: Mathias Fleury <mathias.fleury@protonmail.com>
;; URL: https://bitbucket.org/zmaths/isabelle2019-vsce/

;; Keywords: lisp
;; Version: 0

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Provides two ways to update the output and state buffer.  Once directly and another
;; asynchronously via the async library.

;;; Code:

(eval-when-compile
  (require 'macro-util))

(require 'common)

(require 'isar-goal-mode)
(require 'lsp-isar-decorations)
(require 'dash)

(require 'dom)
(require 'lsp-protocol)
(require 'lsp-isar-types)

(eval-when-compile (require 'subr-x))

(defvar lsp-isar-output-proof-timer nil "Current timer rendering the HTML.")

(defcustom lsp-isar-output-time-before-printing-goal 0.3
  "Time before printing goal.  Use nil to avoid printing."
  :type '(number)
  :group 'isabelle)


;; Turn
;; """
;; have 0 \<le> (\<Sum>j = 1..n. (x\<^bsub>j\<^esub>)\<^sup>2)
;; proof (state)
;;             this:
;;               0 \<le> (\<Sum>j = 1..n. (x\<^bsub>j\<^esub>)\<^sup>2)
;;
;;             goal (1 subgoal):
;;              1. 0 \<le> (\<parallel>x\<parallel> * \<parallel>y\<parallel>)\<^sup>2 - \<bar>x \<cdot> y\<bar>\<^sup>2
;; """
;;
;; Into
;;
;; """
;; have 0 \<le> (\<Sum>j = 1..n. (x\<^bsub>j\<^esub>)\<^sup>2)
;; proof (state)
;; this:
;;   0 \<le> (\<Sum>j = 1..n. (x\<^bsub>j\<^esub>)\<^sup>2)
;;
;; goal (1 subgoal):
;;  1. 0 \<le> (\<parallel>x\<parallel> * \<parallel>y\<parallel>)\<^sup>2 - \<bar>x \<cdot> y\<bar>\<^sup>2
;; """
(defun lsp-isar-output--delete-indentation-safe! (n)
  (cl-assert (numberp n))
  (let ((start (point)))
    (skip-indentation-forward (min (+ start n) (line-end-position)))
    (delete-region start (point))))

(defun lsp-isar-output--fix-indents! ()
  ;; (message "Contents:\n%S" (buffer-substring-no-properties (point-min) (point-max)))
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (search-forward "proof (state)" nil t)
        (let* ((proof-state-pt
                (line-beginning-position))
               (this-indent-and-pt
                (save-excursion
                  (when (search-forward "this:" nil t)
                    (cons (indentation-size)
                          (copy-marker (line-beginning-position))))))
               (goal-indent-and-pt
                (save-excursion
                  (when (re-search-forward (rx "goal" (? "(" (+ (any (?0 . ?9))) " subgoal" (? "s") ")") ":")
                                           nil
                                           t)
                    (cons (indentation-size)
                          (copy-marker (line-beginning-position)))))))

          (when this-indent-and-pt
            (goto-char (cdr this-indent-and-pt))
            (let ((indent-to-remove (car this-indent-and-pt)))
              (with-marker (limit (copy-marker (or (cdr goal-indent-and-pt)
                                                   (point-max))))
                (lsp-isar-output--delete-indentation-safe! indent-to-remove)
                (forward-line 1)
                (while (< (point) limit)
                  (lsp-isar-output--delete-indentation-safe! indent-to-remove)
                  (forward-line 1)))))

          (when goal-indent-and-pt
            (goto-char (cdr goal-indent-and-pt))
            (let ((indent-to-remove (car goal-indent-and-pt)))
              (with-marker (limit (copy-marker (or (cdr goal-indent-and-pt)
                                                   (point-max))))
                (lsp-isar-output--delete-indentation-safe! indent-to-remove)
                (forward-line 1)
                (while (< (point) limit)
                  (lsp-isar-output--delete-indentation-safe! indent-to-remove)
                  (forward-line 1)))))

          (when this-indent-and-pt
            (set-marker (cdr this-indent-and-pt) nil))
          (when goal-indent-and-pt
            (set-marker (cdr goal-indent-and-pt) nil)))))))

(defun lsp-isar-output-recalculate-sync (content lsp-decorations)
  (with-current-buffer (lsp-isar--get-output-buffer)
    (with-inhibited-read-only
     (erase-buffer)
     (insert content)
     (goto-char (point-min))
     (lsp-isar--apply-lsp-decorations! lsp-decorations)
     (lsp-isar-output--fix-indents!))))

(defun lsp-isar--apply-lsp-decorations! (lsp-decorations)
  (cl-assert (vectorp lsp-decorations))
  (dovector (entry lsp-decorations)
    (-let* (((&lsp-isar:DecorationEntry :type :content) entry)
            (face (gethash type lsp-isar-decorations-get-font))
            (curr-line 1))
      (cl-macrolet
          ((advance-to-line! (line)
             `(let ((line-delta (- ,line curr-line)))
                (unless (zerop line-delta)
                  (forward-line line-delta)
                  (setf curr-line ,line)))))
        (goto-char (point-min))
        (cl-assert (vectorp content))
        (dovector (r content)
          (-let (((&lsp-isar:DecorationRange :range) r))
            (cl-assert (vectorp range))
            ;; NB Emacs counts lines from 1.
            (let* ((start-line (1+ (aref range 0)))
                   (start-col (aref range 1))
                   (end-line (1+ (aref range 2)))
                   (end-col (aref range 3)))

              (advance-to-line! start-line)
              (move-to-column start-col)
              (let ((start-pos (point)))
                (advance-to-line! end-line)
                (move-to-column end-col)
                (let ((end-pos (point)))
                  ;; (message "Applying %s to ‘%s’" face (buffer-substring-no-properties start-pos end-pos))
                  (put-text-property start-pos end-pos 'font-lock-face face))))))))

    ;; (let ((point0 (car deco))
    ;;       (point1 (cadr deco))
    ;;       (face (caddr deco)))
    ;;   (put-text-property point0 point1 'font-lock-face face))
    ))

(defun lsp-isar--get-output-buffer ()
  (let ((buf (get-buffer "*lsp-isar-output*")))
    (if (and buf
             (buffer-live-p buf))
        buf
      (let ((fresh-buf (get-buffer-create "*lsp-isar-output*")))
        (with-current-buffer fresh-buf
          (isar-goal-mode)
          (read-only-mode 1))
        fresh-buf))))

(lsp-defun lsp-isar-output-update-state-and-output-buffer (_workspace (&lsp-isar:DynamicOutput :content :decorations))
  "Launch the thread or timer to update the state and the output
panel with CONTENT."
  (when lsp-isar-output-time-before-printing-goal
    (when lsp-isar-output-proof-timer
      (cancel-timer lsp-isar-output-proof-timer))
    (setq lsp-isar-output-proof-timer
	  (run-at-time lsp-isar-output-time-before-printing-goal nil
		       (lambda (content decorations)
			 (lsp-isar-output-recalculate-sync content decorations))
		       content
                       decorations))
    ;; Disable for font testing
    ;; (let ((out-buf (lsp-isar--get-output-buffer)))
    ;;   (unless (buffer-visible-p out-buf)
    ;;     (let ((curr-win (selected-window)))
    ;;       (unwind-protect
    ;;           (pop-to-buffer out-buf nil t)
    ;;         (select-window curr-win)))))
    ))

(defvar lsp-isar-output--last-message-margin nil)

(defun lsp-isar-output-set-size (size)
  "Resize line length of output buffer."
  (interactive)
  (when (or (null lsp-isar-output--last-message-margin)
            (not (= lsp-isar-output--last-message-margin size)))
    (let ((my-message (lsp-make-notification "PIDE/set_message_margin" (list :value size))))
      (setf lsp-isar-output--last-message-margin size)
      (lsp-send-notification my-message))))

(defun lsp-isar-output-adapt-length ()
  "Adapt the size of the buffer"
  (when-let* ((out-buf (lsp-isar--get-output-buffer)))
    (let ((cols (window-body-width (get-buffer-window out-buf))))
      (lsp-isar-output-set-size (- cols 5)))))

(defun lsp-isar-output-adapt-to-change (&optional _frame)
  (lsp-isar-output-adapt-length))

(add-hook 'window-configuration-change-hook #'lsp-isar-output-adapt-to-change)

(modify-coding-system-alist 'file "*lsp-isar-output*" 'utf-8)

(provide 'lsp-isar-output)

;;; lsp-isar-output.el ends here
