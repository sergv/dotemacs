;;; hl-paren.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  5 November 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'common)
;; (require 'timer)

(defconst hl-paren-parentheses (string-to-list "(){}[]"))
(defconst hl-paren-open-parentheses (string-to-list "({["))

(defface hl-paren-selection-face '((t (:underline "#d33682")))
  "Face to highlight parentheses.")

(defstruct hl-paren-state
  first  ;; Current paren overlay
  second ;; Matching paren overaly
  is-enabled?)

(defvar-local hl-paren-state nil
  "Highlighting state for the current buffer. Either an instance of ‘hl-paren-state’ type, in
which case overlays are properly set up or nil meaning no highlighting
is currently done. The state will be created when there’s highlighting to show.")

;; (defvar-local hl-paren-timer nil
;;   "Latest highlighting timer.")

(defsubst hl-paren-move-overlay-to (overlay pos)
  (move-overlay overlay pos (1+ pos)))

(defun hl-paren-make-overlay (pos tag)
  (cl-assert (symbolp tag))
  (cl-assert (memq tag '(first second)))
  (let* ((ov (make-overlay pos (1+ pos))))
    (hl-paren-set-overlay-highlighting ov t)
    (overlay-put ov 'hl-paren-overlay tag)
    ov))

(defsubst hl-paren-set-overlay-highlighting (ov is-enabled?)
  (overlay-put ov 'face (and is-enabled? 'hl-paren-selection-face)))

(defun hl-paren-enable-overlays! ()
  (when (and hl-paren-state
             (not (hl-paren-state-is-enabled? hl-paren-state)))
    (setf (hl-paren-state-is-enabled? hl-paren-state) t)
    (hl-paren-set-overlay-highlighting (hl-paren-state-first hl-paren-state) t)
    (hl-paren-set-overlay-highlighting (hl-paren-state-second hl-paren-state) t)))

(defun hl-paren-disable-overlays! ()
  (when (and hl-paren-state
             (hl-paren-state-is-enabled? hl-paren-state))
    (setf (hl-paren-state-is-enabled? hl-paren-state) nil)
    (hl-paren-set-overlay-highlighting (hl-paren-state-first hl-paren-state) nil)
    (hl-paren-set-overlay-highlighting (hl-paren-state-second hl-paren-state) nil)))

(defsubst hl-paren-cleanup-overlays! ()
  "Remove any currently active overlays."
  (when hl-paren-state
    (cl-assert (overlayp (hl-paren-state-first hl-paren-state)))
    (cl-assert (overlayp (hl-paren-state-second hl-paren-state)))
    (delete-overlay (hl-paren-state-first hl-paren-state))
    (delete-overlay (hl-paren-state-second hl-paren-state))
    (setq-local hl-paren-state nil)))

(defun hl-paren-highlight-matching-paren-at-point ()
  "Highlight paren that is matching for symbol at point.
Turn off highlighting if character at point is not parentheses."
  (if (memq (char-after) hl-paren-parentheses)
      (if-let ((matching-pos (pseudovim-motion-jump-item-to-pos (point) 10000)))
          (if hl-paren-state
              (progn
                ;; move overlays if they already exist
                (hl-paren-move-overlay-to (hl-paren-state-first hl-paren-state) (point))
                (hl-paren-move-overlay-to (hl-paren-state-second hl-paren-state) matching-pos)
                (hl-paren-enable-overlays!))
            (progn
              ;; re-create overlays
              (setq-local hl-paren-state
                          (make-hl-paren-state
                           :first (hl-paren-make-overlay (point) 'first)
                           :second (hl-paren-make-overlay matching-pos 'second)
                           :is-enabled? t))))
        (hl-paren-disable-overlays!))
    (hl-paren-disable-overlays!)))

(defun hl-paren-do-highlight ()
  "Refresh highlighting if last command was a move one."
  (interactive)
  (cond
    ((and (symbolp this-command)
          (memq this-command
                '(autopair-newline
                  paredit-newline
                  newline
                  newline-and-indent)))
     (hl-paren-disable-overlays!))
    ((and hl-paren-state
          (hl-paren-state-is-enabled? hl-paren-state)
          (or (eq (point) (overlay-start (hl-paren-state-first hl-paren-state)))
              (eq (point) (overlay-start (hl-paren-state-second hl-paren-state)))))
     ;; Nothing to do: already highlighting relevant parens.
     nil)
    (t
     (hl-paren-highlight-matching-paren-at-point))))

(define-minor-mode hl-paren-mode
  "Highlight matching parens"
  :init-value nil
  :keymap nil
  (if hl-paren-mode
      (add-hook 'post-command-hook #'hl-paren-schedule nil t)
    (progn
      (hl-paren-cleanup-overlays!)
      (remove-hook 'post-command-hook #'hl-paren-schedule t)))
  ;; (when hl-paren-timer
  ;;   (cancel-timer hl-paren-timer)
  ;;   (setq-local hl-paren-timer nil))
  )

(defun setup-hl-paren ()
  (hl-paren-mode +1))

;; (defun hl-paren--highlight-in-timer ()
;;   (condition-case nil
;;       (hl-paren-do-highlight)
;;     ;; Do not let errors interrupt normal workflow.
;;     (error (hl-paren-cleanup-overlays!)))
;;   (setq-local hl-paren-timer nil))

(defun hl-paren-schedule ()
  (condition-case nil
      (hl-paren-do-highlight)
    ;; Do not let errors interrupt normal workflow.
    (error (hl-paren-cleanup-overlays!)))
  ;; Timers introduce severe input lag, maybe there’s a way to make the lag go awy.
  ;; (if hl-paren-timer
  ;;     (timer-activate-when-idle hl-paren-timer
  ;;                               nil
  ;;                               (cancel-timer-internal hl-paren-timer))
  ;;   (setq-local hl-paren-timer
  ;;               (run-with-idle-timer 0.1 1 #'hl-paren--highlight-in-timer)))
  )

;;;###autoload
(defun hl-paren--fix-state-after-clone ()
  "Fixup ‘hl-paren--match-overlays’ in indirect buffer by detaching from the original buffer."
  (when hl-paren-mode
    (let ((first nil)
          (second nil)
          (result nil))
      (with-all-matching-overlays
          ov
          ov-tag
          (overlay-get ov 'hl-paren-overlay)
        (cl-assert (symbolp ov-tag))
        (cl-assert (memq ov-tag '(first second)))
        (pcase ov-tag
          (`first  (setf first ov))
          (`second (setf second ov)))
        (overlay-put ov 'is-fixed-after-clone? t))
      ;; Overlays are already copied, need to only propagate them to correct variables
      (if (and first second)
          (progn
            (cl-assert (overlayp first))
            (cl-assert (overlayp second))
            (setf (hl-paren-state-first hl-paren-state) first
                  (hl-paren-state-second hl-paren-state) second))
        (hl-paren-cleanup-overlays!)))))

;;;###autoload
(add-hook 'clone-indirect-buffer-hook #'hl-paren--fix-state-after-clone)

(provide 'hl-paren)

;; Local Variables:
;; End:

;;; hl-paren.el ends here
