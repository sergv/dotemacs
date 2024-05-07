;; bkr.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  7 May 2024
;; Description:

;; Browse kill ring without much ado.

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common)
(require 'select-mode)

(defvar-local bkr--current-state nil)

(defstruct bkr-state
  (variable    nil :read-only t)    ;; symbol
  (orig-buffer nil :read-only t) ;; buffer
  (orig-win    nil :read-only t) ;; window

  ;; replace-yanked-text? ;; bool
  (preview-overlay nil :read-only) ;; overlay
  (original-buf-pos nil :read-only)
  )

(defvar bkr-mode-map
  (let ((keymap (make-sparse-keymap)))
    (def-keys-for-map keymap
      +vi-essential-keys+
      +vim-search-keys+
      +vim-word-motion-keys+
      +vim-special-keys+

      (("h" "<down>") select-mode-select-next-item)
      (("t" "<up>")   select-mode-select-previous-item)

      ("<escape>"     bkr-hide)

      ("SPC"          bkr-insert-move-and-quit)
      ("<return>"     bkr-insert-and-quit)
      (","            bkr-delete-selected-item))
    keymap))

(define-derived-mode bkr-mode select-mode "BKR"
  "Browse kill ring using ‘select-mode’.")

(defun bkr-start-for-variable (var buf-name)
  (cl-assert (symbolp var))
  (when (eq major-mode 'bkr-mode)
    (error "Already viewing the kill ring"))
  (let* ((orig-buf (current-buffer))
         (orig-win (selected-window))
         (items (remove-duplicates-hashing (bkr--to-list (buffer-local-value var orig-buf))
                                           #'equal))
         (orig-pos (point))
         (preview-ov (make-overlay orig-pos orig-pos orig-buf)))
    (select-mode-start-selection items
                                 :mode #'bkr-mode
                                 :buffer-name buf-name
                                 :preamble (concat "Contents of ‘" (symbol->string var) "’\n\n")
                                 :before-render-state (lambda ()
                                                        (setq-local bkr--current-state
                                                                    (make-bkr-state
                                                                     :variable var
                                                                     :orig-buffer orig-buf
                                                                     :orig-win orig-win
                                                                     ;; :replace-yanked-text? nil
                                                                     :preview-overlay preview-ov
                                                                     :original-buf-pos orig-pos)))
                                 :item-show-function
                                 (lambda (x)
                                   (cl-assert (stringp x))
                                   (let ((len (length x)))
                                     (cond
                                       ((= len 0)
                                        "\n")
                                       ((= ?\n
                                           (aref x (- len 1)))
                                        x)
                                       (t
                                        (concat x "\n")))))
                                 ;; :on-selection #'bkr--do-select
                                 :on-selection-moved #'bkr--preview-update)))

(defun bkr--get-selected-item ()
  (aref (select-mode--state-items select-mode--current-state)
        (select-mode--state-selected-item select-mode--current-state)))

(defun bkr-delete-selected-item ()
  "Remove the item at point from the kill-ring being shown."
  (interactive)
  (let* ((idx (select-mode--state-selected-item select-mode--current-state))
         (item (aref (select-mode--state-items select-mode--current-state) idx)))
    (bkr--delete-ring-value item)
    (select-mode-remove-item! idx)))

(defun bkr--get-ring-value ()
  (cl-assert (bkr-state-p bkr--current-state))
  (buffer-local-value (bkr-state-variable bkr--current-state)
                      (bkr-state-orig-buffer bkr--current-state)))

(defun bkr--set-ring-value (value)
  (cl-assert (bkr-state-p bkr--current-state))
  (let ((state bkr--current-state))
    (with-current-buffer (bkr-state-orig-buffer state)
      (set (bkr-state-variable state) value))))

(defun bkr--delete-ring-value (item)
  "Delete ITEM from kill ring."
  (let ((val (bkr--get-ring-value)))
    (cond
      ((ring-p val)
       (ring-remove val
                    (ring-member val
                                 item))
       (bkr--set-ring-value val))
      ((listp val)
       (bkr--set-ring-value (delete item val)))
      (t
       (error "Unrecognized kill ring type %s" val)))))

(defun bkr--insert-ring-value (item)
  "Add ITEM to ring."
  (let ((val (bkr--get-ring-value)))
    (cond
      ((ring-p val)
       (ring-insert val item)
       (bkr--set-ring-value val))
      ((listp val)
       (bkr--set-ring-value (cons item val)))
      (t
       (error "Unrecognized kill ring type: %s" val)))))

(defun bkr--to-list (val)
  "Convert contents of kill ring variable to list."
  (cond
    ((ring-p val)
     (ring-elements val))
    ((listp val)
     val)
    (t
     (error "Don't know how to convert to list: %s" val))))

(defun bkr--insert-into-orig-buf! (str)
  (cl-assert (stringp str))
  (let ((state bkr--current-state))
    (with-selected-window (bkr-state-orig-win state)
      (with-current-buffer (bkr-state-orig-buffer state)
        (goto-char (bkr-state-original-buf-pos state))
        (insert str)))))

(defun bkr-insert-move-and-quit ()
  (interactive)
  (let ((item (bkr--get-selected-item))
        (state bkr--current-state))
    (bkr--delete-ring-value item)
    (if (eq (bkr-state-variable state) 'kill-ring)
        (kill-new item)
      (bkr--insert-ring-value item))
    (bkr--insert-into-orig-buf! item)
    (bkr--finish)))

(defun bkr-insert-and-quit ()
  (interactive)
  (bkr--insert-into-orig-buf! (bkr--get-selected-item))
  (bkr--finish))

(defun bkr--finish ()
  (let ((state bkr--current-state))
    (select-mode-exit)
    (awhen (bkr-state-preview-overlay state)
      (delete-overlay it))))

(defun bkr--preview-update (idx)
  "Show what item would be inserted in the original buffer."
  (cl-assert (bkr-state-p bkr--current-state))
  (let* ((selected (aref
                    (select-mode--state-items select-mode--current-state)
                    idx))
         (selected-highlighted
          (awhen selected
            (propertize it 'face 'highlight))))
    (overlay-put (bkr-state-preview-overlay bkr--current-state)
                 'before-string
                 selected-highlighted)))

(defun bkr-hide ()
  "Stop showing browse-kill-ring buffer but don’t kill it."
  (interactive)
  (overlay-put (bkr-state-preview-overlay bkr--current-state)
               'before-string
               nil)
  (select-mode-hide))

(provide 'bkr)

;; Local Variables:
;; End:

;; bkr.el ends here
