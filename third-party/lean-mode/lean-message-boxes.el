;;  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016 David Christiansen.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: David Christiansen
;;
;;; Code:

(require 'cl-lib)
(require 's)
(require 'lean-server)

(defface lean-message-boxes-content-face
  '((t :inherit font-lock-doc-face))
  "Face for Lean message box contents."
  :group 'lean)

(defcustom lean-message-boxes-enabled-captions '("check result" "eval result" "print result" "reduce result")
  "Which captions should result in boxes?"
  :group 'lean
  :type '(repeat (choice (const "check result")
                         (const "eval result")
                         (const "print result")
                         (const "reduce result")
                         (const "trace output"))))

(defcustom lean-message-boxes-enabledp nil
  "Whether or not to display message boxes."
  :group 'lean
  :type 'boolean)
(make-variable-buffer-local 'lean-message-boxes-enabledp)

(defun lean-message-boxes--ask-for-messages ()
  "Get the current messages out of the Lean server session."
  (let ((buf (current-buffer)))
    (if lean-server-session
        (cl-remove-if-not (lambda (msg)
                            (equal (buffer-file-name buf)
                                   (plist-get msg :file_name)))
                          (lean-server-session-messages lean-server-session))
      '())))

(defun lean-message-boxes--set-enabledp (enabledp)
  "Enable the boxes if ENABLEDP is non-nil."
  (setq lean-message-boxes-enabledp enabledp)
  (lean-message-boxes-display (lean-message-boxes--ask-for-messages)))

(defun lean-message-boxes-toggle ()
  "Toggle the display of message boxes."
  (interactive)
  (lean-message-boxes--set-enabledp (not lean-message-boxes-enabledp)))

(defun lean-message-boxes-enable ()
  "Enable the display of message boxes."
  (interactive)
  (lean-message-boxes--set-enabledp t))

(defun lean-message-boxes-disable ()
  "Disable the display of message boxes."
  (interactive)
  (lean-message-boxes--set-enabledp nil))

(defvar lean-message-boxes--overlays '()
  "The overlays in the current buffer from Lean messages.")
(make-variable-buffer-local 'lean-message-boxes--overlays)

(defun lean-message-boxes--kill-overlays ()
  "Delete all Lean message overlays in the current buffer."
  (dolist (o lean-message-boxes--overlays)
    (delete-overlay o))
  (setq lean-message-boxes--overlays '()))

(defun lean-message-boxes--pad-to (str width)
  "Pad the string STR to a particular WIDTH."
  (concat str (make-string (max 0 (- width (length str))) ?\ )))

(defun lean-message-boxes-display (msgs)
  "Show the messages MSGS in the Lean buffer as boxes when `lean-message-boxes-enabledp' is non-nil."
  (lean-message-boxes--kill-overlays)
  (when lean-message-boxes-enabledp
    (dolist (msg msgs)
      (let ((end-line (plist-get msg :end_pos_line))
            (end-col (plist-get msg :end_pos_col))
            (caption (plist-get msg :caption))
            (text (plist-get msg :text)))
        (when (member caption lean-message-boxes-enabled-captions)
          (let ((overlay (lean-message-boxes--make-overlay
                          end-line end-col
                          caption text)))
            (push overlay lean-message-boxes--overlays)))))))

(defun lean-message-boxes--as-string (caption str)
  "Construct a propertized string representing CAPTION and STR."
  (let* ((str-copy (s-trim str)))
    (put-text-property 0 (length str-copy)
                       'face 'lean-message-boxes-content-face
                       str-copy)
    (let* ((lines (s-lines str-copy))
           (w (apply #'max (mapcar #'length (cons caption lines)))))
      (apply #'concat
             (mapcar
              (lambda (l)
                (concat "│ "
                        (lean-message-boxes--pad-to l w)
                        "\n"))
              lines)))))

(defun lean-message-boxes--make-overlay (end-line end-col caption text)
  "Construct a message box overlay at LINE and COL with CAPTION and TEXT."
  (let* ((end-pos (save-excursion (goto-char (point-min))
                                  (forward-line (1- end-line))
                                  (forward-char (1- end-col))
                                  (while (looking-at-p "[\t\r\n ]")
                                    (forward-char -1))
                                  (end-of-line)
                                  (1+ (point))))
         (overlay (make-overlay end-pos (1+ end-pos)))
         (as-box (lean-message-boxes--as-string caption text)))
    (put-text-property 0 1 'cursor t as-box)
    (overlay-put overlay 'before-string as-box)
    (overlay-put overlay 'help-echo caption)
    (overlay-put overlay 'lean-is-output-overlay t)
    (overlay-put overlay 'evaporate t)
    overlay))

(add-hook 'lean-server-show-message-hook 'lean-message-boxes-display)
(provide 'lean-message-boxes)
