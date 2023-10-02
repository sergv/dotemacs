;; vim-insert-mode.el - VIM visual mode. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;; TODO:

;;   - when calling a non-vim-mode-command the region should be modified
;;     s.t. the emacs command uses the correct region for block-mode
;;   - check interaction with region (deactivate-mark-hook and others)

;;; Init:

(eval-when-compile
  (require 'cl)
  (require 'macro-util)
  (require 'search)

  (defvar vim--last-undo))

(require 'common)
(require 'current-column-fixed)
(require 'search)

(require 'vim-macs)
(require 'vim-modes)
(require 'vim-normal-mode)
(require 'vim-insert-mode)
(require 'vim-ex)
(require 'vim-commands)
(require 'vim-undo)

;;; Code:

(defgroup vim-visual-mode nil
  "Visual mode"
  :group 'vim-mode)

(defface vim-visual-region-face `((t (:inherit region)))
  "Face of the highlighted region."
  :group 'vim-visual-mode)

(vim-define-keymap visual-mode "visual mode" :map-command vmap)

(vim-define-mode visual "VIM visual mode\n\nVisual mode keymap:\n\\{vim-visual-mode-keymap}\n\nOperator pending mode keymap:\n\\{vim-operator-pending-mode-keymap}\n\nMotion mode keymap:\n\\{vim-motion-mode-keymap}"
  :ident "V"
  :keymaps '(vim-visual-mode-keymap
             vim-motion-mode-keymap
             vim-operator-pending-mode-keymap)
  :command-function #'vim--visual-mode-command
  :cursor 'hollow)

(add-hook 'vim-visual-mode-on-hook #'vim-visual-mode--activate)
(add-hook 'vim-visual-mode-off-hook #'vim-visual-mode--deactivate)

(defvar-local vim-visual--mode-type 'normal
  "Type of visual mode, should be 'normal, 'linewise or 'block.")

;; Last transient-mark-mode.
(defvar-local vim-visual--old-transient-mark-mode nil)

;; Old global variables changed in visual-mode.
(defvar-local vim-visual--old-global-variables nil)

;; List of overlays for the current region.
(defvar-local vim-visual--overlays nil)

;; The last beginning position of the region.
(defvar-local vim-visual--last-begin-pos nil)

;; The last end position of the region.
(defvar-local vim-visual--last-end-pos nil)

(defvar-local vim-visual--key-sequence-for-repeat nil
  "Pointer to a prefix of a sequence of key presses that will be mutably
popuated (in this case mostly by insert mode).")

;; Info-struct to save information for visual-insertion.
(cl-defstruct (vim-visual-insert-info
               (:constructor vim-make-visual-insert-info))
  begin ;; position, fixnum
  end   ;; position, marker
  line-count
  column)

(defun vim--free-visual-insert-info! ()
  (cl-assert vim-visual--last-insert-info)
  (set-marker (vim-visual-insert-info-end vim-visual--last-insert-info) nil)
  (setf (vim-visual-insert-info-begin      vim-visual--last-insert-info) nil
        (vim-visual-insert-info-end        vim-visual--last-insert-info) nil
        (vim-visual-insert-info-line-count vim-visual--last-insert-info) nil
        (vim-visual-insert-info-column     vim-visual--last-insert-info) nil))

(defun vim--init--vim-visual-insert-info-end! (first last column)
  (let* ((begin (min first last))
         (end (max first last))
         (line-count (1- (count-lines-fixed begin end))))
    (if vim-visual--last-insert-info
        (setf (vim-visual-insert-info-begin      vim-visual--last-insert-info) begin
              (vim-visual-insert-info-end        vim-visual--last-insert-info) (copy-marker end)
              (vim-visual-insert-info-line-count vim-visual--last-insert-info) line-count
              (vim-visual-insert-info-column     vim-visual--last-insert-info) column)
      (setf vim-visual--last-insert-info
            (vim-make-visual-insert-info :begin begin
                                         :end (copy-marker end)
                                         :line-count line-count
                                         :column column)))))

;; The last motion used to insert something in visual mode.
(defvar-local vim-visual--last-insert-info nil)

;; The undo-mark of the last visual mode insert command.
(defvar-local vim-visual--last-insert-undo nil)

;; If non-nil, the last region will be used when visual mode is activated.
(defvar-local vim-visual--reactivate-last-region nil)

;;; System variables which must temporarily be buffer local.
(defconst vim-visual--temporary-local-variables
  '(transient-mark-mode
    deactivate-mark-hook))

;;; Commands that deactivate the mark (and so visual-mode).
(defconst vim-visual--deactivate-mark-commands
  '(clear-rectangle
    clipboard-kill-ring-save
    copy-rectangle
    copy-rectangle-to-register
    kill-rectangle
    open-rectangle
    string-rectangle
    yank-rectangle
    keyboard-escape-quit
    keyboard-quit))

(defun vim-activate-visual (type)
  "Activates visual-mode with certain type."
  (if (vim-visual-mode-p)
      (unless (eq vim-visual--mode-type type)
        (setq vim-visual--mode-type type)
        (vim-visual--highlight-region)
        (vim-notify
         (pcase vim-visual--mode-type
           (`normal   "-- VISUAL --")
           (`linewise "-- VISUAL LINE --")
           (`block    "-- VISUAL BLOCK --")
           (_         (error "Unknown visual mode type: %s" vim-visual--mode-type)))))
    (progn
      (setq vim-visual--mode-type type)
      (vim-activate-visual-mode))))

(defun vim-visual--toggle-mode (type)
  "Switches to visual mode of certain type or deactivates the mode."
  (if (and (vim-visual-mode-p)
           (eq vim-visual--mode-type type)
           (vim--toplevel-execution?))
      (vim:visual-mode-exit:wrapper)
    (vim-activate-visual type)))

(vim-defcmd vim:visual-toggle-normal (nonrepeatable keep-visual)
  "Switches to normal visual-mode or deactivates it."
  (vim-visual--toggle-mode 'normal))

(vim-defcmd vim:visual-toggle-linewise (nonrepeatable keep-visual)
  "Switches to linewise visual-mode or deactivates it."
  (vim-visual--toggle-mode 'linewise))

(vim-defcmd vim:visual-toggle-block (nonrepeatable keep-visual)
  "Switches to block visual-mode or deactivates it."
  (vim-visual--toggle-mode 'block))

(defun vim-visual--normal-mode-p ()
  "Returns `t' iff charwise visual mode is activated, nil otherwise."
  (and vim-visual-mode
       (eq vim-visual--mode-type 'normal)))

(defun vim-visual--linewise-mode-p ()
  "Returns `t' iff linewise visual mode is activated, nil otherwise."
  (and vim-visual-mode
       (eq vim-visual--mode-type 'linewise)))

(defun vim-visual--block-mode-p ()
  "Returns `t' iff block visual mode is activated, nil otherwise."
  (and vim-visual-mode
       (eq vim-visual--mode-type 'block)))

(vim-defcmd vim:visual-mode-exit (nonrepeatable)
  "Deactivates visual mode, returning to normal-mode."
  (vim:visual-mode-exit--impl))

(defsubst vim:visual-mode-exit--impl ()
  (vim-activate-normal-mode))

(vim-defcmd vim:visual-mode-reactivate (nonrepeatable)
  "Called when the last visual region should be reactivated."
  (unless (and vim-visual--mode-type
               vim-visual--last-begin-pos
               vim-visual--last-end-pos)
    (error "No former visual selection"))
  (setq vim-visual--reactivate-last-region t)
  (vim-activate-visual vim-visual--mode-type))

(defun vim-visual-mode--activate ()
  "Called when visual mode is activated."
  (setq cursor-type vim-visual-mode-cursor)
  (if vim-visual--reactivate-last-region
      (progn
        (set-mark vim-visual--last-begin-pos)
        (goto-char vim-visual--last-end-pos)
        (setq vim-visual--reactivate-last-region nil))
    (set-mark (point)))

  (vim-notify
   (pcase vim-visual--mode-type
     (`normal   "-- VISUAL --")
     (`linewise "-- VISUAL LINE --")
     (`block    "-- VISUAL BLOCK --")
     (_         (error "Unknown visual mode type: %s" vim-visual--mode-type))))

  (setq vim-visual--overlays nil
        vim-visual--old-transient-mark-mode transient-mark-mode
        vim-visual--old-global-variables
        ;; Remember which system variables weren't buffer local
        (-remove #'local-variable-p vim-visual--temporary-local-variables))

  ;; The make them all buffer local, too.
  (mapc #'make-local-variable vim-visual--temporary-local-variables)
  (setq transient-mark-mode nil)
  (add-hook 'post-command-hook #'vim-visual--post-command)
  (add-hook 'pre-command-hook #'vim-visual--normalize-region)
  (add-hook 'post-command-hook #'vim-visual--denormalize-region)
  (add-hook 'deactivate-mark-hook #'vim:visual-mode-exit--hook))

(defun vim-visual-mode--deactivate ()
  "Called when visual mode is deactivated."
  ;; hide the selection
  (vim-visual--hide-region!)
  ;; cleanup local variables
  (remove-hook 'pre-command-hook #'vim-visual--normalize-region)
  (remove-hook 'post-command-hook #'vim-visual--denormalize-region)
  (remove-hook 'post-command-hook #'vim-visual--post-command post-command-hook)
  (remove-hook 'deactivate-mark-hook #'vim:visual-mode-exit--hook)
  (setq transient-mark-mode vim-visual--old-transient-mark-mode)
  (vim-visual--delete-overlays! vim-visual--overlays)
  (mapc #'kill-local-variable vim-visual--old-global-variables)
  (deactivate-mark))

(defun vim:visual-mode-exit--hook ()
  "Call ‘vim:visual-mode-exit’ from hooks."
  (vim:visual-mode-exit:wrapper))

(defun vim--visual-mode-command (command)
  "Executes a command in visual mode."
  (pcase (vim--cmd-type command)
    (`simple  (vim-visual--execute-command command))
    (`complex (vim-visual--execute-command command))
    (`special (error "no special so far"))
    (_        (vim-visual--execute-motion command))))

(defun vim-visual--execute-command (command)
  "Called to execute a command in visual mode."
  ;; save the last region
  (let ((m (mark t))
        (p (point)))
    (setf vim-visual--last-begin-pos m
          vim-visual--last-end-pos p)
    (vim-set-mark ?< m)
    (vim-set-mark ?> p))

  (when (vim--cmd-char-arg-p command)
    (setq vim--current-cmd-arg (read-char-exclusive)))

  (if (vim--cmd-motion-p command)
      (progn
        (vim--prepare-buffer-undo-list!)
        (let ((vim--last-undo buffer-undo-list)
              (repeatable? (vim--cmd-repeatable-p command)))
          (vim--funcall-save-buffer command
                                    (vim-visual--current-motion) ;; motion
                                    nil                          ;; count
                                    vim--current-cmd-arg         ;; argument
                                    nil                          ;; force
                                    vim--current-register        ;; register
                                    )

          (when repeatable?
            (vim--overwrite-repeat-events! vim--current-key-sequence))
          (vim--command-finalize! vim--last-undo t)
          (vim--adjust-point)))
    (vim--normal-execute-simple-command command))
  ;; deactivate visual mode unless the command should keep it
  (when (and vim-visual-mode
             (not (vim--cmd-keep-visual-p command)))
    (vim:visual-mode-exit:wrapper)))

(defun vim-visual--execute-motion (command)
  "Called to execute a motion in visual mode."
  (setq vim--current-motion command)
  (when current-prefix-arg
    (setq vim--current-motion-count (prefix-numeric-value current-prefix-arg)))
  (when (vim--cmd-arg-p command)
    (setq vim--current-motion-arg (read-char-exclusive)))
  (condition-case _
      (vim--visual-adjust-region (vim-execute-current-motion))
    (error (beep)))
  (vim--adjust-point)
  (vim--forget-command-keys!)
  (vim--reset-key-state!))

(defun vim-visual--post-command ()
  (when (vim-visual-mode-p)
    (if (or deactivate-mark
            (memq this-command vim-visual--deactivate-mark-commands))
        (condition-case nil
            (vim:visual-mode-exit:wrapper)
          (error nil))
      (condition-case info
          (vim-visual--highlight-region)
        (error
         (ding)
         (message "visual-mode trouble: %s" info)
         (condition-case nil
             (vim:visual-mode-exit:wrapper)
           (error nil)))))))

(defun vim-visual--highlight-region ()
  "Highlights the selected region depending on `point' and `mark'.
This function is also responsible for setting the X-selection."
  (let* ((p (point))
         (m (mark t))
         (start (min p m))
         (end (max p m)))
    (pcase vim-visual--mode-type
      (`normal
       ;; Must highlight region ourselves because emacs will not extend region
       ;; to include last character.
       (vim-visual--highlight-normal start end))
      (`linewise
       ;; Linewise must be shown by us because Emacs will only highlight
       ;; region between mark and cursor.
       (vim-visual--highlight-linewise start end))
      (`block
       (vim-visual--highlight-block start end))
      (_
       (error "Unknown visual mode %s" vim-visual--mode-type))))
  (when (eq window-system 'x)
    (let ((len (length vim-visual--overlays)))
      (cond
        ((= 1 len)
         (gui-set-selection nil (car vim-visual--overlays)))
        ((< 1 len)
         (let ((text (mapconcat (lambda (x)
                                  (buffer-substring-no-properties (overlay-start x)
                                                                  (overlay-end x)))
                                vim-visual--overlays
                                "\n")))
           (gui-set-selection nil text)))))))

(defun vim-visual--highlight-normal (start end)
  "Adjusts the normal region between `start' and `end'."
  (setq vim-visual--overlays
        (list (vim-visual--create-or-update-overlay (car vim-visual--overlays)
                                                    start
                                                    (1+ end)))))

(defun vim-visual--highlight-linewise (start end)
  "Adjusts the linewise region between `start' and `end'."
  (let ((start-line (save-excursion
                      (goto-char start)
                      (line-beginning-position)))
        (end-line (save-excursion
                    (goto-char end)
                    (line-end-position))))
    (setq vim-visual--overlays
          (list (vim-visual--create-or-update-overlay (car vim-visual--overlays)
                                                      start-line
                                                      (1+ end-line))))))

(defun vim-visual--highlight-block (start end)
  "Adjusts the block region between `start' and `end'."
  ;; Adapted from: rm-highlight-rectangle
  ;; This function is used to highlight the rectangular region from
  ;; START to END.  We do this by putting an overlay on each line
  ;; within the rectangle.  Each overlay extends across all the
  ;; columns of the rectangle.  We try to reuse overlays where
  ;; possible because this is more efficient and results in less
  ;; flicker.
  (save-excursion
    ;; Calculate the rectangular region represented by point and mark,
    ;; putting start in the north-west corner and end in the
    ;; south-east corner.
    (let ((start-col (save-excursion
                       (goto-char start)
                       (current-column-fixed-uncached)))
          (end-col (save-excursion
                     (goto-char end)
                     (current-column-fixed-uncached))))
      (when (> start-col end-col)
        (cl-rotatef start-col end-col)
        (setq start (save-excursion
                      (goto-char start)
                      (move-to-column start-col nil)
                      (point))
              end (save-excursion
                    (goto-char end)
                    (move-to-column end-col nil)
                    (point))))
      ;; Force a redisplay so we can do reliable window start/end
      ;; calculations.
      (redisplay)
      (let* ((old vim-visual--overlays)
             (new nil)
             overlay
             (window-start (max (window-start) start))
             (window-end (min (window-end) end))
             (nlines (vim-count-lines-with-correction
                      window-start
                      (min (1+ window-end)
                           (point-max)))))
        ;; Iterate over those lines of the rectangle which are visible
        ;; in the currently selected window.
        (goto-char window-start)
        (dotimes (_ nlines)
          (let ((row-start (progn
                             (move-to-column start-col nil)
                             (point)))
                (row-end (progn
                           (move-to-column end-col nil)
                           (min (1+ (point))
                                (line-end-position)))))
            ;; Trim old leading overlays.
            (while (and old
                        (setq overlay (car old))
                        (< (overlay-start overlay) row-start)
                        (/= (overlay-end overlay) row-end))
              (delete-overlay overlay)
              (setq old (cdr old)))
            ;; Reuse an overlay if possible, otherwise create one.
            (if (and old
                     (setq overlay (car old))
                     (or (= (overlay-start overlay) row-start)
                         (= (overlay-end overlay) row-end)))
                (progn
                  (move-overlay overlay row-start row-end)
                  (setq new (cons overlay new)
                        old (cdr old)))
              (progn
                (setq overlay (make-overlay row-start row-end))
                (overlay-put overlay 'face 'vim-visual-region-face)
                (overlay-put overlay 'priority 99)
                (setq new (cons overlay new)))))
          (forward-line 1))
        ;; Trim old trailing overlays.
        (vim-visual--delete-overlays! old)
        (setq vim-visual--overlays (nreverse new))))))

(defun vim-visual--create-or-update-overlay (ov start end)
  "Creates a new overlay or updates the given overlay."
  (if (overlayp ov)
      (progn
        (vim-visual--delete-overlays! (cdr vim-visual--overlays))
        (setcdr vim-visual--overlays nil)
        (move-overlay ov start end)
        ov)
    (let ((ov (make-overlay start end)))
      (vim-visual--hide-region!)
      (overlay-put ov 'face 'vim-visual-region-face)
      (overlay-put ov 'priority 99)
      ov)))

(defun vim-visual--hide-region! ()
  "Removes the highlighting."
  (vim-visual--delete-overlays! vim-visual--overlays)
  (setq vim-visual--overlays nil))

(defun vim-visual--delete-overlays! (overlays)
  "Deletes all overlays in `overlays'."
  (mapc #'delete-overlay overlays))

(defun vim-visual--current-motion ()
  "Returns a motion representing the current region."
  (pcase vim-visual--mode-type
    (`normal   (vim-visual--current-normal-motion))
    (`linewise (vim-visual--current-linewise-motion))
    (`block    (vim-visual--current-block-motion))))

(defun vim-visual--current-normal-motion ()
  "Returns a motion representing the current normal region."
  (let ((p (point))
        (m (mark t)))
    (vim-make-motion :has-begin t
                     :begin     (min p m)
                     :end       (max p m)
                     :type      'inclusive)))

(defun vim-visual--current-linewise-motion ()
  "Returns a motion representing the current linewise region."
  (let ((p (point))
        (m (mark t)))
    (vim-make-motion :has-begin t
                     :begin     (min p m)
                     :end       (max p m)
                     :type      'linewise)))

(defun vim-visual--current-block-motion ()
  "Returns a motion representing the current block region."
  (vim-make-motion :has-begin t
                   :begin     (region-beginning)
                   :end       (region-end)
                   :type      'block))

(defun vim--visual-adjust-region (motion)
  "Adjusts the region according to a certain motion."
  (when (vim-motion-has-begin motion)
    (vim-activate-visual (pcase (vim-motion-type motion)
                           (`linewise 'linewise)
                           (`block    'block)
                           (_         'normal)))
    (let* ((b (vim-motion-begin motion))
           (e (vim-motion-end motion))
           (beg (min b e))
           (end (max b e)))
      (if (>= (point) end)
          (progn
            (set-mark beg)
            (goto-char end))
        (progn
          (set-mark end)
          (goto-char beg))))))

(vim-defcmd vim:visual-insert (motion)
  "Starts insertion at the left column of a visual region."
  (vim--init--vim-visual-insert-info-end! (vim-motion-begin motion)
                                          (vim-motion-end motion)
                                          (vim-motion-first-col motion))
  (vim-visual--start-insert))

(defun vim-visual--start-insert ()
  "Starts a new multi-line insert operation with `vim-visual--last-insert-info'."
  (goto-char (vim-visual-insert-info-begin vim-visual--last-insert-info))
  (move-to-column (vim-visual-insert-info-column vim-visual--last-insert-info) t)
  (vim-visual--record-undo-pos! (point))
  (let ((undo-inhibit-record-point t))
    (pcase vim-visual--mode-type
      (`block
       ;; TODO: ensure the right command is run on repetition.
       ;; this is really a dirty hack
       (setf vim-visual--key-sequence-for-repeat (cons [?i] (vim--obtain-intermediate-handle!)))
       (vim:cmd-insert:wrapper :count 1)
       (setq-local vim-insert-mode-on-exit #'vim--insert-block-copies))
      (`linewise
       ;; TODO: ensure the right command is run on repetition.
       ;; this is really a dirty hack
       (setf vim-visual--key-sequence-for-repeat (cons [?I] (vim--obtain-intermediate-handle!)))
       (vim:cmd-Insert:wrapper :count 1)
       (setq-local vim-insert-mode-on-exit #'vim--insert-linewise-copies))
      (`normal
       (error "visual insert is not supported in normal visual mode"))))
  (setq vim-visual--last-insert-undo vim--last-insert-undo))

(defun vim--finalize-copy-inserts! ()
  (vim--command-finalize! (setq vim--last-undo vim-visual--last-insert-undo) t))

(defun vim--insert-block-copies ()
  "Called to repeat the last block-insert."
  (setq-local vim-insert-mode-on-exit nil)
  (let ((col (vim-visual-insert-info-column vim-visual--last-insert-info))
        (count (vim-visual-insert-info-line-count vim-visual--last-insert-info))
        (undo-inhibit-record-point t)
        (events (apply #'vconcat vim-visual--key-sequence-for-repeat)))
    (unwind-protect
        (save-excursion
          (goto-char (vim-visual-insert-info-end vim-visual--last-insert-info))
          (dotimes (_ count)
            (when (>= (save-excursion
                        (end-of-line)
                        (current-column-fixed-uncached))
                      col)
              (move-to-column col t)
              (save-excursion
                (vim--cmd-repeat-impl 1 events nil)))
            (forward-line -1))
          (vim--finalize-copy-inserts!))
      (vim--free-visual-insert-info!))))

(defun vim--insert-linewise-copies ()
  "Called to repeat the last linewise-insert."
  (setq-local vim-insert-mode-on-exit nil)
  (let ((count (vim-visual-insert-info-line-count vim-visual--last-insert-info))
        (undo-inhibit-record-point t)
        (events (apply #'vconcat vim-visual--key-sequence-for-repeat)))
    (unwind-protect
        (save-excursion
          (goto-char (vim-visual-insert-info-end vim-visual--last-insert-info))
          (dotimes (_ count)
            (save-excursion
              (vim--cmd-repeat-impl 1 events nil))
            (forward-line -1))
          (vim--finalize-copy-inserts!))
      (vim--free-visual-insert-info!))))

(defun vim-visual--record-undo-pos! (pos)
  ;; If undos’s not disabled...
  (unless (eq buffer-undo-list t)
    (when (car buffer-undo-list)
      ;; Delimit undo block
      (push nil buffer-undo-list))
    (push pos buffer-undo-list)))

(vim-defcmd vim:visual-repeat (motion)
  "Repeat last command over selected region."
  (pcase vim-visual--mode-type
    (`normal
     (error "Normal visual mode is not supported"))
    (`linewise
     (error "Linewise visual mode is not supported"))
    (`block
     (let ((beg (vim-motion-begin motion))
           (end (vim-motion-end motion))
           (col (vim-motion-last-col motion))
           ;; This is needed so that undo and visual block pastes
           ;; play nicely and after undo the point will return to
           ;; the expected place at the first line of the visual block.
           ;;
           ;; We’ll record the point ourselves.
           (undo-inhibit-record-point t))
       (vim-visual--record-undo-pos! beg)
       (goto-char beg)
       (move-to-column col t)
       (save-excursion
         (goto-char end)
         (dotimes (_ (1+ (vim-count-lines-with-correction beg end)))
           (move-to-column col t)
           (save-excursion
             (vim:cmd-repeat))
           (vim--cmd-paste-after 1 t)
           (forward-line 1)))))))

(vim-defcmd vim:visual-paste-after (motion)
  "‘vim:cmd-paste-after’ extended to visual region."
  (setf vim--cmd-paste-after-counter 0)
  (pcase vim-visual--mode-type
    ((or `normal `linewise)
     (vim--cmd-delete-impl motion nil)
     (vim--cmd-paste-after 1 t))
    (`block
     (let ((beg (vim-motion-begin motion))
           (end (vim-motion-end motion))
           (col (vim-motion-last-col motion))
           ;; This is needed so that undo and visual block pastes
           ;; play nicely and after undo the point will return to
           ;; the expected place at the first line of the visual block.
           ;;
           ;; We’ll record the point ourselves.
           (undo-inhibit-record-point t))
       (vim-visual--record-undo-pos! beg)
       (goto-char beg)
       (move-to-column col t)
       (vim--cmd-paste-after 1 t)
       (save-excursion
         (dotimes (_ (vim-count-lines-with-correction beg end))
           (forward-line 1)
           (move-to-column col t)
           (vim--cmd-paste-after 1 t)))))))

(vim-defcmd vim:visual-paste-before (motion)
  "‘vim:cmd-paste-before’ extended to visual region."
  (setf vim--cmd-paste-before-impl-counter 0)
  (pcase vim-visual--mode-type
    ((or `normal `linewise)
     (vim--cmd-delete-impl motion nil)
     (vim--cmd-paste-before-impl 1))
    (`block
     (let ((beg (vim-motion-begin motion))
           (end (vim-motion-end motion))
           (col (vim-motion-first-col motion))
           (undo-inhibit-record-point t))
       (vim-visual--record-undo-pos! beg)
       (goto-char beg)
       (vim--cmd-paste-before-impl 1)
       (save-excursion
         (dotimes (_ (vim-count-lines-with-correction beg end))
           (forward-line 1)
           (move-to-column col t)
           (vim--cmd-paste-before-impl 1)))))))

(vim-defcmd vim:visual-append (motion)
  "Starts insertion at the right column of a visual block."
  (vim--init--vim-visual-insert-info-end! (vim-motion-begin motion)
                                          (vim-motion-end motion)
                                          (vim-motion-last-col motion))
  (vim-visual--start-append))

(defun vim-visual--start-append ()
  "Starts a new multi-line append operation with `vim-visual--last-insert-info'."
  (goto-char (vim-visual-insert-info-begin vim-visual--last-insert-info))
  (move-to-column (vim-visual-insert-info-column vim-visual--last-insert-info) t)
  (vim-visual--record-undo-pos! (point))
  (let ((undo-inhibit-record-point t))
    (pcase vim-visual--mode-type
      (`block
       ;; TODO: ensure the right command is run on repeat.
       ;; this is really a dirty hack
       (setf vim-visual--key-sequence-for-repeat (cons [?a] (vim--obtain-intermediate-handle!)))
       (vim:cmd-append:wrapper :count 1)
       (setq-local vim-insert-mode-on-exit #'vim--append-block-copies))
      (`linewise
       ;; TODO: ensure the right command is run on repeat
       ;; this is really a dirty hack
       (setf vim-visual--key-sequence-for-repeat (cons [?A] (vim--obtain-intermediate-handle!)))
       (vim:cmd-Append:wrapper :count 1)
       (setq-local vim-insert-mode-on-exit #'vim--insert-linewise-copies))
      (_
       (error "visual append is not supported in normal visual mode"))))
  (setq vim-visual--last-insert-undo vim--last-insert-undo))

(defun vim--append-block-copies ()
  "Called to repeat the last block-insert."
  (setq-local vim-insert-mode-on-exit nil)
  (let ((col (vim-visual-insert-info-column vim-visual--last-insert-info))
        (count (vim-visual-insert-info-line-count vim-visual--last-insert-info))
        (undo-inhibit-record-point t)
        (events (apply #'vconcat vim-visual--key-sequence-for-repeat)))
    (vim-visual--record-undo-pos! (vim-visual-insert-info-begin vim-visual--last-insert-info))
    (unwind-protect
        (save-excursion
          (let ((col-past (1+ col)))
            (goto-char (vim-visual-insert-info-end vim-visual--last-insert-info))
            (dotimes (_ count)
              (move-to-column col-past t) ;; extend the newline at the end
              (move-to-column col)        ;; no need to force again
              (save-excursion
                (vim--cmd-repeat-impl 1 events nil))
              (forward-line -1))
            (vim--finalize-copy-inserts!)))
      (vim--free-visual-insert-info!))))

(vim-defcmd vim:visual-exchange-point-and-mark (nonrepeatable keep-visual)
  "Exchanges point and mark."
  (exchange-point-and-mark))

(vim-defcmd vim:visual-jump-point (nonrepeatable keep-visual)
  "In normal and linewise visual mode, this is the same as
`vim:visual-exchange-point-and-mark'.  In block visual-mode the
cursor jumps to the other corner of the selected region in the
current line."
  (pcase vim-visual--mode-type
    ((or `normal `linewise)
     (vim:visual-exchange-point-and-mark))
    (`block
     (let ((mark-col (save-excursion
                       (goto-char (mark t))
                       (current-column-fixed-uncached)))
           (point-col (current-column-fixed-uncached)))
       (set-mark (save-excursion
                   (goto-char (mark t))
                   (move-to-column point-col t)
                   (point)))
       (move-to-column mark-col t)))
    (_ (error "Not in visual mode"))))

(vim-defcmd vim:visual-ex-read-command (nonrepeatable)
  "Starts ex-mode with visual-marks as initial input."
  (vim-ex-read-command "'<,'>"))

(defvar-local vim-visual--last-point nil
  "The position of point before a region-command.")
(defvar-local vim-visual--last-mark nil
  "The position of mark before a region-command.")
(defvar-local vim-visual--new-point nil
  "The position of modified point before a region-command." )

(defun vim-visual--normalize-region ()
  (when (vim-visual-mode-p)
    (let* ((iform (interactive-form this-command))
           (use-region (or (eq this-command 'execute-extended-command)
                           (and iform
                                (cdr iform)
                                (stringp (cadr iform))
                                (string= (cadr iform) "r")))))
      (when use-region
        (setq vim-visual--last-point (point)
              vim-visual--last-mark (mark))
        (pcase vim-visual--mode-type
          (`normal
           (if (> (point) (mark)) (forward-char) (set-mark (1+ (mark)))))
          (`linewise
           (if (> (point) (mark))
               (progn
                 (end-of-line)
                 (forward-char)
                 (set-mark (save-excursion
                             (goto-char (mark))
                             (line-beginning-position))))
             (progn
               (beginning-of-line)
               (set-mark (save-excursion
                           (goto-char (mark))
                           (1+ (line-end-position)))))))
          (`block))
        (setq vim-visual--new-point (point))))))

(defun vim-visual--denormalize-region ()
  (when (and vim-visual--last-point vim-visual--last-mark)
    (set-mark vim-visual--last-mark)
    (when (and vim-visual--new-point
               (= (point) vim-visual--new-point))
      (goto-char vim-visual--last-point))
    (setq vim-visual--last-point nil
          vim-visual--last-mark  nil
          vim-visual--new-point  nil)))

(defun region-active-p--take-vim-visual-mode-into-account (is-active)
  "This function advices `region-active-p' to teach it about vim's visual mode."
  (or is-active
      (and (vim-visual-mode-p)
           (not (eq vim-visual--mode-type 'block)))))

(advice-add 'region-active-p :filter-return #'region-active-p--take-vim-visual-mode-into-account)
(advice-add 'use-region-p :filter-return #'region-active-p--take-vim-visual-mode-into-account)

(defun region-beginning--fix-start-for-vim (region-start)
  (if (and (vim-visual-mode-p)
           (eq vim-visual--mode-type 'linewise))
      (save-excursion
        (goto-char region-start)
        (line-beginning-position))
    region-start))

(advice-add 'region-beginning :filter-return #'region-beginning--fix-start-for-vim)

(defun region-end--fix-end-for-vim (region-end)
  (if (vim-visual-mode-p)
      (pcase vim-visual--mode-type
        (`normal
         (+ region-end 1))
        (`linewise
         (save-excursion
           (goto-char region-end)
           (line-end-position)))
        (_
         region-end))
    region-end))

(advice-add 'region-end :filter-return #'region-end--fix-end-for-vim)

;;; Search currently selected text

(defun vim-visual--get-normal-or-linewise-region-bounds ()
  (pcase vim-visual--mode-type
    (`normal)
    (`linewise)
    (`block    (error "Block region is not supported")))
  (with-region-bounds-unadj beg end
      (cons beg end)))

(search--make-search-for-thing
    search-for-selected-region-forward
    search-for-selected-region-forward-new-color
    (vim-visual--get-normal-or-linewise-region-bounds)
    (lambda (x)
      `(progn
         (vim:visual-mode-exit:wrapper)
         (search--next-impl (or ,x 1))))
  :is-forward t
  :error-message "No symbol at point"
  :force-include-bounds-to nil)

(search--make-search-for-thing
    search-for-selected-region-backward
    search-for-selected-region-backward-new-color
    (vim-visual--get-normal-or-linewise-region-bounds)
    (lambda (x)
      `(progn
         (vim:visual-mode-exit:wrapper)
         (search--prev-impl (or ,x 1))))
  :is-forward nil
  :error-message "No region selected"
  :force-include-bounds-to nil)

(vimmize-function search-for-selected-region-forward
                  :name vim:search-for-selected-region-forward
                  :repeatable nil)
(vimmize-function search-for-selected-region-forward-new-color
                  :name vim:search-for-selected-region-forward-new-color
                  :repeatable nil)
(vimmize-function search-for-selected-region-backward
                  :name vim:search-for-selected-region-backward
                  :repeatable nil)
(vimmize-function search-for-selected-region-backward-new-color
                  :name vim:search-for-selected-region-backward-new-color
                  :repeatable nil)

;;; End

(provide 'vim-visual-mode)

;; Local Variables:
;; End:

;; vim-visual-mode.el ends here
