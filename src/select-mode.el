;; select-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 21 November 2012
;; Description:

;; Utility mode that provides convenient interface for selecting among
;; multiple candidates

(eval-when-compile (require 'cl-lib))

;; Config variables

(defcustom select-mode-restore-windows-configuration-on-hide nil
  "Whether to restore windows configuration prior to selection buffer appearance (t)
or just to bury selection buffer, leaving it's windows inplace (nil).)")

(defun select-make-bold-separator (base)
  (propertize base
              'face 'bold
              'font-lock-face 'bold))

(defface select-mode-selection-face '((t (:inherit secondary-selection)))
  "Face to highlight currently selected item")

(defparameter select-mode-bold-separator (select-make-bold-separator "--------\n"))


(defstruct select-mode--state
  (init-buffer           nil                    :read-only t) ;; Buffer that was active when selection was initiated.
  (init-window           nil                    :read-only t) ;; Window that was active when selection was initiated.
  (init-window-config    nil                    :read-only t) ;; Window configuration before selection buffer was shown.

  (item-show-function    #'identity             :read-only t) ;; A function of one item to be displayed. Items will be passed to this function before insertion into buffer.
  (on-selection-function #'ignore               :read-only t) ;; A function of three arguments: index of currently selected item, the selected item and selection type which is either 'same-window or 'other-window.
  (preamble              nil                    :read-only t) ;; String that will be inserted at the top of the buffer;
  (epilogue              nil                    :read-only t) ;; String that will be inserted at the bottom of the buffer;
  (separator             select-mode-bold-separator :read-only t) ;; Separator

  (selected-item         nil) ;; Number, index of selected item withit items field.
  (items                 nil) ;; Vector of possible selections.
  (item-positions        nil) ;; Vector of positions for tracking current selection.
  (items-count           nil) ;; Number of items and item-positions

  (selection-overlay     nil :read-only t) ;; Overlay that displays currently selected item.

  )

(defvar-local select-mode--current-state nil
  "Buffer-local instance of `select-mode--state' struct.")

(defvar select-mode-map
  (let ((kmap (make-sparse-keymap)))
    (def-keys-for-map kmap
      ("<up>"     select-mode-select-previous-item)
      ("<down>"   select-mode-select-next-item)
      ("<return>" select-mode-do-select-same-window)
      ("SPC"      select-mode-do-select-other-window)
      ("<escape>" select-mode-hide)
      ("C-g"      select-mode-exit))
    kmap))

;;; utilities

(defsubst select-mode--list->vector (items)
  (coerce items 'vector))

;;; mode definition

(define-derived-mode select-mode text-mode "Selection"
  "Major mode for queries in auxiliary buffer."
  (linum-mode -1) ;; fringe line tracking
  (setq-local mode-line-format
              '(" %[%b%] "
                (:eval (when buffer-read-only
                         "(RO)"))
                ("("
                 mode-name
                 (:eval (format "[%s/%s]"
                                (select-mode--state-selected-item select-mode--current-state)
                                (select-mode--state-items-count select-mode--current-state)))
                 ")")
                (:eval
                 (when (buffer-narrowed?)
                   "(Narrowed)"))))
  (add-hook 'post-command-hook #'select-mode--update-selected-item nil t)
  ;; (add-hook 'kill-buffer-hook #'select-mode--finish-selection nil t)
  )

;; TODO: add option to use recursive edit?
;; API for user
(defun* select-mode-start-selection
    (items
     &key
     (buffer-name "Selection")
     after-init
     (on-selection #'ignore)
     item-show-function
     (preamble "")
     (epilogue "")
     (separator select-mode-bold-separator)
     (working-directory nil))
  "Initiate select session.

ON-SELECTION - function of 2 arguments, index of selected item inside ITEMS collection
and symbol, specifying selection type. Currently, selection type may be either
'same-window or 'other-window.

WORKING-DIRECTORY can be either string specifying a directory or nil, in which
case `default-directory' will be used.
"
  (cl-assert (functionp item-show-function))
  (cl-assert (functionp on-selection))
  (cl-assert (stringp preamble))
  (cl-assert (stringp epilogue))
  (cl-assert (or (null separator) (stringp separator)))
  (cl-assert (or (null working-directory)
                 (and (stringp working-directory)
                      (file-directory-p working-directory))))
  (let ((items-count (length items))
        (init-buffer (current-buffer))
        (init-window (selected-window))
        (init-window-config (current-window-configuration))
        (working-dir (or working-directory
                         default-directory)))
    (cl-assert (< 0 items-count))
    (with-current-buffer (switch-to-buffer-other-window buffer-name)
      (read-only-mode -1)
      (select-mode)

      (cd working-dir)
      ;; Disable undo tracking in this buffer
      (setq-local buffer-undo-list t)

      (let ((selection-overlay (make-overlay (point-min) (point-min))))
        (overlay-put selection-overlay
                     'face
                     'select-mode-selection-face)
        (overlay-put selection-overlay
                     'font-lock-face
                     'select-mode-selection-face)


        (setq-local select-mode--current-state
                    (make-select-mode--state
                     :init-buffer init-buffer
                     :init-window init-window
                     :init-window-config init-window-config

                     :item-show-function item-show-function
                     :on-selection-function on-selection
                     :preamble preamble
                     :epilogue epilogue
                     :separator separator

                     :selected-item 0
                     :items (if (listp items)
                                (select-mode--list->vector items)
                              items)
                     :item-positions (make-vector items-count nil)
                     :items-count items-count

                     :selection-overlay selection-overlay
                     )))

      (select-mode--render-state select-mode--current-state)

      (when after-init
        (funcall after-init))
      (set-buffer-modified-p nil)
      (read-only-mode +1))))

(defun select-mode--move-selection-to (state idx &optional move-point)
  (cl-assert (and (<= 0 idx)
                  (< idx (select-mode--state-items-count state))))
  (setf (select-mode--state-selected-item state) idx)
  (destructuring-bind (start . end)
      (aref (select-mode--state-item-positions state) idx)
    (when move-point
      (goto-char start))
    (move-overlay (select-mode--state-selection-overlay state)
                  start
                  end)
    (force-mode-line-update)))

(defun select-mode--render-state (state)
  "It's assumed that this function is only called inside select buffer."
  (let ((insert-item
         (lambda (i item)
           (let ((start (point)))
             (insert (funcall (select-mode--state-item-show-function state) item))
             (let ((end (point)))
               (setf (aref (select-mode--state-item-positions state) i)
                     (cons start end)))))))
    (erase-buffer)
    (goto-char (point-min))
    (insert (select-mode--state-preamble state))
    (let ((sep (or (select-mode--state-separator state)
                   "")))
      (loop
        for item being the elements of (select-mode--state-items state) using (index i)
        do
        (unless (= i 0) (insert sep))
        (funcall insert-item i item)))
    (insert (select-mode--state-epilogue state))
    (select-mode--move-selection-to state (select-mode--state-selected-item state) t)))

(defun select-mode--update-selected-item ()
  "Set selected item based on the point position inside buffer."
  (let* ((pos (point))
         (pos-inside-pos-pair
          (lambda (pos pos-pair)
            (cl-assert (< (car pos-pair) (cdr pos-pair)))
            (and (<= (car pos-pair) pos)
                 (< pos (cdr pos-pair)))))
         (positions (select-mode--state-item-positions select-mode--current-state))
         (selection-idx
          (bisect pos
                  positions
                  0
                  (select-mode--state-items-count select-mode--current-state)
                  pos-inside-pos-pair
                  (lambda (pos pos-pair)
                    (cl-assert (< (car pos-pair) (cdr pos-pair)))
                    (< pos (car pos-pair))))))
    (if (and selection-idx
             (funcall pos-inside-pos-pair
                      pos
                      (aref positions selection-idx)))
        (select-mode--move-selection-to select-mode--current-state selection-idx nil)
      (move-overlay (select-mode--state-selection-overlay select-mode--current-state)
                    (point-min)
                    (point-min)))))

(defun select-mode-select-previous-item ()
  "Select previous item with wraparound."
  (interactive)
  (select-mode--move-selection-to
   select-mode--current-state
   (mod (- (select-mode--state-selected-item select-mode--current-state) 1)
        (select-mode--state-items-count select-mode--current-state))
   t))

(defun select-mode-select-next-item ()
  "Select next item with wraparound."
  (interactive)
  (select-mode--move-selection-to
   select-mode--current-state
   (mod (+ (select-mode--state-selected-item select-mode--current-state) 1)
        (select-mode--state-items-count select-mode--current-state))
   t))

(defun select-mode--do-select (selection-type)
  (funcall (select-mode--state-on-selection-function select-mode--current-state)
           (select-mode--state-selected-item select-mode--current-state)
           (aref
            (select-mode--state-items select-mode--current-state)
            (select-mode--state-selected-item select-mode--current-state))
           selection-type))

(defun select-mode-do-select-same-window ()
  (interactive)
  (select-mode--do-select 'same-window))

(defun select-mode-do-select-other-window ()
  (interactive)
  (select-mode--do-select 'other-window))


(defun select-mode-hide ()
  (interactive)
  (cl-assert select-mode--current-state)
  (if select-mode-restore-windows-configuration-on-hide
      (set-window-configuration
       (select-mode--state-init-window-config select-mode--current-state))
    (call-interactively #'bury-buffer)))

(defun select-mode--finish-selection ()
  (cl-assert select-mode--current-state)
  (let ((win-config (select-mode--state-init-window-config select-mode--current-state)))
    (awhen (select-mode--state-selection-overlay select-mode--current-state)
      (delete-overlay it))
    ;; (setf (select-mode--state-selection-overlay select-mode--current-state) nil
    ;;       (select-mode--state-init-buffer select-mode--current-state) nil
    ;;       (select-mode--state-init-window select-mode--current-state) nil
    ;;       (select-mode--state-init-window-config select-mode--current-state) nil)
    (setq-local select-mode--current-state nil)
    (set-window-configuration win-config)))

(defun select-mode-exit ()
  (interactive)
  (let ((buf (current-buffer)))
    (select-mode--finish-selection)
    (kill-buffer buf)))

;;; This is for users of select-mode.

(defun select-mode-extend-keymap-with (new-keymap)
  "Add NEW-KEYMAP to the select-mode's current keymap."
  (use-local-map
   (make-composed-keymap new-keymap select-mode-map)))

(defsubst select-mode-get-selected-index ()
  (select-mode--state-selected-item select-mode--current-state))

(defun select-mode-update-items (items new-selection-index)
  (cl-assert select-mode--current-state)
  (read-only-mode -1)
  (unwind-protect
      (let* ((items-vector (if (listp items)
                               (select-mode--list->vector items)
                             items))
             (items-count (length items-vector)))
        (setf (select-mode--state-selected-item select-mode--current-state)  new-selection-index
              (select-mode--state-items select-mode--current-state)          items-vector
              (select-mode--state-item-positions select-mode--current-state) (make-vector items-count nil)
              (select-mode--state-items-count select-mode--current-state)    items-count)
        (select-mode--render-state select-mode--current-state))
    (set-buffer-modified-p nil)
    (read-only-mode +1)))

(provide 'select-mode)

;; Local Variables:
;; End:

;; select-mode.el ends here
