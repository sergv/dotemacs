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

(defvar select-restore-windows-configuration-on-hide nil
  "Whether to restore windows configuration prior to selection buffer appearance (t)
or just to bury selection buffer, leaving it's windows inplace (nil).)")

;; Global state

(defvar-local select--init-window-config nil)
(defvar-local select--init-window nil
  "Window from which select-mode's instance was invoked")
(defvar-local select--init-buffer nil
  "Buffer from which select-mode's instance was invoked")


(defvar-local select--selected-item nil
  "Index (number) of selected item")
(defvar-local select--items nil
  "Vector of possible selections")
(defvar-local select--item-positions nil
  "Vector of positions for tracking current selection.")

(defvar-local select--selection-overlay nil
  "Overlay that displays ")

(defface select-selection-face '((t (:inherit secondary-selection)))
  "Face to highlight currently selected item")

(defvar-local select--item-show-function #'identity
  "This should be a function of one item to be displayed.

Items will be passed to this function before insertion into buffer.")

(defvar-local select--on-selection-function #'ignore
  "This should be a function of one argument - index of currently selected item.")
(defvar-local select--preamble-function (lambda () "")
  "Function that returns contents at the top of the buffer")
(defvar-local select--epilogue-function (lambda () "")
  "Function that returns contents at the bottom of the buffer")

(defvar-local select--separator-function (lambda () "")
  "Function of no arguments that returns current separator to use.

Separator is used like this
<...>
foo
---------------
bar
---------------
baz
<...>")

(defun select-make-bold-separator (base)
  (propertize base
              'face 'bold
              'font-lock-face 'bold))

(defvar select--bold-separator (select-make-bold-separator "--------\n"))

(defvar select-mode-map
  (let ((kmap (make-sparse-keymap)))
    (def-keys-for-map kmap
      ("<up>"     select-move-selection-up)
      ("<down>"   select-move-selection-down)
      ("<return>" select-do-select-same-window)
      ("SPC"      select-do-select-other-window)
      ("<escape>" select-hide)
      ("q"        select-hide)
      ("C-q"      select-hide)
      ("C-g"      select-hide))
    kmap))

;;; utilities

(defmacro select--with-disabled-undo (&rest body)
  (declare (indent 0))
  (let ((store '#:store))
    `(let ((,store buffer-undo-list)
           ;; this disables further undo recording
           (buffer-undo-list t))
       ,@body)))

(defmacro select--with-preserved-buffer-modified-p (&rest body)
  "Execute BODY and restore `buffer-modified-p' flag after its done."
  (declare (indent 0))
  (let ((store '#:store))
    `(let ((,store (buffer-modified-p)))
       (unwind-protect
           (progn
             ,@body)
         (set-buffer-modified-p ,store)))))

(defmacro select--with-inhibited-read-only (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     ,@body))

(defun select--bisect (item items start end eq? less?)
  "Binary search. Returns index into vector ITEMS.
LESS? is predicate on items and elements of ITEMS.

START is inclusive and END is exclusive in ITEMS."
  (let ((orig-start start)
        (orig-end end))
    (while (< start end)
      (let* ((mid (/ (+ end start) 2))
             (mid-item (aref items mid)))
        (cond ((funcall less? item mid-item)
               (setf end mid))
              ((funcall eq? item mid-item)
               (setf start mid
                     end mid))
              (t
               (setf start (+ mid 1))))))
    start))

(defsubst select--list->vector (items)
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
                                select--selected-item
                                (length select--items)))
                 ")")
                (:eval
                 (when (buffer-narrowed?)
                   "(Narrowed)"))))
  (add-hook 'post-command-hook #'select--update-selected-item nil t)
  ;; (add-hook 'kill-buffer-hook #'select-finish-selection nil t)
  )

;; TODO: add option to use recursive edit?
;; API for user
(defun* select-start-selection (items
                                &key
                                (buffer-name "Selection")
                                after-init
                                (on-selection 'ignore)
                                item-show-function
                                (preamble-function (lambda () ""))
                                (epilogue-function (lambda () ""))
                                (separator-function
                                 (lambda ()
                                   select--bold-separator)))
  "Initiate select session.

ON-SELECTION - function of 2 arguments, index of selected item inside ITEMS collection
and symbol, specifying selection type. Currently, selection type may be either
'same-window or 'other-window.
"
  (cl-assert (< 0 (length items)))
  (cl-assert item-show-function)
  (let ((init-buffer (current-buffer))
        (init-window (selected-window))
        (init-window-config (current-window-configuration)))
    (with-current-buffer (switch-to-buffer-other-window buffer-name)
      (select--with-disabled-undo
        (select-mode)

        (setq-local select--init-window-config init-window-config)
        (setq-local select--init-window init-window)
        (setq-local select--init-buffer init-buffer)
        ;; display-related items
        (setq-local select--item-show-function item-show-function)
        (setq-local select--on-selection-function on-selection)
        (setq-local select--preamble-function preamble-function)
        (setq-local select--epilogue-function epilogue-function)
        (setq-local select--separator-function separator-function)

        (setq-local select--selected-item 0)
        (setq-local select--items (if (listp items)
                                    (select--list->vector items)
                                    items))
        (setq-local select--item-positions (make-vector (length items) nil))

        (setq-local select--selection-overlay (make-overlay (point-min) (point-min)))
        (overlay-put select--selection-overlay
                     'face
                     'select-selection-face)
        (overlay-put select--selection-overlay
                     'font-lock-face
                     'select-selection-face)
        (select--with-preserved-buffer-modified-p
          (select--with-inhibited-read-only
            (select--render-items select--items)))
        (when after-init
          (funcall after-init))

        (set-buffer-modified-p nil)
        (read-only-mode +1)
        ;; (setf buffer-read-only t)
        ))))


(defun* select--move-selection-to (idx &key (move-point t))
  (assert (and (<= 0 idx)
               (< idx (length select--items))))
  (destructuring-bind (start . end)
      (aref select--item-positions idx)
    (when move-point
      (goto-char start))
    (move-overlay select--selection-overlay
                  start
                  end)
    (force-mode-line-update)))

(defun select--render-items (items)
  "It's assumed that this function is only called inside select buffer."
  (let ((insert-item
         (lambda (index item)
           (let ((start (point)))
             (insert (funcall select--item-show-function item))
             (let ((end (point)))
               (setf (aref select--item-positions index) (cons start end)))))))
    (erase-buffer)
    (goto-char (point-min))
    (insert (funcall select--preamble-function))
    (let ((sep (when select--separator-function
                 (funcall select--separator-function))))
      (loop
        for item being the elements of items using (index i)
        do
        (unless (= i 0)
          (when sep (insert sep)))
        (funcall insert-item i item)))
    (insert (funcall select--epilogue-function))
    (select--move-selection-to select--selected-item)))

(defun select--update-selected-item ()
  "Set selected item based on the point position inside buffer."
  (let* ((pos (point))
         (pos-inside-pos-pair
          (lambda (pos pos-pair)
            (assert (< (car pos-pair) (cdr pos-pair)))
            (and (<= (car pos-pair) pos)
                 (< pos (cdr pos-pair)))))
         (selection-idx
          (select--bisect pos
                          select--item-positions
                          0
                          (length select--item-positions)
                          pos-inside-pos-pair
                          (lambda (pos pos-pair)
                            (assert (< (car pos-pair) (cdr pos-pair)))
                            (< pos (car pos-pair))))))
    (if (and selection-idx
             (funcall pos-inside-pos-pair pos
                      (aref select--item-positions selection-idx)))
      (progn
        (setq-local select--selected-item selection-idx)
        (select--move-selection-to select--selected-item :move-point nil))
      (move-overlay select--selection-overlay (point-min) (point-min)))))

(defun select-move-selection-up ()
  (interactive)
  (setf select--selected-item
        (if (= 0 select--selected-item)
          (- (length select--items) 1)
          (- select--selected-item 1)))
  (select--move-selection-to select--selected-item))

(defun select-move-selection-down ()
  (interactive)
  (setf select--selected-item
        (if (= select--selected-item (- (length select--items) 1))
          0
          (+ select--selected-item 1)))
  (select--move-selection-to select--selected-item))

(defun select-do-select (selection-type)
  (if select--on-selection-function
    (funcall select--on-selection-function select--selected-item selection-type)
    (error "No on-selection function defined")))

(defun select-do-select-same-window ()
  (interactive)
  (select-do-select 'same-window))

(defun select-do-select-other-window ()
  (interactive)
  (select-do-select 'other-window))

(defun select--restore-window-config ()
  (when select--init-window-config
    (set-window-configuration select--init-window-config)
    ;; (select-window select--init-window)
    (setf select--init-window-config nil
          select--init-window nil)))

(defun select-finish-selection ()
  (select--restore-window-config)
  (when select--selection-overlay
    (delete-overlay select--selection-overlay)
    (setq-local select--selection-overlay nil))
  ;; (let ((err (lambda (&rest args)
  ;;              (error "Some function not set, check your use of select-mode"))))
  ;;   (setf select--init-buffer           nil
  ;;         select--selected-item         nil
  ;;         select--items                 nil
  ;;
  ;;         select--separator-function    err
  ;;         select--item-show-function    err
  ;;         select--on-selection-function err
  ;;         select--preamble-function     err
  ;;         select--epilogue-function     err))
  )

(defun select-hide ()
  (interactive)
  (if select-restore-windows-configuration-on-hide
    (select--restore-window-config)
    (call-interactively #'bury-buffer)))

(defun select-exit ()
  (interactive)
  (select-finish-selection)
  (kill-buffer))

;;; This is for users of select-mode.

(defun select-extend-keymap (new-keymap)
  (use-local-map
   (make-composed-keymap new-keymap select-mode-map)))

(defsubst select-get-selected-index ()
  select--selected-item)

(provide 'select-mode)

;; Local Variables:
;; End:

;; select-mode.el ends here
