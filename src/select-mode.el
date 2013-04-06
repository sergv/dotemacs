;; select-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 21 November 2012
;; Description:

;; Utility mode that provides convenient interface for selecting among
;; multiple candidates

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'more-scheme)
(require 'keys-def)


(when (or (require 'tabbar-setup nil t)
          (require 'tabbar nil t))
  (defun select-inhibit-tabbar ()
    (eq major-mode 'select-mode))

  (add-to-list 'tabbar-inhibit-functions
               #'select-inhibit-tabbar))


(defvar select/init-window-config nil)
(defvar select/init-window nil
  "Window from which select-mode's instance was invoked")
(defvar select/init-buffer nil
  "Buffer from which select-mode's instance was invoked")


(defvar select/selected-item nil
  "Index (number) of selected item")
(defvar select/items nil
  "List of possible selections")
(defvar select/selection-overlay nil
  "Overlay that displays ")
(defvar select/selection-buffer nil
  "Buffer where selection takes place")

(defface select-selection-face '((t (:inherit secondary-selection)))
  "Face to highlight currently selected item")

;; (defface select-selection-face '((t (:bold t)))
;;   "Face to highlight currently selected item")


(defvar select/separator-function (lambda () "")
  "Function of no arguments that returns current separator to use.

Separator is used like this
<...>
foo
---------------
bar
---------------
baz
<...>")
(defvar select/predisplay-function #'identity
  "This should be a function of one item to be displayed.

Items will be passed to this function before insertion into buffer.")

(defvar select/on-selection-function #'ignore
  "This should be a function of one argument - index of currently selected item.")
(defvar select/preamble-function (lambda () "")
  "Function that returns contents at the top of the buffer")
(defvar select/epilogue-function (lambda () "")
  "Function that returns contents at the bottom of the buffer")

(define-derived-mode select-mode text-mode "Selection"
  "Major mode for queries in auxiliary buffer."
  ;; fringe line tracking
  (linum-mode -1)
  ;; modeline line tracking
  (column-number-mode -1)
  (setq-local mode-line-format
              '(" %[%b%] "
                (:eval (when buffer-read-only
                         "(RO)"))
                ("("
                 mode-name
                 (:eval (format "[%s/%s]"
                                select/selected-item
                                (length select/items)))
                 ")")
                (:eval
                 (when (buffer-narrowed?)
                   "(Narrowed)"))))
  (add-hook 'post-command-hook #'select/update-selected-item nil t)
  ;; (add-hook 'kill-buffer-hook #'select-finish-selection nil t)
  )

(setf select-mode-map
      (let ((map (make-sparse-keymap)))
        (def-keys-for-map map
          +vi-essential-keys+
          +vi-search-keys+
          +control-x-prefix+
          +vim-word-motion-keys+
          +vim-special-keys+

          ("n"        select-move-selection-up)
          ("t"        select-move-selection-down)
          ("<up>"     select-move-selection-up)
          ("<down>"   select-move-selection-down)
          ("<return>" select-do-select)
          ("<escape>" select-exit))
        map))


;; TODO: add option to use recursive edit?
(defun* select-start-selection (items
                                &key
                                (buffer-name "Selection")
                                (after-init #'ignore)
                                (on-selection #'ignore)
                                (predisplay-function
                                 (lambda (x) (concat x "\n")))
                                (preamble-function (lambda () ""))
                                (epilogue-function (lambda () ""))
                                (separator-function
                                 (lambda ()
                                   (select-make-bold-separator "--------\n"))))
  "Initiate select session"
  (assert (< 0 (length items)))
  (let ((win-config (current-window-configuration))
        (init-win (selected-window))
        (init-buf (current-buffer)))
    (with-current-buffer (switch-to-buffer-other-window buffer-name)
      (with-disabled-undo
       (setf select/selection-buffer (current-buffer))

       (setf select/init-window-config win-config
             select/init-window init-win
             select/init-buffer init-buf)
       (select-mode)

       ;; display-related items
       (when predisplay-function
         (setf select/predisplay-function predisplay-function))
       (when preamble-function
         (setf select/preamble-function preamble-function))
       (when epilogue-function
         (setf select/epilogue-function epilogue-function))

       (setf select/separator-function separator-function
             select/on-selection-function on-selection
             select/selection-overlay (make-overlay (point-min) (point-min)))
       (overlay-put select/selection-overlay
                    'face
                    'select-selection-face)
       (overlay-put select/selection-overlay
                    'font-lock-face
                    'select-selection-face)
       (select-setup-items items :selected-item 0)
       (select-refresh-items)
       (funcall after-init)

       (set-buffer-modified-p nil)
       (setf buffer-read-only t)))))

(defun* select-setup-items (items &key (selected-item 0))
  (setf select/selected-item (or selected-item 0)
        select/items items
        select/item-positions (make-vector (length items) nil)))


(defun* select/move-selection-to (idx &key (move-point t))
  (assert (and (<= 0 idx)
               (< idx (length select/items))))
  (destructuring-bind (start . end)
      (aref select/item-positions idx)
    (when move-point
      (goto-char start))
    (move-overlay select/selection-overlay
                  start
                  end)
    (force-mode-line-update)))

(defun select-refresh-items ()
  "It's assumed that this function is only called inside select buffer."
  (let ((insert-item
         (lambda (item)
           (let ((start (point)))
             (insert (funcall select/predisplay-function item))
             (values start (point))))))
    (with-current-buffer select/selection-buffer
      (with-disabled-undo
       (with-preserved-buffer-modified-p
        (with-inhibited-read-only
         (erase-buffer)
         (goto-char (point-min))
         (insert (funcall select/preamble-function))

         (multiple-value-bind (start end)
             (funcall insert-item (car select/items))
           (setf (aref select/item-positions 0) (cons start end)))
         (let ((sep (when select/separator-function
                      (funcall select/separator-function))))
           (loop
             for i from 1
             for item in (cdr select/items)
             do (when sep (insert sep))
             (multiple-value-bind (start end)
                 (funcall insert-item item)
               (setf (aref select/item-positions i) (cons start end)))))
         (insert (funcall select/epilogue-function))
         (select/move-selection-to select/selected-item)))))))


(defun select/update-selected-item ()
  "Set selected item based on the point position inside buffer."
  (let* ((pos (point))
         (pos-inside-pos-pair
          (lambda (pos pos-pair)
            (assert (< (car pos-pair) (cdr pos-pair)))
            (and (<= (car pos-pair) pos)
                 (< pos (cdr pos-pair)))))
         (selection-idx
          (bisect pos
                  select/item-positions
                  0
                  (length select/item-positions)
                  pos-inside-pos-pair
                  (lambda (pos pos-pair)
                    (assert (< (car pos-pair) (cdr pos-pair)))
                    (< pos (car pos-pair))))))
    (if (and selection-idx
             (funcall pos-inside-pos-pair pos
                      (aref select/item-positions selection-idx)))
      (progn
        (setf select/selected-item selection-idx)
        (select/move-selection-to select/selected-item :move-point nil))
      (move-overlay select/selection-overlay (point-min) (point-min)))))




(defun select-move-selection-up ()
  (interactive)
  (setf select/selected-item
        (if (= 0 select/selected-item)
          (- (length select/items) 1)
          (- select/selected-item 1)))
  (select/move-selection-to select/selected-item))

(defun select-move-selection-down ()
  (interactive)
  (setf select/selected-item
        (if (= select/selected-item (- (length select/items) 1))
          0
          (+ select/selected-item 1)))
  (select/move-selection-to select/selected-item))

(defun select-do-select ()
  (interactive)
  (if select/on-selection-function
    (funcall select/on-selection-function select/selected-item)
    (error "No on-selection function defined")))

(defun select-finish-selection ()
  (when select/selection-buffer
    (when select/init-window-config
      (set-window-configuration select/init-window-config)
      ;; (select-window select/init-window)
      (setf select/init-window-config nil
            select/init-window nil))
    (when select/selection-overlay
      (delete-overlay select/selection-overlay)
      (setf select/selection-overlay nil))
    (let ((err (lambda (&rest args)
                 (error "Some function not set, check your use of select-mode"))))
      (setf select/init-buffer           nil
            select/selected-item         nil
            select/items                 nil
            select/selection-buffer      nil

            select/separator-function    err
            select/predisplay-function   err
            select/on-selection-function err
            select/preamble-function     err
            select/epilogue-function     err))))

(defun select-exit ()
  (interactive)
  (kill-buffer select/selection-buffer)
  (select-finish-selection))

;;;; this is for invokers of select-mode

(defun select-extend-keymap (new-keymap)
  (use-local-map
   (make-composed-keymap new-keymap select-mode-map)))

(defun select-make-bold-separator (base)
  (propertize base
              'face 'bold
              'font-lock-face 'bold))

(provide 'select-mode)

;; Local Variables:
;; End:

;; select-mode.el ends here
