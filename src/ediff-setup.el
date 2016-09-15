;; ediff-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 November 2012
;; Description:


;;;###autoload
(defun ediff-keymap-setup ()
  (def-keys-for-map ediff-mode-map
    ("h"        ediff-next-difference)
    ("t"        ediff-previous-difference)
    ("<home>"   next-f)
    ("<end>"    prev-f)
    ("S-<home>" swap-buffers-forward-through-frames)
    ("S-<end>"  swap-buffers-backward-through-frames)
    ("<left>"   prev-w)
    ("<right>"  next-w)
    ("<down>"   ediff-next-difference)
    ("<up>"     ediff-previous-difference)
    ("<escape>" ediff-quit)))

;;;###autoload
(defun* ediff-diff-texts-recursive-edit (text-a
                                         text-b
                                         &key
                                         (read-only t)
                                         (a-buf-name "text A")
                                         (b-buf-name "text B"))
  "Quickly show difference between two texts TEXT-A and TEXT-B using ediff.
Register quick exit function and show difference in recursive edit."
  (let ((buf-a    (get-buffer-create a-buf-name))
        (buf-b    (get-buffer-create b-buf-name))
        (win-conf (current-window-configuration)))
    (with-current-buffer buf-a
      (erase-buffer)
      (insert text-a))
    (with-current-buffer buf-b
      (erase-buffer)
      (insert text-b))
    (let ( ;; (ediff-quit-hook
          ;;   (append ediff-quit-hook
          ;;           (list (lambda ()
          ;;                   (exit-recursive-edit)))))
          (ediff-make-buffers-readonly-at-startup read-only)
          (orig-ediff-quit (symbol-function #'ediff-quit))
          (new-ediff-quit
           (lambda (reverse-default-keep-variants)
             (interactive "P")
             (ediff-barf-if-not-control-buffer)
             (let ((minibuffer-auto-raise t))
               (ediff-really-quit reverse-default-keep-variants)
               (exit-recursive-edit)))))
      (fset 'ediff-quit new-ediff-quit)
      (ediff-buffers buf-a buf-b)
      ;; protect in case of abort-recursive-edit
      (unwind-protect
          (recursive-edit)
        (set-window-configuration win-conf)
        (kill-buffer buf-a)
        (kill-buffer buf-b)
        (fset 'ediff-quit orig-ediff-quit)))))

;;;###autoload
(defun* ediff-diff-files-recursive-edit (file-a
                                         file-b
                                         &key
                                         (read-only t))
  "Run `ediff' on a pair of files. Also register quick exit function and restore
window configuration on end of ediff session."
  (let ((win-conf (current-window-configuration)))
    (let ( ;; (ediff-quit-hook
          ;;   (append ediff-quit-hook
          ;;           (list (lambda ()
          ;;                   (exit-recursive-edit)))))
          (ediff-make-buffers-readonly-at-startup read-only))
      (ediff file-a file-b)
      (let ((orig-ediff-quit (symbol-function #'ediff-quit))
            (new-ediff-quit
             (lambda (reverse-default-keep-variants)
               (interactive "P")
               (ediff-barf-if-not-control-buffer)
               (let ((minibuffer-auto-raise t))
                 (ediff-really-quit reverse-default-keep-variants)
                 (exit-recursive-edit)))))
        (fset 'ediff-quit new-ediff-quit)
        ;; protect in case of abort-recursive-edit
        (unwind-protect
            (recursive-edit)
          (set-window-configuration win-conf)
          (fset 'ediff-quit orig-ediff-quit))))))

;;;###autoload
(defun ediff/line-in-buffer? (buffer line-num)
  (with-current-buffer buffer
    (let ((line-min 1)
          (line-max (count-lines (point-min) (point-max))))
      (and (<= line-min line-num)
           (<= line-num line-max)))))

;;;###autoload
(defun ediff/read-line-in-buffer (prompt
                                  buffer
                                  error-func)
  (let ((num (read-number prompt)))
    (unless (ediff/line-in-buffer? buffer num)
      (funcall error-func num line-min line-max))
    num))

;;;###autoload
(defun ediff-two-regions (buffer-a start-a end-a buffer-b start-b end-b)
  (interactive
   (let* ((buffer-a (completing-read-buffer "Buffer A to diff: " nil t))
          (start-a (ediff/read-line-in-buffer
                    "Buffer A start line: "
                    buffer-a
                    (lambda (line-num line-min line-max)
                      (error "Start line for buffer A (%s), %s, is out of bounds, [%s, %s]"
                             buffer-a
                             line-num
                             line-min
                             line-max))))
          (end-a (ediff/read-line-in-buffer
                  "Buffer A end line: "
                  buffer-a
                  (lambda (line-num line-min line-max)
                    (error "End line for buffer A (%s), %s, is out of bounds, [%s, %s]"
                           buffer-a
                           line-num
                           line-min
                           line-max))))

          (buffer-b (completing-read-buffer "Buffer B to diff: " nil t))
          (start-b (ediff/read-line-in-buffer
                    "Buffer B start line: "
                    buffer-b
                    (lambda (line-num line-min line-max)
                      (error "Start line for buffer B (%s), %s, is out of bounds, [%s, %s]"
                             buffer-b
                             line-num
                             line-min
                             line-max))))
          (end-b (ediff/read-line-in-buffer
                  "Buffer B end line: "
                  buffer-b
                  (lambda (line-num line-min line-max)
                    (error "End line for buffer B (%s), %s, is out of bounds, [%s, %s]"
                           buffer-b
                           line-num
                           line-min
                           line-max)))))
     (list buffer-a start-a end-a buffer-b start-b end-b)))
  (let ((text-a (with-current-buffer buffer-a
                  (text-between-lines start-a end-a)))
        (text-b (with-current-buffer buffer-b
                  (text-between-lines start-b end-b))))
    (ediff-diff-texts-recursive-edit text-a
                                     text-b
                                     :read-only t
                                     :a-buf-name (format "Buffer A (%s)" buffer-a)
                                     :b-buf-name (format "Buffer B (%s)" buffer-b))))


(provide 'ediff-setup)

;; Local Variables:
;; End:

;; ediff-setup.el ends here
