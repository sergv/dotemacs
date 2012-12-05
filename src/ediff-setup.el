;; ediff-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 November 2012
;; Description:

;; don't spawn separate ediff frame
(setf ediff-window-setup-function 'ediff-setup-windows-plain)

;; don't be fooled by function names here:
;; #'split-window-horizontally causes windows to be split in vertical,
;; and #'split-window-vertically causes windows to be split in horizontal!
(setf ediff-split-window-function
      (if (platform-use? 'netbook)
        ;; when on netbook, the screen isn't wide enough so split horizontally
        #'split-window-vertical
        ;; otherwise split vertically
        #'split-window-horizontally))
(setf ediff-merge-split-window-function ediff-split-window-function)

(setf ediff-custom-diff-options "-u --ignore-tab-expansion --ignore-space-change --ignore-blank-lines"
      ediff-diff-options "--ignore-tab-expansion --ignore-space-change --ignore-blank-lines"
      ediff-patch-options "")

(eval-after-load
 "ediff"
 '(progn
   (add-hook 'ediff-keymap-setup-hook
    #'(lambda ()
        (def-keys-for-map ediff-mode-map
          +control-x-prefix+
          ("<down>"   ediff-next-difference)
          ("<up>"     ediff-previous-difference)
          ("<escape>" ediff-quit))))))

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
    (let (;; (ediff-quit-hook
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

(defun* ediff-diff-files-recursive-edit (file-a
                                         file-b
                                         &key
                                         (read-only t))
  "Run `ediff' on a pair of files. Also register quick exit function and restore
window configuration on end of ediff session."
  (let ((win-conf (current-window-configuration)))
    (let (;; (ediff-quit-hook
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
      (ediff file-a file-b)
      ;; protect in case of abort-recursive-edit
      (unwind-protect
           (recursive-edit)
        (set-window-configuration win-conf)
        (fset 'ediff-quit orig-ediff-quit)))))

(provide 'ediff-setup)

;; Local Variables:
;; End:

;; ediff-setup.el ends here
