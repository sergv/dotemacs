;; ediff-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 November 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util)
  (require 'keys-def))

;;;###autoload
(defun ediff-keymap-setup ()
  (def-keys-for-map ediff-mode-map
    +vim-interbuffer-navigation-keys+
    ("H"            ediff-update-diffs)
    (("h" "<down>") ediff-next-difference)
    (("t" "<up>")   ediff-previous-difference)
    ("<escape>"     ediff-quit)))

;;;###autoload
(cl-defun ediff-diff-texts-recursive-edit (text-a
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

;;;###autoload
(cl-defun ediff-diff-files-recursive-edit (file-a
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

(provide 'ediff-setup)

;; Local Variables:
;; End:

;; ediff-setup.el ends here
