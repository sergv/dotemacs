;; cc-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  8 October 2012
;; Description:

(require 'custom)

(require 'c-eldoc)
(require 'ctypes)
(require 'find-file)
(require 'etags)


(defun* cc-setup (&key (define-special-keys t)
                       (use-c-eldoc (not (and (platform-os-type? 'windows)
                                              (platform-use? 'work)))))
  (init-common :use-render-formula t)
  (autopair-mode 1)
  (hs-minor-mode 1)
  (which-function-mode -1)
  (when use-c-eldoc
    (c-turn-on-eldoc-mode)
    (set (make-variable-buffer-local 'eldoc-idle-delay) 0.2))

  (modify-syntax-entry ?_ "_")

  (setf whitespace-line-column 80
        whitespace-style '(tabs lines-tail)
        ;; affects only tab display
        tab-width 4)

  (set (make-variable-buffer-local 'vim:shift-width)
       (if (and (platform-os-type? 'windows)
                (platform-use? 'work))
         4
         8))

  (setf c-tab-always-indent t)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  (setq vim:normal-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g TAB" c-indent-defun)

    ("g t"   c-end-of-defun)
    ("g n"   c-beginning-of-defun)

    ("z o"   hs-show-block)
    ("z v"   hs-show-block)
    ("z c"   hs-hide-block)
    ("z C"   hs-hide-all)
    ("z O"   hs-show-all)

    ("M-."   find-tag)
    ("M-,"   pop-tag-mark))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("g a"   align)
    ("g t"   c-end-of-defun)
    ("g n"   c-beginning-of-defun))

  (when define-special-keys
    (def-keys-for-map vim:normal-mode-local-keymap
      ("SPC SPC" ff-find-related-file)))
  (when use-c-eldoc
    (def-keys-for-map vim:normal-mode-local-keymap
      (", ?"     c-eldoc-show-current-symbol-declaration))))

(provide 'cc-setup)

;; Local Variables:
;; End:

;; cc-setup.el ends here
