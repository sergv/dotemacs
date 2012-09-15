;; cython-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 12 June 2012
;; Description:

(require 'common)
(require 'python-setup)

(autoload 'cython-mode "cython-mode" "Cythom mode." t)

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

(defun cython-setup ()
  (init-common :use-yasnippet t
               :use-render-formula t)

  ;; make ' a string delimiter
  (modify-syntax-entry ?' "\"")
  (modify-syntax-entry ?_ "_")

  (setf autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))
  (autopair-mode 1)
  (hs-minor-mode 1)

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    (", d"     pylookup-lookup)
    ("<f9>"    cython-compile)

    (", s s"   vim:replace-symbol-at-point)

    ("z o"     hs-show-block)
    ("z c"     hs-hide-block)
    ("z C"     python-hide-all)
    ("z O"     hs-show-all))

  (pabbrev-mode 1)
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("M-/"     pabbrev-show-menu ;; pabbrev-expand-maybe
               ))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("g t" py-end-of-def-or-class)
    ("g n" py-beginning-of-def-or-class)

    ("*"   search-for-symbol-at-point-forward)
    ("#"   search-for-symbol-at-point-backward))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("<f1>"  py-execute-region-no-switch)
    ("j"     py-execute-region-no-switch)
    ("g a"   nil)
    ("g a =" python-align-on-equals))

  (python-abbrev+-setup)
  (when pabbrev-mode
    (pabbrev-scavenge-buffer))

  (setup-outline +python-section-header-re+))

(add-hook 'cython-mode-hook #'cython-setup)


(provide 'cython-setup)

;; Local Variables:
;; End:

;; cython-setup.el ends here
