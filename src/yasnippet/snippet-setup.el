;; snippet-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(defun snippet-setup ()
  ;; don't use init-common here ;; upd: why?
  (linum-mode t)
  (comment-util-mode t)
  (autopair-mode)

  (setq undo-tree-visualizer-timestamps    t
        undo-tree-visualizer-parent-buffer t)
  (setq case-fold-search nil)

  (setq vim:normal-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f9>"    yas/load-snippet-buffer)
    ("S-<f9>"  yas/tryout-snippet))

  (def-keys-for-map snippet-mode-map
    ("C-c C-c" nil)
    ("C-c C-t" nil)
    ("<f9>"    yas/load-snippet-buffer)
    ("S-<f9>"  yas/tryout-snippet)))


(provide 'snippet-setup)

;; Local Variables:
;; End:

;; snippet-setup.el ends here
