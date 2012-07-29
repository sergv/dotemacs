

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
    '(((kbd "<f9>")    yas/load-snippet-buffer)
      ((kbd "S-<f9>")  yas/tryout-snippet)))

  (def-keys-for-map snippet-mode-map
    '(((kbd "C-c C-c") nil)
      ((kbd "C-c C-t") nil)
      ((kbd "<f9>")    yas/load-snippet-buffer)
      ((kbd "S-<f9>")  yas/tryout-snippet))))


(provide 'snippet-setup)

