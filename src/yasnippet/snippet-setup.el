;; snippet-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(defun yas-load-snippet-buffer-no-kill (&optional prompt-table)
  "Load snippet from current buffer. If PROMPT-TABLE is non-nil then
propmt user for snippet table to load into and try to infer one
otherwise."
  (interactive "P")
  (save-excursion
    (yas-load-snippet-buffer (if prompt-table
                               (yas--read-table)
                               (first (yas--compute-major-mode-and-parents
                                       buffer-file-name)))
                             nil)))

(defun snippet-setup ()
  ;; don't use init-common here ;; upd: why?
  (linum-mode 1)
  (comment-util-mode 1)

  (setf undo-tree-visualizer-timestamps    t
        undo-tree-visualizer-parent-buffer t
        case-fold-search nil)

  (setf vim:normal-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f9>"    yas-load-snippet-buffer-no-kill)
    ("S-<f9>"  yas-tryout-snippet))

  (def-keys-for-map snippet-mode-map
    ("C-c C-c" nil)
    ("C-c C-t" nil)
    ("<f9>"    yas-load-snippet-buffer-no-kill)
    ("S-<f9>"  yas-tryout-snippet)))


(provide 'snippet-setup)

;; Local Variables:
;; End:

;; snippet-setup.el ends here
