;; haskell-rename.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 19 May 2024
;; Description:

(require 'treesit)
(require 'treesit-setup)

(defconst haskell-ts-rename--variable-query
  (treesit-query-compile 'haskell '((variable) @variable)))

(defface haskell-ts-rename-candidate-face
  `((t :box (:line-width
             ,(or (when-emacs-version (>= it 28)
                    '(-1 . -1))
                  -1)
             :color
             "#2aa198")))
  "Face for names currently being renamed.")

(defvar haskell-ts-rename-keymap
  (let ((map (make-sparse-keymap)))
    (def-keys-for-map map
      (("<enter>" "<return>" "RET" "C-c C-c" "<escape>") haskell-ts-rename-done))
    map))

(defvar-local haskell-ts-rename--current-overlays nil
  "List of overlays of things currently being renamed.")

(defvar-local haskell-ts-rename--should-restore-vim-normal-mode? nil)

;;;###autoload
(defun haskell-ts-rename-at-point ()
  (interactive "*")
  (let ((node (treesit-node-at (point) 'haskell)))
    (unless node
      (error "No treesitter node at point to rename"))
    (unless (equal "variable" (treesit-node-type node))
      (error "Cannot rename non-variable. Node at point is: %s" (treesit-node-type node)))
    (let ((topmost-parent
           (treesit-utils-find-topmost-parent
            node
            (lambda (x)
              (member (treesit-node-type x)
                      '("function"
                        "signature"
                        "data_type"
                        "newtype"
                        "bind"
                        "class"))))))
      (unless topmost-parent
        (error "Internal error: failed to find scoping node above variable at point"))
      (let* ((closest-scope
              (if (equal "class" (treesit-node-type topmost-parent))
                  (treesit-utils-find-topmost-parent
                   node
                   (lambda (x)
                     (member (treesit-node-type x)
                             '("function"
                               "signature"
                               "data_type"
                               "newtype"
                               "bind"))))
                topmost-parent))
             (ovs nil)
             (mod-hooks (list #'haskell-ts-rename--modification-hook))
             (node-text (treesit-node-text node
                                           t ;; no properties
                                           ))
             (node-text-len (length node-text))
             (relevant-scopes (list closest-scope)))

        (pcase (treesit-node-type closest-scope)
          ("function"
           ;; Find signature for current function by searching backwards.
           (when-let ((func-name (treesit-node-child-by-field-name closest-scope "name")))
             (let ((continue t)
                   (n (treesit-node-prev-sibling closest-scope)))
               (while (and n
                           continue)
                 (pcase (treesit-node-type n)
                   ("function"
                    ;; Found previous function’s definition.
                    (setf continue nil))
                   ("signature"
                    (when-let ((sig-name (treesit-node-child-by-field-name n "name")))
                      (when (equal (treesit-node-text func-name)
                                   (treesit-node-text sig-name))
                        (push n relevant-scopes)
                        (setf continue nil)))))
                 (setf n (treesit-node-prev-sibling n))))))
          ("signature"
           ;; Find function for current signature by searching forwards.
           (when-let ((func-name (treesit-node-child-by-field-name closest-scope "name")))
             (let ((continue t)
                   (n (treesit-node-next-sibling closest-scope)))
               (while (and n
                           continue)
                 (pcase (treesit-node-type n)
                   ("function"
                    (when-let ((sig-name (treesit-node-child-by-field-name n "name")))
                      (when (equal (treesit-node-text func-name)
                                   (treesit-node-text sig-name))
                        (push n relevant-scopes)
                        (setf continue nil))))
                   ("signature"
                    ;; Found next function’s signature.
                    (setf continue nil)))
                 (setf n (treesit-node-next-sibling n)))))))

        (dolist (scope relevant-scopes)
          (let ((variables
                 (treesit-query-capture scope
                                        haskell-ts-rename--variable-query
                                        nil
                                        nil
                                        t ;; Don’t capture names, we only ask for variables.
                                        )))
            (dolist (v variables)
              (let ((start (treesit-node-start v))
                    (end (treesit-node-end v)))
                (when (and (= (- end start) node-text-len)
                           (equal node-text (buffer-substring-no-properties start end)))
                  (let ((ov (make-overlay start
                                          end
                                          nil
                                          nil ;; Include text inserted at front
                                          t ;; Include text inserted at back
                                          )))
                    (overlay-put ov 'face                  'haskell-ts-rename-candidate-face)
                    (overlay-put ov 'modification-hooks    mod-hooks)
                    (overlay-put ov 'insert-in-front-hooks mod-hooks)
                    (overlay-put ov 'insert-behind-hooks   mod-hooks)
                    (overlay-put ov 'keymap                haskell-ts-rename-keymap)
                    (push ov ovs)))))))
        (mapc #'delete-overlay haskell-ts-rename--current-overlays)
        (setf haskell-ts-rename--current-overlays nil)
        (when ovs
          (setf haskell-ts-rename--current-overlays ovs
                haskell-ts-rename--should-restore-vim-normal-mode? vim-normal-mode)

          (goto-char (treesit-node-end node))
          (vim-activate-insert-mode))))))

(defun haskell-ts-rename--modification-hook (ov changed beg end &optional len)
  (when changed
    (let ((ov-start (overlay-start ov))
          (ov-end (overlay-end ov)))
      (unless (eq ov-start ov-end)
        (let ((current-text (buffer-substring ov-start ov-end))
              (inhibit-modification-hooks t))
          (save-excursion
            (dolist (o haskell-ts-rename--current-overlays)
              (unless (eq o ov)
                (let ((start (overlay-start o)))
                  (goto-char start)
                  (delete-region start (overlay-end o))
                  (insert current-text)
                  (setf (overlay-start o) start
                        (overlay-end o) (point)))))))))))

;;;###autoload
(defun haskell-ts-rename-done ()
  (interactive)
  (mapc #'delete-overlay haskell-ts-rename--current-overlays)
  (setf haskell-ts-rename--current-overlays nil)
  (when (and haskell-ts-rename--should-restore-vim-normal-mode?
             vim-insert-mode)
    (vim-activate-normal-mode)))

(provide 'haskell-rename)

;; Local Variables:
;; End:

;; haskell-rename.el ends here
