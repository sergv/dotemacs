;; haskell-rename.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 19 May 2024
;; Description:

(require 'haskell-ts-getters)
(require 'haskell-ts-mode)

(require 'buffer-span)
(require 'treesit)
(require 'treesit-setup)
(require 'treesit-utils)

(defconst haskell-ts-rename--renameable-identifier-query
  (haskell-ts-query-compile
   '([(variable) (name) (constructor) (operator)] @variable)))

(defconst haskell-ts-rename--pragma-query
  (haskell-ts-query-compile
   '((pragma) @pragma)))

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

(defun haskell-ts-rename--find-initial-node (node)
  (let ((typ (treesit-node-type node))
        (parent nil))
    (cond
      ((member typ '("variable" "name" "constructor" "operator" "pragma"))
       node)
      ((and (string= typ "(")
            (string= (treesit-node-type (setf parent
                                              (treesit-node-parent node)))
                     "prefix_id"))
       (haskell-ts-getters--extract-prefix-id-operator parent))
      (t
       (error "Cannot rename non-variable. Attempted to rename node: %s" node)))))

;;;###autoload
(defun haskell-ts-rename-at-point ()
  (interactive "*")
  (let* ((initial-node (haskell-ts-rename--find-initial-node
                        (treesit-node-at (point) haskell-ts-buffer-lang)))
         (initial-node-type (treesit-node-type initial-node))
         (is-pragma? (string= initial-node-type "pragma"))
         (inline-pragma (when is-pragma?
                          (treesit-haskell-parse-inline-pragma initial-node)))
         (initial-name-span (if inline-pragma
                                (make-buffer-span
                                 (treesit-haskell-inline-pragma/function-name-start inline-pragma)
                                 (treesit-haskell-inline-pragma/function-name-end inline-pragma))
                              initial-node)))
    (unless initial-node
      (error "No treesitter node at point to rename"))
    (when (and is-pragma? (not inline-pragma))
      (error "Cannot rename in non-inline pragma: %s" (treesit-node-text-no-properties-unsafe initial-node)))

    (let ((closest-scope
           (treesit-utils-find-topmost-parent
            initial-node
            (lambda (x)
              (member (treesit-node-type x)
                      '("function"
                        "signature"
                        "data_type"
                        "newtype"
                        "bind"
                        "class"
                        "instance"
                        "type_family"
                        "pragma"))))))
      (unless closest-scope
        (error "Internal error: failed to find scoping node above variable at point"))
      (let* ((ovs nil)
             (mod-hooks (list #'haskell-ts-rename--modification-hook))
             (node-text (if inline-pragma
                            (treesit-haskell-inline-pragma/function-name inline-pragma)
                          (treesit-node-text initial-node
                                             t ;; no properties
                                             )))
             (node-text-len (length node-text))
             (relevant-scopes (list (if inline-pragma
                                        (if (treesit-haskell-inline-pragma-strictly-inside-node? inline-pragma closest-scope)
                                            closest-scope
                                          inline-pragma)
                                      closest-scope)))
             (editing-start-pos
              (if inline-pragma
                  (treesit-haskell-inline-pragma/function-name-end inline-pragma)
                (treesit-node-end initial-node))))

        (pcase (treesit-node-type closest-scope)
          ((or "instance" "class")
           (dolist (internal-pragma (treesit-query-capture closest-scope
                                                           (haskell-ts-query-resolve
                                                            haskell-ts-rename--pragma-query)
                                                           nil
                                                           nil
                                                           t ;; Don’t capture names.
                                                           ))
             (when-let* ((parsed (treesit-haskell-parse-inline-pragma internal-pragma))
                         ((treesit-haskell-inline-pragma-name-same-as-span? parsed initial-name-span)))
               (push parsed relevant-scopes))))
          ((or "function" "bind" "signature" "pragma")
           (when-let ((func-name-span (if inline-pragma
                                          initial-name-span
                                        (haskell-ts-getters--extract-function-bind-or-signature-name-resolving-operator closest-scope))))
             ;; Find signature for current function by searching backwards.
             (let ((continue? t)
                   (n (treesit-node-prev-sibling closest-scope))
                   (earliest-start (treesit-node-start closest-scope)))
               (while (and n
                           continue?)
                 (pcase (treesit-node-type n)
                   ((or "function" "bind")
                    ;; Found previous function’s definition.
                    (setf continue? nil))
                   ("pragma"
                    (when-let* ((pragma (treesit-haskell-parse-inline-pragma n))
                                ((treesit-haskell-inline-pragma-name-same-as-span? pragma func-name-span)))
                      (push pragma relevant-scopes)))
                   ("signature"
                    (when-let ((sig-name (haskell-ts-getters--extract-function-bind-or-signature-name-resolving-operator n)))
                      (when (buffer-span-texts-in-current-buffer= func-name-span sig-name)
                        (push n relevant-scopes)))))
                 (setf earliest-start (treesit-node-start n)
                       n (treesit-node-prev-sibling n)))
               (when (and (not inline-pragma)
                          earliest-start)
                 (save-excursion
                   ;; Work around grammar quirk where first function’s pragma is placed
                   ;; outside declarations block for whole module.
                   (goto-char earliest-start)
                   (skip-whitespace-backward)
                   (when-let* ((node (treesit-node-at (point)))
                               (pragma (treesit-haskell-parse-inline-pragma node))
                               ((treesit-haskell-inline-pragma-name-same-as-span? pragma func-name-span)))
                     (push pragma relevant-scopes)))))

             ;; Find function for current signature by searching forwards.
             (let ((continue? t)
                   (n (treesit-node-next-sibling closest-scope)))

               ;; Work around grammar quirk where first function’s pragma is placed
               ;; outside declarations block for whole module.
               (when (and n
                          (string= (treesit-node-type n) "declarations"))
                 (setf n (car (treesit-node-children n))))
               ;; (unless n
               ;;   (let ((tmp closest-scope)
               ;;         (continue2? t))
               ;;     (message "tmp = %s, line = %s" tmp (debug-current-line))
               ;;     (while (and tmp
               ;;                 continue2?)
               ;;       (goto-char (treesit-node-end tmp))
               ;;       (skip-whitespace-forward)
               ;;       (setf tmp (treesit-node-at (point)))
               ;;       (message "loop: tmp = %s, line = %s" tmp (debug-current-line))
               ;;       (when (and tmp
               ;;                  (not (treesit-haskell--is-comment-node-type? (treesit-node-type tmp))))
               ;;         (setf continue2? nil)))
               ;;     (setf n tmp)))
               (while (and n
                           continue?)
                 (pcase (treesit-node-type n)
                   ((or "function" "bind")
                    (when-let ((func-name (haskell-ts-getters--extract-function-bind-or-signature-name-resolving-operator n)))
                      (when (buffer-span-texts-in-current-buffer= func-name-span func-name)
                        (push n relevant-scopes))))
                   ("pragma"
                    (when-let* ((pragma (treesit-haskell-parse-inline-pragma n))
                                ((treesit-haskell-inline-pragma-name-same-as-span? pragma func-name-span)))
                      (push pragma relevant-scopes)))
                   ("signature"
                    (when-let ((sig-name (haskell-ts-getters--extract-function-bind-or-signature-name-resolving-operator n)))
                      (when (buffer-span-texts-in-current-buffer= func-name-span sig-name)
                        (push n relevant-scopes)))
                    ;; Found next function’s signature.
                    ;; (setf continue? nil)
                    ))
                 (setf n (treesit-node-next-sibling n)))))))

        (dolist (scope relevant-scopes)
          (let ((bounds
                 (cond
                   ((treesit-node-p scope)
                    (--map (cons (treesit-node-start it) (treesit-node-end it))
                           (treesit-query-capture scope
                                                  (haskell-ts-query-resolve
                                                   haskell-ts-rename--renameable-identifier-query)
                                                  nil
                                                  nil
                                                  t ;; Don’t capture names, we only ask for variables.
                                                  )))
                   ((treesit-haskell-inline-pragma-p scope)
                    (list (cons (treesit-haskell-inline-pragma/function-name-start scope)
                                (treesit-haskell-inline-pragma/function-name-end scope))))
                   (t
                    (error "Invalid renaming scope: %s" scope)))))
            (dolist (v bounds)
              (let ((start (car v))
                    (end (cdr v)))
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

          (goto-char editing-start-pos)
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
