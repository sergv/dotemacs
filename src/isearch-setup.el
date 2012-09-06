;;; isearch-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 14 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'util-vim-replace)

(defun isearch-remove-highlight ()
  "Remove any isearch highlighting."
  (interactive)
  (when isearch-lazy-highlight
    (isearch-lazy-highlight-cleanup))
  (isearch-dehighlight))


;; (defun isearch-yank-regexp (regexp)
;;   "Pull REGEXP into search regexp."
;;   (let ((isearch-regexp nil)) ;; Dynamic binding of global.
;;     (isearch-yank-string regexp))
;;   (unless isearch-regexp
;;     (isearch-toggle-regexp))
;;   (isearch-search-and-update))


;; I-search with initial contents
(defvar *isearch-initial-string* nil)
(defvar *isearch-direction* nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook #'isearch-set-initial-string)
  (setf isearch-string *isearch-initial-string*
        isearch-message (mapconcat #'isearch-text-char-description
                                   *isearch-initial-string*
                                   "")
        isearch-yank-flag t)
  (let ((p (point)))
    (isearch-push-state)
    (isearch-search-and-update)
    (unless (= p (point))
      ;; move point only if it wasn't moved yet
      (case *isearch-direction*
        (forward (isearch-repeat-forward))
        (backward (isearch-repeat-backward)))))
  (isearch-done)
  (setf *isearch-direction* nil))

(defun isearch-symbol-at-point (non-strict)
  "Utility function for `isearch-symbol-at-point-forward' and
`isearch-symbol-at-point-backward'."
  (let* ((sym (symbol-at-point)))
    (if (null sym)
      (message "No symbol at point")
      (progn
        (vim:save-position)
        (setf *isearch-initial-string*
              (concat (unless non-strict "\\_<")
                      (regexp-quote (symbol-name sym))
                      (unless non-strict "\\_>")))
        (add-hook 'isearch-mode-hook #'isearch-set-initial-string)
        ;; don't use recursive edit here
        (isearch-forward-regexp nil t)))))

(defun isearch-symbol-at-point-forward (&optional non-strict)
  "Interactive search forward for the sym at point."
  (interactive "p")
  (setf *isearch-direction* 'forward)
  (isearch-symbol-at-point non-strict))

(defun isearch-symbol-at-point-backward (&optional non-strict)
  "Interactive search backward for the sym at point."
  (interactive "p")
  (setf *isearch-direction* 'backward)
  (isearch-symbol-at-point non-strict))

;;
;; (defun my-isearch-word-at-point ()
;;   (interactive)
;;   (call-interactively 'isearch-forward-regexp))
;;
;; (defun my-isearch-yank-word-hook ()
;;   (when (equal this-command 'my-isearch-word-at-point)
;;     (let ((string (concat "\\<"
;;                           (buffer-substring-no-properties
;;                            (progn (skip-syntax-backward "w_") (point))
;;                            (progn (skip-syntax-forward "w_") (point)))
;;                           "\\>")))
;;       (setq isearch-string string
;;             isearch-message
;;             (concat isearch-message
;;                     (mapconcat 'isearch-text-char-description
;;                                string ""))
;;             isearch-yank-flag t)
;;       (isearch-search-and-update))))
;;
;; (remove-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

;; (defun isearch-search-for-symbol-at-point (&optional non-strict)
;;   "Search for symbol at point."
;;   (interactive)
;;   (let ((sym (symbol-at-point)))
;;     (if (null sym)
;;       (message "No symbol at point")
;;       (progn
;;         (isearch-yank-regexp
;;          (concat (unless non-strict "\\_<")
;;                  (regexp-quote sym)
;;                  (unless non-strict "\\_>")))))))



(def-keys-for-map isearch-mode-map
  ("<escape>" isearch-abort)
  ("<f5>"     isearch-del-char)
  ("<f6>"     isearch-yank-word-or-char)
  ("<down>"   isearch-repeat-forward)
  ("<up>"     isearch-repeat-backward)

  ("C-<down>" isearch-ring-advance)
  ("C-<up>"   isearch-ring-retreat)
  ("S-<down>" isearch-ring-advance)
  ("S-<up>"   isearch-ring-retreat)

  ;; paste
  ("C-p"      isearch-yank-kill)
  ("C-e"      isearch-edit-string)
  ("C-w"      nil)
  ("C-y"      nil)

  ;; ("<end>"    isearch-repeat-forward)
  ;; ("<home>"   isearch-repeat-backward)
  ;; pgup/pgdown
  ("<next>"   isearch-repeat-forward)
  ("<prior>"  isearch-repeat-backward))

(def-keys-for-map minibuffer-local-isearch-map
  ("<escape>"    isearch-abort)
  ("<up>"        next-history-element)
  ("<down>"      previous-history-element)
  ("<backspace>" nil))





(provide 'isearch-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; isearch-setup.el ends here
