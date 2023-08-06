;; tabbar-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 October 2021
;; Description:

(require 'el-patch)

(defsubst tabbar--at-least-2-elements? (x)
  (cdr x))

;;;###autoload
(defun next-tab-or-frame (arg)
  (interactive "p")
  (if (tabbar--at-least-2-elements? (tab-bar-tabs))
      (tab-next arg)
    (next-f arg)))

;;;###autoload
(defun prev-tab-or-frame (arg)
  (interactive "p")
  (if (tabbar--at-least-2-elements? (tab-bar-tabs))
      (tab-previous arg)
    (prev-f arg)))

;;;; tab bar

(defvar-local tab-bar--truncated-buf-name nil
  "Cached truncated buffer name to show in tab bar.")

(defun tab-bar--get-truncated-buf-name (buf)
  (aif (buffer-local-value 'tab-bar--truncated-buf-name buf)
      it
    (with-current-buffer buf
      (setf tab-bar--truncated-buf-name
            (let ((name (buffer-name buf)))
              (if (< (length name) tab-bar-tab-name-truncated-max)
                  name
                (truncate-string-to-width name
                                          tab-bar-tab-name-truncated-max
                                          nil
                                          nil
                                          (if (char-displayable-p ?…) "…" "..."))))))))

(defun tab-bar-tab-name-current-truncated-with-count ()
  "Like ‘tab-bar-tab-name-current-with-count’ but truncates name if its tool long."
  (let* (;; (count
         ;;  (length
         ;;   (if (window-live-p lv-wnd)
         ;;       (delq lv-wnd (window-list-1 nil 'nomini))
         ;;     (window-list-1 nil 'nomini))))
         (buf (window-buffer (minibuffer-selected-window)))
         (name (tab-bar--get-truncated-buf-name buf)))
    name
    ;; (if (> count 1)
    ;;     (format "%s (%d)" name count)
    ;;   name)
    ))

;; hide tab bar if there's only one tab
(setf tab-bar-show 1
      ;; Open current buffer in new tab
      tab-bar-new-tab-choice t
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-tab-hints t
      tab-bar-tab-name-truncated-max 32
      tab-bar-tab-name-function #'tab-bar-tab-name-current-truncated-with-count
      tab-bar-new-tab-to 'right
      tab-bar-separator " "

      tab-bar-format '(tab-bar-format-tabs)
      tab-bar-auto-width nil)

(el-patch-defun tab-bar--format-tab (tab i)
  "Format TAB using its index I and return the result as a keymap."
  (append
   (el-patch-swap
     `((,(intern (format "sep-%i" i)) menu-item ,(tab-bar-separator) ignore))
     (when (< 1 i)
       `((,(intern (format "sep-%i" i)) menu-item ,(tab-bar-separator) ignore))))
   (cond
    ((eq (car tab) 'current-tab)
     `((current-tab
        menu-item
        ,(funcall tab-bar-tab-name-format-function tab i)
        ignore
        :help "Current tab")))
    (t
     `((,(intern (format "tab-%i" i))
        menu-item
        ,(funcall tab-bar-tab-name-format-function tab i)
        ,(alist-get 'binding tab)
        :help "Click to visit tab"))))
   (when (alist-get 'close-binding tab)
     `((,(if (eq (car tab) 'current-tab) 'C-current-tab (intern (format "C-tab-%i" i)))
        menu-item ""
        ,(alist-get 'close-binding tab))))))

;; The tab bar will appear automatically once new tab is created
;; thanks to setting ‘tab-bar-show’ to 1.
;; (tab-bar-mode 1)

(provide 'tabbar-setup)

;; Local Variables:
;; End:

;; tabbar-setup.el ends here
