;; tabbar-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 October 2021
;; Description:

(eval-when-compile
  (require 'dash)
  (require 'macro-util)
  (require 'set-up-platform)
  (require 'tab-bar))

(require 'common)
(require 'el-patch)

(defsubst tabbar--at-least-2-elements? (x)
  (cdr x))

(setf tab-bar-select-tab-modifiers nil)

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

(defun next-w (n)
  "Go to next Nth window"
  (interactive "p")
  (if (eq 1 (count-windows))
      (next-tab-or-frame n)
    (other-window n)))

(defun prev-w (n)
  "Go to previous Nth window"
  (interactive "p")
  (if (eq 1 (count-windows))
      (prev-tab-or-frame n)
    (other-window (- n))))

;; tab-bar-mode--tab-key-bind

;;;###autoload
(defun tab-bar-init ()
  (remove-key! (key-parse "t") ctl-x-map)
  (map-keymap-internal (lambda (key _def)
                         (remove-key! (vector key) tab-bar-mode-map))
                       tab-bar-mode-map)

  (setf tab-bar-mode-map (make-sparse-keymap)
        tab-prefix-map (make-sparse-keymap)))

;;;###autoload
(with-eval-after-load 'tab-bar
  (tab-bar-init))

;;;; tab bar

(defvar-local tab-bar--truncated-buf-name nil
  "Cached truncated buffer name to show in tab bar.")

(defun tab-bar--reset-truncated-buf-name (new-name &optional _unique)
  (awhen (get-buffer new-name)
    (setf (buffer-local-value 'tab-bar--truncated-buf-name it) nil)))

(advice-add 'rename-buffer :after #'tab-bar--reset-truncated-buf-name)

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
      tab-bar-tab-hints t
      tab-bar-tab-name-truncated-max 32
      tab-bar-tab-name-function #'tab-bar-tab-name-current-truncated-with-count
      tab-bar-new-tab-to 'right
      tab-bar-separator " "

      tab-bar-format '(tab-bar-format-tabs)
      tab-bar-auto-width nil)

(when-emacs-version (<= 30 it)
  (el-patch-defun tab-bar--format-tab (tab i)
    "Format TAB using its index I and return the result as a keymap."
    (append
     (el-patch-wrap 2 0
       (when (< 1 i)
         `((,(intern (format "sep-%i" i)) menu-item ,(tab-bar-separator) ignore))))
     (cond
       ((eq (car tab) 'current-tab)
        `((current-tab
           menu-item
           ,(funcall tab-bar-tab-name-format-function tab i)
           ignore
           :help ,(alist-get 'name tab))))
       (t
        `((,(intern (format "tab-%i" i))
           menu-item
           ,(funcall tab-bar-tab-name-format-function tab i)
           ,(alist-get 'binding tab)
           :help ,(alist-get 'name tab)))))
     (when (alist-get 'close-binding tab)
       `((,(if (eq (car tab) 'current-tab) 'C-current-tab
             (intern (format "C-tab-%i" i)))
          menu-item ""
          ,(alist-get 'close-binding tab)))))))

;;;###autoload
(defun tab-bar-close-tabs-to-the-right ()
  "Close tabs to the right of the selected one."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-index (tab-bar--current-tab-index tabs)))
    (tab-bar--close-tabs-by-predicate tabs
                                      current-index
                                      (lambda (idx _)
                                        (< current-index idx)))))

;;;###autoload
(defun tab-bar-close-tabs-to-the-left ()
  "Close tabs to the right of the selected one."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-index (tab-bar--current-tab-index tabs)))
    (tab-bar--close-tabs-by-predicate tabs
                                      current-index
                                      (lambda (idx _)
                                        (< idx current-index)))))

(defun tab-bar--close-tabs-by-predicate (tabs _current-index predicate)
  "Close all tabs on the selected frame for which PREDICATE returns t.

PREDICATE should accept two arguments: tab index and tab structure and return
t if that tab should be deleted."
  (let* ((total-tabs (length tabs))
         (frame (selected-frame))
         (num-deleted 0)
         (index 0)
         (removed nil))

    (dolist (tab tabs)
      (when (and (funcall predicate index tab)
                 (let ((last-tab-p (= 1 (- total-tabs num-deleted))))
                   (not (run-hook-with-args-until-success
                         'tab-bar-tab-prevent-close-functions
                         tab
                         last-tab-p))))
        (push (cons index
                    `((frame . ,frame)
                      (index . ,index)
                      (tab . ,tab)))
              removed)
        (run-hook-with-args 'tab-bar-tab-pre-close-functions tab nil)
        ;; O(N^2) complexity, but probably OK since there’s never
        ;; more than 100 tabs (N <= 100).
        (setq tabs (delq tab tabs)
              num-deleted (1+ num-deleted)))
      (setq index (1+ index)))

    (setf tab-bar-closed-tabs
          (append (-map #'cdr
                        ;; Removed tabs should be in order of
                        ;; increasing indices so that undoing
                        ;; their close will put them at correct index.
                        (sort removed
                              :key #'car
                              :lessp #'<
                              :in-place t))
                  tab-bar-closed-tabs))
    (tab-bar-tabs-set tabs)

    ;; Recalculate tab-bar-lines and update frames
    (tab-bar--update-tab-bar-lines)

    (force-mode-line-update)))

;; The tab bar will appear automatically once new tab is created
;; thanks to setting ‘tab-bar-show’ to 1.
;; (tab-bar-mode 1)

(provide 'tabbar-setup)

;; Local Variables:
;; End:

;; tabbar-setup.el ends here
