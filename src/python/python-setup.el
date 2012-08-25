;;; python-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'python-abbrev+)
(require 'macro-util)
(require 'comint-setup)
(require 'browse-kill-ring-setup)
(require 'common)
(require 'outline-headers)

(autoload 'python-mode "python-mode" "Pythom mode." t)

(autoload 'ipython "ipython" "Ipython repl." t)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(defun python-initialize-vars ()
  (setf py-use-number-face-p nil
        py-underscore-word-syntax-p nil)

  (setf py-install-directory (concat +emacs-config-path+
                                     "/third-party/python/python-mode")

        python-python-command "ipython"
        python-python-command-args
        (append python-python-command
                '("--pprint"
                  "--color-info"
                  "--colors"
                  "Linux"
                  "--nosep"
                  "--no-confirm-exit"
                  "--deep-reload")))

  (setenv "PYTHONPATH"
          (mapconcat
           #'identity
           (list
            "/home/sergey/projects/python/modules/"
            "/home/sergey/projects/python/webcam/collect-data/local/lib/python2.7/site-packages")
           ":"))
  (setenv "IPYTHONDIR" "/home/sergey/emacs/prog-data/ipython")

  ;; pymacs
  ;; (setf pymacs-load-path (list ;; (concat
  ;;                         ;;  py-install-directory
  ;;                         ;;  "/Pymacs")
  ;;                         (concat +emacs-config-path+
  ;;                                 "/src/python/modules")))
  ;; (add-to-list 'load-path (concat +emacs-config-path+
  ;;                                 "/src/python/pymacs"))
  ;; (setenv "PYMACS_PYTHON" "python2.7")
  ;; (setenv "PYMACS_INSTALL_DIR" py-install-directory)
  )

(python-initialize-vars)

(eval-after-load
 "python-mode"
 '(progn
   (load-library "python-highlight")
   (load-library "python-customizations")))

;;;; pylookup
(add-to-list 'load-path (concat +emacs-config-path+
                                "/third-party/python/pylookup/"))

(setf pylookup-program (concat +emacs-config-path+
                               "/third-party/python/pylookup/pylookup.py")
      pylookup-db-file (concat +prog-data-path+
                               "/pylookup.db"))

(autoload 'pylookup-lookup "pylookup"
          "Lookup SEARCH-TERM in the Python HTML indexes."
          t)
(autoload 'pylookup-update "pylookup"
          "Run pylookup-update and create the database at `pylookup-db-file'."
          t)

;;;; helper functions, inside-string?, inside-comment?, aligns etc

(defun python-point-inside-string-or-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
   (save-match-data
    (let* ((end (point))
           (begin (line-beginning-position)))
      (when begin
        (let ((state (parse-partial-sexp begin end)))
          (or (elt state 3)
              (elt state 4))))))))

(defun python-point-inside-string? ()
  "Return t if point is positioned inside a string."
  (save-excursion
   (save-match-data
    (let* ((end (point))
           (begin (line-beginning-position)))
      (when begin
        (elt (parse-partial-sexp begin
                                 end)
             3))))))

(defun python-point-inside-string-and-not-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
   (save-match-data
    (let* ((end (point))
           (begin (line-beginning-position)))
      (when begin
        (let ((state (parse-partial-sexp begin
                                         end)))
          (and (elt state 3)
               (null (elt state 4)))))))))

(make-align-function python-align-on-equals
                     "\\(\\+\\|-\\|/\\|\\*\\*?\\|\\)?=[^=]"
                     :require-one-or-more-spaces t)

;;;; run scripts compilation mode

(defconst +python-run-error-regexp+
  "^ *File \"\\([-a-zA-Z _0-9.,()\\\\/]+\\)\", line \\([0-9]+\\), in .*")

(defun python-run-script (&optional use-pypy)
  "Run script associated with current buffer."
  (interactive (list current-prefix-arg))
  (compilation-start (concat (if use-pypy
                               "pypy"
                               "python")
                             " "
                             (file-name-nondirectory (buffer-file-name)))
                     #'python-run-mode))

(define-compilation-mode python-run-mode "Python run"
  "Mode for running python scripts."

  (set (make-local-variable 'compilation-error-regexp-alist)
       (list
        (list +python-run-error-regexp+
              1 ;; FILE
              2 ;; LINE
              nil ;; COLUMN - no column present
              nil ;; TYPE - error
              )))

  (set (make-local-variable '*compilation-jump-error-regexp*)
       +python-run-error-regexp+)

  (set (make-local-variable 'compilation-first-column) t)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) t)

  (def-keys-for-map2 python-run-mode-map
    ("<up>"   compilation-jump-to-prev-error)
    ("<down>" compilation-jump-to-next-error)))

(defun python-hide-all ()
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward (rx bol
                                 (* whitespace)
                                 "def"
                                 (+ whitespace)
                                 (* not-newline)
                                 eol)
                             nil
                             t)
     (goto-char (match-end 0))
     (hs-hide-block)
     (forward-line 1))))

;;;; actual setups

(defun python-setup ()
  (add-hook 'after-save-hook #'make-script-file-exec nil t)
  (init-common :use-yasnippet t
               :use-render-formula t)
  (setf highlight-indentation nil)

  ;; make ' a string delimiter
  (modify-syntax-entry ?' "\"")
  ;; make _ a symbol constituent
  (modify-syntax-entry ?_ "_")

  (setf autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))
  (autopair-mode 1)
  (hs-minor-mode 1)

  ;; (set (make-local-variable 'hs-set-up-overlay)
  ;;      (lambda (overlay)
  ;;        (overlay-put ov 'display "...")))

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap))

  (def-keys-for-map2 vim:normal-mode-local-keymap
    (", d"     pylookup-lookup)
    ("<f1>"    py-execute-buffer-no-switch)
    ("<f9>"    python-run-script)

    ("SPC SPC" switch-to-python)
    (", s s"   vim:replace-symbol-at-point)

    ("z o"     hs-show-block)
    ("z c"     hs-hide-block)
    ("z C"     python-hide-all)
    ("z O"     hs-show-all))

  ;; pabbrev isn't powerful enough
  ;; (pabbrev-mode 1)
  ;; (def-keys-for-map2 (vim:normal-mode-local-keymap
  ;;                     vim:insert-mode-local-keymap)
  ;;   ("M-/"     pabbrev-show-menu ;; pabbrev-expand-maybe
  ;;              ))

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap)
    ("M-/"     ipython-complete))

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:visual-mode-local-keymap)
    ("g t" py-end-of-def-or-class)
    ("g n" py-beginning-of-def-or-class)

    ("*"   search-for-symbol-at-point-forward)
    ("#"   search-for-symbol-at-point-backward))

  (def-keys-for-map2 vim:visual-mode-local-keymap
    ("<f1>"  py-execute-region-no-switch)
    ("j"     py-execute-region-no-switch)
    ("g a"   nil)
    ("g a =" python-align-on-equals))

  (python-abbrev+-setup)
  (when pabbrev-mode
    (pabbrev-scavenge-buffer))

  (setup-outline-headers :header-start "^[ \t]*"
                         :header-symbol "#"
                         :length-min 3
                         :length-max 9))

(define-switch-to-interpreter
    switch-to-python
  ("*IPython*")
  (ipython)
  :doc "Pop to ipython repl."
  :save-buffer t
  :error-msg "Can't switch to IPython repl"
  :try-count 2)

;; (define-switch-to-interpreter
;;     switch-to-python
;;   ("*Python*")
;;   (py-shell)
;;   :doc "Pop to python repl."
;;   :save-buffer t
;;   :error-msg "Can't switch to Python repl"
;;   :try-count 2)

(defun python-interpreter-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-nxhtml-menu nil)
  (init-repl)
  (comint-setup)

  ;; make ' a string delimiter
  (modify-syntax-entry ?' "\"")
  (modify-syntax-entry ?_ "_")

  ;; these triple-quotes greatly confuse autopair when run in
  ;; python repl so it's best to avoid them altogether
  ;; moreover I virtually never use triple quotes at the repl

  ;; (setf autopair-handle-action-fns
  ;;       (list #'autopair-default-handle-action
  ;;             #'autopair-python-triple-quote-action))
  (autopair-mode)

  (setf tab-width 4)

  (setf vim:normal-mode-local-keymap           (make-keymap)
        ;; vim:visual-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        ;; vim:motion-mode-local-keymap           (make-keymap)
        )

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap)
    ("M-p"      browse-kill-ring)
    ("M-P"      browse-comint-input-history)

    ("C-SPC"    comint-clear-buffer-above-prompt)
    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt))

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt)))

(add-hook 'python-mode-hook #'python-setup)
(add-hook 'py-shell-hook #'python-interpreter-setup)
;; this would be run in py-shell-hook anyway
;; (add-hook 'python-repl-mode-hook #'python-interpreter-setup)

;;;; end

(provide 'python-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; python-setup.el ends here
