;; python-setup.el --- -*- lexical-binding: t; -*-

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
(require 'compile)

(require 'python-highlight)
(require 'python)

(font-lock-add-keywords 'python-mode
                        *python-font-lock-keywords*
                        'set)
(font-lock-add-keywords 'inferior-python-mode
                        *python-font-lock-keywords*
                        'set)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (setf python-indent-offset 4
;;       ;; ipython setup
;;       python-shell-buffer-name "python repl"
;;       python-shell-interpreter "python"
;;       python-shell-internal-buffer-name " python-repl-internal"
;;       python-shell-interpreter-args "-i"
;;
;;       ;; python-shell-prompt-regexp "> "
;;       ;; python-shell-prompt-block-regexp ">> "
;;       ;; python-shell-prompt-output-regexp ""
;;
;;       python-shell-enable-font-lock t
;;
;;       ;;  python-shell-completion-setup-code
;;       ;;    "from IPython.core.completerlib import module_completion"
;;       ;;  python-shell-completion-module-string-code
;;       ;;    "';'.join(module_completion('''%s'''))\n"
;;       ;;  python-shell-completion-string-code
;;       ;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;
;;       )

(setf python-indent-offset 4
      ;; ipython setup
      python-shell-buffer-name "python repl"
      python-shell-interpreter "ipython"
      python-shell-internal-buffer-name " ipython-repl-internal"
      python-shell-interpreter-args "--pprint --color-info --colors Linux --nosep --no-confirm-exit --deep-reload"

      python-shell-prompt-regexp "> "
      python-shell-prompt-block-regexp ">> "
      python-shell-prompt-output-regexp ""

      python-shell-enable-font-lock t

      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
      "';'.join(get_ipython().Completer.complete('''%s''')[1]) #PYTHON-MODE SILENT\n"
      )

(defun python-complete ()
  "Try to complete the python symbol before point. Only knows about the stuff
in the current *Python* session."
  (interactive)
  (let ((completion-accum nil))
    (let* ((sep ";")
           (python-process (or (get-buffer-process (current-buffer))
                               (get-buffer-process (get-buffer (concat "*" python-shell-buffer-name "*")))
                               (error "Python REPL not found")
                               ;;XXX hack for .py buffers
                               ;; (get-process py-which-bufname)
                               ))
           (prompt-beg (overlay-end comint-last-prompt-overlay))
           ;; XXX currently we go backwards to find the beginning of an
           ;; expression part; a more powerful approach in the future might be
           ;; to let ipython have the complete line, so that context can be used
           ;; to do things like filename completion etc.
           (beg (save-excursion (skip-chars-backward "a-z0-9A-Z_./" (point-at-bol))
                                (point)))
           (end (point))
           (pattern (buffer-substring-no-properties prompt-beg end))
           (completions nil)
           completion
           (comint-preoutput-filter-functions
             (append comint-preoutput-filter-functions
                     `(ansi-color-filter-apply
                       ,(lambda (string)
                          (setq completion-accum (concat completion-accum string))
                          ""))))
           (completion-command-template
             "';'.join(get_ipython().Completer.complete('''%s''')[1]) #PYTHON-MODE SILENT\n"))
      (process-send-string python-process
                           (format completion-command-template pattern))
      (accept-process-output python-process)
      (setq completions
            (split-string (substring completion-accum
                                     0
                                     (position ?\n completion-accum))
                          sep))

      (cond
        ((null completions)
         (error "Can't find completion for \"%s\"" pattern))
        ((null? (cdr completions))
         ;; got only one completion
         (setf completion (car completions)))
        (t
         (setf completion (completing-read-vanilla ""
                                                   completions
                                                   nil
                                                   nil
                                                   pattern))))
      (if (or (string= pattern completion)
              (= 0 (length completion)))
        (error "No completions found for \"%s\"" pattern)
        (progn
          (delete-region beg end)
          (insert completion))))))


(setenv "PYTHONPATH"
        (mapconcat
         #'identity
         (list
          "/home/sergey/projects/python/modules/"
          "/home/sergey/projects/python/webcam/collect-data/local/lib/python2.7/site-packages")
         ":"))
(setenv "IPYTHONDIR" (concat +prog-data-path+ "/ipython"))

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
   (let* ((end (point))
          (begin (line-beginning-position)))
     (when begin
       (let ((state (parse-partial-sexp begin end)))
         (or (elt state 3)
             (elt state 4)))))))

(defun python-point-inside-string? ()
  "Return t if point is positioned inside a string."
  (save-excursion
   (let* ((end (point))
          (begin (line-beginning-position)))
     (when begin
       (elt (parse-partial-sexp begin end)
            3)))))

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
                     (rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                             ">>=" "<<=" "&=" "^=" "|=")
                         (regexp "[^=]"))
                     :require-one-or-more-spaces t)

(defun python-backward-sexp (&optional count)
  (interactive "p")
  (python-nav-forward-sexp (- (or count 1))))

(defun python-forward-sexp (&optional count)
  (interactive "p")
  (python-nav-forward-sexp (or count 1)))

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

  (setq-local compilation-error-regexp-alist
              (list
               (list +python-run-error-regexp+
                     1 ;; FILE
                     2 ;; LINE
                     nil ;; COLUMN - no column present
                     nil ;; TYPE - error
                     )))

  (setq-local *compilation-jump-error-regexp*
              +python-run-error-regexp+)

  (setq-local compilation-first-column t)
  (setq-local compilation-disable-input t)
  (setq-local compilation-scroll-output t)

  (def-keys-for-map python-run-mode-map
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

  ;; ;; make ' a string delimiter
  ;; (modify-syntax-entry ?' "\"")

  ;; make _ a symbol constituent, mostly for me
  (modify-syntax-entry ?_ "_")
  ;; make . a symbol constituent, mostly for me too
  (modify-syntax-entry ?. ".")

  (setf autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))
  (autopair-mode 1)
  (hs-minor-mode 1)

  ;; autopair relies on default `forward-sexp' to be accessible, but
  ;; python sets it to `python-nav-forward-sexp' which
  ;; a. also navigates python statements as "sexps"
  ;; b. somewhat heavy and causes noticeable delay on inserting (, " or """
  ;; => bad alternative for `forward-sexp', but nice function on its own right
  (setq-local forward-sexp-function nil)

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    (", d"     pylookup-lookup)
    (", ?"     pylookup-lookup)
    ("<f1>"    python-shell-send-buffer)
    ("<f9>"    python-run-script)
    ("S-<f9>"  python-check)

    ("j"       python-shell-send-defun)

    ("SPC SPC" switch-to-python)
    (", s s"   vim:replace-symbol-at-point)

    ("z o"     hs-show-block)
    ("z c"     hs-hide-block)
    ("z C"     python-hide-all)
    ("z O"     hs-show-all))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("<f1>"  python-shell-send-region)
    ("j"     python-shell-send-region)
    ("g a"   nil)
    ("g a =" python-align-on-equals))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("g t"    end-of-defun)
    ("g n"    beginning-of-defun)
    ("<up>"   python-nav-backward-block)
    ("<down>" python-nav-forward-block)

    ("="      python-nav-backward-up-list)
    ("q"      python-nav-up-list)
    ("<home>" python-backward-sexp)
    ("<end>"  python-forward-sexp)

    ("*"      search-for-symbol-at-point-forward)
    ("#"      search-for-symbol-at-point-backward))

  (python-abbrev+-setup)

  ;; pabbrev isn't powerful enough
  ;; (pabbrev-mode 1)
  ;; (def-keys-for-map (vim:normal-mode-local-keymap
  ;;                     vim:insert-mode-local-keymap)
  ;;   ("M-/"     pabbrev-show-menu ;; pabbrev-expand-maybe
  ;;              ))
  ;; (when pabbrev-mode
  ;;   (pabbrev-scavenge-buffer))

  (setup-outline-headers :header-start "^[ \t]*"
                         :header-symbol "#"
                         :length-min 3
                         :length-max 9))

;; (define-switch-to-interpreter
;;     switch-to-python
;;   ("*IPython*")
;;   (ipython)
;;   :doc "Pop to ipython repl."
;;   :save-buffer t
;;   :error-msg "Can't switch to IPython repl"
;;   :try-count 2)

(define-switch-to-interpreter
    switch-to-python
  ((concat "*" python-shell-buffer-name "*"))
  (run-python (python-shell-assemble-command))
  :doc "Pop to python repl."
  :save-buffer t
  :error-msg "Can't switch to python repl"
  :try-count 2)


;; (define-switch-to-interpreter
;;     switch-to-python
;;   ("*Python*")
;;   (py-shell)
;;   :doc "Pop to python repl."
;;   :save-buffer t
;;   :error-msg "Can't switch to Python repl"
;;   :try-count 2)

(defun inferior-python-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-nxhtml-menu nil)
  (init-repl)
  (comint-setup)
  (comint-read-input-ring t)

  ;; make ' a string delimiter
  (modify-syntax-entry ?' "\"")
  (modify-syntax-entry ?_ "_")

  ;; these triple-quotes greatly confuse autopair when run in
  ;; python repl so it's best to avoid them altogether
  ;; moreover I virtually never use triple quotes at the repl

  (setf autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))
  (autopair-mode 1)

  (setf tab-width 4)

  (setf vim:normal-mode-local-keymap           (make-keymap)
        ;; vim:visual-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        ;; vim:motion-mode-local-keymap           (make-keymap)
        )

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("M-p"      browse-kill-ring)
    ("M-P"      browse-comint-input-history)

    ("C-SPC"    comint-clear-buffer-above-prompt)
    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)

    ("<tab>"    python-complete))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt)))

(add-hook 'python-mode-hook #'python-setup)
(add-hook 'inferior-python-mode-hook #'inferior-python-setup)
;; (add-hook 'py-shell-hook #'python-interpreter-setup)

;; this would be run in py-shell-hook anyway
;; (add-hook 'python-repl-mode-hook #'python-interpreter-setup)

;;;; end

(provide 'python-setup)

;; Local Variables:
;; End:

;; python-setup.el ends here
