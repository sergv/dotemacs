;; python-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 January 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl))
(require 'macro-util)
(require 'comint-setup)
(require 'browse-kill-ring-setup)
(require 'common)
(require 'compile)
(require 'ctags-setup)

(require 'python-common)
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
      (concat "sys.stdout.write("
                     "\"%s\".join(get_ipython().Completer.complete(\"\"\"%s\"\"\")[1])"
                     "+ \"\\x00\\n\""
                     ") #PYTHON-MODE SILENT\n")
      ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
      ;; "';'.join(get_ipython().Completer.complete('''%s''')[1]) #PYTHON-MODE SILENT\n"
      )

(defvar python-setup-shell-completion-code
  "import sys #PYTHON-MODE SILENT")
(defvar python-setup-numpy-code
  "try:
    import numpy as np
except ImportError:
    print(\"Numpy is not accessible\")")
(add-to-list 'python-shell-setup-codes
             'python-setup-shell-completion-code)
(add-to-list 'python-shell-setup-codes
             'python-setup-numpy-code)

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
           (pattern (buffer-substring-no-properties beg end))
           (completions nil)
           completion
           (comint-preoutput-filter-functions
             (append comint-preoutput-filter-functions
                     `(ansi-color-filter-apply
                       ,(lambda (string)
                          (setq completion-accum (concat completion-accum string))
                          "")))))
      (process-send-string python-process
                           (format python-shell-completion-string-code sep pattern))
      (accept-process-output python-process)
      (setq completions
            (split-string (substring completion-accum
                                     0
                                     (position ?\0 completion-accum))
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
          (insert (string-trim-whitespace completion)))))))


(setenv "PYTHONPATH"
        (mapconcat
         #'identity
         (list
          "/home/sergey/projects/python/modules/"
          "/home/sergey/projects/python/webcam/collect-data/local/lib/python2.7/site-packages")
         ":"))
(setenv "IPYTHONDIR" (concat +prog-data-path+ "/ipython"))

(register-python-hideshow 'python-mode)


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

;;;; actual setups

(defun python-setup ()
  (python-common-setup)
  (add-hook 'after-save-hook #'make-script-file-exec nil t)
  (setup-ctags-symbols))

(define-switch-to-interpreter
    switch-to-python
  ((concat "*" python-shell-buffer-name "*"))
  (run-python (python-shell-assemble-command))
  :doc "Pop to python repl."
  :save-buffer t
  :error-msg "Can't switch to python repl"
  :try-count 2)


(defun inferior-python-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-nxhtml-menu nil)
  (init-repl)
  (comint-setup)
  (comint-read-input-ring t)

  ;; make ' a string delimiter ;; is this necessary?
  (modify-syntax-entry ?' "\"")
  (modify-syntax-entry ?_ "_")
  (modify-syntax-entry ?. ".")

  ;; these triple-quotes greatly confuse autopair when run in
  ;; python repl so it's best to avoid them altogether
  ;; moreover I virtually never use triple quotes at the repl

  (setf autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))
  (autopair-mode 1)

  (setf tab-width 4)
  (setq-local forward-sexp-function nil)

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

;;;; end

(provide 'python-setup)

;; Local Variables:
;; End:

;; python-setup.el ends here
