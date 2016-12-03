;; python-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 January 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(require 'browse-kill-ring-setup)
(require 'comint-setup)
(require 'common)
(require 'compile)
(require 'eproj-setup)
(require 'macro-util)
(require 'outline-headers)

(require 'python)
(require 'python-abbrev+)
(require 'python-highlight)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("python[0-9.]*" . python-mode))

(font-lock-add-keywords 'python-mode
                        *python-font-lock-keywords*
                        'set)
(font-lock-add-keywords 'inferior-python-mode
                        *python-repl-font-lock-keywords*
                        'set)

(setf python-indent-offset 4)

(defparameter python-exec "python3"
  "Python executable to use for script running.")
(put 'python-exec 'safe-local-variable #'string?)
(put 'python-shell-interpreter 'safe-local-variable #'string?)

;; (setf python-shell-buffer-name "python repl"
;;       python-shell-interpreter "python3.3" ;; "python2.7"
;;       python-shell-internal-buffer-name " python-repl-internal"
;;       python-shell-interpreter-args "-i"
;;
;;       python-shell-prompt-regexp ">>> "
;;       python-shell-prompt-block-regexp "\\.\\.\\. "
;;       ;; python-shell-prompt-output-regexp ""
;;
;;       python-shell-enable-font-lock t
;;
;;       python-shell-completion-setup-code
;;       "try:
;;     import readline
;; except ImportError:
;;     def __COMPLETER_all_completions(text): []
;; else:
;;     import rlcompleter
;;     readline.set_completer(rlcompleter.Completer().complete)
;;     def __COMPLETER_all_completions(text):
;;         import sys
;;         completions = []
;;         try:
;;             i = 0
;;             while True:
;;                 res = readline.get_completer()(text, i)
;;                 if not res: break
;;                 i += 1
;;                 completions.append(res)
;;         except NameError:
;;             pass
;;         return completions"
;;
;;       ;;    "from IPython.core.completerlib import module_completion"
;;
;;       python-shell-completion-module-string-code
;;       ""
;;
;;       python-shell-completion-string-code
;;       (concat "sys.stdout.write("
;;               "\"%s\".join(__COMPLETER_all_completions(\"\"\"%s\"\"\"))"
;;               "+ \"\\x00\\n\""
;;               ")\n"))


;; ipython setup
(setf python-shell-buffer-name "python repl"
      python-shell-interpreter "ipython3" ;; "ipython"
      python-shell-internal-buffer-name " ipython-repl-internal"
      python-shell-interpreter-args "--pprint --color-info --colors Linux --nosep --no-confirm-exit --deep-reload"

      ;; python-shell-prompt-regexp "> "
      ;; python-shell-prompt-block-regexp ">> "
      ;; python-shell-prompt-output-regexp ""

      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-block-regexp "   \\.\\.*\\.: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "

      python-shell-enable-font-lock t

      python-shell-completion-setup-code
      "\n\nfrom IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion(\"\"\"%s\"\"\"))\n"
      python-shell-completion-string-code
      ;; use "_ =" to ignore return value of write function
      (concat "_ = sys.stdout.write("
              "\"%s\".join(get_ipython().Completer.complete(\"\"\"%s\"\"\")[1])"
              "+ \"\\x00\\n\""
              "); #PYTHON-MODE SILENT\n"
              ;;" SILENT\n"
              ))

(defparameter python-setup-pprint-code
  "\n\nimport pprint
import sys

orig_displayhook = sys.displayhook

def pprint_displayhook(value):
    if value != None:
        __builtins__._ = value
        pprint.pprint(value)

#sys.displayhook = pprint_displayhook

#__builtins__.pprint_on = lambda: setattr(sys, 'displayhook', pprint_displayhook)
#__builtins__.pprint_off = lambda: setattr(sys, 'displayhook', orig_displayhook)
")
(defparameter python-setup-numpy-code
  "\n\ntry:
    import numpy as np
except ImportError:
    print(\"Numpy is not accessible\")")
(add-to-list 'python-shell-setup-codes
             'python-shell-completion-setup-code)
(add-to-list 'python-shell-setup-codes
             'python-setup-pprint-code)
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
           (prompt-beg (cdr comint-last-prompt))
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
      (let ((compl-end-pos (position ?\0 completion-accum)))
        (setq completions
              (split-string (substring completion-accum
                                       0
                                       compl-end-pos)
                            sep))

        (cond
          ((null completions)
           (error "Can't find completion for \"%s\"" pattern))
          ((null? (cdr completions))
           ;; got only one completion
           (setf completion (car completions)))
          (t
           (setf completion (ido-completing-read ""
                                                 completions
                                                 nil
                                                 nil
                                                 pattern)))))
      (if (or (string= pattern completion)
              (= 0 (length completion)))
        (error "No completions found for \"%s\"" pattern)
        (progn
          (delete-region beg end)
          (insert (trim-whitespace completion)))))))


(setenv "PYTHONPATH"
        (join-lines
         (list "/home/sergey/projects/python/modules/")
         ":"))
(setenv "IPYTHONDIR" (concat +prog-data-path+ "/ipython"))

(setf hs-special-modes-alist
      (cons `(python-mode ,(rx line-start
                               (* (syntax whitespace))
                               symbol-start
                               (or "def"
                                   "class"
                                   "for"
                                   "if"
                                   "elif"
                                   "else"
                                   "while"
                                   "try"
                                   "except"
                                   "finally")
                               symbol-end)
                          nil
                          "#"
                          ,(lambda (arg)
                             (python-forward-indentation-level))
                          nil)
            (assq-delete-all 'python hs-special-modes-alist)))

;;; helper functions

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

(defun python-forward-indentation-level ()
  "Move forward to the end of indentation block that has the same or
greater indenation as current line."
  (interactive)
  (beginning-of-line)
  (let ((start-column
         (lambda ()
           (save-excursion
             (beginning-of-line)
             (skip-syntax-forward "-")
             (current-column)))))
    (let ((c (funcall start-column)))
      (forward-line)
      (while (and (not (eob?))
                  (< c (funcall start-column)))
        (forward-line))
      (backward-line)
      (end-of-line))))

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

(defun python-convolute-lines ()
  "Python translation of `paredit-convolute-sexp'."
  (interactive)
  (save-excursion
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    vim:shift-width)
    (forward-line -1)
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    (- vim:shift-width))))

(vim:defcmd vim:python-shell-send-buffer (nonrepeatable)
  (python-shell-send-buffer))

;;; run scripts compilation mode

(defconst +python-run-error-regexp+
  "^ *File \"\\([-a-zA-Z _0-9.,()\\\\/]+\\)\", line \\([0-9]+\\), in .*")

(defun python-run-script (&optional use-pypy)
  "Run script associated with current buffer."
  (interactive (list current-prefix-arg))
  (save-buffer-if-modified)
  (compilation-start (concat (if use-pypy
                               "pypy"
                               python-exec)
                             " "
                             (file-name-nondirectory buffer-file-name))
                     #'python-run-mode))

(define-compilation-mode python-run-mode "Python run"
  "Mode for running python scripts."

  (setq-local compilation-error-regexp-alist
              (list
               (list +python-run-error-regexp+
                     1   ;; FILE
                     2   ;; LINE
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

;;; actual setups

;;;###autoload
(defun python-setup ()
  (init-common :use-yasnippet t
               :use-render-formula t
               :sp-slurp-sexp-insert-space nil
               :use-whitespace 'tabs-only
               :use-fci t)
  (setup-hs-minor-mode)

  (setup-indent-size 4)
  (setq-local whitespace-style '(face lines-tail))

  ;; ;; make ' a string delimiter
  ;; (modify-syntax-entry ?' "\"")

  ;; make _ a symbol constituent, mostly for me
  (modify-syntax-entry ?_ "_")
  ;; make . a symbol constituent, mostly for me too
  (modify-syntax-entry ?. ".")

  (setf hs-block-end-regexp nil)

  ;; By default this is set to `python-nav-forward-sexp' which is too
  ;; heavyweight alternative to `forward-sexp' for general-purpose use
  ;; (causes noticeable delay on inserting (, " or """)
  (setq-local forward-sexp-function nil)

  (vim:local-emap "load" #'vim:python-shell-send-buffer)
  (vim:local-emap "l"    #'vim:python-shell-send-buffer)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f6>"    python-shell-send-buffer)
    ("<f9>"    python-run-script)
    ("S-<f9>"  python-check)

    ("j"       python-shell-send-defun)
    ("M-?"     python-convolute-lines)

    ("SPC SPC" switch-to-python)
    ("g s s"   vim-replace-symbol-at-point))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("<f6>"  python-shell-send-region)
    ("j"     python-shell-send-region)
    ("g a"   nil)
    ("g a =" python-align-on-equals))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("g t"      beginning-of-defun)
    ("g h"      end-of-defun)

    ("C-<up>"   python-nav-backward-block)
    ("C-<down>" python-nav-forward-block)

    ("="        python-nav-backward-up-list)
    ("q"        python-nav-up-list)

    ("*"        search-for-symbol-at-point-forward)
    ("M-*"      search-for-symbol-at-point-forward-new-color)
    ("#"        search-for-symbol-at-point-backward)
    ("M-#"      search-for-symbol-at-point-backward-new-color))

  (bind-tab-keys #'tab-to-tab-stop
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t)
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
                         :length-min 3)
  (add-hook 'after-save-hook #'make-script-file-exec nil t)
  (setup-eproj-symbnav))

;;;###autoload
(add-hook 'python-mode-hook #'python-setup)

;; (defun python--assemble-shell-command ()
;;   "Calculate the string used to execute the inferior Python process."
;;   (let ((process-environment (python-shell-calculate-process-environment))
;;         (exec-path (python-shell-calculate-exec-path)))
;;     (format "%s %s"
;;             (executable-find python-shell-interpreter)
;;             python-shell-interpreter-args)))

;;;###autoload (autoload 'switch-to-python "python-setup" nil t)
(define-switch-to-interpreter
  switch-to-python
  ((concat "*" python-shell-buffer-name "*"))
  (run-python ;; (python--assemble-shell-command)
   )
  :doc "Pop to python repl."
  :save-buffer t
  :error-msg "Can't switch to python repl"
  :try-count 2)

;;;###autoload
(defun inferior-python-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :sp-slurp-sexp-insert-space nil
               :use-whitespace nil
               :use-fci nil)
  (init-repl :create-keymaps nil)
  (comint-setup)
  (comint-read-input-ring t)

  ;; make ' a string delimiter ;; is this necessary?
  (modify-syntax-entry ?' "\"")
  (modify-syntax-entry ?_ "_")
  (modify-syntax-entry ?. ".")

  (setf tab-width 4)
  (setq-local forward-sexp-function nil)

  (vim:local-emap "clear" #'vim:comint-clear-buffer-above-prompt)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("C-SPC"    vim:comint-clear-buffer-above-prompt)
    ("M-p"      browse-comint-input-history)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-t"      comint-previous-prompt)
    ("C-h"      comint-next-prompt)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)

    ("<tab>"    python-complete)))

;;;###autoload
(add-hook 'inferior-python-mode-hook #'inferior-python-setup)

(provide 'python-setup)

;; Local Variables:
;; End:

;; python-setup.el ends here
