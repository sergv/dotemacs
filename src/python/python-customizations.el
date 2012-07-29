;; python-customizations.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  2 May 2012
;; Description:

(require 'common)
(require 'shell)
(require 'ipython)
(require 'pabbrev)
(require 'python-repl-mode)


(redefun py-beginning-of-block (&optional indent)
  "Looks up for nearest opening block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (ignore-errors (cdr (py-go-to-keyword py-block-re -1 indent))))

(redefun py-end-base (regexp orig &optional iact)
  "Used internal by functions going to the end forms. "
  (let ((erg (if (py-statement-opens-block-p regexp)
               (point)
               (py-go-to-keyword regexp -1)
               (when (py-statement-opens-block-p regexp)
                 (point))))
        ind)
    (if erg
      (progn
        (setq ind (+ py-indent-offset (current-indentation)))
        (py-end-of-statement)
        (forward-line 1)
        (setq erg (py-travel-current-indent (cons ind (point)))))
      (py-look-downward-for-beginning regexp)
      (unless (eobp)(py-end-base regexp orig iact)))
    (if (< orig (point))
      (setq erg (point))
      (setq erg (py-look-downward-for-beginning regexp))
      (when erg (py-end-base regexp orig iact)))
    erg))


(python-initialize-vars)


(defun py-shell-save-input-history ()
  (comint-write-input-ring))


;; fix history files and make it call python-interpreter-setup
(redefun py-shell (&optional argprompt dedicated pyshellname)
  "Start an interactive Python interpreter in another window.

With optional \\[universal-argument] user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.
Returns variable `py-process-name' used by function `get-process'.
"
  (interactive "P")
  (let* ((py-shell-name "ipython")
         (py-shell-default-name py-shell-name)
         (args py-python-command-args)
         (py-process-name (py-process-name py-shell-name dedicated))
         ipython-version version)
    ;; if no process with such name exists then create one
    (unless (get-process py-process-name)
      (py-set-shell-completion-environment)
      ;; comint
      (set-buffer (apply 'make-comint py-process-name py-shell-name nil args))
      (setf py-shell-name py-shell-default-name)

      (python-repl-mode)

      ;; load this startup code into ipython session
      (let ((proc (get-process py-process-name))
            ;; this value is in megabytes
            (ipython-memory-limit 2048))
        (process-send-string
         proc
         (format
          (concat
           "from __future__ import print_function, division; "
           "import resource; "
           "resource.setrlimit(resource.RLIMIT_AS, (%d * 1048576L, -1L)); "
           "import numpy as np; "
           " #PYTHON-MODE SILENT\n")
          ipython-memory-limit))
        (accept-process-output proc))

      (when (or py-shell-switch-buffers-on-execute (interactive-p))
        (switch-to-buffer (current-buffer)))
      (goto-char (point-max)))
    py-process-name))

(defun py-clean-up-buffer-contents (python-name str)
  "Remove things like \"from __future__ import print_function\" if underlying
python shell PYTHNO-NAME (e.g. IPython) doesn't like them."
  (if (string-match-p "IPython" python-name)
    (let ((case-fold-search t))
      (replace-regexp-in-string
       "from __future__ import print_function\\(?:, division\\)?\n"
       ""
       str))
    str))


;; Local Variables:
;; lexical-binding: t
;; End:

;; python-customizations.el ends here
