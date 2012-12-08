;; python-repl-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  3 May 2012
;; Description:

(require 'common)
;; (require 'ipython)
;; (require 'python-highlight)


;; (define-derived-mode python-repl-mode comint-mode "Python REPL"
;;   "Major mode for interacting with an inferior python repl like ipython.
;; Runs repl as a subprocess of Emacs, with it's I/O through an Emacs
;; buffer.
;;
;; Entry to this mode successively runs the hooks `comint-mode-hook' and
;; `python-repl-mode-hook'."
;;
;;   (setf mode-line-process '(":%s"))
;;
;;   (setf font-lock-defaults
;;         '(python-repl-font-lock-keywords
;;           t ;; keywords-only
;;           nil ;; case-fold
;;           ))
;;   (set (make-local-variable 'parse-sexp-lookup-properties) t)
;;   (set (make-local-variable 'parse-sexp-ignore-comments) t)
;;   (set (make-local-variable 'comment-start) "# ")
;;   (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
;;   (set (make-local-variable 'comment-column) 40)
;;   (set (make-local-variable 'comment-indent-function) #'py-comment-indent-function)
;;   (set (make-local-variable 'indent-region-function) 'py-indent-region)
;;   (set (make-local-variable 'indent-line-function) 'py-indent-line)
;;   (set (make-local-variable 'comint-prompt-regexp)
;;        "\\(^>>? \\|^In \\[[0-9]+\\]: *\\|^   [.][.][.]+: *\\|\\(?:\nIn \\[[0-9]+\\]: *.*\n----+> \\(.*\n\\)[\n]?\\)\\|\\(?:\nIn \\[[0-9]+\\]: *\\(.*\n\\)\\)\\|^[ ]\\{3\\}[.]\\{3,\\}: *\\(.*\n\\)\\|^Out\\[[0-9]+\\]: \\|\n[(<]*[Ii]?[Pp]y?db[>)]+ \\|^[(]*ipydb[>)]+ \\)")
;;
;;
;;   (add-hook 'comint-output-filter-functions
;;             'py-comint-output-filter-function)
;;
;;   (setf comint-input-sender 'py-shell-simple-send
;;         comint-input-ring-file-name
;;         (if (string-equal py-shell-name "ipython")
;;           (if (getenv "IPYTHON_DIR")
;;             (concat (getenv "IPYTHON_DIR") "/history")
;;             (concat +prog-data-path+
;;                     "/ipython/history"))
;;           (if (getenv "PYTHONHISTORY")
;;             (concat (getenv "PYTHONHISTORY") "/" py-shell-name "_history")
;;             (concat +prog-data-path+
;;                     "/"
;;                     py-shell-name
;;                     "_history"))))
;;
;;   (comint-read-input-ring t)
;;   (set-process-sentinel (get-buffer-process (current-buffer))
;;                         #'shell-write-history-on-exit)
;;   (add-hook 'kill-buffer-hook #'py-shell-save-input-history nil t)
;;   ;; pdbtrack
;;   (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
;;   (setq py-pdbtrack-do-tracking-p t)
;;   ;;
;;   (set-syntax-table py-mode-syntax-table)
;;   (ansi-color-for-comint-mode-on)
;;   ;; ToDo: has only effect \w IPython
;;   (add-hook 'py-shell-hook 'py-dirstack-hook)
;;   (run-hooks 'py-shell-hook)
;;
;;   (ansi-color-for-comint-mode-on))

;; (def-keys-for-map python-repl-mode-map
;;   ("<tab>" ipython-complete)
;;   ("TAB"   ipython-complete))



(provide 'python-repl-mode)

;; Local Variables:
;; End:

;; python-repl-mode.el ends here
