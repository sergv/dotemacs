;; json-ts-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 16 September 2023
;; Description:

(eval-when-compile
  (require 'dash))

(require 'json-mode)
(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

(defconst json-ts-mode-syntax-table json-mode-syntax-table)

(defface json-ts-error
  '((t (:inherit error :extend t)))
  "Face to highlight malformed json."
  :group 'json)

(defconst json-ts-font-lock-rules
  (append
   ;; Errors
   '(:language
     json
     :feature error
     :override t
     ((ERROR) @json-ts-error ;; _ @json-ts-error
      ))
   (--mapcat
    (cons :language (cons 'json (cons :feature it)))
    '((comment
       ((comment) @font-lock-comment-face))
      (constant
       ((number) @font-lock-constant-face))
      (word-constant
       ([(true) (false) (null)] @font-lock-keyword-face))
      (string
       ((string) @font-lock-string-face))

      ;; Override keys which look like strings
      (key
       :override t
       ((pair key: (string) @json-mode-object-name-face)))

      ;; ;; Fontify malformed objects which couldn’t be parsed in full due to narrowing
      ;; '(key
      ;;   :override t
      ;;   ((["," "{"] (string) @json-mode-object-name-face ":")))
      )))
  )

(defconst json-ts-indent-rules
  `(((node-is   ,(rx (any ?\] ?\})))         parent-bol 0)
    ((parent-is ,(rx (or "object" "array"))) parent-bol 2)
    (no-node                                 parent     0)))

;;;###autoload
(define-derived-mode json-ts-mode prog-mode "JSON[ts]"
  "Major mode for editing JavaScript Object Notation (JSON) data files with tree-sitter."

  ;; Important to disable long lines optimizations because they make font locking
  ;; operate in narrowed buffer which could make treesitter miss things.
  (setq-local long-line-optimizations-region-size 0)
  ;; Fast font lock mode is too imprecise and can also make treesitter miss things.
  (setq-local treesit--font-lock-fast-mode nil)

  (setq-local font-lock-defaults nil
              treesit-font-lock-feature-list
              '((comment error)
                (string key constant word-constant)))

  (let ((res (treesit-language-available-p 'json t)))
    (unless (car res)
      (error "JSON treesitter not available: %s" (cdr res))))


  ;; Associate parser with current buffer.
  (treesit-parser-create 'json (current-buffer))

  ;; Font locking
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules json-ts-font-lock-rules))

  ;; Indentation
  (setq-local treesit-simple-indent-rules
              (list (cons 'json json-ts-indent-rules)))

  (treesit-major-mode-setup))

(provide 'json-ts-mode)

;; Local Variables:
;; End:

;; json-ts-mode.el ends here
