;; c-like-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 12 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'custom-predicates)
(eval-when-compile
 (require 'cl))

(autoload 'c-turn-on-eldoc-mode "c-eldoc" nil t)


(unless (assoc* "my-style" c-style-alist :test #'string=?)
  ;; inherited from linux style
  (push '("my-style"
          (c-basic-offset  . 8)
          (indent-tabs-mode . nil)
          (c-comment-only-line-offset . 0)
          (c-hanging-braces-alist . ((brace-list-open)
                                     (brace-entry-open)
                                     (substatement-open after)
                                     (block-close . c-snug-do-while)
                                     (arglist-cont-nonempty)))
          (c-cleanup-list . (brace-else-brace))
          (c-offsets-alist . ((statement-block-intro . +)
                              (knr-argdecl-intro     . 0)
                              (substatement-open     . 0)
                              (substatement-label    . 0)
                              (label                 . 0)
                              (statement-cont        . +)
                              (innamespace           . 0))))
        c-style-alist))

(setf c-default-style
      `((java-mode . "java")
        (awk-mode . "awk")
        (other . ,(cond ((assoc* "my-style" c-style-alist :test #'string=?)
                         "my-style")
                        (t
                         "linux")))))
(setq-default c-basic-offset 8)

(eval-after-load
 "c-eldoc"
 '(progn
   ;; make cache buffer names start with a space to make them invisible
   ;; quote buffer-file-name when sending it to shell
   (redefun c-eldoc-get-buffer (function-name)
     "Call the preprocessor on the current file"
     ;; run the first time for macros
     (let ((output-buffer (cache-gethash (current-buffer) c-eldoc-buffers)))
       (if output-buffer output-buffer
         (let* ((this-name (concat " *" buffer-file-name "-preprocessed*"))
                (cur-buffer (current-buffer))
                (output-buffer (generate-new-buffer this-name)))
           (dolist (cpp-arguments (list c-eldoc-cpp-macro-arguments
                                        c-eldoc-cpp-normal-arguments))
             (call-process-shell-command
              (concat c-eldoc-cpp-command " "
                      cpp-arguments " "
                      c-eldoc-includes " "
                      (shell-quote-argument buffer-file-name))
              nil
              output-buffer
              nil))
           (cache-puthash cur-buffer output-buffer c-eldoc-buffers)
           output-buffer))))

   ;; fix current-function-regexp, current-macro-regexp
   (redefun c-eldoc-print-current-symbol-info ()
     "Returns documentation string for the current symbol."
     (let* ((current-function-cons (c-eldoc-function-and-argument (- (point) 1000)))
            (current-function (car current-function-cons))
            (current-function-regexp
              (concat "[ \t\n]*[a-zA-Z_][0-9a-zA-Z_]*[ \t\n*]+"
                      current-function
                      "[ \t\n]*("))
            (current-macro-regexp
              (concat "#[ \t\n]*define[ \t\n]+"
                      current-function
                      "[ \t\n]*("))
            (tag-buffer)
            (function-name-point)
            (arguments)
            (type-face 'font-lock-type-face))
       (when (and current-function
                  (not (member current-function c-eldoc-reserved-words))
                  (setq tag-buffer (c-eldoc-get-buffer current-function)))
         (with-current-buffer tag-buffer
           ;; setup the buffer
           (goto-char (point-min))
           (prog1
               ;; protected regexp search
               (when (condition-case nil
                         (progn
                           (if (not (re-search-forward current-macro-regexp (point-max) t))
                             (re-search-forward current-function-regexp))
                           t)
                       (error (prog1 nil
                                (message "Function doesn't exist..."))))
                 ;; move outside arguments list
                 (search-backward "(")
                 (c-skip-ws-backward)
                 (setq function-name-point (point))
                 (forward-sexp)
                 (setq arguments (buffer-substring-no-properties
                                  function-name-point (point)))
                 (goto-char function-name-point)
                 (backward-char (length current-function))
                 (c-skip-ws-backward)
                 (setq function-name-point (point))
                 (search-backward-regexp "[};/#]" (point-min) t)
                 ;; check for macros
                 (if (= (char-after) ?#)
                   (let ((is-define (looking-at-pure? "#[[:space:]]*define"))
                         (preprocessor-point (point)))
                     (while (prog2 (end-of-line)
                                (= (char-before) ?\\)
                              (forward-char)))
                     (when (and is-define (> (point) function-name-point))
                       (goto-char preprocessor-point)
                       (setq type-face 'font-lock-preprocessor-face)))
                   (forward-char)
                   (when (looking-back "//")
                     (end-of-line)))
                 (c-skip-ws-forward)
                 ;; colorize
                 (concat (propertize (buffer-substring-no-properties
                                      (point)
                                      function-name-point)
                                     'face type-face)
                         " "
                         (propertize current-function
                                     'face 'font-lock-function-name-face)
                         " "
                         (c-eldoc-format-arguments-string arguments
                                                          (cdr current-function-cons)))))))))

   (defun c-eldoc-show-current-symbol-declaration ()
     "Shows declaration of function/macro at point."
     (interactive)
     (let* ((current-function (symbol->string (symbol-at-point)))
            (current-function-regexp
              (concat "[ \t\n]*[a-zA-Z_][0-9a-zA-Z_]*[ \t\n*]+"
                      current-function
                      "[ \t\n]*("))
            (current-macro-regexp (concat "#[ \t\n]*define[ \t\n]+"
                                          current-function
                                          "[ \t\n]*("))
            (tag-buffer)
            (function-name-point)
            (arguments)
            (type-face 'font-lock-type-face))
       (when (and current-function
                  (not (member current-function c-eldoc-reserved-words))
                  (setq tag-buffer (c-eldoc-get-buffer current-function)))
         (save-match-data
          (with-current-buffer tag-buffer
            ;; setup the buffer
            (goto-char (point-min))
            ;; protected regexp search
            (condition-case nil
                (progn
                  (if (not (re-search-forward current-macro-regexp (point-max) t))
                    (re-search-forward current-function-regexp))
                  t)
              (error (error "Cannot find function declaration")))
            ;; move outside arguments list
            (search-backward "(")
            (c-skip-ws-backward)
            (setq function-name-point (point))
            (forward-sexp)
            (setq arguments (buffer-substring-no-properties function-name-point
                                                            (point)))
            (goto-char function-name-point)
            (backward-char (length current-function))
            (c-skip-ws-backward)
            (setq function-name-point (point))
            (search-backward-regexp "[};/#]" (point-min) t)
            ;; check for macros
            (if (= (char-after) ?#)
              (let ((is-define (looking-at-p "#[[:space:]]*define"))
                    (preprocessor-point (point)))
                (while (prog2 (end-of-line)
                           (= (char-before) ?\\)
                         (forward-char)))
                (when (and is-define (> (point) function-name-point))
                  (goto-char preprocessor-point)
                  (setq type-face 'font-lock-preprocessor-face)))
              (forward-char)
              (when (looking-back "//")
                (end-of-line)))
            (c-skip-ws-forward)
            ;; colorize
            (message (concat (propertize (buffer-substring-no-properties
                                          (point)
                                          function-name-point)
                                         'face type-face)
                             " "
                             (propertize current-function
                                         'face 'font-lock-function-name-face)
                             (replace-regexp-in-string " *\\([()]\\) *"
                                                       "\\1"
                                                       arguments))))))))))

;; (autoload 'c-c++-switch-header-and-source "c-setup" nil t)
(autoload 'c-setup "c-setup")
(add-hook 'c-mode-hook #'c-setup)

(autoload 'c++-setup "c++-setup")
(add-hook 'c++-mode-hook #'c++-setup)

(when (platform-use? 'work)
  (add-to-list 'auto-mode-alist '("\\.in\\(?:l\\|c\\|cl\\)\\'" . c++-mode)))


(defun c++-file-magic-function ()
  (let ((ext (file-name-extension (buffer-file-name))))
    ;; check for null since .emacs doesn't have extension
    (when (and ext
               (member* ext '("h" "inl" "inc" "incl")
                        :test #'string=))
      (save-excursion
       (save-match-data
        (re-search-forward (rx
                            (or "class"
                                "namespace"
                                "::"
                                ;; it's quite rare to see other template
                                ;; open brace styles so lets accomodate
                                ;; only for frequently used ones
                                (regex "template[[:space:]]*<")
                                (regex "\\(?:public\\|protected\\|private\\)[[:space:]]*:")))
                           nil
                           t))))))

;; this will make sure that *.h c++ header will be correctly handled
(push (cons #'c++-file-magic-function #'c++-mode) magic-mode-alist)



;; Local Variables:
;; End:

;; c-like-setup.el ends here
