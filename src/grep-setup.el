;; grep-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:

;; Some enchances to standard grep.el by redefining
;; some of its functions

(require 'el-patch)
(require 'grep)
(require 'keys-def)
(require 'compilation-setup)
(require 'haskell-autoload)

(eval-after-load
    "grep"
  '(progn
     (def-keys-for-map grep-mode-map
       +vi-keys+
       +vim-search-keys+
       +vim-special-keys+
       +vim-mock:word-motion-keys+
       ("h"        compilation-jump-to-next-error)
       ("t"        compilation-jump-to-prev-error)
       ("<up>"     compilation-jump-to-prev-error)
       ("<down>"   compilation-jump-to-next-error)

       ("<escape>" kill-grep)
       ("C-c C-c"  kill-grep)
       ("q"        remove-buffer)

       ("C-v"      set-mark-command)
       ("C-y"      copy-region-as-kill)
       ("v"        set-mark-command)
       ("y"        copy-region-as-kill)

       ("<return>" compilation/goto-error)
       ("SPC"      compilation/goto-error-other-window)
       ("o"        compilation/goto-error-other-window))

     (defvar *grep-latest-dir* nil
       "Latest directory used for `rgrep', `rzgrep' or alike.")

     (setf grep-expand-keywords
           (cons '("<E>" . (if (funcall fixed-string? regexp) "-F" "-E"))
                 grep-expand-keywords))

     ;; make use of inlined grep-expand-keywords and set *grep-latest-dir*
     ;; pay attention to rgrep-ignore-case
     (el-patch-defun grep-expand-template (template &optional regexp files dir excl)
       "Patch grep COMMAND string replacing <C>, <D>, <F>, <R>, and <X>."
       (el-patch-wrap 2 0
         ((setf dir (or dir "."))
          (setf *grep-latest-dir* dir)
          (let* ((command template)
                 (env `((opts . ,(let (opts)
                                   (when (el-patch-wrap 2 0
                                           (or rgrep-ignore-case
                                               (and case-fold-search
                                                    (isearch-no-upper-case-p regexp t))))
                                     (push "-i" opts))
                                   (cond
                                     ((eq grep-highlight-matches 'always)
                                      (push "--color=always" opts))
                                     ((eq grep-highlight-matches 'auto)
                                      (push "--color" opts)))
                                   opts))
                        (excl . ,excl)
                        (dir . ,dir)
                        (files . ,files)
                        (regexp . ,regexp)))
                 (case-fold-search nil))
            (dolist (kw grep-expand-keywords command)
              (if (string-match (car kw) command)
                  (setq command
                        (replace-match
                         (or (if (symbolp (cdr kw))
                                 (eval (cdr kw) env)
                               (save-match-data (eval (cdr kw) env)))
                             "")
                         t t command))))))))

     (defadvice grep-filter (before grep-filter-make-relative-filename-advice
                                    activate
                                    compile)
       "This advice is simply AWESOME! It replaces common long filename prefixes with \".\"."
       (save-match-data
         (save-excursion
           (let ((end (line-beginning-position))
                 (beg (progn
                        (goto-char compilation-filter-start)
                        (line-beginning-position)))
                 (dir (awhen *grep-latest-dir* (strip-trailing-slash it))))
             (when dir
               (let ((re (concat "^" (regexp-quote dir))))
                 (goto-char beg)
                 (while (re-search-forward re end t)
                   (replace-match "."))))))))))

(defun grep-set-up-error-regexp (buffer msg)
  "Set up `*compilation-jump-error-regexp*' from `compilation-error-regexp-alist'."
  (with-current-buffer buffer
    (when (eq? major-mode 'grep-mode)
      (setf *compilation-jump-error-regexp*
            "^./\\(?:[^/]+/\\)*[^/:]+\.[a-zA-Z0-9_]+:[0-9]+:"
            ;; (join-lines (-map #'car compilation-error-regexp-alist)
            ;;             "\\|")
            ))))


(defparameter rgrep-ignore-case nil
  "Dynamically-bound variable that controls whether current
rgrep invocation should be case-insensetive.")

(defun rgrep-wrapper (regexp &optional files dir ignore-case)
  "Similar to `rgrep' but ignores case if universal argument was supplied
more than once"
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
       ((not grep-find-template)
        (error "grep.el: No `grep-find-template' available"))
       (t (let* ((regexp (grep-read-regexp))
                 (files (grep-read-files regexp))
                 (dir (read-directory-name "Base directory: "
                                           nil default-directory t))
                 (ignore-case (and (not (null? current-prefix-arg))
                                   (<= 16 (first current-prefix-arg)))))
            (list regexp files dir ignore-case))))))
  (let* ((rgrep-ignore-case ignore-case)
         (user-supplied-files (split-string files))
         (grep-find-ignored-files
          (remove-if (comp (partial-first #'member user-supplied-files))
                     grep-find-ignored-files)))
    (rgrep regexp files dir current-prefix-arg)))

(add-to-list 'compilation-finish-functions #'grep-set-up-error-regexp)

(defun rgrep-region (str ignore-case)
  (interactive (list (get-region-string-no-properties)
                     (and (not (null? current-prefix-arg))
                          (<= 16 (first current-prefix-arg)))))
  (let* ((regexp (read-string "Search for: "
                              str
                              'grep-regexp-history
                              str))
         (files (grep-read-files regexp))
         (dir (read-directory-name "Base directory: "
                                   nil default-directory t)))
    (rgrep-wrapper regexp files dir ignore-case)))

(provide 'grep-setup)

;; Local Variables:
;; End:

;; grep-setup.el ends here
