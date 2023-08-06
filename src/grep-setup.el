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

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'set-up-platform)
  (require 'macro-util))

(require 'el-patch)
(require 'grep)
(require 'keys-def)
(require 'compilation-setup)
(require 'haskell-autoload)

(when-emacs-version (<= 28 it)
  (el-patch-defun grep-expand-template (template &optional regexp files dir excl more-opts)
    "Expand grep COMMAND string replacing <C>, <D>, <F>, <R>, and <X>."
    (el-patch-wrap 2 0
      ((setf dir (or dir "."))
       (setf *grep-latest-dir* dir)
       (let* ((command template)
              (env `((opts . ,(let ((opts more-opts))
                                (when (el-patch-wrap 2 0
                                        (or rgrep-ignore-case
                                            (and case-fold-search
                                                 (isearch-no-upper-case-p regexp t))))
                                  (push "-i" opts))
                                (cond
                                  ((eq grep-highlight-matches 'always)
                                   (push "--color=always" opts))
                                  ((eq grep-highlight-matches 'auto)
                                   (push "--color=auto" opts)))
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
                      t t command)))))))))

(when-emacs-version (= 28 it)
  (el-patch-defun grep-read-files (regexp)
    "Read a file-name pattern arg for interactive grep.
The pattern can include shell wildcards.  As SPC can triggers
completion when entering a pattern, including it requires
quoting, e.g. `\\[quoted-insert]<space>'.

REGEXP is used as a string in the prompt."
    (let* ((bn (funcall grep-read-files-function))
	   (fn (and bn
		    (stringp bn)
		    (file-name-nondirectory bn)))
	   (default-alias
	    (and fn
		 (let ((aliases (remove (assoc "all" grep-files-aliases)
				        grep-files-aliases))
		       alias)
		   (while aliases
		     (setq alias (car aliases)
			   aliases (cdr aliases))
		     (if (string-match (mapconcat
				        #'wildcard-to-regexp
				        (split-string (cdr alias) nil t)
				        "\\|")
				       fn)
			 (setq aliases nil)
		       (setq alias nil)))
		   (cdr alias))))
	   (default-extension
	    (and fn
		 (let ((ext (file-name-extension fn)))
		   (and ext (concat "*." ext)))))
	   (default
	    (or default-alias
	        default-extension
	        (car grep-files-history)
	        (car (car grep-files-aliases))))
	   (files (completing-read
		   (concat "Search for \"" regexp
			   "\" in files matching wildcard"
			   (if default (concat " (default " default ")"))
			   ": ")
                   (el-patch-swap
                     #'read-file-name-internal
                     (delete-dups
                      (delq nil (append (list default default-alias default-extension)
                                        (mapcar 'car grep-files-aliases)))))
		   nil nil nil 'grep-files-history
		   (delete-dups
		    (delq nil
                          (append (list default default-alias default-extension)
				  (mapcar #'car grep-files-aliases)))))))
      (and files
	   (or (cdr (assoc files grep-files-aliases))
	       files)))))

(defun grep-init-after-load ()
  (def-keys-for-map grep-mode-map
    +vi-keys+
    +vim-search-keys+
    +vim-special-keys+
    +vim-word-motion-keys+
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
                (replace-match ".")))))))))

(eval-after-load
    "grep"
  '(grep-init-after-load))

(defvar rgrep-ignore-case nil
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
          (-filter (lambda (x) (not (member x user-supplied-files)))
                   grep-find-ignored-files)))
    (rgrep regexp files dir current-prefix-arg)))

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
