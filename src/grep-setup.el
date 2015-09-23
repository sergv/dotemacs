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

(require 'grep)
(require 'keys-def)
(require 'compilation-setup)
(require 'haskell-autoload)

(eval-after-load
    "grep"
  '(progn
     (def-keys-for-map grep-mode-map
       +vi-keys+
       +vi-search-keys+
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

     ;; make use of inlined grep-expand-keywords and set *grep-latest-dir*
     ;; pay attention to rgrep-ignore-case
     (redefun grep-expand-template (template &optional regexp files dir excl)
       "Patch grep COMMAND string replacing <C>, <D>, <F>, <R>, and <X>.
Fixed version. Also recognized <E>, which will be replaced by -E or -F depending
on whether supplied patterns is regexp or fixed string."
       (setf *grep-latest-dir* dir)
       (save-match-data
         (let* ((command template)
                (case-fold-search nil)
                (fixed-string?
                 (lambda (x)
                   ;; conservative regexp detection - if there's no regexp
                   ;; metacharacters then it's fixed string
                   ;; (string-match-pure? "^[-a-zA-Z0-9_'\"%#@!`~ :;<>/]+$" x)
                   (string-match-pure? (rx bol
                                           (+ (not (any ?+ ?* ?? ?| ?\( ?\) ?\[ ?\] ?\{ ?\} ?^ ?$ ?\\)))
                                           eol)
                                       x)))
                (func (lambda (token text)
                        (when (string-match token command)
                          (setq command
                                (replace-match (or text "") t t command))))))
           (funcall func "<C>" (when (or rgrep-ignore-case
                                         (and case-fold-search
                                              (isearch-no-upper-case-p regexp t)))
                                 "-i"))
           (funcall func "<D>" dir)
           (funcall func "<F>" files)
           (funcall func "<N>" null-device)
           (funcall func "<X>" excl)
           (funcall func "<E>" (if (funcall fixed-string? regexp)
                                 "-F"
                                 "-E"))
           (funcall func "<R>" regexp)
           command)))

     (redefun zrgrep (regexp &optional files dir confirm grep-find-template)
       "Recursively grep for REGEXP in gzipped FILES in tree rooted at DIR.
Like `rgrep' but uses `zgrep' for `grep-program', sets the default
file name to `*.gz', and sets `grep-highlight-matches' to `always'."
       (interactive
        (progn
          ;; Compute standard default values.
          (grep-compute-defaults)
          ;; Compute the default zrgrep command by running `grep-compute-defaults'
          ;; for grep program "zgrep", but not changing global values.
          (let ((grep-program "zgrep")
                ;; Don't change global values for variables computed
                ;; by `grep-compute-defaults'.

                (grep-find-template
                 (replace-regexp-in-string "grep" "zgrep" grep-find-template ))
                (grep-find-command nil)
                (grep-host-defaults-alist nil)
                ;; Use for `grep-read-files'
                (grep-files-aliases '(("all" . "* .*")
                                      ("gz"  . "*.gz"))))
            ;; Recompute defaults using let-bound values above.
            (grep-compute-defaults)
            (cond
              ((and (not (null? grep-find-command))
                    (not (null? current-prefix-arg))
                    (= 16 (first current-prefix-arg)))
               (list (read-from-minibuffer "Run: " grep-find-command
                                           nil nil 'grep-find-history)))
              ((not grep-find-template)
               (error "my-grep.el: No `grep-find-template' available"))
              (t (let* ((regexp (grep-read-regexp))
                        (files (grep-read-files regexp))
                        (dir (read-directory-name "Base directory: "
                                                  nil default-directory t))
                        (confirm (and (not (null? current-prefix-arg))
                                      (= 4 (first current-prefix-arg)))))
                   (list regexp files dir confirm grep-find-template)))))))
       ;; Set `grep-highlight-matches' to `always'
       ;; since `zgrep' puts filters in the grep output.
       (let ((grep-highlight-matches 'always))
         ;; `rgrep' uses the dynamically bound value `grep-find-template'
         ;; from the argument `grep-find-template' whose value is computed
         ;; in the `interactive' spec.
         (rgrep regexp files dir confirm)))

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
            ;; (join-lines (map #'car compilation-error-regexp-alist)
            ;;             "\\|")
            ))))


(defparameter rgrep-ignore-case nil
  "Dynamically-bound variable that controls whether current
rgrep invokation should be case-insensetive.")

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
                              str)
                 ;; (grep-read-regexp)
                 )
         (files (grep-read-files regexp))
         (dir (read-directory-name "Base directory: "
                                   nil default-directory t)))
    (rgrep-wrapper regexp files dir ignore-case)))

(provide 'grep-setup)

;; Local Variables:
;; End:

;; grep-setup.el ends here
