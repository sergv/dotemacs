;; grep+.el --- -*- lexical-binding: t; -*-

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

(eval-after-load
 "grep"
 '(progn
   (def-keys-for-map grep-mode-map
     +control-x-prefix+
     +vi-keys+
     +vim-special-keys+
     +vim-word-motion-keys+
     ("<down>"     compilation-next-error)
     ("<up>"       compilation-previous-error)
     ("t"          compilation-next-error)
     ("n"          compilation-previous-error)

     ("<escape>"   kill-grep)
     ("C-c C-c"    kill-grep)

     ("C-v"        set-mark-command)
     ("C-y"        copy-region-as-kill)
     ("v"          set-mark-command)
     ("y"          copy-region-as-kill))

   (defconst grep+-expand-keywords
     '(("<C>" . (and cf (isearch-no-upper-case-p regexp t) "-i"))
       ("<D>" . dir)
       ("<F>" . files)
       ("<N>" . null-device)
       ("<X>" . excl)
       ("<R>" . (or regexp "")))
     "List of substitutions performed by `grep-expand-template'.
If car of an element matches, the cdr is evalled in to get the
substitution string.  Note dynamic scoping of variables.")

   (redefun grep-expand-template (template &optional regexp files dir excl)
     "Patch grep COMMAND string replacing <C>, <D>, <F>, <R>, and <X>.
Fixed version."
     (let ((command template)
           (cf case-fold-search)
           (case-fold-search nil))
       (dolist (kw grep+-expand-keywords command)
         (when (string-match (car kw) command)
           (setq command
                 (replace-match
                  (or (if (symbolp (cdr kw))
                        (symbol-value (cdr kw))
                        (save-match-data (eval (cdr kw))))
                      "")
                  t t command))))))

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
            ((and grep-find-command (equal current-prefix-arg '(16)))
             (list (read-from-minibuffer "Run: " grep-find-command
                                         nil nil 'grep-find-history)))
            ((not grep-find-template)
             (error "my-grep.el: No `grep-find-template' available"))
            (t (let* ((regexp (grep-read-regexp))
                      (files (grep-read-files regexp))
                      (dir (read-directory-name "Base directory: "
                                                nil default-directory t))
                      (confirm (equal current-prefix-arg '(4))))
                 (list regexp files dir confirm grep-find-template)))))))
     ;; Set `grep-highlight-matches' to `always'
     ;; since `zgrep' puts filters in the grep output.
     (let ((grep-highlight-matches 'always))
       ;; `rgrep' uses the dynamically bound value `grep-find-template'
       ;; from the argument `grep-find-template' whose value is computed
       ;; in the `interactive' spec.
       (rgrep regexp files dir confirm)))))

(setq grep-command "grep -nHE -e "
      grep-template
      "grep <X> <C> -nHE -e '<R>' <F>"
      grep-find-command
      "find . -type f -print0 | xargs -0 -e grep -nHE -e "
      grep-find-template
      "find \"<D>\" <X> -type f <F> -print0 | xargs -0 -e grep <C> -nHE -e \"<R>\""

      grep-files-aliases
      (let ((cl-extensions
              (mapconcat (lambda (x) (concat "*." x))
                         +common-lisp-file-extensions+
                         " "))
            (scheme-extensions
              (mapconcat (lambda (x) (concat "*." x))
                         +scheme-file-extensions+
                         " ")))
        `(("all"   . "*")
          ("el"    . "*.el")
          ("c"     . "*.c")
          ("h"     . "*.h")
          ("ch"    . "*.c *.C *.h *.H")
          ("hh"    . "*.hxx *.hpp *.H *.h *.HH *.h++")
          ("cc"    . "*.cc *.cxx *.cpp *.C *.CC *.c++")
          ("cchh"  . "*.cc *.CC *.cxx *.CXX *.hxx *.HXX *.cpp *.CPP *.hpp *.HPP *.C *.H *.h *.hh *.HH *.c++ *.C++ *.h++ *.H++")
          ("l"     . ,cl-extensions)
          ("cl"    . ,cl-extensions)
          ("lsp"   . ,cl-extensions)
          ("lisp"  . ,cl-extensions)
          ("clisp" . ,cl-extensions)
          ("scm"   . ,scheme-extensions)
          ("m"     . "[Mm]akefile*")
          ("make"  . "[Mm]akefile*")
          ("tex"   . "*.tex")
          ("texi"  . "*.texi")
          ("asm"   . "*.[sS]")
          ("hs"    . "*.hs *.hsc *.lhs")
          ("py"    . "*.py *.pyx *.pxd *.pxi"))))


(provide 'grep+)

;; Local Variables:
;; End:

;; grep+.el ends here
