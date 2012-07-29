;;; ibuffer-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 11 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'ibuffer)


(defalias 'list-buffers 'ibuffer)

(eval-after-load
 "ibuffer"
 '(progn
   (require 'ibuf-ext)

   (redefun ibuffer-jump-to-buffer (name)
     "Move point to the buffer whose name is NAME.

If called interactively, prompt for a buffer name and go to the
corresponding line in the Ibuffer buffer.  If said buffer is in a
hidden group filter, open it.

If `ibuffer-jump-offer-only-visible-buffers' is non-nil, only offer
visible buffers in the completion list.  Calling the command with
a prefix argument reverses the meaning of that variable."
     (interactive (list
                   (let ((only-visible ibuffer-jump-offer-only-visible-buffers))
                     (when current-prefix-arg
                       (setq only-visible (not only-visible)))
                     (if only-visible
                       (let ((table (mapcar #'(lambda (x)
                                                (buffer-name (car x)))
                                            (ibuffer-current-state-list))))
                         (when (null table)
                           (error "No buffers!"))
                         (completing-read-vanilla "Jump to buffer: "
                                                  table
                                                  nil
                                                  t))
                       (read-buffer "Jump to buffer: " nil t)))))
     (when (not (string= "" name))
       (let (buf-point)
         ;; Blindly search for our buffer: it is very likely that it is
         ;; not in a hidden filter group.
         (ibuffer-map-lines #'(lambda (buf _marks)
                                (when (string= (buffer-name buf) name)
                                  (setq buf-point (point))
                                  nil))
                            t nil)
         (when (and
                (null buf-point)
                (not (null ibuffer-hidden-filter-groups)))
           ;; We did not find our buffer.  It must be in a hidden filter
           ;; group, so go through all hidden filter groups to find it.
           (catch 'found
             (dolist (group ibuffer-hidden-filter-groups)
               (ibuffer-jump-to-filter-group group)
               (ibuffer-toggle-filter-group)
               (ibuffer-map-lines #'(lambda (buf _marks)
                                      (when (string= (buffer-name buf) name)
                                        (setq buf-point (point))
                                        nil))
                                  t group)
               (if buf-point
                 (throw 'found nil)
                 (ibuffer-toggle-filter-group)))))
         (if (null buf-point)
           ;; Still not found even though we expanded all hidden filter
           ;; groups: that must be because it's hidden by predicate:
           ;; we won't bother trying to display it.
           (error "No buffer with name %s" name)
           (goto-char buf-point)))))

   (redefun ibuffer-switch-to-saved-filter-groups (name)
     "Set this buffer's filter groups to saved version with NAME.
The value from `ibuffer-saved-filter-groups' is used."
     (interactive
      (list
       (if (null ibuffer-saved-filter-groups)
         (error "No saved filters")
         (completing-read-vanilla "Switch to saved filter group: "
                                  ibuffer-saved-filter-groups nil t))))
     (setq ibuffer-filter-groups (cdr (assoc name ibuffer-saved-filter-groups))
           ibuffer-hidden-filter-groups nil)
     (ibuffer-update nil t))

   (add-hook 'ibuffer-mode-hook
    (lambda ()
      (ibuffer-switch-to-saved-filter-groups "default")))

   (define-ibuffer-filter name-not-matches
    "Toggle current view to buffers with name not matching QUALIFIER."
    (:description "buffer name, no match"
     :reader (read-from-minibuffer "Filter by not matching (regexp): "))
    (not (string-match-p qualifier (buffer-name buf))))



   (symbol-macrolet
       ((lisp-filter `(predicate
                       .
                       (and (member* major-mode
                                     '(cl-mode
                                       lisp-mode
                                       common-lisp-mode)
                                     :test #'eq)
                            (not (string-match-p "^\\*.+\\*$"
                                                 (buffer-name))))))
        (slime-filter `(or (mode . slime-repl-mode)
                           (mode . sldb-mode)
                           (name . ,(rx "*"
                                        (or (seq (or "slime-repl"
                                                     "sldb")
                                                 (+ " ")
                                                 (or "sbcl"
                                                     "sbcl-full"
                                                     "cmucl"
                                                     "clisp"
                                                     "ccl"
                                                     "ecl"
                                                     "clozure"
                                                     "chicken"
                                                     "bigloo"
                                                     "guile"
                                                     "lisp"
                                                     "scheme")
                                                 (? "/"
                                                    (+ digit)))
                                            (or "slime-events"
                                                "slime-description"
                                                "slime-trace"
                                                "slime-compilation"
                                                "slime-xref"
                                                "slime-apropos"
                                                "slime-inspector"
                                                "slime-macroexpansion"
                                                "inferior-lisp"
                                                "lisp-interaction"
                                                "fuzzy completions"))
                                        "*"
                                        ;; (? "<"
                                        ;;    (+ digit)
                                        ;;    ">")
                                        ))))
        (emacs-lisp-filter `(predicate
                             .
                             (and (member* major-mode
                                           '(emacs-lisp-mode
                                             inferior-emacs-lisp-mode)
                                           :test #'eq)
                                  (not (string-match-p "^\\*.+\\*$"
                                                       (buffer-name))))))
        (scheme-filter `(or (mode . scheme-mode)
                            (name . ,(rx (or "*scheme*"
                                             "* Guile REPL *")
                                         (? "<"
                                            (+ digit)
                                            ">")))))
        (c-c++-filter `(or (mode . c-mode)
                           (mode . c++-mode)))
        (haskell-filter `(or (mode . haskell-mode)
                             (mode . inferior-haskell-mode)
                             (mode . inferior-hugs-mode)
                             (mode . haskell-hugs-mode)
                             (mode . ghc-core-mode)
                             (mode . hugs-mode)
                             (name . ,(rx "*haskell*"
                                          (? "<"
                                             (+ digit)
                                             ">")))))
        (prolog-filter `(or (mode . prolog-mode)
                            (name . ,(rx "*prolog*"
                                         (? "<"
                                            (+ digit)
                                            ">")))))
        (octave-filter `(or (mode . octave-mode)
                            (mode . inferiro-octave-mode)
                            (name . ,(rx "*Octave*"
                                         (? "<"
                                            (+ digit)
                                            ">")))))
        (python-filter `(or (mode . python-mode)
                            (mode . python-repl-mode)
                            (mode . inferior-python-mode)
                            (mode . python-run-mode)
                            (name . ,(rx (or "*Python*"
                                             "*IPython*"
                                             "*Python Output*")
                                         (? "<"
                                            (+ digit)
                                            ">")))))
        (cython-filter `(or (mode . cython-mode)
                            (mode . cython-compilation-mode)))
        (maxima-filter `(or (mode . maxima-mode)
                            (mode . maxima-noweb-mode)
                            (mode . inferior-maxima-mode)
                            (name . ,(rx (or "*maxima*"
                                             "*imaxima*")
                                         (? "<"
                                            (+ digit)
                                            ">")))))
        (org-filter     `(or (mode . org-mode)
                             (mode . org-agenda-mode)
                             (mode . diary-mode)
                             (mode . calendar-mode)))
        (latex-filter   `(or (mode . latex-mode)
                             (mode . tex-mode)
                             (mode . LaTeX-mode)))
        (web-filter     `(or (mode . html-mode)
                             (mode . sgml-mode)
                             (mode . nxhtml-mode)
                             (mode . nxhtml-muamo-mode)
                             (mode . nxml-mode)
                             (mode . css-mode)
                             (mode . js-mode)
                             (mode . django-nxhtml-mumamo-mode)
                             (mode . django-html-mumamo-mode)
                             (mode . rnc-mode)))
        (vc-filter      `(or (mode . magit-mode)
                             (mode . magit-commit-mode)
                             (mode . magit-diff-mode)
                             (mode . magit-key-mode)
                             (mode . magit-log-edit-mode)
                             (mode . magit-log-mode)
                             (mode . magit-reflog-mode)
                             (mode . magit-show-branches-mode)
                             (mode . magit-stash-mode)
                             (mode . magit-status-mode)
                             (mode . magit-wazzup-mode)
                             (mode . gitignore-mode)
                             (name . ,(rx bol "*magit" (* nonl) "*" eol))))
        (utility-filter `(or (name . ,(rx bol
                                          (or "*Tags List*"
                                              "makefile"
                                              "Makefile"
                                              "GNUMakefile")
                                          eol))
                             (predicate . (get-buffer-process
                                           (current-buffer)))
                             (mode . comint-mode)
                             (mode . compilation-mode)
                             (mode . grep-mode)
                             (mode . latex-compilation-mode)
                             (mode . haskell-compilation-mode)
                             (mode . hs-lint-mode)
                             (mode . hs-scan-mode)
                             (mode . gnuplot-run-mode)
                             (mode . eshell-mode)
                             (mode . shell-mode)))
        (dired-filter `(or (mode . dired-mode)))
        (common-filter `(or (name . ,(rx "*scratch*"))
                            (name . ,(rx "*Messages*"))
                            (name . ,(rx "*Pp Eval Output*"))
                            (mode . help-mode)
                            (mode . apropos-mode)
                            (mode . Info-mode)
                            (mode . Man-mode)
                            (mode . ibuffer-mode)
                            ;; handle everything
                            (predicate . t))))
     (setf ibuffer-saved-filter-groups
           `(("lisp"
              ("lisp" ,lisp-filter)
              ("slime" ,slime-filter)
              ("emacs lisp" ,emacs-lisp-filter)
              ("scheme" ,scheme-filter)

              ("org" ,org-filter))

             ("math"
              ("haskell" ,haskell-filter)
              ("prolog" ,prolog-filter)
              ("octave" ,octave-filter)
              ("maxima" ,maxima-filter)

              ("org" ,org-filter))

             ("default"
              ("lisp" ,lisp-filter)
              ("slime" ,slime-filter)
              ("emacs lisp" ,emacs-lisp-filter)
              ("scheme" ,scheme-filter)

              ("haskell" ,haskell-filter)
              ("prolog" ,prolog-filter)
              ("octave" ,octave-filter)
              ("maxima" ,maxima-filter)

              ("c/cpp" ,c-c++-filter)
              ("python" ,python-filter)
              ("cython" ,cython-filter)
              ("org" ,org-filter)
              ("latex" ,latex-filter)
              ("web" ,web-filter)
              ("vc" ,vc-filter)

              ("utility" ,utility-filter)
              ("dired"  ,dired-filter)
              ("common" ,common-filter)

              ;; ("version control" (or (mode . svn-status-mode)
              ;;                        (mode . svn-log-edit-mode)
              ;;                        (name . "^\\*svn-")
              ;;                        (name . "^\\*vc\\*$")
              ;;                        (name . "^\\*Annotate")
              ;;                        (name . "^\\*git-")
              ;;                        (name . "^\\*vc-")))
              ;; ("emacs" (or (name . "^\\*scratch\\*$")
              ;;              (name . "^\\*Messages\\*$")
              ;;              (name . "^TAGS\\(<[0-9]+>\\)?$")
              ;;              (name . "^\\*Help\\*$")
              ;;              (name . "^\\*info\\*$")
              ;;              (name . "^\\*Occur\\*$")
              ;;              (name . "^\\*grep\\*$")
              ;;              (name . "^\\*Compile-Log\\*$")
              ;;              (name . "^\\*Backtrace\\*$")
              ;;              (name . "^\\*Process List\\*$")
              ;;              (name . "^\\*gud\\*$")
              ;;              (name . "^\\*Man")
              ;;              (name . "^\\*WoMan")
              ;;              (name . "^\\*Kill Ring\\*$")
              ;;              (name . "^\\*Completions\\*$")
              ;;              (name . "^\\*tramp")
              ;;              (name . "^\\*shell\\*$")
              ;;              (name . "^\\*compilation\\*$")))
              ;; ("emacs source" (or (mode . emacs-lisp-mode)
              ;;                     (filename . "/Applications/Emacs.app")
              ;;                     (filename . "/bin/emacs")))
              ;; ("agenda" (or (name . "^\\*Calendar\\*$")
              ;;               (name . "^diary$")
              ;;               (name . "^\\*Agenda")
              ;;               (name . "^\\*org-")
              ;;               (name . "^\\*Org")
              ;;               (mode . org-mode)
              ;;               (mode . muse-mode)))
              ;; ("latex" (or (mode . latex-mode)
              ;;              (mode . LaTeX-mode)
              ;;              (mode . bibtex-mode)
              ;;              (mode . reftex-mode)))
              ;; ("dired" (or (mode . dired-mode)))
              )
             ("all"))))

   (setf ibuffer-never-show-predicates
         (list "^\\*Completions\\*$"))

   (defun ibuffer-mark-using-mode (&optional by-regexp)
     (interactive (list current-prefix-arg))
     (if by-regexp
       (call-interactively #'ibuffer-mark-by-mode-regexp)
       (call-interactively #'ibuffer-mark-by-mode)))

   ;; (def-keys-for-map1 ibuffer-mode-map +vi-essential-keys+)
   (def-keys-for-map1 ibuffer-mode-map +control-x-prefix+)
   (def-keys-for-map1 ibuffer-mode-map +vim-special-keys+)
   (def-keys-for-map1 ibuffer-mode-map
     ( ;;("q"        remove-buffer)
      ("C-k"      remove-buffer)
      ("C-S-k"    remove-buffer-and-window)

      ("f"        nil)
      ("f m"      ibuffer-filter-by-mode)
      ("f n"      ibuffer-filter-by-name)
      ("f c"      ibuffer-filter-by-content)
      ("f f"      ibuffer-filter-by-filename)
      ("f p"      ibuffer-pop-filter)
      ("f o"      ibuffer-or-filter)
      ("SPC"      ibuffer-filter-disable)
      ("* m"      ibuffer-mark-using-mode)
      ("* M"      ibuffer-mark-modified-buffers)
      ("* d"      ibuffer-mark-dired-buffers)
      ("* n"      ibuffer-mark-by-name-regexp)
      ("* f"      ibuffer-mark-by-file-name-regexp)
      ("U"        ibuffer-unmark-all)
      ("T"        ibuffer-toggle-marks)
      ("K"        ibuffer-do-delete)

      ("G"        ibuffer-switch-to-saved-filter-groups)

      ("C-z"      nil)
      ("/"        ibuffer-jump-to-buffer)

      ("<tab>"    ibuffer-forward-filter-group)
      ("S-<tab>"  ibuffer-backward-filter-group)

      ("r"        ibuffer-update)
      ("t"        ibuffer-cycle-buffers-forward)
      ("n"        ibuffer-cycle-buffers-backward)
      ("<escape>" remove-buffer)
      ("<down>"   ibuffer-cycle-buffers-forward)
      ("<up>"     ibuffer-cycle-buffers-backward)))))


(provide 'ibuffer-setup)

;; Local Variables:
;; lexical-binding: nil
;; End:

;;; ibuffer-setup.el ends here
