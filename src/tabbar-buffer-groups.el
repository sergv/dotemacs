;; tabbar-buffer-groups.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 April 2012
;; Description:

(defun tabbar-buffer-groups+ (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (let ((starred-buffer-re (rx bol "*" (+ nonl) "*" eol))
          (bufname (buffer-name)))
      (cond
        ((and (memq major-mode
                    '(cl-mode
                      lisp-mode
                      common-lisp-mode))
              (not (string-match-p starred-buffer-re bufname)))
         '("Lisp"))

        ;; count repl as both lisp and slime mode
        ((eq major-mode 'slime-repl-mode)
         '("Lisp" "SLIME"))

        ((and (memq major-mode
                    '(emacs-lisp-mode
                      inferior-emacs-lisp-mode))
              (not (member bufname
                           '("*scratch*"
                             "*Pp Eval Output*"))))
         '("Emacs Lisp"))

        ((string-match-p (rx "*"
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
                             )
                         (downcase bufname))
         (memq major-mode
               ;; count relp here too, just in case
               '(slime-repl-mode
                 sldb-mode))
         '("SLIME"))

        ((or (eq major-mode 'scheme-mode)
             (string-match-p (rx (or "*scheme*"
                                     "* Guile REPL *")
                                 (? "<"
                                    (+ digit)
                                    ">"))
                             bufname))
         '("Scheme"))

        ((memq major-mode
               '(c-mode
                 c++-mode))
         '("C/C++"))

        ((or (memq major-mode
                   '(haskell-mode
                     inferior-haskell-mode
                     inferior-hugs-mode
                     haskell-hugs-mode
                     ghc-core-mode
                     hugs-mode))
             (string-match-p (rx "*haskell*"
                                 (? "<"
                                    (+ digit)
                                    ">"))
                             bufname))
         '("Haskell"))

        ((or (memq major-mode
                   '(prolog-mode))
             (string-match-p (rx "*prolog*"
                                 (? "<"
                                    (+ digit)
                                    ">"))
                             bufname))
         '("Prolog"))

        ((or (memq major-mode
                   '(octave-mode
                     inferior-octave-mode))
             (string-match-p (rx "*Octave*"
                                 (? "<"
                                    (+ digit)
                                    ">"))
                             bufname))
         '("Octave"))

        ((or (memq major-mode
                   '(python-mode
                     python-repl-mode
                     inferior-python-mode
                     python-run-mode))
             (string-match-p (rx (or "*Python*"
                                     "*IPython*"
                                     "*Python Output*")
                                 (? "<"
                                    (+ digit)
                                    ">"))
                             bufname))
         '("Python"))

        ((memq major-mode
               '(cython-mode
                 cython-compilation-mode))
         '("Cython"))

        ((or (memq major-mode
                   '(maxima-mode
                     maxima-noweb-mode
                     inferior-maxima-mode))
             (string-match-p (rx (or "*maxima*"
                                     "*imaxima*")
                                 (? "<"
                                    (+ digit)
                                    ">"))
                             bufname))
         '("Maxima"))

        ((or (memq major-mode
                   '(org-mode
                     org-agenda-mode
                     diary-mode
                     calendar-mode)))
         '("Org"))

        ((or (memq major-mode '(LaTeX-mode latex-mode tex-mode)))
         '("LaTeX"))

        ((or (memq major-mode '(html-mode
                                sgml-mode
                                nxhtml-mode
                                nxhtml-muamo-mode
                                nxml-mode
                                css-mode
                                js-mode
                                django-nxhtml-mumamo-mode
                                django-html-mumamo-mode
                                rnc-mode)))
         '("Web"))

        ((or (memq major-mode
                   '(magit-mode
                     magit-commit-mode
                     magit-diff-mode
                     magit-key-mode
                     magit-log-edit-mode
                     magit-log-mode
                     magit-reflog-mode
                     magit-show-branches-mode
                     magit-stash-mode
                     magit-status-mode
                     magit-wazzup-mode
                     ))
             (string-match-p (rx bol "*magit" (* nonl) "*" eol) bufname))
         '("Version Control"))

        ((or (member bufname
                     '("*Tags List*"
                       "makefile"
                       "Makefile"
                       "GNUMakefile"))
             (get-buffer-process (current-buffer))
             (memq major-mode
                   '(comint-mode
                     compilation-mode
                     grep-mode
                     latex-compilation-mode
                     haskell-compilation-mode
                     hs-lint-mode
                     hs-scan-mode
                     gnuplot-run-mode
                     eshell-mode
                     shell-mode)))
         '("Utility"))


        ((memq major-mode
               '(rmail-mode
                 rmail-edit-mode
                 vm-summary-mode
                 vm-mode
                 mail-mode
                 mh-letter-mode
                 mh-show-mode
                 mh-folder-mode
                 gnus-summary-mode
                 message-mode
                 gnus-group-mode
                 gnus-article-mode
                 score-mode
                 gnus-browse-killed-mode))
         '("Mail"))

        ;; put *scratch* buffer in "Common" group
        ((or (string-match-p starred-buffer-re bufname)
             (member bufname
                     '("*scratch*"
                       "*Messages*"
                       "*Pp Eval Output*"))
             (memq major-mode
                   '(dired-mode
                     help-mode
                     apropos-mode
                     Info-mode
                     Man-mode
                     ibuffer-mode)))
         '("Common"))

        (t '("Text"))))))


(provide 'tabbar-buffer-groups)

;; Local Variables:
;; lexical-binding: t
;; End:

;; tabbar-buffer-groups.el ends here
