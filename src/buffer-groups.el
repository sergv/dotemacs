;; buffer-groups.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 31 July 2012
;; Description:
;; Common information as to how classify buffers based on ther name, major mode etc


(defconst +buffer-groups+
  (symbol-macrolet
      ((lisp-filter `(or (predicate
                          .
                          (and (memq major-mode
                                     '(cl-mode
                                       lisp-mode
                                       common-lisp-mode))
                               (not (string-match-pure? "^\\*.+\\*$"
                                                        (buffer-name)))))))
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
                                                    "lisp"
                                                    "scheme"
                                                    "chicken"
                                                    "bigloo"
                                                    "scheme48"
                                                    "guile"
                                                    "gambit"
                                                    "gauche"
                                                    "mit")
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
       (emacs-lisp-filter `(or (predicate
                                .
                                (and (memq major-mode
                                           '(emacs-lisp-mode
                                             inferior-emacs-lisp-mode))
                                     (not (string-match-pure? "^\\*.+\\*$"
                                                              (buffer-name)))))))
       (scheme-filter `(or (mode . scheme-mode)
                           (name . ,(rx (or (seq "*"
                                                 (? (or "chicken"
                                                        "bigloo"
                                                        "scheme48"
                                                        "guile"
                                                        "gambit"
                                                        "gauche"
                                                        "mit")
                                                    "-")
                                                 "scheme*")
                                            "* Guile REPL *")
                                        (? "<"
                                           (+ digit)
                                           ">")))))
       (c-c++-filter `(or (mode . c-mode)
                          (mode . c++-mode)
                          (mode . glsl-mode)))
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
       (book-filter    `(or (mode . doc-view-mode)
                            (name . ,(rx bol
                                         (+ anything)
                                         (or ".pdf"
                                             ".djvu"
                                             ".ps"
                                             ".dvi")
                                         eol))))
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
       (lowlevel-prog-filter `(or (mode . asm-mode)))
       (other-prog-filter `(or (name . ,(rx bol
                                            (or "makefile"
                                                "Makefile"
                                                "GNUMakefile")
                                            eol))
                               (mode . makefile-mode)
                               (mode . makefile-automake-mode)
                               (mode . makefile-gmake-mode)
                               (mode . makefile-makepp-mode)
                               (mode . makefile-bsdmake-mode)
                               (mode . makefile-imake-mode)
                               (mode . cmake-mode)
                               (mode . shell-script-mode)
                               (mode . sh-mode)
                               (mode . sh-script-mode)
                               (mode . conf-space-mode)
                               (mode . conf-mode)
                               (mode . conf-xdefaults-mode)
                               (mode . lua-mode)))
       (utility-filter `(or (name . ,(rx bol (or "*Tags List*") eol))
                            (predicate . (get-buffer-process (current-buffer)))
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
       (other-filter `(or (name . ,(rx "*scratch*"))
                          (name . ,(rx "*Messages*"))
                          (name . ,(rx "*Pp Eval Output*"))
                          (mode . help-mode)
                          (mode . apropos-mode)
                          (mode . Info-mode)
                          (mode . Man-mode)
                          (mode . ibuffer-mode)
                          ;; handle everything
                          (predicate . t))))
    `(("lisp"       ,lisp-filter)
      ("slime"      ,slime-filter)
      ("emacs lisp" ,emacs-lisp-filter)
      ("scheme"     ,scheme-filter)

      ("haskell"    ,haskell-filter)
      ("prolog"     ,prolog-filter)
      ("octave"     ,octave-filter)
      ("maxima"     ,maxima-filter)

      ("c/cpp"      ,c-c++-filter)
      ("python"     ,python-filter)
      ("cython"     ,cython-filter)
      ("org"        ,org-filter)
      ("books"      ,book-filter)
      ("latex"      ,latex-filter)
      ("web"        ,web-filter)
      ("vc"         ,vc-filter)
      ("lowlevel programming" ,lowlevel-prog-filter)
      ("other programming" ,other-prog-filter)

      ("utility"    ,utility-filter)
      ("dired"      ,dired-filter)
      ("other"      ,other-filter)))
  "Alist of (<group-name> <group-definition>) where <group-definition>
is in format required by `ibuffer-saved-filter-groups'.")


(provide 'buffer-groups)

;; Local Variables:
;; End:

;; buffer-groups.el ends here
