;; buffer-groups.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 31 July 2012
;; Description:
;; Common information as to how classify buffers based on ther name, major mode etc

(eval-when-compile
  (require 'cl))

(defvar +buffer-groups+
  (cl-symbol-macrolet
      ((haskell-filter `(or (mode . haskell-mode)
                            (mode . haskell-ts-mode)
                            (mode . haskell-literate-mode)
                            (mode . haskell-c-mode)
                            (mode . haskell-c2hs-mode)
                            (mode . haskell-hsc-mode)
                            (mode . haskell-cabal-mode)
                            (mode . ghc-core-mode)
                            (mode . cmm-mode)
                            (mode . hugs-mode)
                            (mode . alex-mode)
                            (mode . happy-mode)
                            (mode . uuag-mode)
                            (name . ,(rx bos
                                         "*"
                                         (or "haskell" "ghci")
                                         "*"
                                         (? "<" (+ digit) ">")
                                         eos))))
       (proof-assistants-filter `(or (mode . agda2-mode)
                                     (mode . coq-mode)
                                     (mode . lean-mode)
                                     (name . ,(rx bos
                                                  (or "*Agda information*"
                                                      (seq "*"
                                                           (or (seq "agda"
                                                                    (? "2"))
                                                               "lean"
                                                               "coq")
                                                           "*"
                                                           (? "<" (+ digit) ">")))
                                                  eos))))
       (nix-filter `(or (mode . nix-mode)))
       (hdl-filter `(or (mode . verilog-mode)
                        (mode . vhdl-mode)
                        (mode . ucf-mode)))
       (clojure-filter `(or (mode . clojure-mode)
                            (mode . kibit-check-mode)))
       (lisp-filter `(or (mode . lisp-mode)
                         (mode . common-lisp-mode)
                         (mode . scheme-mode)))
       (emacs-lisp-filter `(or (predicate
                                .
                                (and (memq major-mode
                                           '(emacs-lisp-mode
                                             inferior-emacs-lisp-mode))
                                     (not (string-match-p "^\\*.+\\*$"
                                                          (buffer-name)))))))
       (c-c++-filter `(or (mode . c-mode)
                          (mode . c++-mode)
                          (mode . glsl-mode)))
       (rust-filter `(or (mode . rust-mode)
                         (mode . cargo-toml-mode)))
       (isabelle-filter `(or (mode . isar-mode)))
       (ml-filter `(or (mode . sml-mode)
                       (name . ,(rx bos
                                    "*\\(?:sml\\)-repl*"
                                    (? "<" (+ digit) ">")
                                    eos))))
       (ocaml-filter `(or (mode . tuareg-mode)
                          (mode . tuareg-interactive-mode)
                          (name . ,(rx bos
                                       "*\\(?:ocaml\\)-repl*"
                                       (? "<" (+ digit) ">")
                                       eos))))
       (array-filter `(or (mode . j-mode)
                          (mode . inferior-j-mode)))
       (octave-filter `(or (mode . octave-mode)
                           (mode . inferior-octave-mode)
                           (name . ,(rx bos
                                        "*Octave*"
                                        (? "<"
                                           (+ digit)
                                           ">")
                                        eos))))
       (python-filter `(or (mode . python-mode)
                           (mode . python-repl-mode)
                           (mode . inferior-python-mode)
                           (mode . python-run-mode)
                           (name . ,(rx bos
                                        (or "*Python*"
                                            "*IPython*"
                                            "*Python Output*")
                                        (? "<"
                                           (+ digit)
                                           ">")
                                        eos))))
       (ptx-filter `(or (mode . ptx-mode)))

       (prolog-filter `(or (mode . prolog-mode)
                           (name . ,(rx bos
                                        "*prolog*"
                                        (? "<"
                                           (+ digit)
                                           ">")
                                        eos))))
       (org-filter     `(or (mode . org-mode)
                            (mode . org-agenda-mode)
                            (mode . diary-mode)
                            (mode . calendar-mode)))
       (book-filter    `(or (mode . doc-view-mode)
                            (name . ,(rx bos
                                         (+ anything)
                                         (or ".pdf"
                                             ".djvu"
                                             ".ps"
                                             ".dvi")
                                         eos))))
       (latex-filter   `(or (mode . latex-mode)
                            (mode . tex-mode)
                            (mode . LaTeX-mode)))
       (java-filter    `(or (mode . java-mode)
                            (mode . groovy-mode)))
       (elm-filter     `(or (mode . elm-mode)))
       (web-filter     `(or (mode . html-mode)
                            (mode . sgml-mode)
                            (mode . nxhtml-mode)
                            (mode . nxhtml-muamo-mode)
                            (mode . css-mode)
                            (mode . js-mode)
                            (mode . js2-mode)
                            (mode . web-mode)
                            (mode . django-nxhtml-mumamo-mode)
                            (mode . django-html-mumamo-mode)
                            (mode . rnc-mode)))
       (config-filter  `(or (mode . nxml-mode)
                            (mode . markdown-mode)
                            (mode . yaml-mode)
                            (mode . rst-mode)
                            (mode . toml-mode)
                            (mode . json-mode)
                            (mode . json-ts-mode)
                            (mode . lisp-data-mode)))
       (lowlevel-prog-filter `(or (mode . asm-mode)
                                  (mode . nasm-mode)
                                  (mode . llvm-mode)
                                  (mode . tablegen-mode)))
       (other-prog-filter `(or (name . ,(rx bos
                                            (or "makefile"
                                                "Makefile"
                                                "GNUMakefile")
                                            eos))
                               (mode . autoconf-mode)
                               (mode . cmake-mode)
                               (mode . conf-colon-mode)
                               (mode . conf-javaprop-mode)
                               (mode . conf-mode)
                               (mode . conf-ppd-mode)
                               (mode . conf-space-mode)
                               (mode . conf-unix-mode)
                               (mode . conf-windows-mode)
                               (mode . conf-xdefaults-mode)
                               (mode . diff-mode)
                               (mode . lua-mode)
                               (mode . makefile-automake-mode)
                               (mode . makefile-bsdmake-mode)
                               (mode . makefile-gmake-mode)
                               (mode . makefile-imake-mode)
                               (mode . makefile-makepp-mode)
                               (mode . makefile-mode)
                               (mode . sh-mode)
                               (mode . sh-script-mode)
                               (mode . shell-script-mode)
                               (mode . tcl-mode)))
       (git-filter `(or (mode . git-commit-mode)
                        (mode . git-rebase-mode)
                        (mode . gitconfig-mode)
                        (mode . gitignore-mode)
                        (mode . magit-cherry-mode)
                        (mode . magit-commit-mode)
                        (mode . magit-diff-mode)
                        (mode . magit-diff-mode)
                        (mode . magit-key-mode)
                        (mode . magit-log-edit-mode)
                        (mode . magit-log-mode)
                        (mode . magit-log-mode)
                        (mode . magit-log-select-mode)
                        (mode . magit-merge-preview-mode)
                        (mode . magit-mode)
                        (mode . magit-mode)
                        (mode . magit-popup-mode)
                        (mode . magit-popup-mode)
                        (mode . magit-process-mode)
                        (mode . magit-reflog-mode)
                        (mode . magit-reflog-mode)
                        (mode . magit-refs-mode)
                        (mode . magit-refs-mode)
                        (mode . magit-repolist-mode)
                        (mode . magit-repolist-mode)
                        (mode . magit-revision-mode)
                        (mode . magit-revision-mode)
                        (mode . magit-show-branches-mode)
                        (mode . magit-stashes-mode)
                        (mode . magit-stash-mode)
                        (mode . magit-stash-mode)
                        (mode . magit-status-mode)
                        (mode . magit-status-mode)
                        (mode . magit-submodule-list-mode)
                        (mode . magit-submodule-list-mode)
                        (mode . magit-wazzup-mode)
                        (name . ,(rx bos "*magit" (* nonl) (? "*") eos))))
       (utility-filter `(or (name . ,(rx bos (or "*Tags List*") eos))
                            (predicate . (get-buffer-process (current-buffer)))
                            (mode . comint-mode)
                            (mode . compilation-mode)
                            (mode . grep-mode)
                            (mode . clojure-compilation-mode)
                            (mode . latex-compilation-mode)
                            (mode . haskell-compilation-mode)
                            (mode . rust-compilation-mode)
                            (mode . hs-lint-mode)
                            (mode . hs-scan-mode)
                            (mode . gnuplot-run-mode)
                            (mode . eshell-mode)
                            (predicate . (and (or (get-buffer-process (current-buffer))
                                                  (eq major-mode 'shell-mode))
                                              (not (string-match-p "Async Shell Command"
                                                                   (buffer-name)))))))
       (dired-filter `(or (mode . dired-mode)))
       (other-filter `(or (name . ,(rx (or "*scratch*"
                                           "*Messages*"
                                           "*Pp Eval Output*")))
                          (mode . help-mode)
                          (mode . apropos-mode)
                          (mode . Info-mode)
                          (mode . Man-mode)
                          (mode . ibuffer-mode)
                          (mode . ebuf-mode)
                          ;; handle everything
                          (predicate . t))))
    `(("haskell"              ,haskell-filter)
      ("proof assistants"     ,proof-assistants-filter)
      ("rust"                 ,rust-filter)
      ("c/c++"                ,c-c++-filter)
      ("ptx"                  ,ptx-filter)
      ("array"                ,array-filter)
      ("octave"               ,octave-filter)
      ("hdl"                  ,hdl-filter)
      ("nix"                  ,nix-filter)
      ("clojure"              ,clojure-filter)
      ("lisp"                 ,lisp-filter)
      ("emacs lisp"           ,emacs-lisp-filter)
      ("isabelle-filter"      ,isabelle-filter)
      ("ml"                   ,ml-filter)
      ("ocaml"                ,ocaml-filter)
      ("python"               ,python-filter)

      ("prolog"               ,prolog-filter)

      ("org"                  ,org-filter)
      ("books"                ,book-filter)
      ("latex"                ,latex-filter)
      ("java/android"         ,java-filter)
      ("elm"                  ,elm-filter)
      ("web"                  ,web-filter)
      ("config"               ,config-filter)
      ("lowlevel programming" ,lowlevel-prog-filter)
      ("other programming"    ,other-prog-filter)

      ("git"                  ,git-filter)
      ("utility"              ,utility-filter)
      ("dired"                ,dired-filter)
      ("other"                ,other-filter)))
  "Alist of (<group-name> <group-definition>) where <group-definition>
is in format required by `ibuffer-saved-filter-groups'.")

(provide 'buffer-groups)

;; Local Variables:
;; End:

;; buffer-groups.el ends here
