;; grep-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 20 August 2015
;; Description:

(eval-when-compile
  (require 'set-up-platform))

(setf find-program
      (fold-platform-os-type
       "find"
       (if (executable-find "unixfind")
           "unixfind"
         "find")))

(setf grep-command "grep -HnE -e "
      grep-template
      "grep <X> <C> -nH <E> -e '<R>' <F>"
      grep-find-command
      (format "%s -L -O3 . -type f -print0 | xargs -0 -e grep -HnE -e "
              find-program)
      grep-find-template
      (format "%s -L -O3 \"<D>\" <X> -type f <F> -print0 | xargs -0 -e grep <C> -Hn <E> -e \"<R>\""
              find-program)

      grep-files-aliases
      (eval-when-compile
        (let ((haskell-exts
               (string-join (cons
                             "cabal.project"
                             (cons
                              "cabal.project.local"
                              (--map (concat "*." it)
                                     (cons "cabal" +haskell-extensions+))))
                            " "))
              (mk-globs (lambda (xs)
                          (string-join (--map (concat "*." it) xs) " "))))
          `(("all"      . "*")
            ("el"       . "*.el .emacs")
            ("c"        . ,(funcall mk-globs +c-source-exts+))
            ("h"        . ,(funcall mk-globs +c-header-exts+))
            ("ch"       . ,(funcall mk-globs (append +c-header-exts+ +c-source-exts+)))
            ("hh"       . ,(funcall mk-globs +cpp-header-exts+))
            ("cc"       . ,(funcall mk-globs +cpp-source-exts+))
            ("cchh"     . ,(funcall mk-globs (append +cpp-header-exts+ +cpp-source-exts+)))
            ("clj"      . "*.clj")
            ("clojure"  . "*.clj")
            ("java"     . "*.java")
            ("android"  . "*.java *.kt *.xml *.gradle *.gradle.kts *.toml")
            ("mk"       . "[Mm]akefile* *.mk")
            ("make"     . "[Mm]akefile* *.mk")
            ("makefile" . "[Mm]akefile* *.mk")
            ("rs"       . "*.rs")
            ("rust"      . "*.rs")
            ("tex"      . "*.tex")
            ("texi"     . "*.texi")
            ("asm"      . "*.s")
            ("llvm"     . "*.ll")
            ("xml"      . "*.xml")
            ("hs"       . ,haskell-exts)
            ("haskell"  . ,haskell-exts)
            ("nix"      . "*.nix")
            ("lua"      . "*.lua")
            ("lisp"     . "*.lisp *.lsp *.system *.asdf")
            ("py"       . "*.py *.pyx *.pxd *.pxi"))))

      grep-find-ignored-directories
      (eval-when-compile
        (append +ignored-directories+
                (--map (concat it "*") +ignored-directory-prefixes+))))

(autoload 'grep-set-up-error-regexp "grep-setup")
(autoload 'rgrep-wrapper "grep-setup" nil t)
(autoload 'rgrep-region "grep-setup" nil t)

(eval-after-load "grep" '(require 'grep-setup))

(autoload 'egrep "egrep" nil t)
(autoload 'egrep-region "egrep" nil t)

(provide 'grep-autoload)

;; Local Variables:
;; End:

;; grep-autoload.el ends here
