;; grep-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 20 August 2015
;; Description:

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
      (let ((haskell-exts
             (join-lines (--map (concat "*." it)
                                (cons "cabal" *haskell-extensions*))
                         " ")))
        `(("all"      . "*")
          ("el"       . "*.el")
          ("c"        . "*.c")
          ("h"        . "*.h")
          ("ch"       . "*.c *.h")
          ("hh"       . "*.hh *.hxx *.hpp *.h *.h++")
          ("cc"       . "*.cc *.cxx *.cpp *.c *.c++")
          ("cchh"     . "*.c *.cc *.cxx *.cpp *.c++ *.h *.hh *.hxx *.hpp *.h++ *.inl *.inc *.incl")
          ("clj"      . "*.clj")
          ("clojure"  . "*.clj")
          ("java"     . "*.java")
          ("mk"       . "[Mm]akefile* *.mk")
          ("make"     . "[Mm]akefile* *.mk")
          ("makefile" . "[Mm]akefile* *.mk")
          ("tex"      . "*.tex")
          ("texi"     . "*.texi")
          ("asm"      . "*.s")
          ("llvm"     . "*.ll")
          ("xml"      . "*.xml")
          ("hs"       . ,haskell-exts)
          ("haskell"  . ,haskell-exts)
          ("py"       . "*.py *.pyx *.pxd *.pxi")))

      grep-find-ignored-directories
      (append *ignored-directories*
              (--map (concat it "*") *ignored-directory-prefixes*)))


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
