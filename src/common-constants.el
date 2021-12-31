;; common-constants.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 August 2021
;; Description:

(eval-when-compile
  (require 'set-up-platform))

(defconst +build-products-extensions+
  (append
   '(".cmi" ".cmxa" ".cma" ".cmx" ".cmo"
     ".o" ".obj" ".hi" ".chi" ".p_o" ".p_hi" ".prof_o" ".prof_hi" ".dyn_o" ".dyn_hi"
     ".a"
     ".mix" ".tix"
     ".fasl" ".lo" ".la" ".gmo" ".mo"
     ".pyc" ".pyo"
     ".class" ".dex"
     ".ibc" ".agdai"
     ".elc")
   (fold-platform-os-type
    nil
    '(".pdb" ".lib")))
  "List of file name endings to generally ignore.")

(defconst +ignored-file-extensions+
  (append
   +build-products-extensions+
   '(".annot"
     "~" ".bin" ".out" ".lbin" ".elc" ".glo" ".idx" ".lot"
     ".bbl" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps"
     ".fns" ".kys" ".pgs" ".tps" ".vrs"
     ".gz" ".tar" ".bz2" ".xz" ".7z")
   (fold-platform-os-type
    '(".so")
    '(".dll")))
  "List of file name endings to generally ignore.")

(defconst +version-control-directories+
  [".svn" ".git" ".hg" "_darcs" ".pijul"]
  "List of directory names used by version-control systems.")

(defconst +ignored-directories+
  (vconcat +version-control-directories+
           ["dist" "node_modules" ".HTF" ".idea"])
  "List of directory names to generally ignore.")

(defconst +ignored-directory-prefixes+
  [".cabal-sandbox" ".stack-work" "dist-"]
  "List of directory names to generally ignore as a prefixes.")

(defconst +tar-regexp+
  (rx "."
      (or "tgz"
          "t7z"
          "tbz"
          "tbz2"
          "txz"
          "taz"
          "tar"
          "tar.gz"
          "tar.bz2"
          "tar.7z"
          "tar.xz"
          "tar.lz"
          "tar.lzip")
      eos))

(defconst +archive-regexp+
  (rx "."
      (or "tgz"
          "t7z"
          "tbz"
          "tbz2"
          "txz"
          "taz"
          "tar"
          "tar.gz"
          "tar.bz2"
          "tar.7z"
          "tar.xz"
          "tar.lz"
          "tar.lzip"

          "arj"
          "lzh"
          "zip"
          "z"
          "Z"
          "gz"
          "bz"
          "deb"
          "rpm"
          "lzip"
          "lz"
          "xz")
      eos))

(provide 'common-constants)

;; Local Variables:
;; End:

;; common-constants.el ends here
