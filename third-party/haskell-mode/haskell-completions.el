;;; haskell-completions.el --- Haskell Completion package -*- lexical-binding: t -*-

;; Copyright © 2015-2016 Athur Fayzrakhmanov. All rights reserved.

;; This file is part of haskell-mode package.
;; You can contact with authors using GitHub issue tracker:
;; https://github.com/haskell/haskell-mode/issues

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides completions related functionality for
;; Haskell Mode such grab completion prefix at point, and etc..

;; Some description
;; ================
;;
;; For major use function `haskell-completions-grab-prefix' is supposed, and
;; other prefix grabbing functions are used internally by it.  So, only this
;; function have prefix minimal length functionality and invokes predicate
;; function `haskell-completions-can-grab-prefix'.

;;; Code:

(require 'haskell-mode)

(defvar haskell-completions--pragma-names
  (list "DEPRECATED"
        "INCLUDE"
        "INCOHERENT"
        "INLINABLE"
        "INLINE"
        "LANGUAGE"
        "LINE"
        "MINIMAL"
        "NOINLINE"
        "NOUNPACK"
        "OPTIONS"
        "OPTIONS_GHC"
        "OVERLAPPABLE"
        "OVERLAPPING"
        "OVERLAPS"
        "RULES"
        "SCC"
        "SOURCE"
        "SPECIALIZE"
        "UNPACK"
        "WARNING")
  "A list of supported pragmas.
This list comes from GHC documentation (URL
`https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/pragmas.html'.")

(defvar haskell-completions--instance-pragma-names
  (list "INCOHERENT"
        "OVERLAPPABLE"
        "OVERLAPPING"
        "OVERLAPS")
  "A list of pragmas that can be used within instances.")

(provide 'haskell-completions)
;;; haskell-completions.el ends here
