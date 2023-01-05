;;; haskell-customize.el --- Customization settings -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.
;;               2020 Marc Berkowitz <mberkowitz@github.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization variables

(defgroup haskell nil
  "Major mode for editing Haskell programs."
  :link '(custom-manual "(haskell-mode)")
  :group 'languages
  :prefix "haskell-")

(defvar haskell-mode-pkg-base-dir (file-name-directory load-file-name)
  "Package base directory of installed `haskell-mode'.
Used for locating additional package data files.")

(defcustom haskell-completing-read-function 'ido-completing-read
  "Default function to use for completion."
  :group 'haskell
  :type '(choice
          (function-item :tag "ido" :value ido-completing-read)
          (function-item :tag "helm" :value helm--completing-read-default)
          (function-item :tag "completing-read" :value completing-read)
          (function :tag "Custom function")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defcustom haskell-doc-prettify-types t
  "Replace some parts of types with Unicode characters like \"∷\"
when showing type information about symbols."
  :group 'haskell-doc
  :type 'boolean
  :safe 'booleanp)

(defcustom haskell-import-mapping
  '()
  "Support a mapping from module to import lines.

E.g. '((\"Data.Map\" . \"import qualified Data.Map as M
import Data.Map (Map)
\"))

This will import

import qualified Data.Map as M
import Data.Map (Map)

when Data.Map is the candidate.

"
  :type '(repeat (cons (string :tag "Module name")
                       (string :tag "Import lines")))
  :group 'haskell-interactive)

(defcustom haskell-stylish-on-save nil
  "Whether to run stylish-haskell on the buffer before saving.
If this is true, `haskell-add-import' will not sort or align the
imports."
  :group 'haskell
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessor functions

(defvar inferior-haskell-root-dir nil
  "The path which is considered as project root, this is determined by the
presence of a *.cabal file or stack.yaml file or something similar.")

(defun haskell-build-type ()
  "Looks for cabal and stack spec files.
   When found, returns a pair (TAG . DIR)
   where TAG is 'cabal-project, 'cabal-sandbox. 'cabal, or 'stack;
   and DIR is the directory containing cabal or stack file.
   When none found, DIR is nil, and TAG is 'ghc"
  ;; REVIEW maybe just 'cabal is enough.
  (let ((cabal-project (locate-dominating-file default-directory "cabal.project"))
        (cabal-sandbox (locate-dominating-file default-directory "cabal.sandbox.config"))
        (stack         (locate-dominating-file default-directory "stack.yaml"))
        (cabal         (locate-dominating-file
                        default-directory
                        (lambda (d)
                          (cl-find-if
                           (lambda (f) (string-match-p ".\\.cabal\\'" f))
                           (directory-files d))))))
    (cond
     ((and cabal-project (executable-find "cabal"))
      (cons 'cabal-project cabal-project))
     ((and cabal-sandbox (executable-find "cabal"))
      (cons 'cabal-sandbox cabal-sandbox))
     ((and stack (executable-find "stack"))
      (cons 'stack stack))
     ((and cabal (executable-find "cabal"))
      (cons 'cabal cabal))
     ((executable-find "ghc") (cons 'ghc nil))
     (t (error "Could not find any installation of GHC.")))))

(provide 'haskell-customize)
