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

(provide 'haskell-customize)
