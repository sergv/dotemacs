;;; haskell-compile.el --- Haskell/GHC compilation sub-mode -*- lexical-binding: t -*-

;; Copyright (C) 2013  Herbert Valerio Riedel

;; Author: Herbert Valerio Riedel <hvr@gnu.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple GHC-centric compilation sub-mode; see info node
;; `(haskell-mode)compilation' for more information

;;; Code:

(require 'compile)
(require 'haskell-cabal)

(require 'haskell-autoload)

;;;###autoload
(defgroup haskell-compile nil
  "Settings for Haskell compilation mode"
  :link '(custom-manual "(haskell-mode)compilation")
  :group 'haskell)

(defcustom haskell-compile-cabal-build-command
  "cd %s && cabal build --ghc-option=-ferror-spans"
  "Default build command to use for `haskell-cabal-build' when a cabal file is detected.
The `%s' placeholder is replaced by the cabal package top folder."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-cabal-build-command-presets
  ()
  "Predefined build commands for `haskell-compile'. Should be alist of
(<preset-name> <command-with-%s>) pairs. <preset-name> will be used to prompt
user.")

;;;###autoload
(defcustom haskell-compile-command
  "ghc -O -Wall -Werror -ferror-spans -fforce-recomp -c %s"
  "Default build command to use for `haskell-cabal-build' when no cabal file is detected.
The `%s' placeholder is replaced by the current buffer's filename."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-ghc-filter-linker-messages
  t
  "Filter out unremarkable \"Loading package...\" linker messages during compilation."
  :group 'haskell-compile
  :type 'boolean)

(defconst haskell-compilation-error-main-filename-regexp
  (eval-when-compile
    (let ((ext-re
           (rx-to-string (list 'seq "."
                               (cons 'or
                                     +haskell-extensions+))))
          (lines-and-columns-re
           (concat
            "\\(?:"
            "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
            "\\|"
            "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
            "\\)")))
      (list
       (concat
        "\\(?:"
        (concat
         "^\\(?1:[^ \n\r\v\t\f].*?" ext-re "\\):"
         lines-and-columns-re
         ":\\(?6:[ \t\r\n]+[Ww]arning:\\)?")
        "\\)\\|\\(?:"
        (concat
         "^[ \t]+\\(?1:[^ \n\r\v\t\f].*?" ext-re "\\):"
         lines-and-columns-re
         ":\\(?:[ \t]+[Ee]rror:\\|\\(?6:[ \t\r\n]+[Ww]arning:\\)\\)")
        "\\)")
       1            ;; file
       (cons 2 3)   ;; line
       (cons 4 5)   ;; column
       (cons 6 nil) ;; type - error/warning
       ))))

(defconst haskell-compilation-error-auxiliary-filename-regexp
  (eval-when-compile
    `(,(eval
        `(rx (group-n 1
                      (+ (not (any ?\s ?\t ?\n ?\r)))
                      "."
                      ,(cons 'or
                             +haskell-extensions+))
             ":"
             (group-n 2
                      (+ numeric))
             ":"
             (group-n 3
                      (+ numeric))
             (? "-"
                (group-n 4
                         (+ numeric)))
             ;; Require paren or \n at end because this regexp aims to
             ;; highlight auxiliary file names, not the actuall
             ;; errors/warnings (cf type of this regexp).
             (or ")"
                 eol)))
      1
      2
      (3 . 4)
      0 ;; type - info
      )))

(defconst haskell-compilation-error-regexp-alist
  (eval-when-compile
    (let ((ext-re
           (rx-to-string (list 'seq "."
                               (cons 'or
                                     +haskell-extensions+)))))
      (list
       haskell-compilation-error-main-filename-regexp

       ;; multiple declarations
       (list
        (concat
         "^[ \t]+\\(?:Declared\\|Defined\\)[ \t]at:[ \t]+\\(?1:.+?\\." ext-re "\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$")
        1 ;; file
        2 ;; line
        4 ;; column
        0 ;; type - info
        )

       haskell-compilation-error-auxiliary-filename-regexp)))
  "Regexps used for matching GHC compile messages.
See `compilation-error-regexp-alist' for semantics.")

(defvar haskell-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map))
  "Keymap for `haskell-compilation-mode' buffers.
This is a child of `compilation-mode-map'.")

(defun haskell-compilation-filter-hook ()
  "Local `compilation-filter-hook' for `haskell-compilation-mode'."
  (when haskell-compile-ghc-filter-linker-messages
    (delete-matching-lines "^ *Loading package [^ \t\r\n]+ [.]+ linking [.]+ done\\.$"
                           (save-excursion (goto-char compilation-filter-start)
                                           (line-beginning-position))
                           (point))))

(defvar haskell-compilation-extra-error-modes
  '(4bssd watcom sun msft lcc gnu java ibm epc edg-1 edg-2 borland aix absoft)
  "Extra modes from `compilation-error-regexp-alist-alist' whose warnings
will be colorized in `haskell-compilation-mode'.")

(define-compilation-mode haskell-compilation-mode "Haskell Compilation"
  "Haskell/GHC specific `compilation-mode' derivative.
This mode provides support for GHC 7.[46]'s compile
messages. Specifically, also the `-ferror-spans` source location
format is supported, as well as info-locations within compile
messages pointing to additional source locations."
  (setq-local compilation-error-regexp-alist
              (append haskell-compilation-error-regexp-alist
                      '(4bssd watcom sun msft lcc gnu java ibm epc edg-1 edg-2 borland aix absoft)))
  (add-hook 'compilation-filter-hook
            'haskell-compilation-filter-hook nil t))

(defvar haskell-compile--build-presets-history nil)

;;;###autoload
(defun haskell-compile (&optional edit-command)
  "Compile the Haskell program including the current buffer.
Tries to locate the next cabal description in current or parent
folders via `haskell-cabal-find-dir' and if found, invoke
`haskell-compile-cabal-build-command' from the cabal package root
folder. If no cabal package could be detected,
`haskell-compile-command' is used instead.

If prefix argument EDIT-COMMAND is non-nil, `haskell-compile' prompts for
one of the predefined compile commands.

`haskell-compile' uses `haskell-compilation-mode' which is
derived from `compilation-mode'. See Info
node `(haskell-mode)compilation' for more details."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                          compilation-save-buffers-predicate)
  (let* ((cabdir (haskell-cabal-find-dir))
         (raw-command
          (if edit-command
            (let* ((preset
                    (intern
                     (completing-read "build preset: "
                                      haskell-compile-cabal-build-command-presets
                                      nil
                                      t
                                      nil
                                      'haskell-compile--build-presets-history)))
                   (command
                    (cadr
                     (assoc preset haskell-compile-cabal-build-command-presets))))
              ;; remember command so it will be called again in the future
              (setf haskell-compile-cabal-build-command command)
              command)
            haskell-compile-cabal-build-command))
         (srcname (buffer-file-name))
         (command (cond (cabdir
                         (format raw-command (expand-file-name cabdir)))
                        ((and srcname (derived-mode-p 'haskell-mode))
                         (format haskell-compile-command srcname))
                        (t
                         raw-command))))
    ;; (when (and edit-command (not (eq edit-command '-)))
    ;;   (setq command (compilation-read-command command)))

    (compilation-start command 'haskell-compilation-mode)))

(provide 'haskell-compile)
;;; haskell-compile.el ends here
