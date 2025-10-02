;;; attrap.el --- ATtempt To Repair At Point  -*- lexical-binding: t -*-

;; Copyright (c) 2018 Jean-Philippe Bernardy


;; Author: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; Maintainer: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; URL: https://github.com/jyp/attrap
;; Created: February 2018
;; Keywords: programming, tools
;; Package-Requires: ((dash "2.12.0") (emacs "25.1") (f "0.19.0") (s "1.11.0"))
;; Version: 0.2

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

;; Attrap! provides a command to attempt to fix the flycheck error at point.
;;
;; Users: Invoke the command `attrap-attrap' when point is on a
;; flycheck or flymake error, and check the results.  (If several
;; fixes apply you will be asked which one to apply.) Attrap!
;; currently comes with builtin fixers for haskell (GHC messages) and
;; elisp.
;;
;; Configuration: `attrap-flymake-backends-alist' is an alist from
;; flymake backend to attrap fixer.  `attrap-flycheck-checkers-alist'
;; is an alist from flycheck checker symbol to attrap fixer.  All the
;; See below for the definition of a fixer.
;;
;; A fixer is a element is a side-effect-free function mapping an
;; error message MSG to a list of options.  An option is a cons of a
;; description and a repair.  (Thus a list of options is an alist.)
;; The repair is a function of no argument which is meant to apply one
;; fix suggested by MSG in the current buffer, at point.  The
;; description is meant to be a summarized user-facing s-expr which
;; describes the repair.  This description can be used for example for
;; selecting the best repair.  An option can be conveniently defined
;; using `attrap-option'.  A singleton option list can be conveniently
;; defined using `attrap-one-option'.

(eval-when-compile
  (require 'macro-util))

;;; Code:
(require 'common)
(require 'dash)
(require 'eproj)
(require 'haskell-misc)
(require 'haskell-regexen)
(require 's)
(require 'vim-motions)

(declare-function flycheck-error-message "ext:flycheck" (cl-x))
(declare-function flycheck-overlays-at "ext:flycheck" (pos))
(declare-function flycheck-get-checker-for-buffer "ext:flycheck" ())
(declare-function flymake-diagnostics "ext:flycheck" (&optional beg end))
(declare-function flymake-diagnostic-backend "flymake" (diag))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))

(require 'common-whitespace)

(defcustom attrap-flycheck-checkers-alist '((lsp . attrap-ghc-fixer)
                                            (haskell-dante . attrap-ghc-fixer)
                                            (haskell-cabal-build . attrap-ghc-fixer)
                                            (emacs-lisp . attrap-elisp-fixer))
  "An alist from flycheck checker symbol to attrap fixer."
  :type '(alist :key-type symbol :value-type function)
  :group 'attrap)

(declare-function flymake-flycheck-diagnostic-function-for 'flymake-flycheck (checker))
(with-eval-after-load 'flymake-flycheck
  (defalias 'attrap-flymake-hlint
    (flymake-flycheck-diagnostic-function-for 'haskell-hlint)))

(defcustom attrap-flymake-backends-alist
  '((dante-flymake . attrap-ghc-fixer)
    (LaTeX-flymake . attrap-LaTeX-fixer)
    (attrap-flymake-hlint . attrap-hlint-fixer)
    (elisp-flymake-byte-compile . attrap-elisp-fixer)
    (elisp-flymake-checkdoc . attrap-elisp-fixer))
  "An alist from flymake backend to attrap fixer."
  :type '(alist :key-type symbol :value-type function)
  :group 'attrap)

(defvar attrap-select-predefined-option nil
  "Pick this option instead of asking user if defined.")

(defun attrap-select-and-apply-option (options)
  "Ask the user which of OPTIONS is best, then apply it."
  (when (not options) (error "No fixer applies to the issue at point"))
  (let ((selected-fix (if (eq 1 (length options))
                          (car options)
                        (let ((named-options (--map (cons (format "%s" (car it)) (cdr it))
                                                    options)))
                          (if noninteractive
                              (if attrap-select-predefined-option
                                  (or (assoc attrap-select-predefined-option named-options)
                                      (error "Predefined option ‘%s’ does not match available options: %s\n%s"
                                             attrap-select-predefined-option
                                             named-options
                                             (equal attrap-select-predefined-option (car (cdr named-options)))))
                                (error "Multiple repair options apply: %s"
                                       named-options))
                            (assoc (completing-read "repair using: "
                                                    named-options
                                                    nil
                                                    t)
                                   named-options))))))
    ;; (message "SELECTED-FIX: %s" selected-fix)
    ;; (message "Applied %s" (car selected-fix))
    (save-excursion
      (funcall (cl-second selected-fix))
      (awhen (cl-third selected-fix)
        (cl-assert (overlayp it))
        (let ((err (overlay-get it 'flycheck-error)))
          (cl-assert (flycheck-error-p err))
          ;; Remove the error we’re looking at.
          (setq flycheck-current-errors
                (remq err flycheck-current-errors))
          (delete-overlay it))))))

;;;###autoload
(defun attrap-flymake (pos)
  "Attempt to repair the flymake error at POS."
  (interactive "d")
  (let ((diags (flymake-diagnostics pos)))
    (when (not diags) (error "No flymake diagnostic at point"))
    (attrap-select-and-apply-option
     (--map (list (car it) (cdr it) nil)
            (-non-nil (--mapcat (let ((fixer (alist-get (flymake-diagnostic-backend it)
                                                        attrap-flymake-backends-alist)))
                                  (when fixer (funcall fixer
                                                       (substring-no-properties (flymake-diagnostic-text it))
                                                       (flymake-diagnostic-beg it)
                                                       (flymake-diagnostic-end it))))
                                diags))))))

;;;###autoload
(defun attrap--find-fixes-for-flycheck-messages (checker messages)
  "
MESSAGES should be list of pairs (<error message str> <responsible flycheck overlay>)

Returns list of triples

(<fix name> <fix action, lambda of 0 args> <responsible flycheck overlay>)"
  (let ((fixers (-map #'cdr (--filter (eq (car it) checker) attrap-flycheck-checkers-alist))))
    (when (not fixers) (error "No fixers for flycheck-checker %s" checker))
    (--filter (car it)
              (mapcan (lambda (msg)
                        (mapcan (lambda (fixer)
                                  (cl-assert (functionp fixer))
                                  (let ((ov (cdr msg)))
                                    (cl-assert (overlayp ov))
                                    (--map (list (car it) (cdr it) ov)
                                           (funcall fixer
                                                    (substring-no-properties (car msg))
                                                    (overlay-start ov)
                                                    (overlay-end ov)))))
                                fixers))
                      messages))))

(defmacro attrap-flycheck--with-messages-and-checker (pos checker-var messages-var &rest body)
  (declare (indent 3))
  (let ((pos-var '#:pos-tmp))
    `(let* ((,pos-var ,pos)
            (,messages-var (--filter-nondet
                            (car-sure it)
                            (--map (cons (flycheck-error-message (overlay-get it 'flycheck-error))
                                         it)
                                   (--filter-nondet
                                    (and (<= (overlay-start it) ,pos-var)
                                         (<= ,pos-var (overlay-end it)))
                                    (flycheck-enhancements--get-error-overlays)))))
            (,checker-var (flycheck-get-checker-for-buffer)))
       (when (not ,messages-var) (error "No flycheck message at point"))
       (when (not ,checker-var) (error "No flycheck-checker for current buffer"))
       ,@body)))

;;;###autoload
(defun attrap-flycheck (pos)
  "Attempt to repair the flycheck error at POS."
  (interactive "d")
  (attrap-flycheck--with-messages-and-checker pos checker messages
    (attrap-select-and-apply-option
     (attrap-flycheck--collect-fixes checker messages))))

(defun attrap-flycheck--collect-fixes (checker messages)
  (remove-duplicates-by-hashing-projections
   #'car
   #'equal
   (attrap--find-fixes-for-flycheck-messages checker messages)))

;;;###autoload
(defun attrap-attrap (pos)
  "Attempt to repair the error at POS."
  (interactive "d")
  (cond
   ((and (bound-and-true-p flyspell-mode)
         (fboundp 'flyspell-overlay-p)
         (-any #'flyspell-overlay-p (overlays-at (point))))
    (if (fboundp 'flyspell-correct-at-point)
        (flyspell-correct-at-point)))
   ((bound-and-true-p flymake-mode) (attrap-flymake pos))
   ((bound-and-true-p flycheck-mode) (attrap-flycheck pos))
   (t (error "Expecting flymake or flycheck to be active"))))

(defcustom attrap-haskell-extensions
  '("AllowAmbiguousTypes"
    "BangPatterns"
    "ConstraintKinds"
    "ConstrainedClassMethods"
    "DataKinds"
    "DefaultSignatures"
    "DeriveAnyClass"
    "DeriveDataTypeable"
    "DeriveFoldable"
    "DeriveFunctor"
    "DeriveGeneric"
    "DeriveTraversable"
    "DerivingStrategies"
    "DerivingVia"
    "EmptyCase"
    "EmptyDataDecls"
    "EmptyDataDeriving"
    "ExistentialQuantification"
    "ExplicitNamespaces"
    "FlexibleContexts"
    "FlexibleInstances"
    "FunctionalDependencies"
    "GADTs"
    "GeneralizedNewtypeDeriving"
    "ImportQualifiedPost"
    "InstanceSigs"
    "KindSignatures"
    "LambdaCase"
    "LinearTypes"
    "MonoLocalBinds"
    "MultiParamTypeClasses"
    "NamedFieldPuns"
    "NumericUnderscores"
    "OrPatterns"
    "PackageImports"
    "ParallelListComp"
    "PartialTypeSignatures"
    "PatternGuards"
    "PatternSynonyms"
    "PolyKinds"
    "QuantifiedConstraints"
    "RankNTypes"
    "RecordWildCards"
    "RoleAnnotations"
    "ScopedTypeVariables"
    "StandaloneDeriving"
    "StandaloneKindSignatures"
    ; "StarIsType" ; not a 'cool' extension
    "TemplateHaskell"
    "TransformListComp"
    "TupleSections"
    "TypeApplications"
    "TypeFamilies"
    "TypeFamilyDependencies"
    "TypeInType"
    "TypeOperators"
    "TypeSynonymInstances"
    "UndecidableSuperClasses"
    "UndecidableInstances"
    "UnliftedNewtypes"
    "UnliftedFFITypes"
    "ViewPatterns")
  "Language extensions that Attrap can use to fix errors."
  :type '(repeat string)
  :group 'attrap)

(defmacro attrap-option (description &rest body)
  "Create an attrap option with DESCRIPTION and BODY.
The body is code that performs the fix."
  (declare (indent 1))
  `(let ((saved-match-data (match-data)))
     (cons (attrap--format-option-description ,description)
           (lambda ()
             (set-match-data saved-match-data 'evaporate)
             ,@body))))

(defun attrap--format-option-description (descr)
  (cond
    ((stringp descr)
     descr)
    ((symbolp descr)
     (symbol-name descr))
    ((listp descr)
     (cl-assert (--every? (or (stringp it) (symbolp it)) descr))
     (attrap-strip-parens (format "%s" descr)))
    (t
     (error "Invalid option description: %s" descr))))

(defmacro attrap-one-option (description &rest body)
  "Create an attrap option list with a single element of DESCRIPTION and BODY."
  (declare (indent 1))
  `(list (attrap-option ,description ,@body)))

(defmacro attrap-alternatives (&rest clauses)
  "Append all succeeding clauses.
Each clause looks like (CONDITION BODY...).  CONDITION is
evaluated and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is a list which is appended to the result of
`attrap-alternatives'.  Usage: (attrap-alternatives CLAUSES...)"
  `(append ,@(mapcar (lambda (c) `(when ,(car c) ,@(cdr c))) clauses)))

(defun attrap-elisp-fixer (msg _beg _end)
  "An `attrap' fixer for any elisp warning given as MSG."
  (append
   (when-let ((match (s-match "You should have a section marked \"\\(.*\\)\"" msg nil t)))
     (attrap-one-option 'insert-section-header
       (beginning-of-line)
       (insert (nth 1 match) "\n")))
   (when (string-match "Lisp symbol ‘\\(.*\\)’ should appear in quotes" msg)
     (let ((sym (match-string 1 msg)))
       (attrap-one-option 'kill-message-period
         (re-search-forward sym)
         (replace-match (concat "`" sym "'") nil t nil 0))))
   (when (string-match "Error messages should \\*not\\* end with a period" msg)
     (attrap-one-option 'kill-message-period
       (let ((case-fold-search nil))
         (re-search-forward "\\.\"" (line-end-position))
         (replace-match "\"" nil t nil 0))))
   (when (string-match "Name emacs should appear capitalized as Emacs" msg)
     (attrap-one-option 'capitalize-emacs
       (let ((case-fold-search nil))
         (re-search-forward "emacs" (line-end-position))
         (replace-match "Emacs" nil t nil 0))))
   (when (string-match "should be capitalized" msg)
     (attrap-one-option 'capitalize
       (capitalize-word 1)))
   (when (string-match "You should have a section marked \"" msg)
     (attrap-one-option 'capitalize
       (capitalize-word 1)))
   (when (string-match "White space found at end of line" msg)
     (attrap-one-option 'delete-trailing-space
       (end-of-line)
       (delete-region (point) (progn (skip-chars-backward "\t ") (point)))))
   (when (string-match "There should be two spaces after a period" msg)
     (attrap-one-option 'add-space
       (beginning-of-line)
       (re-search-forward "\\(\\.\\) [^ ]" (line-end-position))
       (replace-match ". " nil t nil 1)))
   (when (string-match "might as well have a documentation" msg)
     (attrap-one-option 'add-empty-doc
       (beginning-of-line)
       (insert "  \"\"\n")))
   (when (string-match "should have documentation" msg)
     (attrap-one-option 'add-empty-doc
       (beginning-of-line)
       (insert "  \"\"\n")))
   (when (string-match "The footer should be: " msg)
     (let ((footer (s-replace "\\n" "\n" (substring msg (match-end 0)))))
       (attrap-one-option 'add-footer
         (end-of-line)
         (insert (concat "\n" footer)))))
   (when (string-match "First line is not a complete sentence" msg)
     (attrap-one-option 'merge-lines
       (end-of-line)
       (delete-char 1)))
   (when (string-match "First sentence should end with punctuation" msg)
     (attrap-one-option 'add-punctuation
       (insert ".")))))

;;;###autoload
(defun attrap-do-insert-language-pragma (pragma)
  (let ((have-language-pragmas? nil))
    (save-match-data
      (goto-char (point-min))
      (if (re-search-forward "{-#[ \t]*LANGUAGE\\_>" nil t)
          (progn
            (goto-char (match-beginning 0))
            (setf have-language-pragmas? t))
        (progn
          (attrap-skip-shebangs)
          (when (looking-at-p "^module\\_>")
            (insert "\n")
            (forward-line -1))))
      (let ((start (point)))
        (insert (concat "{-# LANGUAGE " pragma " #-}\n"))
        (when (and (not have-language-pragmas?)
                   ;; When not on empty line.
                   (not (= (line-beginning-position)
                           (line-end-position))))
          (insert-char ?\n))
        (haskell-align-language-pragmas start)))
    (notify "Added %s" pragma)))

(defmacro attrap-insert-language-pragma (pragma)
  `(attrap-option (list 'use-extension ,pragma)
     (attrap-do-insert-language-pragma ,pragma)))

(defun attrap-add-to-import--impl (missing parent line col)
  (cl-assert (stringp missing))
  (cl-assert (or (null parent) (stringp parent)))
  (cl-assert (numberp line))
  (cl-assert (numberp col))
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column (1- col))
  (skip-chars-backward " \t")
  (unless (looking-back "\(" (- (point) 2)) (insert-char ?,) (insert-char ?\s))
  (if parent
      (insert (attrap-add-operator-parens parent) "(" (attrap-add-operator-parens missing) ")")
    (insert (attrap-add-operator-parens missing)))
  (vim-save-position))

(defun attrap-add-to-haskell-import--add-parent-from-eproj-tags-if-needed
    (identifier mod-name line col is-constructor? is-type-or-class?)
  (cl-assert (stringp identifier))
  (cl-assert (stringp mod-name))
  (cl-assert (numberp line))
  (cl-assert (numberp col))

  (let* ((candidate-tags (attrap-import--get-matching-tags-for-proj (eproj-get-project-for-buf-lax (current-buffer)) identifier))

         (filtered-tags (attrap-haskell-import--filter-tags-by-type candidate-tags is-constructor? is-type-or-class?))

         (filename-stem-no-ext
          (haskell-misc--module-name-to-file-name mod-name))

         (filtered-by-module
          (--filter
           (s-contains-p filename-stem-no-ext (eproj-tag/file (cl-second it)))
           filtered-tags))

         (parent-name
          (when-let ((filtered-by-module)
                     ;; When there’s only one candidate
                     ((null (cdr filtered-by-module)))
                     (tag-entry (car filtered-by-module))
                     (tag (cl-second tag-entry)))
            (attrap-haskell-import--resolve-tag-parent-name tag))))

    (attrap-add-to-import--impl identifier parent-name line col)))

(defmacro attrap-add-to-import (missing module line col)
  "Action: insert MISSING to the import of MODULE.
The import ends at LINE and COL in the file."
  `(attrap-option (format "add to import list of ‘%s’" ,module)
     (attrap-add-to-import--impl ,missing nil (string-to-number ,line) (string-to-number ,col))))

(defun attrap-ghc-fixer (msg pos _end)
  "An `attrap' fixer for any GHC error or warning.
Error is given as MSG and reported between POS and END."
  (let ((normalized-msg (s-collapse-whitespace msg)))
  (rx-let ((parens (body) (seq "(" body ")"))
           (lin-col (l c) (seq "(" (group-n l (* num)) "," (group-n c (* num)) ")"))
           (multiline-span (l1 c1 l2 c2) (seq (lin-col l1 c1) "-" (lin-col l2 c2)))
           (monoline-span (l1 c1 l2 c2) (seq (group-n l2 (group-n l1 (* num))) ":" (group-n c1 (* num)) "-" (group-n c2 (* num))))
           (any-span (l1 c1 l2 c2) (or (monoline-span l1 c1 l2 c2) (multiline-span l1 c1 l2 c2)))
           (src-loc (l1 c1 l2 c2) (seq (* (not ":")) ":" (any-span l1 c1 l2 c2)))
           (module-name (+ (any "_." alphanumeric)))
           (identifier (n) (seq "‘" (group-n n (* (not "’"))) "’"))
           (ws (any ?\n ?\r ?\s ?\t))
           (spaces (+ ?\s))
           (name-capture (n) (group-n n (+ (not (any ?\n ?\r ?\s ?\t)))))
           (ghc-warning (n name)
                        (seq (or "error:" "warning:")
                             (optional spaces "[GHC-" n "]")
                             (optional spaces "[-W" name
                                       (optional "," spaces "Werror=" name)
                                       "]")))
           (ghc-error (n) (seq "error:" (optional " [GHC-" n "]"))))
  (append
   (when (string-match-p "Parse error in pattern: pattern" msg)
     (list (attrap-insert-language-pragma "PatternSynonyms")))
   (when (string-match "No explicit implementation for" msg)
    (attrap-one-option "insert method"
      (let ((missings (s-match-strings-all "‘\\([^’]*\\)’"
                                           (car (s-split-up-to "In the instance declaration" msg 1)))))
        (end-of-line)
        (let ((start (point)))
          (skip-whitespace-backward)
          (delete-region (point) start))
        (let* ((start (point))
               (have-where?
                (save-excursion
                  (backward-word)
                  (string= "where" (trim-whitespace (buffer-substring-no-properties (point) start))))))
          (unless have-where?
            (insert " where"))
          (dolist (missing missings)
            (insert (format "\n  %s = _" (nth 1 missing))))))))
   (when (string-match "No explicit associated type or default declaration for ‘\\(.*\\)’" msg)
    (let ((type (match-string 1 msg)))
      (attrap-one-option "insert type"
        (end-of-line)
        (insert (format "\n  type %s = _" type)))))
   (when (s-matches? (rx "Using ‘*’ (or its Unicode variant) to mean ‘Data.Kind.Type’") msg)
    (attrap-one-option "replace star by Type"
      (goto-char pos)
      (delete-char 1)
      (insert "Type")
      (unless (search-backward-regexp "import.*Data\.Kind" nil t)
        (search-backward-regexp "^module")
        (end-of-line)
        (insert "\nimport Data.Kind (Type)"))))
   (when (string-match "Valid hole fits include" msg)
    (let* ((options (-map 'cadr (-non-nil (--map (s-match "[ ]*\\([^ ]*\\) ::" it) (s-split "\n" (substring msg (match-end 0))))))))
      (--map (attrap-option (list 'plug-hole it)
               (goto-char pos)
               (delete-char 1)
               (insert it))
             options)))
   (when (string-match "Redundant constraints?: (?\\([^,)\n]*\\)" msg)
    (let ((constraint (match-string 1 msg)))
      (attrap-one-option "delete redundant constraint"
        (search-forward constraint)     ; find type sig
        (delete-region (match-beginning 0) (match-end 0))
        (when (looking-at "[ \t]*,")
          (delete-region (point) (search-forward ",")))
        (when (looking-at "[ \t]*=>")
          (delete-region (point) (search-forward "=>"))))))
   ;; error: [GHC-44432]
   ;;     The type signature for ‘withSystemTempFileContents’
   ;;       lacks an accompanying binding
   (when (string-match
          (rx (ghc-error "44432")
              (+ ws)
              "The type signature for "
              (identifier 1)
              (* ws)
              "lacks an accompanying binding")
          msg)
     (let ((name (match-string-no-properties 1 msg)))
       (attrap-one-option "add binding"
         (skip-to-indentation)
         (let ((col (current-column)))
           (forward-line)
           (while (< col (indentation-size))
             (forward-line)))
         (insert (concat name " = _\n")))))
   (when (string-match "add (\\(.*\\)) to the context of[\n ]*the type signature for:[ \n]*\\([^ ]*\\) ::" msg)
    (let ((missing-constraint (match-string 1 msg))
          (function-name (match-string 2 msg)))
      (attrap-one-option "add constraint to context"
        (search-backward-regexp (concat (regexp-quote function-name) "[ \t]*::[ \t]*" )) ; find type sig
        (goto-char (match-end 0))
        (when (looking-at "forall\\|∀") ; skip quantifiers
          (search-forward "."))
        (skip-chars-forward "\n\t ")    ; skip spaces
        (insert (concat missing-constraint " => ")))))
   (when (string-match "Unticked promoted constructor: ‘\\(.*\\)’" msg)
    (let ((constructor (match-string 1 msg)))
      (attrap-one-option "tick promoted constructor"
        (goto-char pos)
        ;; when the constructor is infix, flycheck reports the wrong position.
        (search-forward constructor)
        (backward-char (length constructor))
        (insert "'"))))
   (when (string-match "Patterns not matched:" msg)
    (attrap-one-option "add missing patterns"
      (let ((patterns (mapcar #'trim-whitespace
                              ;; patterns to match
                              (split-string (substring msg (match-end 0))
                                            "\n"
                                            t
                                            " "))))
        (if (string-match "In an equation for ‘\\(.*\\)’:" msg)
            (let ((function-name (match-string 1 msg)))
              (end-of-line)
              (dolist (pattern patterns)
                (insert (concat "\n" function-name " " pattern " = _"))))
          (end-of-line) ;; assuming that the case expression is on multiple lines and that "of" is at the end of the line
          (dolist (pattern patterns)
            (insert "\n     ") ;; fixme: guess how much indent is needed.
            (insert (concat pattern " -> _")))))))
   (when (string-match "A do-notation statement discarded a result of type" msg)
    (attrap-one-option "explicitly discard result"
      (goto-char pos)
      (insert "_ <- ")))
   (when (string-match "\\(Failed to load interface for\\|Could not find module\\) ‘\\(.*\\)’\n[ ]*Perhaps you meant[ \n]*\\([^ ]*\\)" msg)
    (let ((old (match-string 2 msg))
          (replacement (match-string 3 msg)))
      (attrap-one-option "rename module import"
        ;; delete-region may garble the matches
        (search-forward old)
        (delete-region (match-beginning 0) (point))
        (insert replacement))))
   (when (string-match "Unsupported extension: \\(.*\\)\n[ ]*Perhaps you meant ‘\\([^‘]*\\)’" msg)
     (let ((ext (match-string-no-properties 1 msg))
           (replacement (match-string 2 msg)))
       (attrap-one-option "rename extension"
         ;; Delete-region may garble the matches
         (goto-char pos)
         (search-forward ext)
         (delete-region (match-beginning 0) (point))
         (insert replacement))))
   (when-let ((match (s-match (rx "Perhaps you want to add " (identifier 1)
                                  " to the import list in the import of " (identifier 2)
                                  " " (parens (src-loc 3 4 5 6)))
                              normalized-msg
                              nil
                              t)))
     (list (attrap-add-to-import (nth 1 match) (nth 2 match) (nth 5 match) (nth 6 match))))
   (when-let ((match (s-match (rx "Perhaps you want to add " (identifier 1)
                                  " to one of these import lists:")
                              normalized-msg
                              nil
                              t)))
     (--map (attrap-add-to-import (nth 1 match) (nth 2 it) (nth 5 it) (nth 6 it))
            (s-match-strings-all (rx (identifier 2) " " (parens (src-loc 3 4 5 6))) msg)))
    ;; Not in scope: data constructor ‘SimpleBroadcast’
    ;; Perhaps you meant ‘SimpleBroadCast’ (imported from TypedFlow.Types)
    ;; Not in scope: ‘BackCore.argmax’
    ;;     Perhaps you meant one of these:
    ;;       ‘BackCore.argMax’ (imported from TensorFlow.GenOps.Core),
    ;;       ‘BackCore.argMax'’ (imported from TensorFlow.GenOps.Core),
    ;;       ‘BackCore.max’ (imported from TensorFlow.GenOps.Core)
    ;;       ‘BackCore.maxx’ (line 523)
   (when-let ((match
               (s-match (rx (or (seq (or "Data constructor" "Variable") " not in scope:"
                                     (* (any " \n\t")) (group-n 1 (+ (not (any " \n")))))
                                (seq "Not in scope: "
                                     (or "" "data constructor " "type constructor or class " "record field ")
                                     (identifier 1))))
                        msg)))
    (let* ((delete (nth 1 match))
           (delete-has-paren (eq ?\( (elt delete 0)))
           (delete-no-paren (if delete-has-paren (substring delete 1 (1- (length delete))) delete))
           (rest (nth 1 (s-match (rx "Perhaps " (or "you meant" "use") (? " one of these:") (group (+ anychar))) normalized-msg)))
           (replacements (s-match-strings-all
                          (rx (identifier 1) " "
                              (parens (or (seq "imported from " (group-n 2 module-name))
                                          (group-n 2 (seq "line "  (* num))))))
                          rest)))
      (--map (attrap-option (list 'replace delete-no-paren 'by (nth 1 it) 'from (nth 2 it))
               (goto-char pos)
               (let ((case-fold-search nil))
                 (search-forward delete-no-paren (+ (length delete) pos))
                 (replace-match (nth 1 it) t)))
             replacements)))
    (when (string-match "It could refer to" msg) ;; ambiguous identifier
     (let ((replacements (--map (nth 1 it) (s-match-strings-all  (rx (identifier 1) ",") msg))))
       (--map (attrap-option (list 'rename it)
                (apply #'delete-region (dante-ident-pos-at-point))
                (insert it))
              replacements)))
   (when (string-match "\\(Top-level binding\\|Pattern synonym\\) with no type signature:[\n ]*" msg)
    (attrap-one-option "add signature"
      (beginning-of-line)
      (insert (concat (substring-no-properties msg (match-end 0)) "\n"))))
   (when (and (string-match-p "Defined but not used" msg)
              (not (string-match-p
                  (rx (ghc-warning "40910" "unused-top-binds") (+ ws)
                        "Defined but not used: "
                        (identifier 1))
                    msg)))
     (attrap-one-option "add underscore"
       (goto-char pos)
       (insert "_")))
   (when (string-match "Unused quantified type variable ‘\\(.*\\)’" msg)
    (attrap-one-option "delete type variable"
      ;; note there can be a kind annotation, not just a variable.
      (delete-region (point) (+ (point) (- (match-end 1) (match-beginning 1))))))
   ;;     Module ‘TensorFlow.GenOps.Core’ does not export ‘argmax’.
   (when-let ((m (s-match (rx "No module named " (identifier 1) " is imported.")
                          msg
                          nil
                          t)))
     (attrap-one-option (list "add import" (nth 1 m))
       (goto-char 1)
       (search-forward-regexp (rx "module" (*? anychar) "where"))
       (insert "\n" "import " (nth 1 m) "\n")))
   (when-let ((match (s-match (rx (or (seq "The " (? "qualified ") "import of " (identifier 1) (* (any ?\s ?\t ?\n ?\r))
                                           "from module " (identifier 2) " is redundant")
                                      (seq bos (ghc-error "61689") " "
                                           "Module " (identifier 2) " does not export " (identifier 1))))
                              normalized-msg)))
    (attrap-one-option "delete import"
      (let ((redundant (nth 1 match)))
        (save-excursion
          (let ((names-to-remove (s-split "[, \n\r\t]+" redundant t)))
            (if (derived-mode-p 'haskell-ts-base-mode)
                (haskell-ts-remove-from-import-statement-at (point) names-to-remove)
              (attrap-remove-from-import-statement-at-point names-to-remove)))))))
   (when (string-match
          (rx (ghc-warning "66111" "unused-imports") (+ ws)
              "The " (? "qualified ") "import of " (identifier 1) " is redundant")
          msg)
    (attrap-one-option "delete module import"
      (save-match-data
        (save-excursion
          (beginning-of-line)
          (delete-region
           (point)
           (progn
             (unless (looking-at haskell-regexen/pre-post-qualified-import-line)
               (error "Import statement not found"))
             (goto-char (if-let ((import-list-start (match-beginning 11)))
                            import-list-start
                          (match-end 0)))
             (skip-chars-forward "\t ")
             (let ((sexp-end
                    (save-excursion
                      (skip-chars-forward "\r\n\t ")
                      (when (eq (char-after) ?\() ; skip the import list if any
                        (forward-sexp)
                        (skip-chars-forward "\t ")
                        (point)))))
               (when sexp-end
                 (goto-char sexp-end)))
             (when (eq (char-after) ?\r)
               (forward-char 1))
             (when (eq (char-after) ?\n)
               (forward-char 1))
             (point)))))))
   (when (string-match "Found type wildcard ‘\\(.*\\)’[ \t\n]*standing for ‘\\([^’]*\\)’" msg)
     (let ((wildcard  (match-string-no-properties 1 msg))
           (type-expr (match-string-no-properties 2 msg)))
       (attrap-one-option "explicit type wildcard"
         (goto-char pos)
         (search-forward wildcard)
         (replace-match
          (if (string-contains? ?\s type-expr) (concat "(" type-expr ")") type-expr)
          t
          t))))
   (when (and (string-match-p "parse error on input ‘case’" msg) ; Obsolete with GHC 9, which appears to recognize Lambda case specially.
              (save-excursion
                (goto-char pos)
                (string-match-p (rx "\\case\\_>") (buffer-substring-no-properties pos (line-end-position)))))
     (list (attrap-insert-language-pragma "LambdaCase")))
   (when (s-matches? (rx (or "Illegal symbol ‘forall’ in type"
                             (seq "Perhaps you intended to use"
                                  (* anything)
                                  "language extension to enable explicit-forall syntax")))
                     normalized-msg)
     (list (attrap-insert-language-pragma "ScopedTypeVariables")))
   (when-let (match (s-match (rx "Fields of " (identifier 1) " not initialised: "
                                 (group-n 2 (+ (not (any "•")))) "•")
                             msg
                             nil
                             t))
     (attrap-one-option "initialize fields"
       (let ((fields (s-split "," (nth 2 match) t)))
         (search-forward "{")
         (dolist (f fields)
           (insert (format ",%s = _\n" (s-trim f)))))))
   (when (string-match (rx "• "
                           (or "No instance for"
                               "Could not deduce")
                           " ‘"
                           (group-n 1 (or "Generic" "Pretty"))
                           (+ (any ?\s ?\t ?\r ?\n))
                           (group-n 2 (+ (not (any ?\’))))
                           "’")
                       msg)
     (let ((class-name (match-string-no-properties 1 msg))
           (type-name (replace-regexp-in-string "[ \r\n]+" " " (match-string-no-properties 2 msg))))
       (attrap-one-option "derive pretty instance"
         (unless (eq (current-column) 0)
           (haskell-move-to-topmost-start)
           (forward-line -1))
         (cond
           ((equal class-name "Pretty")
            (insert "deriving via PPGeneric " type-name " instance Pretty " type-name "\n"))
           ((equal class-name "Generic")
            (insert "deriving instance Generic " type-name "\n"))
           (t
            (error "Unhandled class case: %s" class-name))))))
   (--map (attrap-insert-language-pragma it)
          (--filter (s-matches? it normalized-msg) attrap-haskell-extensions))

   (when (string-match
          (rx (or (seq (ghc-warning "88464" "deferred-out-of-scope-variables") (+ ws)
                       (or "Variable"
                           (group-n 2 "Data constructor"))
                       " not in scope:"
                       (+ ws)
                       (name-capture 1)
                       (? (+ ws)
                          "::"))
                  ;; error: [GHC-76037]
                  ;;     • Not in scope: ‘osstr’
                  ;;     • In the quasi-quotation: [osstr|fully-pinned-cabal.config|]
                  ;; error: [GHC-76037]
                  ;;     Not in scope: data constructor ‘Compose’
                  (seq (ghc-error "76037")
                       (+ ws)
                       (or (seq "Not in scope: "
                                (or (group-n 3 "type constructor or class")
                                    (group-n 2 "data constructor"))
                                " "
                                (identifier 1))
                           (seq "• Not in scope: "
                                (identifier 1))))))
          normalized-msg)
     (let ((identifier (attrap-strip-parens (match-string-no-properties 1 normalized-msg)))
           (is-constructor? (not (null (match-beginning 2))))
           (is-type-or-class? (not (null (match-beginning 3)))))
       (if-let ((have-specific-import-location?
                 (string-match
                  (rx (or (seq "Suggested fix:"
                               (* ws) "Add" (* ws) (identifier 1) (* ws)
                               "to the import list in the import of"
                               (* ws) (identifier 2) (* ws) (parens (src-loc 3 4 5 6)))
                          (seq "Suggested fixes:" (* ws)
                               (* "•" (+ (not ?•)))
                               "• Add" (* ws) (identifier 1) (* ws)
                               "to the import list in the import of"
                               (* ws) (identifier 2) (* ws) (parens (src-loc 3 4 5 6)))))
                  normalized-msg)))
           (let ((module (match-string-no-properties 2 normalized-msg))
                 (line (match-string-no-properties 5 normalized-msg))
                 (col (match-string-no-properties 6 normalized-msg)))
             (attrap-one-option (format "add to import list of ‘%s’" module)
               (attrap-add-to-haskell-import--add-parent-from-eproj-tags-if-needed identifier
                                                                                   module
                                                                                   (string-to-number line)
                                                                                   (string-to-number col)
                                                                                   is-constructor?
                                                                                   is-type-or-class?)))
         (when-let ((proj (eproj-get-project-for-buf-lax (current-buffer)))
                    (candidate-tags (attrap-import--get-matching-tags-for-proj proj identifier)))
           (attrap-one-option "add import"
             (attrap--add-haskell-import proj
                                         candidate-tags
                                         identifier
                                         is-constructor?
                                         is-type-or-class?))))))

    ;; error: [GHC-76037]
    ;;     Not in scope: type constructor or class ‘MonadMask’
    ;;     Suggested fix:
    ;;       Perhaps use ‘MC.MonadMask’ (imported from Control.Monad.Catch)
    ;;
    ;; warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]
    ;;     Variable not in scope:
    ;;       withWindow
    ;;         :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO a0
    ;;     Suggested fix:
    ;;       Perhaps use ‘GLFW.hideWindow’ (imported from Graphics.UI.GLFW)
    (when (string-match
           (rx (or (ghc-error "76037")
                   (ghc-warning "88464" "deferred-out-of-scope-variables"))
               (+ ws)
               (or (seq "Not in scope: "
                        (or "data constructor"
                            "type constructor or class")
                        (+ ws)
                        (identifier 1)
                        (+ ws))
                   (seq (or "Variable"
                            "Data constructor")
                        " not in scope:"
                        (+ ws)
                        (name-capture 1)
                        (+ ws)
                        "::"
                        (+ anything)))
               "Suggested fix:"
               (+ ws)
               "Perhaps use "
               (identifier 2)
               (? " (imported from "
                  (group-n 3 (+ (not ?\))))
                  ")"))
           msg)
      (let* ((delete (match-string-no-properties 1 msg))
             (delete-has-paren (eq 40 ;; ?(
                                   (elt delete 0)))
             (delete-no-paren (if delete-has-paren (substring delete 1 (1- (length delete))) delete))
             (new-ident (match-string-no-properties 2 msg))
             (new-module (match-string-no-properties 3 msg)))
        (attrap-one-option (append (list 'replace delete-no-paren 'by new-ident)
                                   (when new-module
                                     (list 'from new-module)))
          (goto-char pos)
          (let ((case-fold-search nil))
            (search-forward delete-no-paren (+ (length delete) pos))
            (replace-match new-ident t)))))

    ;; warning: [GHC-40910] [-Wunused-top-binds]
    ;;     Defined but not used: ‘withSystemTempFileContents’
    (when (string-match
           (rx (ghc-warning "40910" "unused-top-binds") (+ ws)
               "Defined but not used: "
               (identifier 1))
           msg)
      (let ((identifier (match-string-no-properties 1 msg)))
        (attrap-one-option (list 'export identifier)
          (haskell--export-ident identifier))))))))

(defun attrap-remove-from-import-statement-at-point (names-to-remove)
  (save-match-data
    ;; If there are several things redundant, the message starts at 'import'.
    (when (looking-at-p (rx "import"))
      ;; The imported things are after the parenthesis.
      (search-forward "("))
    (dolist (r names-to-remove)
      (save-excursion
        ;; Transform Executable(buildInfo) -> buildInfo for when
        ;; we imported a type’s accessor but GHC reports it together
        ;; with parent type name.
        (when-let (m (s-match (rx
                               (* (any ?_))
                               (any (?A . ?Z))
                               (* alphanumeric)
                               "("
                               (group-n 1 (+ (not ?\))))
                               ")")
                              r))
          (setf r (nth 1 m)))
        (re-search-forward (rx-to-string (if (s-matches? (rx bol alphanumeric) r)
                                             `(or (seq word-start ,r word-end)
                                                  (seq "(" ,r ")")) ; regular ident
                                           `(seq "(" ,r ")")))) ; operator
        (replace-match "")
        (when (looking-at-p "(..)") (delete-char 4))
        (when (or (looking-at (rx (* space)
                                  ","
                                  (* space)
                                  (? "(" (* space) ")")))
                  (looking-back (rx ","
                                    (* space)
                                    (? "(" (* (any ?\n ?\r ?\s ?\t)) ")"))
                                (line-beginning-position)))
          (replace-match ""))))))

(defun attrap-strip-parens (identifier)
  (let* ((i 0)
         (j (length identifier))
         (non-empty? (not (zerop j))))
    (when (and non-empty?
               (eq (aref identifier 0) ?\())
      (cl-incf i))
    (when (and non-empty?
               (eq (aref identifier (1- j)) ?\)))
      (cl-decf j))
    (substring identifier i j)))

(cl-defstruct attrap--fixed-mod-name
  (new-name :read-only t)
  (is-authoritative? :read-only t))

(defconst attrap--module-name-fixes
  (eval-when-compile
    (alist->hash-table-with
     (lambda (x y)
       (cond
         ((and (hash-table-p x)
               (hash-table-p y))
          (hash-table-merge x y))
         ((null y)
          x)
         (t
          (error "Cannot combine ‘%s’ and ‘%s’" x y))))
     (mapcan (lambda (x)
               (let ((source-modules (car x))
                     (target-module (cadr x))
                     (identifiers (caddr x))
                     ;; Should be ‘t’ for base and ‘nil’ for most others.
                     (is-authoritative? (cadddr x)))
                 (cl-assert (listp source-modules))
                 (cl-assert (stringp target-module))
                 (mapcar (lambda (xx)
                           (cons xx
                                 (if (eq identifiers t)
                                     ;; All identifiers are in scope.
                                     (make-attrap--fixed-mod-name :new-name target-module
                                                                  :is-authoritative? is-authoritative?)
                                   ;; Only listed identifiers are in scope.
                                   (alist->hash-table
                                    (mapcar (lambda (y)
                                              (cons y (make-attrap--fixed-mod-name
                                                       :new-name target-module
                                                       :is-authoritative? is-authoritative?)))
                                            identifiers)))))
                         source-modules)))
             (append
              ;; All names, Not authoritative
              (--map (list (list (car it)) (cdr it) t nil)
                     '(("GHC.Internal.Control.Exception.Base" . "Control.Exception")
                       ("GHC.Internal.Data.OldList" . "Data.List")
                       ("GHC.Internal.Data.Maybe" . "Data.Maybe")
                       ("GHC.Internal.Float" . "GHC.Float")))
              (--map
               ;; Make them all authoritative
               (append it '(t))
               (append
                '((("GHC.IO.FD"
                    "GHC.IO.Handle.FD"
                    "GHC.IO.Handle.Text"
                    "GHC.IO.StdHandles"
                    "GHC.IO.Windows.Handle"
                    "GHC.Internal.IO.FD"
                    "GHC.Internal.IO.Handle"
                    "GHC.Internal.IO.Handle.FD"
                    "GHC.Internal.IO.Handle.Text"
                    "GHC.Internal.IO.StdHandles"
                    "GHC.Internal.IO.Windows.Handle")
                   "System.IO"
                   ("hPutStrLn"
                    "hPutStr"
                    "hFlush"
                    "stdin"
                    "stdout"
                    "stderr"))
                  (("GHC.Word")
                   "Data.Word"
                   ("Word8"
                    "Word16"
                    "Word32"
                    "Word64"))
                  (("Basement.Compat.Bifunctor"
                    "Bifunctor")
                   "Data.Bifunctor"
                   ("Bifunctor"
                    "bimap"))
                  (("Data.Generics.Aliases"
                    "Generics.Deriving.Base.Internal"
                    "Generics.SOP.Universe")
                   "GHC.Generics"
                   ("Generic"))

                  (("GHC.Internal.ForeignPtr")
                   "Foreign.ForeignPtr"
                   ("ForeignPtr"
                    "FinalizerPtr"
                    "FinalizerEnvPtr"
                    "newForeignPtr"
                    "newForeignPtr_"
                    "addForeignPtrFinalizer"
                    "newForeignPtrEnv"
                    "addForeignPtrFinalizerEnv"
                    "withForeignPtr"
                    "finalizeForeignPtr"
                    "touchForeignPtr"
                    "castForeignPtr"
                    "plusForeignPtr"
                    "mallocForeignPtr"
                    "mallocForeignPtrBytes"
                    "mallocForeignPtrArray"
                    "mallocForeignPtrArray0"))

                  (("GHC.Internal.Base")
                   "Control.Monad"
                   ("MonadPlus"
                    "mplus"
                    "mzero"
                    "when"
                    "unless"))
                  (("GHC.Internal.Base")
                   "Data.List.NonEmpty"
                   ("NonEmpty"))
                  (("GHC.Internal.Base")
                   "Data.Char"
                   ("ord"))
                  (("GHC.Internal.Base")
                   "GHC.Base"
                   ("unsafeChr"))
                  (("GHC.Internal.Char")
                   "Data.Char"
                   ("chr"))
                  (("GHC.Internal.Int")
                   "Data.Int"
                   ("Int8"
                    "Int16"
                    "Int32"
                    "Int64"))
                  (("GHC.Internal.IO.Unsafe")
                   "System.IO.Unsafe"
                   ("unsafePerformIO"
                    "unsafeDupablePerformIO"
                    "unsafeInterleaveIO"
                    "unsafeFixIO"))
                  (("GHC.Internal.Word")
                   "Data.Word"
                   ("Word"
                    "Word8"
                    "Word16"
                    "Word32"
                    "Word64"))
                  (("GHC.IO")
                   "Control.Exception"
                   ("evaluate"))

                  (("Data.ByteString.Internal.Type")
                   "Data.ByteString"
                   ("ByteString"))
                  (("Data.ByteString.Short.Internal")
                   "Data.ByteString.Short"
                   ("ShortByteString"))
                  (("Data.Map.Internal")
                   "Data.Map.Strict"
                   ("Map"))
                  (("Data.Text.Internal")
                   "Data.Text"
                   ("Text")))
                ;; Wildcards
                (--map (list (list (car it)) (cdr it) t)
                       '(("Prettyprinter.Internal" . "Prettyprinter")
                         ("Data.Bit.Internal" . "Data.Bit")
                         ("GHC.Internal.Conc.Sync" . "Control.Concurrent")
                         ("GHC.Internal.Control.Monad" . "Control.Monad")
                         ("GHC.Internal.Control.Monad.IO.Class" . "Control.Monad.IO.Class")
                         ("GHC.Internal.Data.Foldable" . "Data.Foldable")
                         ("GHC.Internal.Data.Function" . "Data.Function")
                         ("GHC.Internal.Data.Functor" . "Data.Functor")
                         ("GHC.Internal.Data.Functor.Identity" . "Data.Functor.Identity")
                         ("GHC.Internal.Data.Ord" . "Data.Ord")
                         ("GHC.Internal.Data.Proxy" . "Data.Proxy")
                         ("GHC.Internal.Data.Traversable" . "Data.Traversable")
                         ("GHC.Internal.Data.Typeable" . "Data.Typeable")
                         ("GHC.Internal.Foreign.Storable" . "Foreign.Storable")
                         ("GHC.Internal.Stack.Types" . "GHC.Stack.Types")
                         ("GHC.Internal.Stack" . "GHC.Stack")
                         ("GHC.Internal.System.Exit" . "System.Exit")
                         ("GHC.Internal.Unsafe.Coerce" . "Unsafe.Coerce")
                         ("Data.Semigroup.Internal" . "Data.Semigroup"))))))))))

(defun attrap-haskell-import--fix-module-name (identifier mod-name)
  "Returns cons: (NEW-MODULE-NAME IS-AUTHORITATIVE?). If any of the results are authoritative
then all non-authoritative results from that collection should be ignored."
  (cl-assert (stringp identifier))
  (cl-assert (stringp mod-name))
  (if-let ((entry (gethash mod-name attrap--module-name-fixes)))
      (cond
        ((attrap--fixed-mod-name-p entry)
         ;; All identifiers are in scope.
         entry)
        ((hash-table-p entry)
         (if-let ((new-name (gethash identifier entry)))
             new-name
           (make-attrap--fixed-mod-name
            :new-name mod-name
            :is-authoritative? nil)))
        (t
         (make-attrap--fixed-mod-name
          :new-name mod-name
          :is-authoritative? nil)))
    (make-attrap--fixed-mod-name
     :new-name mod-name
     :is-authoritative? nil)))

(defvar attrap--import-history nil)

(defun attrap-import--get-matching-tags-for-proj (proj identifier)
  (when proj
    (let ((effective-major-mode (eproj/resolve-synonym-modes major-mode)))
      ;; Mostly to make tests green but in general too: don’t proceed
      ;; if found project doesn’t define tags for us.
      (when (assq effective-major-mode (eproj-project/tags proj))
        (save-match-data
          (eproj-symbnav/ensure-tags-loaded! effective-major-mode proj))
        (eproj-get-matching-tags proj effective-major-mode identifier nil)))))

(defun attrap-haskell-import--filter-tags-by-type (candidate-tags is-constructor? is-type-or-class?)
  (cond
    (is-constructor?
     (--filter (memq (eproj-tag/type (cl-second it)) '(?C ?p)) candidate-tags))
    (is-type-or-class?
     (--filter (let ((typ (eproj-tag/type (cl-second it))))
                 (or (eq typ ?t)
                     (eq typ ?c)))
               candidate-tags))
    (t
     candidate-tags)))

(defun attrap-haskell-import--resolve-tag-parent-name (tag)
  (cl-assert (eproj-tag-p tag))
  (when-let ((tag-parent (eproj-tag/get-prop 'parent tag)))
    (let ((parent-type (cdr tag-parent)))
      ;; When type is not class.
      (unless (eq parent-type ?c)
        (car tag-parent)))))

(defun attrap--add-haskell-import (proj candidate-tags identifier is-constructor? is-type-or-class?)
  "CANDIDATE-TAGS is the result returned by ‘eproj-get-matching-tags’."
  (cl-assert (eproj-project-p proj))
  (cl-assert (stringp identifier))
  (let* ((filtered-tags
          (attrap-haskell-import--filter-tags-by-type candidate-tags is-constructor? is-type-or-class?))
         (tags-with-fixed-mod-names
          (--map (cons (attrap-haskell-import--fix-module-name
                        identifier
                        (haskell-misc--file-name-to-module-name (eproj-tag/file (cl-second it))))
                       it)
                 filtered-tags))
         (any-authoritative-renamings?
          (--any? (attrap--fixed-mod-name-is-authoritative? (car it))
                  tags-with-fixed-mod-names))
         (tags-with-filtered-authoritative
          (--map (cons (attrap--fixed-mod-name-new-name (car it))
                       (cdr it))
                 (if any-authoritative-renamings?
                     (--filter (attrap--fixed-mod-name-is-authoritative? (car it))
                               tags-with-fixed-mod-names)
                   tags-with-fixed-mod-names)))
         (module-names
          (remove-duplicates-sorting
           tags-with-filtered-authoritative
           (lambda (x y)
             (string= (car x) (car y)))
           (lambda (x y)
             (string< (car x) (car y)))))

         (entry
          (pcase (length module-names)
            (0 (error "No candidates modules defining ‘%s’ found" identifier))
            (1 (car module-names))
            (_ (assoc (completing-read "Choose module: "
                                       module-names
                                       nil
                                       t ;; require match
                                       nil
                                       'attrap-import-history ;; history
                                       )
                      module-names))))
         (mod-name (car entry))
         (candidate-entry (cdr entry))
         (tag (cl-second candidate-entry))
         (import-from-current-project? (eq proj (cl-third candidate-entry))))
    (haskell-misc--add-new-import mod-name
                                  identifier
                                  import-from-current-project?
                                  (attrap-haskell-import--resolve-tag-parent-name tag))
    (notify "Added import of ‘%s’" mod-name)))

(defun attrap-add-operator-parens (name)
  "Add parens around a NAME if it refers to a Haskell operator."
  (if (string-match-p "^[[:upper:][:lower:]_']" name)
      name
    (concat "(" name ")")))

(defun attrap-skip-shebangs ()
  "Skip #! and -- shebangs used in Haskell scripts."
  (let ((do-skip-empty
         (lambda ()
           (while (and (not (eobp))
                       (= (point) (line-end-position)))
             (forward-line 1)))))
    (when (looking-at-p "#!")
      (forward-line 1)
      (skip-whitespace-forward))
    (let ((continue? t))
      (while continue?
        (cond
          ((looking-at-p "--[ \t]*stack\\>")
           (forward-line 1)
           (skip-whitespace-forward))
          ((looking-at-p "[ \t]*{-[ \t]*\\(?:cabal\\|project\\)[ \t]*:")
           (goto-char (pseudovim-motion-jump-item-to-pos (point) nil))
           (when (eq (char-after) ?\})
             (forward-char))
           (skip-whitespace-forward))
          ((and (not (eobp))
                (looking-at-p "--"))
           (while (and (not (eobp))
                       (looking-at-p "--"))
             (forward-line 1))
           (skip-whitespace-forward))
          (t
           (setf continue? nil)))))))

(defun attrap-hlint-fixer (msg pos end)
  "Fixer for any hlint hint given as MSG and reported between POS and END."
  (rx-let ((indented-line (seq space (* not-newline) "\n"))
           (snippet (+ indented-line)))
  (cond
   ((s-matches? (rx (or "Perhaps you should remove it."
                        "Use fewer LANGUAGE pragmas"))
                msg)
    (attrap-one-option 'kill-unused
      (delete-region pos (+ 2 end))))
   ((s-matches? (rx "Redundant $") msg)
    (attrap-one-option 'kill-dollar
      (delete-region pos (+ 1 end))))
   ((s-matches? (rx "Redundant bracket") msg)
    (attrap-one-option 'kill-brackets
      (delete-region pos (1+ pos))
      (delete-region (1- end) end)))
   ((string-match
     (rx "Found:\n"
         (group snippet)
         "Perhaps:\n"
         (group snippet)
         (? (seq "Note: " (+ not-newline) "\n"))
         (* space) "[haskell-hlint]")
     msg)
    (let ((replacement (match-string 2 msg)))
      (attrap-one-option 'replace-as-hinted
        (delete-region pos (+ 1 end))
        (insert (s-trim (s-collapse-whitespace replacement)))))))))

(defun attrap-LaTeX-fixer (msg pos _end)
  (cond
   ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.") msg)
    (list (attrap-option 'fix-open-dquote
            (delete-region pos (1+ pos))
            (insert "``"))
          (attrap-option 'fix-close-dquote
            (delete-region pos (1+ pos))
            (insert "''"))))
   ((s-matches? (rx "Non-breaking space (`~') should have been used.") msg)
    (attrap-one-option 'non-breaking-space
      (if (looking-at (rx space))
          (delete-region pos (1+ pos))
          (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point)))
      (insert "~")))
   ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.") msg)
    (attrap-one-option 'use-interword-spacing
      (delete-region pos (1+ pos))
      (insert "\\ ")))
   ((s-matches? (rx "Delete this space to maintain correct pagereferences.") msg)
    (attrap-one-option 'fix-space-pageref
      (if (looking-back (rx bol (* space)))
          (progn (skip-chars-backward "\n\t ")
                 (insert "%"))
        (delete-region (point) (save-excursion (skip-chars-forward " \t") (point))))))))

(provide 'attrap)
;;; attrap.el ends here

