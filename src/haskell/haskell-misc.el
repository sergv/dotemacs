;; haskell-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 20 September 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(require 'macro-util)
(require 'advices-util)
(require 'common)
(require 'peg)

(require 'abbrev+)
(require 'haskell-compile)
(require 'compilation-setup)

;;; definitions

(defconst +haskell-syntax-modes+ '(haskell-mode haskell-c-mode c2hs-mode)
  "List of modes that use haskell syntax.")

(defconst +haskell-tmp-path+ (concat +tmp-path+ "/haskell-tmp"))

(make-directory +haskell-tmp-path+ t)

(setf haskell-compile-command
      (or (getenv "HASKELL_COMPILE_COMMAND")
          (concat "ghc -W -Wall -fwarn-monomorphism-restriction "
                  "-ferror-spans -fforce-recomp "
                  (when (platform-os-type? 'linux)
                    ;; needed for ghc 7.4 and gold linker
                    "-rtsopts -pgml /usr/bin/gcc ")
                  (format "-hidir %s " +haskell-tmp-path+)
                  (format "-odir %s " +haskell-tmp-path+)
                  (format "-tmpdir %s " +haskell-tmp-path+)
                  ;; llvm
                  ;; "-fllvm -optlc-O3 -optlo-O3 "
                  "-c \"%s\""))
      haskell-program-name
      (let ((extensions "-XLambdaCase -XTemplateHaskell "))
        (cond ((platform-os-type? 'windows)
               (concat "ghc --interactive " extensions "-fbyte-code"))
              ((executable-find "ghci")
               (concat "ghci " extensions "-fobject-code"))
              ((executable-find "ghc")
               (concat "ghc --interactive " extensions "-fbyte-code"))
              (t
               (message "GHC not found")
               nil))))


(defconst +haskell-compile-error-or-warning-regexp+
  (join-lines (map (comp (partial #'concat "\\(?:")
                         (partial-first #'concat "\\)")
                         #'car)
                   haskell-compilation-error-regexp-alist)
              "\\|")
  "Regexp matching both errors and warnings.")


;; for outline
(defconst haskell-type-signature-regexp (rx (not (any ?: ?\n))
                                            "::"
                                            (group (or
                                                    (not (any ?: ?\n))
                                                    eol))))
(defconst haskell-toplevel-signature-regexp (rx bol
                                                (not (any ?\s))
                                                (* nonl)
                                                (or (not (any ?: ?\n))
                                                    (seq (* whitespace)
                                                         "\n"
                                                         (+ whitespace)))
                                                "::"
                                                (group (or
                                                        (not (any ?: ?\n))
                                                        eol))))
(defconst haskell-toplevel-data-declaration-regexp "^[ \t]*data[ \t]+\\(?:.\\|\n\\)+?=")
(defconst haskell-toplevel-class-declaration-regexp "^[ \t]*class[ \t]+\\(?:.\\|\n\\)+?where")
(defconst haskell-toplevel-instance-declaration-regexp "^[ \t]*instance[ \t]+\\(?:.\\|\n\\)+?where")
(defconst haskell-main-function-regexp "^main[ \t]*=[ \t\n\r]*\\(?:do\\)?")
(defconst haskell-commented-line-regexp "^[ \t]*-- ")


(defconst haskell-module-quantification-regexp
  (let ((conid "\\b[[:upper:]][[:alnum:]'_]*\\b"))
    (concat "\\b\\(?:" conid "\\.\\)+")))

(defun haskell-remove-module-qualification (name)
  "Removes hierarchihal modules qualification (e.g. Data.Map.null -> null,
 Prelude.++ -> ++, etc)"
  (save-match-data
    (if (string-match (concat "^\\("
                              haskell-module-quantification-regexp
                              "\\)")
                      name)
      (replace-match "" t t name 1)
      name)))

(defun inf-haskell-send-input-or-jump-to-error ()
  (interactive)
  (if (looking-at-pure? *compilation-jump-error-regexp*)
    (compile-goto-error)
    (comint-send-input)))


(defun haskell-yas-completing-prompt (prompt choices &optional display-fn)
  "Call `yas-completing-prompt' with ignoring case during completion."
  (let ((completion-ignore-case t))
    (yas-completing-prompt prompt
                           choices
                           display-fn)))

(put 'haskell-program-name 'safe-local-variable (lambda (x) (or (string? x) (list? x))))
(put 'haskell-compile-command 'safe-local-variable #'string?)
(put 'haskell-compile-cabal-build-command 'safe-local-variable #'string?)
(put 'haskell-compile-cabal-build-alt-command 'safe-local-variable #'string?)

;;; simple documentation system

(defparameter haskell-language-extensions
  ;; make this list from documentation, e.g.
  ;; http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/flag-reference.html
  ;; command: '<,'>s/^-X\([^\t]+\)\t\([^\t]+\)\t[^\t]+\t-\(?:X\(.*\)\)?/("\1" "\2" "\3")/
  '(("OverlappingInstances" "Enable overlapping instances" "NoOverlappingInstances")
    ("IncoherentInstances" "Enable incoherent instances. Implies -XOverlappingInstances " "NoIncoherentInstances")
    ("UndecidableInstances" "Enable undecidable instances" "NoUndecidableInstances")
    ("Arrows" "Enable arrow notation extension" "NoArrows")
    ("DisambiguateRecordFields" "Enable record field disambiguation" "NoDisambiguateRecordFields")
    ("ForeignFunctionInterface" "Enable foreign function interface (implied by -fglasgow-exts)" "NoForeignFunctionInterface")
    ("Generics" "Deprecated, does nothing. No longer enables generic classes. See also GHC's support for generic programming." "NoGenerics")
    ("ImplicitParams" "Enable Implicit Parameters. Implied by -fglasgow-exts." "NoImplicitParams")
    ("NoImplicitPrelude" "Don't implicitly import Prelude" "ImplicitPrelude")
    ("RebindableSyntax" "Employ rebindable syntax" "NoRebindableSyntax")
    ("NoMonomorphismRestriction" "Disable the monomorphism restriction" "MonomorphismRrestriction")
    ("NoNPlusKPatterns" "Disable support for n+k patterns" "NPlusKPatterns")
    ("NoTraditionalRecordSyntax" "Disable support for traditional record syntax (as supported by Haskell 98) C {f = x}" "TraditionalRecordSyntax")
    ("NoMonoPatBinds" "Make pattern bindings polymorphic" "MonoPatBinds")
    ("RelaxedPolyRec" "Relaxed checking for mutually-recursive polymorphic functions" "NoRelaxedPolyRec")
    ("ExtendedDefaultRules" "Use GHCi's extended default rules in a normal module" "NoExtendedDefaultRules")
    ("OverloadedStrings" "Enable overloaded string literals. " "NoOverloadedStrings")
    ("GADTs" "Enable generalised algebraic data types. " "NoGADTs")
    ("GADTSyntax" "Enable generalised algebraic data type syntax. " "NoGADTSyntax")
    ("TypeFamilies" "Enable type families." "NoTypeFamilies")
    ("ConstraintKinds" "Enable a kind of constraints." "NoConstraintKinds")
    ("DataKinds" "Enable datatype promotion." "NoDataKinds")
    ("PolyKinds" "Enable kind polymorphism. Implies -XKindSignatures." "NoPolyKinds")
    ("ScopedTypeVariables" "Enable lexically-scoped type variables. Implied by -fglasgow-exts." "NoScopedTypeVariables")
    ("MonoLocalBinds" "Enable do not generalise local bindings. " "NoMonoLocalBinds")
    ("TemplateHaskell" "Enable Template Haskell. No longer implied by -fglasgow-exts." "NoTemplateHaskell")
    ("QuasiQuotes" "Enable quasiquotation." "NoQuasiQuotes")
    ("BangPatterns" "Enable bang patterns." "NoBangPatterns")
    ("CPP" "Enable the C preprocessor." "NoCPP")
    ("PatternGuards" "Enable pattern guards." "NoPatternGuards")
    ("ViewPatterns" "Enable view patterns." "NoViewPatterns")
    ("UnicodeSyntax" "Enable unicode syntax." "NoUnicodeSyntax")
    ("MagicHash" "Allow \"#\" as a postfix modifier on identifiers." "NoMagicHash")
    ("ExplicitForAll" "Enable explicit universal quantification. Implied by -XScopedTypeVariables, -XLiberalTypeSynonyms, -XRank2Types, -XRankNTypes, -XPolymorphicComponents, -XExistentialQuantification " "NoExplicitForAll")
    ("PolymorphicComponents" "Enable polymorphic components for data constructors." "NoPolymorphicComponents")
    ("Rank2Types" "Enable rank-2 types." "NoRank2Types")
    ("RankNTypes" "Enable rank-N types." "NoRankNTypes")
    ("ImpredicativeTypes" "Enable impredicative types." "NoImpredicativeTypes")
    ("ExistentialQuantification" "Enable existential quantification." "NoExistentialQuantification")
    ("KindSignatures" "Enable kind signatures." "NoKindSignatures")
    ("EmptyDataDecls" "Enable empty data declarations." "NoEmptyDataDecls")
    ("ParallelListComp" "Enable parallel list comprehensions." "NoParallelListComp")
    ("TransformListComp" "Enable generalised list comprehensions." "NoTransformListComp")
    ("MonadComprehensions" "Enable monad comprehensions." "NoMonadComprehensions")
    ("UnliftedFFITypes" "Enable unlifted FFI types." "NoUnliftedFFITypes")
    ("InterruptibleFFI" "Enable interruptible FFI." "NoInterruptibleFFI")
    ("LiberalTypeSynonyms" "Enable liberalised type synonyms." "NoLiberalTypeSynonyms")
    ("TypeOperators" "Enable type operators." "NoTypeOperators")
    ("ExplicitNamespaces" "Enable using the keyword type to specify the namespace of entries in imports and exports." "NoExplicitNamespaces")
    ("RecursiveDo" "Enable recursive do (mdo) notation." "NoRecursiveDo")
    ("ParallelArrays" "Enable parallel arrays." "NoParallelArrays")
    ("RecordWildCards" "Enable record wildcards." "NoRecordWildCards")
    ("NamedFieldPuns" "Enable record puns." "NoNamedFieldPuns")
    ("DisambiguateRecordFields" "Enable record field disambiguation. " "NoDisambiguateRecordFields")
    ("UnboxedTuples" "Enable unboxed tuples." "NoUnboxedTuples")
    ("StandaloneDeriving" "Enable standalone deriving." "NoStandaloneDeriving")
    ("DeriveDataTypeable" "Enable deriving for the Data and Typeable classes." "NoDeriveDataTypeable")
    ("DeriveGeneric" "Enable deriving for the Generic class." "NoDeriveGeneric")
    ("GeneralizedNewtypeDeriving" "Enable newtype deriving." "NoGeneralizedNewtypeDeriving")
    ("TypeSynonymInstances" "Enable type synonyms in instance heads." "NoTypeSynonymInstances")
    ("FlexibleContexts" "Enable flexible contexts." "NoFlexibleContexts")
    ("FlexibleInstances" "Enable flexible instances. Implies -XTypeSynonymInstances " "NoFlexibleInstances")
    ("ConstrainedClassMethods" "Enable constrained class methods." "NoConstrainedClassMethods")
    ("DefaultSignatures" "Enable default signatures." "NoDefaultSignatures")
    ("MultiParamTypeClasses" "Enable multi parameter type classes." "NoMultiParamTypeClasses")
    ("FunctionalDependencies" "Enable functional dependencies." "NoFunctionalDependencies")
    ("PackageImports" "Enable package-qualified imports." "NoPackageImports")
    ("LambdaCase" "Enable lambda-case expressions." "NoLambdaCase")
    ("MultiWayIf" "Enable multi-way if-expressions." "NoMultiWayIf")
    ("Safe" "Enable the Safe Haskell Safe mode." "")
    ("Trustworthy" "Enable the Safe Haskell Trustworthy mode." "")
    ("Unsafe" "Enable Safe Haskell Unsafe mode." ""))
  "List of Haskell extensions for GHC 7.6.3 release.

See http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/flag-reference.html
for more information.")

(defun haskell-help-for-symbol-at-point ()
  "Show help for entity at point, if any.

Currently only language extensions are supported."
  (interactive)
  (let ((name (haskell-ident-at-point)))
    (aif (assoc name haskell-language-extensions)
      (destructuring-bind (ext-name doc inverse) it
        (message "%s (%s)\n%s"
                 name
                 (if (eq? inverse "") "no inverse" inverse)
                 doc))
      (error "No documentation for %s" name))))

;;; haddock for modules

(defun inferior-haskell-haddock-module (name)
  "Find and open the Haddock documentation of module NAME.
Only works for module in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it."
  (interactive
   (let ((name (haskell-ident-at-point)))
     (list (read-string (if (> (length name) 0)
                          (format "Find documentation of module (default %s): " name)
                          "Find documentation of module: ")
                        nil nil name))))
  (setq name (inferior-haskell-map-internal-ghc-ident name))
  (let ( ;; Find the module and look it up in the alist
        (alist-record (assoc name (inferior-haskell-module-alist))))

    (if alist-record
      (progn ;; if documentation for such module exists at all
        (let* ((package (nth 1 alist-record))
               (file-name (concat (subst-char-in-string ?. ?- name) ".html"))
               (local-path (concat (nth 2 alist-record) "/" file-name))
               (url (if (or (eq inferior-haskell-use-web-docs 'always)
                            (and (not (file-exists-p local-path))
                                 (eq inferior-haskell-use-web-docs 'fallback)))
                      (concat inferior-haskell-web-docs-base package "/" file-name
                              ;; no haddock anchor for module names
                              )
                      (and (file-exists-p local-path)
                           ;; no haddock anchor for module names
                           (concat "file://" local-path)))))
          (if url (browse-url url) (error "Local file doesn't exist"))))
      (error "No documentation for module %s found" name))))

;;; up level navigation

(defun haskell-back-up-indent-level ()
  "Move up to lesser indentation level, skipping empty lines."
  (let* ((get-whitespace-level
          (lambda ()
            (save-excursion
              (back-to-indentation)
              (current-column))))
         (current-level (funcall get-whitespace-level)))
    (while (and (not (bob?))
                (<= current-level
                    (funcall get-whitespace-level)))
      (backward-line 1)
      (while (looking-at-pure? "^$")
        (backward-line 1)))
    (back-to-indentation)))

(defun haskell-move-up ()
  (interactive)
  (or (sp-backward-up-sexp)
      (haskell-back-up-indent-level)))

;;; miscellany

(defalias 'inferior-haskell-haddock-identifier 'inferior-haskell-find-haddock)


;;; Automatized definitions using advices-util and macro-util

;;;; align functions

(make-align-function haskell-align-on-equals
                     "=[^=]"
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-arrows
                     "\\(?:->\\|→\\)\\(?: \\|$\\)")
(make-align-function haskell-align-on-left-arrows
                     "\\(?:<-\\|←\\)\\(?: \\|$\\)")
(make-align-function haskell-align-on-guards
                     "|\\(?:[^|]\\|$\\)"
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-commas
                     ",\\(?:[^,\)]\\|$\\)")
(make-align-function haskell-align-on-comments
                     "--+\\(?: \\|$\\)"
                     :require-one-or-more-spaces t)

(make-align-function haskell-align-on-double-colons
                     "\\(?:::[^:]\\|∷\\)")

;;;; custom queries to inferior-haskell

(haskell/make-query-to-inferior haskell-type               inferior-haskell-type t)
(haskell/make-query-to-inferior haskell-info               inferior-haskell-info)
(haskell/make-query-to-inferior haskell-haddock-identifier inferior-haskell-haddock-identifier)
(haskell/make-query-to-inferior haskell-haddock-module     inferior-haskell-haddock-module)
(haskell/make-query-to-inferior haskell-find-definition    inferior-haskell-find-definition)
(haskell/make-query-to-inferior haskell-hoogle-at-point    haskell-hoogle)
(haskell/make-query-to-inferior haskell-hayoo-at-point     haskell-hayoo)

;;; define forward-haskell-symbol

(defparameter forward-haskell-symbol-re
  (rx (or (group (+ ;; (regexp "[-!#$%&*+./<=>?@^|~:\\]")
                  (any ?\- ?\! ?\# ?\$ ?\% ?\& ?\* ?\+ ?\. ?\/ ?\< ?\= ?\> ?\? ?\@ ?^ ?\| ?\~ ?\: ?\\ )))
          (group
           (seq bow
                ;; allow _ as a first char to fit GHC
                (or (regexp "[_a-z]")
                    ;; allow ' preceding conids because of DataKinds/PolyKinds
                    (regexp "'?[A-Z]"))
                (group
                 (* (regexp "['a-zA-Z_0-9#]")))))))
  "Regexp to recognize haskell symbols as generic enttities for search
(with e..g \"*\" in vim).")

(bounds-of-thing-at-point 'haskell-symbol)

(put 'haskell-symbol 'forward-op #'forward-haskell-symbol)

(defun forward-haskell-symbol (arg)
  "Like `forward-symbol' but for generic Haskell symbols (either operators,
uppercase or lowercase names)."
  (interactive "p")
  (let ((name-chars "a-zA-Z0-9'_#")
        (operator-chars "\\-!#$%&*+./<=>?@\\^|~:\\\\"))
    (if (natnump arg)
      (re-search-forward forward-haskell-symbol-re nil t arg)
      (while (< arg 0)
        (when (re-search-backward forward-haskell-symbol-re nil t)
          (cond ((not (null? (match-beginning 1)))
                 (skip-chars-backward operator-chars)
                 ;; we may have matched # thas ends a name
                 (skip-chars-backward name-chars))
                ((not (null? (match-beginning 2)))
                 ;; (goto-char (match-beginning 2))
                 (when (not (null? (match-beginning 3)))
                   (skip-chars-backward name-chars)))
                (else
                 (error "No group of forward-haskell-symbol-re matched, should not happen"))))
        (setf arg (1+ arg))))))

;;; haskell parsing

;; (defun haskell-tokenize (chars)
;;   "Tokenize a list of characters."
;;   (with-syntax-table haskell-mode-syntax-table
;;     (let ((tokens nil)
;;           (tok nil)
;;           (last-syntax nil)
;;           (syntax-groups
;;            ;; no need to keep one-element groups
;;            '( ;; (?w)
;;              ;; (?_)
;;              ;; (?\")
;;              ;; (?\$)
;;              ;; (?\\)
;;              (?\( ?\))
;;              (?\s ?- ?>))))
;;       (dolist (c chars)
;;         (let ((syntax (char-syntax c)))
;;           (when (and (not (null? last-syntax))
;;                      (not (char=? last-syntax syntax))
;;                      (not (find-if (lambda (group)
;;                                      (and (member last-syntax group)
;;                                           (member syntax group)))
;;                                    syntax-groups)))
;;             (when (not (null? tok))
;;               (push (list->string (nreverse tok)) tokens))
;;             (setf tok nil
;;                   last-syntax syntax))
;;           (unless (or (char=? ?\s syntax)
;;                       (char=? ?\- syntax)
;;                       (char=? ?> syntax))
;;             (push c tok))
;;           (setf last-syntax syntax)
;;           ;; (case syntax
;;           ;;   ((?w ?_)
;;           ;;    (push c tok))
;;           ;;   (())
;;           ;;
;;           ;;   ((?\s ?>)
;;           ;;    (when tok
;;           ;;      (push (list->string (reverse tok)) tokens)
;;           ;;      (setf tok nil))))
;;           ))
;;       (when (not (null? tok))
;;         (push (list->string (nreverse tok)) tokens))
;;       (nreverse tokens))))

;; (haskell-tokenize (string->list "foo    :: a         -> b\nfoo x = bar <$> (x :*! y) <*> baz (x: xs :*: xs)"))

(defmacro haskell-peg-parse-string (rules string)
  "Call `peg-parse-string' with some rules predefined for haskell."
  `(condition-case err
       (peg-parse-string
        ((root ,@rules)
         (typeclass-constraints
          (list (or typeclass-constraints/typeclasses
                    (and "(" some-space-opt
                         typeclass-constraints/typeclasses
                         some-space-opt ")")))
          `(typeclasses -- (cons :typeclasses typeclasses))
          whitespace
          "=>")
         (typeclass-constraints/typeclasses
          typeclass-constraints/typeclass
          (* some-space-opt
             ","
             some-space-opt
             typeclass-constraints/typeclass))
         (typeclass-constraints/typeclass
          (substring qupcase-ident
                     some-space
                     ident
                     (* some-space
                        ident)))
         (whitespace (* (or [?\s ?\t]
                            newline
                            comment)))
         (comment (or (and "--" (* (any)) newline)
                      ncomment))
         (ncomment "{-" (* (or (any) newline)) (opt ncomment) "-}")
         (newline (or "\n"
                      "\n\r"
                      "\r"
                      "\f"))
         (delim "::")
         (arrow (or "->" "→"))
         (func-name (substring (or ident
                                   (and "("
                                        whitespace
                                        op
                                        whitespace
                                        ")"))))
         (some-space (+ [?\s]))
         (some-space-opt (* [?\s]))
         (type-name (substring type-name-func))
         (type-name-func type-name-atomic
                         (or (* some-space-opt
                                arrow
                                some-space-opt
                                type-name-func))
                         (or (* some-space
                                type-name-func)))
         (type-name-atomic (or (and (or qupcase-ident qident)
                                    (opt "#"))
                               "()"
                               (and "(" some-space-opt
                                    type-name-func
                                    (or (* some-space-opt ","
                                           some-space-opt type-name-func))
                                    some-space-opt ")")
                               (and "(" some-space-opt
                                    type-name-func
                                    (or (* some-space-opt arrow
                                           some-space-opt type-name-func))
                                    some-space-opt ")")
                               (and "(#" some-space-opt
                                    type-name-func
                                    (* some-space-opt ","
                                       some-space-opt type-name-func)
                                    some-space-opt "#)")
                               (and "[" some-space-opt
                                    type-name-func
                                    some-space-opt "]")))
         (qupcase-ident (* upcase-ident
                           ".")
                        upcase-ident)
         (upcase-ident [A-Z]
                       (* [?_ ?\' a-z A-Z 0-9]))
         (qident (* upcase-ident
                    ".")
                 ident)
         (ident [?_ a-z]
                (* [?_ ?\' a-z A-Z 0-9]))
         (op (+ [?\! ?\# ?\$ ?\% ?\& ?\* ?\+ ?\. ?\/ ?\< ?\= ?\> ?\? ?\@ ?\\ ?^ ?\| ?\- ?\~ ?\:])))
        ,string
        t)
     (error (message "%s" err))))

;; (defmacro haskell-peg-parse-string (rules string)
;;   "Call `peg-parse-string' with some rules predefined for haskell."
;;   `(peg-parse-string ((root ,@rules)
;;
;;                       ;; TODO: what about parsing a stream of tokens with PEG?
;;
;;                       ;; varid → (small {small | large | digit | ' })⟨reservedid⟩
;;                       (varid (sans-others (seq small
;;                                                (* (or small
;;                                                       large
;;                                                       digit
;;                                                       "'")))
;;                                           reservedid))
;;
;;                       ;; conid → large {small | large | digit | ' }
;;                       ;; reservedid → case | class | data | default | deriving | do | else
;;                       ;;    |         foreign | if | import | in | infix | infixl
;;                       ;;    |         infixr | instance | let | module | newtype | of
;;                       ;;    |         then | type | where | _
;;
;;                       ;; lexeme → qvarid | qconid | qvarsym | qconsym
;;                       ;;         |         literal | special | reservedop | reservedid
;;                       (lexeme (or qvarid
;;                                   qconid
;;                                   qvarsym
;;                                   qconsym
;;                                   literal
;;                                   special
;;                                   reversedop
;;                                   reversedid))
;;
;;                       ;; literal → integer | float | char | string
;;                       (literal integer
;;                                float
;;                                char
;;                                string)
;;
;;                       ;; special → ( | ) | , | ; | [ | ] | ` | { | }
;;                       (special (or "("
;;                                    ")"
;;                                    ","
;;                                    ";"
;;                                    "["
;;                                    "]"
;;                                    "`"
;;                                    "{"
;;                                    "}"))
;;
;;                       ;; whitespace → whitestuff {whitestuff}
;;                       (whitespace (+ whitestuff))
;;
;;                       ;; whitestuff → whitechar | comment | ncomment
;;                       (whitestuf (or whitechar comment ncomment))
;;
;;                       ;; whitechar → newline | vertab | space | tab | uniWhite
;;                       (whitechar (or newline [?\v ?\s ?\t] ;; uniWhite
;;                                      ))
;;
;;                       ;; newline → return linefeed | return | linefeed | formfeed
;;                       (newline (or "\r\n" "\r" "\n" "\f"))
;;                       ;; return → a carriage return
;;                       ;; linefeed → a line feed
;;                       ;; vertab → a vertical tab
;;                       ;; formfeed → a form feed
;;                       ;; space → a space
;;                       ;; tab → a horizontal tab
;;                       ;; uniWhite → any Unicode character defined as whitespace
;;
;;                       ;; comment → dashes [ any⟨symbol⟩ {any} ] newline
;;                       (comment "--" (* "-")
;;                                (opt (not symbol)
;;                                     (* (any-lit)))
;;                                newline)
;;                       ;; dashes → -- {-}
;;
;;                       ;; opencom → {-
;;                       (opencom "{-")
;;                       ;; closecom → -}
;;                       (closecom "-}")
;;
;;                       ;; ncomment → opencom ANY seq {ncomment ANY seq} closecom
;;                       (ncomment opencom ANY-seq closecom)
;;
;;                       ;; ANY seq → {ANY }⟨{ANY } ( opencom | closecom ) {ANY }⟩
;;                       (ANY-seq (sans-others (* ANY)
;;                                             (seq (* ANY)
;;                                                  (or opencom
;;                                                      closecom)
;;                                                  (* ANY))))
;;                       ;; ANY → graphic | whitechar
;;                       (ANY (or graphic whitechar))
;;                       ;; any → graphic | space | tab
;;                       (any-lit (or graphic
;;                                    space
;;                                    tab))
;;
;;                       ;; graphic → small | large | symbol | digit | special | " | '
;;                       (graphic (or small
;;                                    large
;;                                    symbol
;;                                    digit
;;                                    special
;;                                    "\""
;;                                    "'"))
;;                       ;; small → ascSmall | uniSmall | _
;;                       (small (or ascSmall
;;                                  "_"))
;;                       ;; ascSmall → a | b | … | z
;;                       (ascSmall [a-z])
;;                       ;; uniSmall → any Unicode lowercase letter
;;
;;                       ;; large → ascLarge | uniLarge
;;                       (lange (or ascLarge))
;;                       ;; ascLarge → A | B | … | Z
;;                       (ascLange [A-Z])
;;                       ;; uniLarge → any uppercase or titlecase Unicode letter
;;                       ;; symbol → ascSymbol | uniSymbol⟨special | _ | " | '⟩
;;                       (symbol (or ascSymbol))
;;                       ;;
;;                       ;; ascSymbol → ! | # | $ | % | & | ⋆ | + | . | / | < | = | > | ? | @
;;                       ;;         |         \ | ^ | | | - | ~ | :
;;                       (ascSymbol [!#$%&⋆+./<=>?@\^|-~:])
;;                       ;; uniSymbol → any Unicode symbol or punctuation
;;                       ;; digit → ascDigit | uniDigit
;;                       (digit (or ascDigit))
;;                       ;; ascDigit → 0 | 1 | … | 9
;;                       (ascDigit [0-9])
;;                       ;; uniDigit → any Unicode decimal digit
;;                       ;; octit → 0 | 1 | … | 7
;;                       (octit [0-7])
;;                       ;; hexit → digit | A | … | F | a | … | f
;;                       (hexit (or digit
;;                                  [a-fA-F])))
;;                      ,string
;;                      t))

(defun haskell-parse-signature (signature)
  "Returns alist of (:functions <strings>) and (:argument-types <strings>)
entries. Returns nil on failure."
  (cdr-safe
   (haskell-peg-parse-string ((list func-name (* whitespace "," whitespace func-name))
                              whitespace
                              delim
                              `(funcs -- (cons :functions funcs))
                              (opt whitespace
                                   typeclass-constraints)
                              whitespace
                              (list
                               type-name
                               ;; (* whitespace
                               ;;    arrow
                               ;;    whitespace
                               ;;    type-name)
                               )
                              `(types -- (cons :argument-types types))
                              some-space-opt
                              (or newline
                                  (eol)))
                             signature)))


(defun haskell-newline ()
  "Similar to `sp-newline' but autoexpands haskell signatures."
  (interactive)
  (let ((indent
         (lambda ()
           (if (shm-current-node-pair)
             (shm/newline-indent)
             (shm/simple-indent-newline-same-col)))))
    (when (memq major-mode +haskell-syntax-modes+)
      (let ((line (current-line)))
        (if-let (result (haskell-parse-signature (trim-whitespace line)))
          (if-let* (signature
                    result
                    indentation
                    (indentation-size)
                    funcs
                    (cdr-safe (assoc :functions signature))
                    func
                    (car funcs))
            (begin
              (funcall indent)
              ;; Maybe consider using this function instead?
              ;; (shm/simple-indent-newline-same-col)
              (unless (save-excursion
                        (forward-line)
                        (skip-syntax-forward "->")
                        (looking-at-pure? (concat (regexp-quote func)
                                                  "\\_>")))
                (delete-region (line-beginning-position) (point))
                (insert (make-string indentation ?\s)
                        func
                        " ")))
            ;; indent in either case, the key is to indent
            ;; *after* parsing signature on current line
            (funcall indent))
          (funcall indent))))))

(defun haskell-abbrev+-fallback-space ()
  (interactive)
  (if structured-haskell-mode
    (shm/space)
    (insert " ")))

;; (search-def-autoexpand-advices (show-subtree) (haskell-mode))

;;; remember position on query

;; (defadvice:remember-position-on-query inferior-haskell-find-definition)
;; (defadvice:remember-position-on-query haskell-find-definition)


(provide 'haskell-misc)

;; Local Variables:
;; End:

;; haskell-misc.el ends here
