;; haskell-regexen.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 February 2014
;; Description:

(eval-when-compile
  (require 'macro-util))

(defmacro defconst-set (var val &optional doc)
  (declare (indent 1) (docstring 3))
  `(defconst ,var ,val ,doc)
  ;; `(setq ,var ,val)
  )

(defconst-set haskell-regexen/varid
  "\\(?:_\\|\\b[[:lower:]]\\)[[:alnum:]'_#]*")

(defconst-set haskell-regexen/conid
  "\\(?:\\b\\|'\\)[[:upper:]][[:alnum:]'_#]*")
(defconst-set haskell-regexen/modid
  (concat "\\b" haskell-regexen/conid
          "\\(?:\\." haskell-regexen/conid "\\)*\\b"))

(defconst-set haskell-regexen/q/varid
  (concat "\\(" haskell-regexen/modid "\\)\\.\\(" haskell-regexen/varid "\\)"))
(defconst-set haskell-regexen/q/conid
  (concat haskell-regexen/modid "\\." haskell-regexen/conid))

(defconst-set haskell-regexen/q/varid-or-conid
  (concat haskell-regexen/modid "\\."
          "\\(?:" haskell-regexen/varid "\\|" haskell-regexen/conid "\\)"))

(defconst-set haskell-regexen/opt-q/varid-or-conid
  (concat "\\(?:" haskell-regexen/modid "\\.\\)?"
          "\\(?:" haskell-regexen/varid "\\|" haskell-regexen/conid "\\)"))
;; ;; (old-sym "[-!#$%&*+./<=>?@^|~:\\]+")
;; (defconst-set haskell-regexen/sym-constructor
;;    "\\(?::[-!#$%&*+./<=>?@^|~:\\]*\\)")
;;
;; (defconst-set haskell-regexen/sym-nonconstructor
;;    "\\(?:[-!#$%&*+./<=>?^|~\\][-!#$%&*+./<=>?@^|~:\\]*\\|@[-!#$%&*+./<=>?@^|~:\\]+\\)")
;;
;; ;; sym = sym-nonconstructor ∪ sym-constructor = old-sym
;; (defconst-set haskell-regexen/sym
;;   (concat "\\(?:"
;;           haskell-regexen/sym-constructor
;;           "\\|"
;;           haskell-regexen/sym-nonconstructor
;;           "\\)"))

;; let sigma = "!#$%&*+-./:<=>?@\\^|~" in (toEmacsRegex (computeDiffRegex sigma ["", "..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]) sigma)
;; NB: unicode is in todo for now
;; all unicode chars
;; "→←≡≠»≤≥⧺↢↣⤛⤜‥∈∉⇒∷∧∨⊕⊗"
;; reserved unicode chars
;; "→←⇒‥∷"
(defconst-set haskell-regexen/sym
  "\\(?:\\(?:[!#$%&*+/>?^][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\|\\(?:\\(?:\\.\\(?:\\(?:\\(?:[-!#$%&*+/:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\|\\(?:\\.\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\)\\)?\\)\\|\\(?:\\(?::\\(?:\\(?:\\(?:[-!#$%&*+./<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\|\\(?::\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\)\\)?\\)\\|\\(?:\\(?:=\\(?:\\(?:[-!#$%&*+./:<=?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\|\\(?:>\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\)\\)\\|\\(?:\\(?:\\\\\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\|\\(?:\\(?:|\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\|\\(?:\\(?:<\\(?:\\(?:\\(?:[!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\|\\(?:-\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\)\\)?\\)\\|\\(?:\\(?:-\\(?:\\(?:\\(?:[-!#$%&*+./:<=?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\|\\(?:>\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\)\\)?\\)\\|\\(?:\\(?:@\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\|\\(?:~\\(?:[-!#$%&*+./:<=>?@\\\\^|~][-!#$%&*+./:<=>?@\\\\^|~]*\\)\\)\\)\\)\\)\\)\\)\\)\\)\\)\\)")

;; (let* ((s "<-")
;;        (idx (string-match? (concat "^" haskell-regexen/sym "$")
;;                            s)))
;;   (if idx
;;     (list idx
;;           (match-beginning 0)
;;           (match-end 0))
;;     'not-matched))

(defconst-set haskell-regexen/reservedsym
  (rx (or ".."
          "::"
          "="
          "\\"
          "|"
          "<-"
          "->"
          "@"
          "~"
          "=>"
          "→"
          "←"
          "⇒"
          "‥"
          "∷")))

(defconst-set haskell-regexen/reservedsym/w-context
  (concat "\\(?:^\\|[^-!#$%&*+./<=>?@^|~:\\]\\)"
          "\\("
          haskell-regexen/reservedsym
          "\\)"
          "\\(?:$\\|[^-!#$%&*+./<=>?@^|~:\\]\\)"))

(defconst-set haskell-regexen/pragma-start "{-#")
(defconst-set haskell-regexen/pragma-end "#-}")

;; Generated with
;; (dolist (x
;;          '("INCOHERENT"
;;            "OVERLAPPABLE"
;;            "OVERLAPPING"
;;            "OVERLAPS"
;;            "SOURCE"
;;            "UNPACK"
;;            "NOUNPACK"))
;;   (insert "(seq")
;;   (cl-loop
;;    for c across x
;;    do
;;    (insert (format " (char ?%c ?%c)" (downcase c) (upcase c))))
;;   (insert ")\n"))
(defconst haskell-regexen/pragma-without-args-re
  (rx bos
      (or (seq (char ?i ?I) (char ?n ?N) (char ?c ?C) (char ?o ?O) (char ?h ?H) (char ?e ?E) (char ?r ?R) (char ?e ?E) (char ?n ?N) (char ?t ?T))
          (seq (char ?s ?S) (char ?o ?O) (char ?u ?U) (char ?r ?R) (char ?c ?C) (char ?e ?E))
          (seq (char ?u ?U) (char ?n ?N) (char ?p ?P) (char ?a ?A) (char ?c ?C) (char ?k ?K))
          (seq (char ?n ?N) (char ?o ?O) (char ?u ?U) (char ?n ?N) (char ?p ?P) (char ?a ?A) (char ?c ?C) (char ?k ?K))
          (seq (char ?o ?O) (char ?v ?V) (char ?e ?E) (char ?r ?R) (char ?l ?L) (char ?a ?A) (char ?p ?P)
               (or (seq (char ?p ?P)
                        (or (seq (char ?a ?A) (char ?b ?B) (char ?l ?L) (char ?e ?E))
                            (seq (char ?i ?I) (char ?n ?N) (char ?g ?G))))
                   (seq (char ?s ?S)))))
      eos))

(defconst haskell-regexen/scc-pragma-name
  (rx bos
      (char ?s ?S)
      (char ?c ?C)
      (char ?c ?C)
      eos))

(defconst haskell-regexen/language-pragma-name
  (rx bos
      (char ?l ?L)
      (char ?a ?A)
      (char ?n ?N)
      (char ?g ?G)
      (char ?u ?U)
      (char ?a ?A)
      (char ?g ?G)
      (char ?e ?E)
      eos))

(defconst haskell-regexen/language-pragma-prefix
  (rx "{-#"
      (* (any ?\s ?\t ?\n ?\r))
      (char ?l ?L)
      (char ?a ?A)
      (char ?n ?N)
      (char ?g ?G)
      (char ?u ?U)
      (char ?a ?A)
      (char ?g ?G)
      (char ?e ?E)))

(defconst haskell-regexen/options-ghc-pragma-prefix
  (rx "{-#"
      (* (any ?\s ?\t ?\n ?\r))
      (char ?o ?O)
      (char ?p ?P)
      (char ?t ?T)
      (char ?i ?I)
      (char ?o ?O)
      (char ?n ?N)
      (char ?s ?S)
      "_"
      (char ?g ?G)
      (char ?h ?H)
      (char ?c ?C)))

(defconst-set haskell-regexen/function-signature-colons "\\(?:::[^:]\\|∷\\)")

(defconst-set haskell-regexen/empty-line "^[ \t]*$")

(defconst-set haskell-regexen/module-header-start
  "\\_<module\\_>[ \t\r\n]" )

(defconst-set haskell-regexen/module-name-section
  "[[:upper:]][[:alnum:]'_]*")

(defconst-set haskell-regexen/module-name
  (eval-when-compile
    (let ((conid "[[:upper:]][[:alnum:]'_]*"))
      (concat "\\b" haskell-regexen/module-name-section
              "\\(?:\\." haskell-regexen/module-name-section "\\)*\\b"))))

(defconst-set haskell-regexen/pre-post-qualified-import-line
  (rx-let ((ws (any ?\s ?\t ?\r ?\n))
           (constituent
            (any (?A . ?Z) (?a . ?z) (?0 . ?9) ?_ ?- ?.)))
    (rx (group-n 7
                 bow
                 "import"
                 eow
                 (* ws)
                 (? (seq "{-#"
                         (* (any ?\s ?\t ?\n ?\r))
                         (char ?s ?S)
                         (char ?o ?O)
                         (char ?u ?U)
                         (char ?r ?R)
                         (char ?c ?C)
                         (char ?e ?E)
                         (* (any ?\s ?\t ?\n ?\r))
                         "#-}"
                         (* ws)))
                 (? (group-n 1 bow "safe" eow)
                    (* ws)))
        (? (+ ws)
           (group-n 8
                    (group-n 2 bow "qualified" eow)
                    (* ws)))

        (? (* ws)
           ?\"
           (* (any (?A . ?Z) (?a . ?z) (?0 . ?9) ?_ ?- ?.))
           ?\"
           (* ws))

        ;; Module name
        (group-n 10 (+ constituent))

        (? (group-n 9
                    (+ ws)
                    (group-n 3 bow "qualified" eow)))
        (? (group-n 6 (+ ws))
           (group-n 4 bow "as" eow)
           (+ ws)
           (+ constituent))
        (? (+ ws)
           (group-n 5 bow "hiding" eow))
        (* ws)
        (? "("
           (* nonl))

        eol)))

(defconst-set haskell-regexen/module-quantification
  (eval-when-compile
    (concat "\\b\\(?:" haskell-regexen/module-name-section "\\.\\)+")))

;;;###autoload
(defun haskell-remove-module-qualification (name)
  "Removes hierarchihal modules qualification (e.g. Data.Map.null -> null,
 Prelude.++ -> ++, etc)"
  (save-match-data
    (if (string-match (eval-when-compile
                        (concat "^\\("
                                haskell-regexen/module-quantification
                                "\\)"))
                      name)
        (replace-match "" t t name 1)
      name)))

(defconst-set haskell-regexen/ghci-info-definition-site-in-curr-project-for-old-ghci
  (rx "-- Defined at "
      (group-n 1 (+ (not (any ?\r ?\n ?\t))))
      ":"
      (group-n 2 (+ (any (?0 . ?9))))
      ":"
      (group-n 3 (+ (any (?0 . ?9))))
      eol)
  "Similar to ‘haskell-regexen/ghci-info-definition-site’ but for older GHCs (8.6-ish).")

(defconst-set haskell-regexen/ghci-info-definition-site
  (rx "-- Defined in "
      ?\‘
      (group-n 1 (+ (not (any ?\n ?\r ?\t))))
      ?\’
      eol)
  "Regex used to extract location of where a name is defined out of result of ghci’s :i command.

This regexps is for newer ghc (8.10+-ish).")

(defconst-set haskell-regexen/ghci-name-not-in-scope-error
  (rx "error: Not in scope: ‘"
      (+ (not (any ?\r ?\n ?\t)))
      "’"
      eol))

;; On external symbols, GHC may return a location such as integer-gmp-1.0.0.1:integer-gmp-1.0.0.1:GHC.Integer.Type
(defconst-set haskell-regexen/ghci-src-span
  "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))\\'")

(defconst-set haskell-regexen/package-version
  (rx ?-
      (+ (any (?0 . ?9) ?.))
      ;; Unique hash that ghci may print
      (? ?- (+ (any (?a . ?z) (?0 . ?9))))
      eos))

(defconst-set haskell-regexen/ghci-loc-at-external-symbol
  (rx-let ((version (seq ?-
                         (+ (any (?0 . ?9) ?.)))))
    (rx bos
        (seq (group-n 1 (+? (not ?:))) version)
        ?:
        (seq (group-n 2 (+? (not ?:))) version)
        ?:
        (seq (group-n 3 (+? (not ?:))))
        eos)))

(provide 'haskell-regexen)

;; Local Variables:
;; End:

;; haskell-regexen.el ends here
