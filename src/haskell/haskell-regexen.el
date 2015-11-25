;; haskell-regexen.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 February 2014
;; Description:

(defmacro defconst-set (var val)
  (declare (indent 1))
  `(setq ,var ,val))

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

(provide 'haskell-regexen)

;; Local Variables:
;; End:

;; haskell-regexen.el ends here
