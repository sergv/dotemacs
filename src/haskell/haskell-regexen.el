;; haskell-regexen.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 February 2014
;; Description:

(eval-when-compile
  (require 'macro-util))

(defmacro defconst-set (var val)
  (declare (indent 1))
  `(defconst ,var ,val)
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

(defconst-set haskell-regexen/function-signature-colons "\\(?:::[^:]\\|∷\\)")

(defconst-set haskell-regexen/preprocessor-or-empty-line "^\\(?:#\\|[ \t]*$\\)")
(defconst-set haskell-regexen/empty-line "^[ \t]*$")

(defconst-set haskell-regexen/import-line
  "import[ \t\r\n]+\\(?:\"[^\"]+\"[ \t\r\n]+\\)?")

(defconst-set haskell-regexen/qualified-import-line
  "import[ \t\r\n]+\\(?:\"[^\"]+\"[ \t\r\n]+\\)?qualified[ \t\r\n]+")


(defconst-set haskell-module-quantification-regexp
  (let ((conid "\\b[[:upper:]][[:alnum:]'_]*\\b"))
    (concat "\\b\\(?:" conid "\\.\\)+")))

;;;###autoload
(defun haskell-remove-module-qualification (name)
  "Removes hierarchihal modules qualification (e.g. Data.Map.null -> null,
 Prelude.++ -> ++, etc)"
  (save-match-data
    (if (string-match (eval-when-compile
                        (concat "^\\("
                                haskell-module-quantification-regexp
                                "\\)"))
                      name)
        (replace-match "" t t name 1)
      name)))

(provide 'haskell-regexen)

;; Local Variables:
;; End:

;; haskell-regexen.el ends here
