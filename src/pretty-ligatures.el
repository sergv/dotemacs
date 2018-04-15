;; pretty-ligatures.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 April 2018
;; Description:
;; Inspired by https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md.

(require 'dash)

;; Alternative way to add ligatures. Let here for reference
;; NB order is important
;; (defconst pretty-ligatures--definitions
;;   (--map (cons (concat "\\(1:" (car it) "\\)") (cdr it))
;;          '(("<->" . #Xe104)
;;            ("<=>" . #Xe105)
;;            ("<-"  . #Xe100)
;;            ("->"  . #Xe101)
;;            ("<="  . #Xe102)
;;            ("=>"  . #Xe103))))
;; ;;;###autoload
;; (defun pretty-ligatures-install! ()
;;   (font-lock-add-keywords
;;    nil
;;    (--map (let ((re (car it))
;;                 (uni-point (cdr it)))
;;             `(,re (0 (progn
;;                        (compose-region
;;                         (match-beginning 1)
;;                         (match-end 1)
;;                         ,(concat "\t" (list uni-point)))
;;                        nil))))
;;           pretty-ligatures--definitions)))

;; (set-fontset-font t '(#Xe100 . #Xe115) "Iosevka Slab Lig")

(defconst pretty-ligatures--symbol-replacements
  (let* ((ligs
          '(("<-"  . #xe100)
            ("->"  . #xe101)
            ;; ("<="  . #xe102)
            ("=>"  . #xe103)
            ("<->" . #xe104)
            ("<=>" . #xe105)
            ("=="  . #xe106)
            ("/="  . #xe107)
            ("::"  . #xe108)
            ("<<-" . #xe109)
            ("->>" . #xe10a)
            ("<-<" . #xe10b)
            (">->" . #xe10c)
            ("++"  . #xe10d)
            ("+++" . #xe10e)
            ("<>"  . #xe10f)
            ("><"  . #xe110)
            ("<<"  . #xe111)
            (">>"  . #xe112)
            ("<|"  . #xe113)
            ("|>"  . #xe114)

            ("##"   . #xe115)
            ("###"  . #xe116)
            ("####" . #xe117)

            ("<--" . #xe119)
            ("-->" . #xe119)
            ("<==" . #xe11a)
            ("==>" . #xe11b)

            ("=<<" . #xe11c)
            (">>=" . #xe11d)
            ("<=<" . #xe11e)
            (">=>" . #xe11f)
            ("<<=" . #xe120)
            ("=>>" . #xe121)

            ("<="  . #xe122)
            (">="  . #xe123)
            ("||"  . #xe124)
            ("&&"  . #xe125)

            ;;("><" . ?⨝)

            ;; Hasklig
            ;; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
            ;; "&&" "***" "*>" "\\\\" "||" "|>" "::"
            ;; "==" "===" "==>" "=>" "=<<" "!!" ">>"
            ;; ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
            ;; "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
            ;; "<<" "<<<" "<+>" ".." "..." "++" "+++"
            ;; "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->"

            ;; Fira Code
            ;; "www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
            ;; "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
            ;; "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
            ;; "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
            ;; ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
            ;; "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
            ;; "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
            ;; "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
            ;; ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
            ;; "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
            ;; "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
            ;; "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
            ;; "x" ":" "+" "+" "*"
            )))
    (--map (cons (car it) (string ?\t (cdr it) ?\t)) ligs))
  "Symbolic ligatures that work through `prettify-symbols-mode'.")

(defconst pretty-ligatures--word-replacements
  (let* ((make-combinations
          (lambda (func-name module-prefixes code-point)
            (--map (cons it code-point)
                   (-mapcat (lambda (f)
                              (cons (funcall f func-name)
                                    (--map (funcall f (concat it "." func-name)) module-prefixes)))
                            (list #'identity (lambda (x) (concat "`" x "`")))))))
         (standard-prefixes
          '("M" "Map" "S" "Set" "HM" "HashMap" "HS" "HashSet" "IS" "IntSet" "IM" "IntMap"))
         (ligs
          (append
           (funcall make-combinations "mappend" nil #xe10f)
           (funcall make-combinations "union" standard-prefixes #xe130)
           (funcall make-combinations "intersection" standard-prefixes #xe131)
           (funcall make-combinations "elem" nil #xe12b)
           (funcall make-combinations "member" standard-prefixes #xe12b)
           (funcall make-combinations "notElem" nil #xe12c)
           (funcall make-combinations "notMember" standard-prefixes #xe12c)

           ;; possibly equal
           (funcall make-combinations "isSubsetOf" standard-prefixes #xe12f)
           ;; not-equal
           (funcall make-combinations "isProperSubsetOf" standard-prefixes #xe12e)

           (funcall make-combinations "empty" (append '("V" "Vector") standard-prefixes) #xe12a)

           '(;; ("Double"   . ?ℝ)
             ;; ("Int"      . ?ℤ)
             ;; ("Natural"  . ?ℕ)
             ;; ("Rational" . ?ℚ)
             ;; ("Complex"  . ?ℂ)
             ;; ("Bool"     . ?𝔹)

             ;; ("x"        . ?𝓍)
             ;; ("y"        . ?𝓎)
             ;; ("z"        . ?𝓏)

             ;; a 𝒶 𝒷 𝒸 𝒹 ℯ 𝒻 ℊ 𝒽 𝒾 𝒿 𝓀 𝓁 𝓂 𝓃 ℴ 𝓅 𝓆 𝓇 𝓈 𝓉 𝓊 𝓋 𝓌 𝓍 𝓎 𝓏

             ;; bottom
             ("undefined" . #xe127)

             ;; ("mappend" . #xe10f)
             ;; ("`mappend`" . #xe10f)
             ("forall"    . #xe128)

             ("[]"        . #xe12a)
             ("mempty"    . #xe12a)

             ("sum"       . #xe12d)
             ("product"   . #xe132)))))
    (--map (cons (car it) (string ?\t (cdr it) ?\t)) ligs))
  "Replacements of word with single symbols that work through `prettify-symbols-mode'.")

(defconst pretty-ligatures--unsafe-word-replacements
  (let* ((ligs
          '(("not"   . #xe133)
            ("or"    . #xe124)
            ("and"   . #xe125)
            ("error" . #xe127)
            ("all"       . #xe128)
            ("any"       . #xe129))))
    (--map (cons (car it) (string ?\t (cdr it) ?\t)) ligs))
  "Word replacements that are likely to conflict with general use of words, e.g.
in Haskell compilation output. So they're disabled by default.")

(defun python-highlight-disable-pretty-symbols? (&optional position)
  "Predicate that determines, if POSITION is eligible to be part (beginning) of
pretty symbol. Intended for use in `font-lock-keywords' and
`+python-pretty-symbols+'."
  (save-excursion
    (when position
      (goto-char position))
    (or (python-point-inside-string-or-comment?)
        (memq (get-text-property (point) 'face)
              '(font-lock-comment-face
                font-lock-string-face))
        (get-text-property (point) 'disable-pretty-symbols))))

(defun pretty-ligatures--disable-pretty-symbols? (pos)
  "Predicate that determines, if point POS is eligible to be the beginning of
a pretty symbol."
  (or (memq (get-text-property pos 'face)
            '(font-lock-comment-face
              font-lock-string-face))
      (point-inside-string-or-comment? pos)
      ;; (get-text-property pos 'disable-pretty-symbols)
      ))

(defconst pretty-ligatures--special-haskell-ligatures
  (list
   (list
    (rx
     (or (any ?\s ?\() bol)
     (group-n 1 ".")
     (or (any ?\s ?\)) eol))
    '(0
      (unless (pretty-ligatures--disable-pretty-symbols? (match-beginning 0))
        (compose-region (match-beginning 1) (match-end 1) ?∘)
        nil)))
   (list
    (rx
     (or (any ?\s ?$)
         (syntax open-parenthesis)
         (syntax close-parenthesis)
         bol)
     (group-n 1 "\\")
     (or (any ?\s ?-)
         (syntax whitespace)
         (syntax open-parenthesis)
         (syntax close-parenthesis)
         (syntax word)
         eol))
    '(0
      (unless (pretty-ligatures--disable-pretty-symbols? (match-beginning 0))
        (compose-region (match-beginning 1) (match-end 1) ?λ)
        nil))))
  "Special ligature-like symbol replacements that won't fit into constraints
of `prettify-symbols-mode'. For example, some replacements must take context
into accound and do the replacement only within specific circumstances.")

(defun pretty-ligatures--install (ligatures)
  "Add hasklig ligatures for use with prettify-symbols-mode."
  (setq-local prettify-symbols-alist
              (append ligatures
                      prettify-symbols-alist))
  (setq-local prettify-symbols-unprettify-at-point t)
  (prettify-symbols-mode))

;;;###autoload
(defun pretty-ligatures-install! ()
  "Add pretty ligatures for use with `prettify-symbols-mode'."
  (pretty-ligatures--install
   (append pretty-ligatures--symbol-replacements
           pretty-ligatures--word-replacements
           pretty-ligatures--unsafe-word-replacements)))

;;;###autoload
(defun pretty-ligatures-install-safe! ()
  "Install safe subset of pretty ligatures to use with `prettify-symbols-mode'.
This subset in unlikely to conflict with general uses of words and should be
safe to use on any kind of text."
  (pretty-ligatures--install
   (append pretty-ligatures--symbol-replacements
           pretty-ligatures--word-replacements)))

;;;###autoload
(defun pretty-ligatures-install-special-haskell-ligatures! ()
  (font-lock-add-keywords
   nil
   pretty-ligatures--special-haskell-ligatures))

(provide 'pretty-ligatures)

;; Local Variables:
;; End:

;; pretty-ligatures.el ends here
