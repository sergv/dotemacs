;; pretty-ligatures.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 April 2018
;; Description:
;; Inspired by https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md.

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

(defconst pretty-ligatures--glyph-widths
  (eval-when-compile
    (alist->hash-table
     '((#xe100 . 2) ;; "<-"
       (#xe101 . 2) ;; "->"
       (#xe102 . 2) ;; "<=", left short double arrow, not used much since clashes with less-than-or-equal
       (#xe103 . 2) ;; "=>"
       (#xe104 . 3) ;; "<->"
       (#xe105 . 3) ;; "<=>"
       (#xe106 . 2) ;; "=="
       (#xe107 . 2) ;; "/="
       (#xe108 . 2) ;; "::"
       (#xe109 . 3) ;; "<<-"
       (#xe10a . 3) ;; "->>"
       (#xe10b . 3) ;; "<-<"
       (#xe10c . 3) ;; ">->"
       (#xe10d . 2) ;; "++"
       (#xe10e . 3) ;; "+++"
       (#xe10f . 2) ;; "<>"
       (#xe110 . 2) ;; "><"
       (#xe111 . 2) ;; "<<"
       (#xe112 . 2) ;; ">>"
       (#xe113 . 2) ;; "<|"
       (#xe114 . 2) ;; "|>"

       (#xe115 . 2) ;; "##"
       (#xe116 . 3) ;; "###"
       (#xe117 . 4) ;; "####"

       (#xe118 . 3) ;; "<--"
       (#xe119 . 3) ;; "-->"
       (#xe11a . 3) ;; "<=="
       (#xe11b . 3) ;; "==>"

       (#xe11c . 3) ;; "=<<"
       (#xe11d . 3) ;; ">>="
       (#xe11e . 3) ;; "<=<"
       (#xe11f . 3) ;; ">=>"
       (#xe120 . 3) ;; "<<="
       (#xe121 . 3) ;; "=>>"

       (#xe122 . 2) ;; "<="
       (#xe123 . 2) ;; ">="
       (#xe124 . 2) ;; "||"
       (#xe125 . 2) ;; "&&"

       (#xe130 . 2) ;; union
       (#xe131 . 2) ;; intersection
       (#xe12b . 2) ;; elem, member
       (#xe12c . 2) ;; notElem, notMember
       (#xe12f . 2) ;; isSubsetOf
       ;; (#xe12f . t) ;; isSubsetOf, 2 but glyph is broken
       (#xe12e . 2) ;; isProperSubsetOf
       (#xe12a . 2) ;; empty, mempty

       (#xe127 . 2) ;; error, undefined - bottom
       (#xe128 . 2) ;; forall, all
       (#xe129 . 2) ;; exists, any
       (#xe133 . 2) ;; not

       (#xe12d . 2) ;; sum
       (#xe132 . 2) ;; product
       ))))

;; Make [?\s (Bl . Br) ?\s (Bl . Br) ?\s (Bc . Bc) #xe11d] out of #xe11d (">>=").
(defun pretty-ligatures--make-composition (c)
  (if-let ((width (gethash c pretty-ligatures--glyph-widths)))
      (if (eq width t)
          (string ?\t c ?\t)
        (vconcat
         (apply #'vconcat [?\s] (-repeat (1- width) [(Bl . Br) ?\s]))
         (vector '(Bc . Bc) c)))
    (error "No width for character '%s'" c)))

;; ‘>>’ shows up in generic functions in addition to being a shift operator, thus it’s removed.
;; For consistency ‘<<’ is removed as well.
(defconst pretty-ligatures-rust-symbols
  (eval-when-compile
    (let* ((ligs
            '(("<-" . #xe100)
              ("->" . #xe101)
              ("=>" . #xe103)
              ("==" . #xe106)
              ("!=" . #xe107)
              ("<=" . #xe122)
              (">=" . #xe123)
              ("||" . #xe124)
              ("&&" . #xe125)
              ("::" . #xe108))))
      (--map (cons (car it) (pretty-ligatures--make-composition (cdr it))) ligs))))

(defconst pretty-ligatures-c-like-symbols
  (eval-when-compile
    (let* ((ligs
            '(("<<" . #xe111)
              (">>" . #xe112))))
      (append pretty-ligatures-rust-symbols
              (--map (cons (car it) (pretty-ligatures--make-composition (cdr it))) ligs)))))

(defconst pretty-ligatures-python-like-words
  (eval-when-compile
    (let ((ligs
           '(("for" . #xe128)
             ("in"  . #xe12b))))
      (--map (cons (car it) (pretty-ligatures--make-composition (cdr it))) ligs)))
  "Replacements of word with single symbols that work through `prettify-symbols-mode'.")

(defconst pretty-ligatures--symbol-replacements
  (eval-when-compile
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

              ("<--" . #xe118)
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
      (--map (cons (car it) (pretty-ligatures--make-composition (cdr it))) ligs)))
  "Symbolic ligatures that work through `prettify-symbols-mode'.")

(defconst pretty-ligatures--word-replacements
  (eval-when-compile
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

               ;; a 𝒶 𝒷 𝒸 𝒹 ℯaL 𝒻 ℊ 𝒽 𝒾 𝒿 𝓀 𝓁 𝓂 𝓃 ℴ 𝓅 𝓆 𝓇 𝓈 𝓉 𝓊 𝓋 𝓌 𝓍 𝓎 𝓏

               ;; bottom
               ("undefined" . #xe127)

               ;; ("mappend" . #xe10f)
               ;; ("`mappend`" . #xe10f)
               ("forall"    . #xe128)

               ("[]"        . #xe12a)
               ("mempty"    . #xe12a)

               ("sum"       . #xe12d)
               ("product"   . #xe132)))))
      (--map (cons (car it) (pretty-ligatures--make-composition (cdr it))) ligs)))
  "Replacements of word with single symbols that work through `prettify-symbols-mode'.")

(defconst pretty-ligatures--unsafe-word-replacements
  (eval-when-compile
    (let* ((ligs
            '(("not"   . #xe133)
              ("or"    . #xe124)
              ("and"   . #xe125)
              ("error" . #xe127)
              ("all"   . #xe128)
              ("any"   . #xe129))))
      (--map (cons (car it) (pretty-ligatures--make-composition (cdr it))) ligs)))
  "Word replacements that are likely to conflict with general use of words, e.g.
in Haskell compilation output. So they're disabled by default.")

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
  (eval-when-compile
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
       (or (any ?\s ?- ?_)
           (syntax whitespace)
           (syntax open-parenthesis)
           (syntax close-parenthesis)
           (syntax word)
           eol))
      '(0
        (unless (pretty-ligatures--disable-pretty-symbols? (match-beginning 0))
          (compose-region (match-beginning 1) (match-end 1) ?λ)
          nil)))))
  "Special ligature-like symbol replacements that won't fit into constraints
of `prettify-symbols-mode'. For example, some replacements must take context
into accound and do the replacement only within specific circumstances.")

(defun pretty-ligatures-supported? ()
  (and (bound-and-true-p current-font)
       (string-match-p "Iosevka Slab Lig" current-font)))

(defun pretty-ligatures--install (ligatures)
  "Add hasklig ligatures for use with prettify-symbols-mode."
  (when (pretty-ligatures-supported?)
    (setq-local prettify-symbols-alist
                (append ligatures
                        prettify-symbols-alist)
                prettify-symbols-unprettify-at-point t
                prettify-symbols-compose-predicate #'pretty-ligatures--compose-p)
    (prettify-symbols-mode)))

(defun pretty-ligatures--compose-p (start end _match)
  "Do not prettify withing strings, comments or within words/operators."
  (let* ((start-char (char-after start))
         (end-char (char-before end))
         (syntaxes-beg (if (memq (char-syntax start-char) '(?w ?_))
                           '(?w ?_) '(?. ?\\)))
         (syntaxes-end (if (memq (char-syntax end-char) '(?w ?_))
                           '(?w ?_) '(?. ?\\)))
         (following-char (or (char-after end) ?\s)))
    (and (not (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg))
         (or (char-equal following-char ?,)
             (not (memq (char-syntax following-char) syntaxes-end)))
         (not (nth 8 (syntax-ppss))))))

;;;###autoload
(defun pretty-ligatures-install! ()
  "Add pretty ligatures for use with `prettify-symbols-mode'."
  (pretty-ligatures--install
   (append pretty-ligatures--symbol-replacements
           pretty-ligatures--word-replacements
           pretty-ligatures--unsafe-word-replacements))

  ;; ;; This is how proper ligatures may be enabled, left for reference
  ;; (let ((ligatures `((?-  . ,(rx (or "->"))))))
  ;;   (dolist (entry ligatures)
  ;;     (set-char-table-range composition-function-table (car entry)
  ;;                           `([,(cdr entry) 0 font-shape-gstring]))))
  )

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
  (when (pretty-ligatures-supported?)
   (font-lock-add-keywords
    nil
    pretty-ligatures--special-haskell-ligatures)))

(provide 'pretty-ligatures)

;; Local Variables:
;; End:

;; pretty-ligatures.el ends here
