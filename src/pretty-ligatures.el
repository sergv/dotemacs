;; pretty-ligatures.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 April 2018
;; Description:
;; Inspired by https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md.

(eval-when-compile
  (require 'dash))

(require 'el-patch)
(require 'treesit-utils)

(el-patch-feature prog-mode)

(el-patch-defun prettify-symbols--post-command-hook ()
  (cl-labels ((get-prop-as-list
               (prop)
               (remove nil
                       (list (get-text-property (point) prop)
                             (when (and (eq prettify-symbols-unprettify-at-point 'right-edge)
                                        (not (bobp)))
                               (get-text-property (1- (point)) prop))))))
    ;; Re-apply prettification to the previous symbol.
    (when (and prettify-symbols--current-symbol-bounds
	       (or (< (point) (car prettify-symbols--current-symbol-bounds))
		   (> (point) (el-patch-swap (cadr prettify-symbols--current-symbol-bounds)
                                             (cdr prettify-symbols--current-symbol-bounds)))
		   (and (not (eq prettify-symbols-unprettify-at-point 'right-edge))
			(= (point) (el-patch-swap (cadr prettify-symbols--current-symbol-bounds)
                                                  (cdr prettify-symbols--current-symbol-bounds))))))
      (el-patch-swap (apply #'font-lock-flush prettify-symbols--current-symbol-bounds)
                     (font-lock-flush (car prettify-symbols--current-symbol-bounds) (cdr prettify-symbols--current-symbol-bounds)))
      (setq prettify-symbols--current-symbol-bounds nil))
    ;; Unprettify the current symbol.
    (when-let* ((c (get-prop-as-list 'composition))
	        (s (get-prop-as-list 'prettify-symbols-start))
	        (e (get-prop-as-list 'prettify-symbols-end))
	        (s (apply #'min s))
	        (e (apply #'max e)))
      (with-silent-modifications
	(setq prettify-symbols--current-symbol-bounds (el-patch-swap (list s e) (cons s e)))
        (remove-text-properties s e '(composition nil))))))

(defvar-local prettify-symbols--alist-cache nil
  "Hash table that caches content of ‘prettify-symbols-alist’ to speed up ‘prettify-symbols--compose-symbol’.")

(el-patch-defun prettify-symbols--compose-symbol (alist)
  "Compose a sequence of characters into a symbol.
Regexp match data 0 specifies the characters to be composed."
  ;; Check that the chars should really be composed into a symbol.
  (let ((start (match-beginning 0))
        (end (match-end 0))
        (match (match-string 0)))
    (if (and (not (el-patch-swap (equal prettify-symbols--current-symbol-bounds (list start end))
                                 (and (eq (car prettify-symbols--current-symbol-bounds) start)
                                      (eq (cdr prettify-symbols--current-symbol-bounds) end))))
             (funcall prettify-symbols-compose-predicate start end match))
        ;; That's a symbol alright, so add the composition.
        (with-silent-modifications
          (compose-region start end (el-patch-swap (cdr (assoc match alist))
                                                   (gethash match prettify-symbols--alist-cache)))
          (add-text-properties
           start end
           `(prettify-symbols-start ,start prettify-symbols-end ,end)))
      ;; No composition for you.  Let's actually remove any
      ;; composition we may have added earlier and which is now
      ;; incorrect.
      (remove-list-of-text-properties start end
                                      '(composition
                                        prettify-symbols-start
                                        prettify-symbols-end))))
  ;; Return nil because we're not adding any face property.
  nil)

(el-patch-defun prettify-symbols--make-keywords ()
  (if prettify-symbols-alist
      (el-patch-swap
        `((,(regexp-opt (mapcar 'car prettify-symbols-alist) t)
           (0 (prettify-symbols--compose-symbol ',prettify-symbols-alist))))
        (progn
          (setq-local prettify-symbols--alist-cache
                      (alist->hash-table prettify-symbols-alist #'equal))
          `((,(regexp-opt (mapcar 'car prettify-symbols-alist) t)
             (0 (prettify-symbols--compose-symbol nil))))))
    nil))

;;;###autoload
(defun prettify-symbols-decompose-region (start end)
  "Remove compositions in the region from START to END."
  (with-silent-modifications
    (remove-text-properties start end
                            '(composition nil))))

;; (set-fontset-font t '(#Xe100 . #Xe115) "Iosevka Slab Lig")

(defstruct ligature-glyph
  symbol ;; character, >= #xe100
  width  ;; integer
  )

(defconst iosevka-slab-lig-wide-unicode-glyphs
  (eval-when-compile
    (alist->hash-table
     (mapcar
      (lambda (x) (cons (cl-first x) (make-ligature-glyph :symbol (cl-first x) :width (cl-second x))))
      '((?⋁ 2)
        (?⋁ 2)

        ;; Check font: ?⨆, ?⨅, ?⨉
        ;; Good: ?⨁, ?⨂, ?⨀
        (?⋃ 2) ;; union
        (?⋂ 2) ;; intersection
        (?⨆ 2) ;; square union
        (?⨅ 2) ;; square intersection
        (?⨉ 2)
        (?∑ 2)
        (?∏ 2)
        (?∐ 2)
        (?⋀ 2)
        (?⋁ 2))))))

(defconst iosevka-slab-lig-glyphs
  (eval-when-compile
    (alist->hash-table
     (mapcar
      (lambda (x) (cons (cl-first x) (make-ligature-glyph :symbol (cl-second x) :width (cl-third x))))
      '(("<-"   #xe100 2) ;; "<-"
        ("->"   #xe101 2) ;; "->"
        ("<="   #xe102 2) ;; "<=", left short double arrow, not used much it since clashes with less-than-or-equal
        ("=>"   #xe103 2) ;; "=>"
        ("<->"  #xe104 3) ;; "<->"
        ("<=>"  #xe105 3) ;; "<=>"
        ("=="   #xe106 2) ;; "=="
        ("/="   #xe107 2) ;; "/="
        ("::"   #xe108 2) ;; "::"
        ("<<-"  #xe109 3) ;; "<<-"
        ("->>"  #xe10a 3) ;; "->>"
        ("<-<"  #xe10b 3) ;; "<-<"
        (">->"  #xe10c 3) ;; ">->"
        ("++"   #xe10d 2) ;; "++"
        ("+++"  #xe10e 3) ;; "+++"
        ("<>"   #xe10f 2) ;; "<>"
        ("><"   #xe110 2) ;; "><"
        ("<<"   #xe111 2) ;; "<<"
        (">>"   #xe112 2) ;; ">>"
        ("<|"   #xe113 2) ;; "<|"
        ("|>"   #xe114 2) ;; "|>"

        ("##"   #xe115 2) ;; "##"
        ("###"  #xe116 3) ;; "###"
        ("####" #xe117 4) ;; "####"

        ("<--"  #xe118 3) ;; "<--"
        ("-->"  #xe119 3) ;; "-->"
        ("<=="  #xe11a 3) ;; "<=="
        ("==>"  #xe11b 3) ;; "==>"

        ("=<<"  #xe11c 3) ;; "=<<"
        (">>="  #xe11d 3) ;; ">>="
        ("<=<"  #xe11e 3) ;; "<=<"
        (">=>"  #xe11f 3) ;; ">=>"
        ("<<="  #xe120 3) ;; "<<="
        ("=>>"  #xe121 3) ;; "=>>"

        ("LE"   #xe122 2) ;; "<=", but has width of 2 as opposed to ?≤
        ("GE"   #xe123 2) ;; ">=", but has width of 2 as opposed to ?≥
        ("||"   #xe124 2) ;; "||", but is taller than ?⋁
        ("&&"   #xe125 2) ;; "&&", but is taller than ?⋀

        ("elem"             #xe12b 2) ;; elem, member
        ("notElem"          #xe12c 2) ;; notElem, notMember
        ("isSubsetOf"       #xe12f 2) ;; isSubsetOf
        ;; ("isSubsetOf"       #xe12f t) ;; isSubsetOf, 2 but glyph is broken
        ("isProperSubsetOf" #xe12e 2) ;; isProperSubsetOf
        ("emptySet"         #xe12a 2) ;; empty, mempty

        ("bottom"  #xe127 2) ;; error, undefined - bottom
        ("forall"  #xe128 2) ;; forall, all
        ("exists"  #xe129 2) ;; exists, any
        ("nexists" #xe136 2)
        ("not"     #xe133 2) ;; not

        ("-o"      #xe134 2) ;; linear lollipop, -o

        ;; These are less elegant and probably should never be used
        ;; ("union"        #xe130 2) ;; union, but less elegant than ?⋃
        ;; ("intersection" #xe131 2) ;; intersection, but less elegant than ?⋂
        ;; ("sum"          #xe12d 2) ;; sum, but less elegant than ?∑ character
        ;; ("product"      #xe132 2) ;; product, but less elegant than ?∏ character
        ;; ("coproduct"    #xe135 2) ;; coproduct, but less elegant than ?∐ character
        )))))

;; Make [?\s (Bl . Br) ?\s (Bl . Br) ?\s (Bc . Bc) #xe11d] out of #xe11d (">>=").
(defun pretty-ligatures--make-glyph-composition (g &optional override-width)
  "G must denote one of ‘iosevka-slab-lig-glyphs’ glyphs."
  (cl-assert (or (stringp g) (characterp g)))
  (cl-assert (or (gethash g iosevka-slab-lig-wide-unicode-glyphs)
                 (gethash g iosevka-slab-lig-glyphs))
             nil
             "Glyph not found: %s"
             (if (characterp g)
                 (format "?%c (%s)" g g)
               g))
  (if-let* ((glyph (if (characterp g)
                       (gethash g iosevka-slab-lig-wide-unicode-glyphs)
                     (gethash g iosevka-slab-lig-glyphs)))
            (c (ligature-glyph-symbol glyph))
            (glyph-width (ligature-glyph-width glyph))
            (width (or override-width
                       glyph-width)))
      (if (eq width t)
          ;; No width
          (string ?\t c ?\t)
        (vconcat
         (apply #'vconcat [?\s] (-repeat (1- width) [(Br . Bl) ?\s]))
         (vector (if (eq glyph-width width)
                     '(Bc . Bc) ;; Put c’s center in the center of the previously composed whitespace
                   '(Bl . Bl))
                 c)))
    (error "No width for glyph ‘%s’" g)))

(defun pretty-ligatures--make-literal-composition (symbol &optional override-width)
  (cl-assert (characterp symbol))
  (let ((width (or override-width 1)))
    (if (eq width t)
        ;; No width
        (string ?\t symbol ?\t)
      (vconcat
       (apply #'vconcat [?\s] (-repeat (1- width) [(Br . Bl) ?\s]))
       (vector (if (eq 1 width)
                   '(Bc . Bc) ;; Put c’s center in the center of the previously composed whitespace
                 '(Bl . Bl))
               symbol)))))

;; ‘>>’ shows up in generic functions in addition to being a shift operator, thus it’s removed.
;; For consistency ‘<<’ is removed as well.
(defconst pretty-ligatures-rust-symbols
  (eval-when-compile
    (let* ((ligs
            '(("<-" . "<-")
              ("->" . "->")
              ("=>" . "=>")
              ("==" . "==")
              ("!=" . "/=")
              ("<=" . "LE")
              (">=" . "GE")
              ("||" . "||")
              ("&&" . "&&")
              ("::" . "::"))))
      (--map (cons (car it) (pretty-ligatures--make-glyph-composition (cdr it))) ligs))))

(defconst pretty-ligatures-c-like-symbols
  (eval-when-compile
    (let* ((ligs
            '(("<<" . "<<")
              (">>" . ">>"))))
      (append pretty-ligatures-rust-symbols
              (--map (cons (car it) (pretty-ligatures--make-glyph-composition (cdr it))) ligs)))))

(defconst pretty-ligatures-python-like-words
  (eval-when-compile
    (let ((ligs
           '(("for" . "forall")
             ("in"  . "elem"))))
      (--map (cons (car it) (pretty-ligatures--make-glyph-composition (cdr it) (length (car it)))) ligs)))
  "Replacements of word with single symbols that work through `prettify-symbols-mode'.")

(defconst pretty-ligatures--symbol-replacements
  (eval-when-compile
    (let* ((ligs
            '(("<-"  . "<-")
              ("->"  . "->")

              ;; ("<="  . #xe102)
              ("=>"  . "=>")
              ("<->" . "<->")
              ("<=>" . "<=>")
              ("=="  . "==")
              ("/="  . "/=")
              ("::"  . "::")
              ("<<-" . "<<-")
              ("->>" . "-->")
              ("<-<" . "<-<")
              (">->" . ">->")
              ("++"  . "++")
              ("+++" . "+++")
              ("<>"  . "<>")
              ("><"  . "><")
              ("<<"  . "<<")
              (">>"  . ">>")
              ("<|"  . "<|")
              ("|>"  . "|>")

              ("##"   . "##")
              ("###"  . "###")
              ("####" . "####")

              ("<--" . "<--")
              ("-->" . "-->")
              ("<==" . "<==")
              ("==>" . "==>")

              ("=<<" . "=<<")
              (">>=" . ">>=")
              ("<=<" . "<=<")
              (">=>" . ">=>")
              ("<<=" . "<<=")
              ("=>>" . "=>>")

              ("<="  . "LE")
              (">="  . "GE")
              ("||"  . ?⋁)
              ("&&"  . ?⋀)

              ("%1 ->" . "-o")

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
      (--map (cons (car it) (pretty-ligatures--make-glyph-composition (cdr it))) ligs)))
  "Symbolic ligatures that work through `prettify-symbols-mode'.")

(defconst pretty-ligatures--word-replacements
  (eval-when-compile
    (let* ((make-combinations
            (lambda (func-name module-prefixes code-point)
              (--map (cons it code-point)
                     (mapcan (lambda (f)
                               (cons (funcall f func-name)
                                     (--map (funcall f (concat it "." func-name)) module-prefixes)))
                             (list #'identity (lambda (x) (concat "`" x "`")))))))
           (standard-prefixes
            '("M" "Map" "S" "Set" "HM" "HashMap" "HS" "HashSet" "IS" "IntSet" "IM" "IntMap"))
           (ligs
            (append
             (funcall make-combinations "mappend" nil "<>")
             (funcall make-combinations "union" standard-prefixes ?⋃)
             (funcall make-combinations "intersection" standard-prefixes ?⋂)
             (funcall make-combinations "elem" nil "elem")
             (funcall make-combinations "member" standard-prefixes "elem")
             (funcall make-combinations "notElem" nil "notElem")
             (funcall make-combinations "notMember" standard-prefixes "notElem")

             ;; possibly equal
             (funcall make-combinations "isSubsetOf" standard-prefixes "isSubsetOf")
             ;; not-equal
             (funcall make-combinations "isProperSubsetOf" standard-prefixes "isProperSubsetOf")

             (funcall make-combinations "empty" (append '("V" "Vector") standard-prefixes) "emptySet")

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
               ;;   a b c d e f g

               ;; bottom
               ("undefined" . "bottom")

               ;; ("mappend" . "<>")
               ;; ("`mappend`" . "<>")
               ("forall"    . "forall")

               ("[]"        . "emptySet")
               ("mempty"    . "emptySet")

               ("sum"       . ?∑)
               ("product"   . ?∏)
               ("coproduct" . ?∐)))))

      (--map (cons (car it) (pretty-ligatures--make-glyph-composition (cdr it) (length (car it)))) ligs)))
  "Replacements of word with single symbols that work through `prettify-symbols-mode'.")

(defconst pretty-ligatures--unsafe-word-replacements
  (eval-when-compile
    (let* ((ligs
            '(("not"   . "not")
              ("or"    . ?⋁)
              ("and"   . ?⋀)
              ("error" . "bottom")
              ("all"   . "forall")
              ("any"   . "exists"))))
      (--map (cons (car it)
                   (pretty-ligatures--make-glyph-composition
                    (cdr it)
                    ;; Make sure e.g. ‘not’ ligature accupies 3 characters even though
                    ;; its ligature width is only 2 characters wide.
                    (length (car it))))
             ligs)))
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

(defun pretty-ligatures--compose-dot ()
  (when (and (not (pretty-ligatures--disable-pretty-symbols? (match-beginning 0)))
             (if (derived-mode-p 'haskell-ts-mode)
                 (if-let ((node (treesit-haskell--node-at (match-beginning 1))))
                     (not (and (equal (treesit-node-type node) ".")
                               (when-let ((p (treesit-node-parent node)))
                                 (equal (treesit-node-type p) "forall"))))
                   t)
               t))
    (with-silent-modifications
      (compose-region (match-beginning 1) (match-end 1) ?∘))
    nil))

(defun pretty-ligatures--compose-lambda ()
  (unless (pretty-ligatures--disable-pretty-symbols? (match-beginning 0))
    (with-silent-modifications
      (compose-region (match-beginning 1) (match-end 1) ?λ))
    nil))

(defconst pretty-ligatures--special-haskell-ligatures
  (eval-when-compile
    (list
     (list
      (rx
       (or (any ?\s ?\() bol)
       (group-n 1 ".")
       (or (any ?\s ?\)) eol))
      '(0
        (pretty-ligatures--compose-dot)))
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
        (pretty-ligatures--compose-lambda)))))
  "Special ligature-like symbol replacements that won't fit into constraints
of `prettify-symbols-mode'. For example, some replacements must take context
into accound and do the replacement only within specific circumstances.")

(defun pretty-ligatures-supported? ()
  (and (bound-and-true-p current-font)
       (cond
         ((stringp current-font)
          (string-match-p "Iosevka Slab Lig" current-font))
         ((fontp current-font)
          (string-match-p "Iosevka Slab Lig" (font-get current-font :name)))
         (t
          (error "Invalid current font: %s" current-font)))))

(setq-default prettify-symbols-unprettify-at-point t)

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
         ;; Not in string or comment.
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
