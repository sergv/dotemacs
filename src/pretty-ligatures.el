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
  "Hash table that caches content of ‘prettify-symbols-alist’.")

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
       (#xe134 . 2) ;; linear lollipop, -o
       ))))

;; Make [?\s (Bl . Br) ?\s (Bl . Br) ?\s (Bc . Bc) #xe11d] out of #xe11d (">>=").
(defun pretty-ligatures--make-composition (c &optional override-width)
  (if-let ((glyph-width (gethash c pretty-ligatures--glyph-widths)))
      (let ((width (or override-width glyph-width)))
        (if (eq width t)
            ;; No width
            (string ?\t c ?\t)
          (vconcat
           (apply #'vconcat [?\s] (-repeat (1- width) [(Br . Bl) ?\s]))
           (vector (if (eq width glyph-width)
                       '(Bc . Bc) ;; Put c’s center in the center of the previously composed whitespace
                     '(Bl . Bl))
                   c))))
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
      (--map (cons (car it) (pretty-ligatures--make-composition (cdr it) (length (car it)))) ligs)))
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

              ("%1 ->" . #xe134)

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
                     (mapcan (lambda (f)
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
               ;;   a b c d e f g

               ;; bottom
               ("undefined" . #xe127)

               ;; ("mappend" . #xe10f)
               ;; ("`mappend`" . #xe10f)
               ("forall"    . #xe128)

               ("[]"        . #xe12a)
               ("mempty"    . #xe12a)

               ("sum"       . #xe12d)
               ("product"   . #xe132)))))
      (--map (cons (car it) (pretty-ligatures--make-composition (cdr it) (length (car it)))) ligs)))
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
      (--map (cons (car it) (pretty-ligatures--make-composition (cdr it) (length (car it)))) ligs)))
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
  (unless (pretty-ligatures--disable-pretty-symbols? (match-beginning 0))
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
       (string-match-p "Iosevka Slab Lig" current-font)))

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
