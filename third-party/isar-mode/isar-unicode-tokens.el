;;; -*- coding: utf-8; lexical-binding: t; -*-
;; isar-unicode-tokens.el --- Tokens for Unicode Tokens package
;;
;; Copyright(C) 2008, 2009 David Aspinall / LFCS Edinburgh
;; Author:    David Aspinall <David.Aspinall@ed.ac.uk>
;;
;; This file is loaded by proof-auxmodes.el for proof-unicode-tokens.el.
;;
;; It sets the variables defined at the top of unicode-tokens.el,
;; unicode-tokens-<foo> is set from isar-<foo>.  See the corresponding
;; variable for documentation.
;;

(require 'unicode-tokens)
(declare-function isar-markup-ml "isar")

;;
;; Customization
;;

(defgroup isabelle-tokens nil
  "Variables which configure Isabelle tokens for Unicode Tokens mode."
  :group 'isabelle
  :prefix "isar-")

(defun isar-set-and-restart-tokens (sym val)
  "Function to restart Unicode Tokens when a token value is adjusted."
  (set-default sym val)
  ;; (when (featurep 'isar-unicode-tokens) ; not during loading
  ;;   (isar-init-token-symbol-map)
  ;;   (isar-init-shortcut-alists)
  ;;   (if (featurep 'unicode-tokens)
  ;;       (unicode-tokens-initialise)))
  )

;;
;; Controls
;;

(defconst isar-control-region-format-regexp
  "\\(\\\\<\\^%s>\\)\\(.*?\\)\\(\\\\<\\^%s>\\)")

(defconst isar-control-char-format-regexp
  (concat
   "\\(\\\\<\\^%s>\\)\\("
   "\\(?:\\\\<[A-Za-z]+>\\|[^\\]\\)" ; cf isar-ext-first
   "\\)"))

(defconst isar-control-char-format         "\\<^%s>")
(defconst isar-control-region-format-start "\\<^%s>")
(defconst isar-control-region-format-end   "\\<^%s>")


(defcustom isar-control-characters
  '(("Subscript" "sub" sub)
    ("Id subscript" "isub" sub)
    ("Superscript" "sup" sup)
    ("Id superscript" "isup" sup)
    ("Loc" "loc" keyword)
    ("Constant" "const" keyword)
    ("Bold" "bold" bold)
    ;; unofficial/unsupported:
    ("Italic" "italic" italic))
  "Control character tokens for Isabelle."
  :group 'isabelle-tokens
  :type '(list string string symbol)
  :set 'isar-set-and-restart-tokens)

(defcustom isar-control-regions
  '(("Subscript" "bsub" "esub" sub)
    ("Superscript" "bsup" "esup" sup)
    ;; unofficial/unsupported:
    ("Id subscript" "bisub" "eisub" sub)
    ("Id superscript" "bisup" "eisup" sup)
    ("Bold" "bbold" "ebold" bold)
    ("Italic" "bitalic" "eitalic" italic)
    ("Script" "bscript" "escript" script)
    ("Frakt" "bfrakt" "efrakt" frakt)
    ("Roman" "bserif" "eserif" serif)
    ("Sans" "bsans" "esans" sans)
    ("Overline" "boverline" "eoverline" overline)
    ("Underline" "bunderline" "eunderline" underline)
    ("Big"   "bbig" "ebig" big)
    ("Small" "bsmall" "esmall" small)
;    ("Large symbols" "bbigsyms" "ebigsyms" large-symbols)
    )
  "Control sequence tokens for Isabelle."
  :group 'isabelle-tokens
  :type '(list string string symbol)
  :set 'isar-set-and-restart-tokens)

;;
;; Symbols
;;

(defconst isar-token-format "\\<%s>")

;(defconst isar-token-variant-format-regexp
;  "\\\\<\\(%s\\)\\([:][a-zA-Z0-9]+\\)?>") ; syntax change required
(defconst isar-token-variant-format-regexp
  "\\\\<\\(%s\\)[0-9]*>") ; unofficial interpretation of usual syntax

(defcustom isar-greek-letters-tokens
  '(("alpha" "α")
    ("beta" "β")
    ("gamma" "γ")
    ("delta" "δ")
    ("epsilon" "ε") ; varepsilon (some is epsilon), although PG can use dups
    ("zeta" "ζ")
    ("eta" "η")
    ("theta" "θ")
    ("iota" "ι")
    ("kappa" "κ")
    ("lambda" "λ")
    ("mu" "μ")
    ("nu" "ν")
    ("xi" "ξ")
    ("pi" "π")
    ("rho" "ρ")
    ("sigma" "σ")
    ("tau" "τ")
    ("upsilon" "υ")
    ("phi" "φ")
    ("chi" "χ")
    ("psi" "ψ")
    ("omega" "ω")
    ("Gamma" "Γ")
    ("Delta" "Δ")
    ("Theta" "Θ")
    ("Lambda" "Λ")
    ("Xi" "Ξ")
    ("Pi" "Π")
    ("Sigma" "Σ")
    ("Upsilon" "Υ")
    ("Phi" "Φ")
    ("Psi" "Ψ")
    ("Omega" "Ω"))
  "Greek letter token map for Isabelle."
  :group 'isabelle-tokens
  :set 'isar-set-and-restart-tokens)

(defcustom isar-misc-letters-tokens
  '(("bool" "B" bold underline)
    ("complex" "ℂ")
    ("nat" "ℕ")
    ("rat" "ℚ")
    ("real" "ℝ")
    ("int" "ℤ"))
  "Miscellaneous letter token map for Isabelle."
  :group 'isabelle-tokens
  :set 'isar-set-and-restart-tokens)

(defcustom isar-symbols-tokens
  '(("A" "𝒜")
    ("B" "ℬ")
    ("C" "𝒞")
    ("D" "𝒟")
    ("E" "ℰ")
    ("F" "ℱ")
    ("G" "𝒢")
    ("H" "ℋ")
    ("I" "ℐ")
    ("J" "𝒥")
    ("K" "𝒦")
    ("L" "ℒ")
    ("M" "ℳ")
    ("N" "𝒩")
    ("O" "𝒪")
    ("P" "𝒫")
    ("Q" "𝒬")
    ("R" "ℛ")
    ("S" "𝒮")
    ("T" "𝒯")
    ("U" "𝒰")
    ("V" "𝒱")
    ("W" "𝒲")
    ("X" "𝒳")
    ("Y" "𝒴")
    ("Z" "𝒵")
    ("a" "𝖺")
    ("b" "𝖻")
    ("c" "𝖼")
    ("d" "𝖽")
    ("e" "𝖾")
    ("f" "𝖿")
    ("g" "𝗀")
    ("h" "𝗁")
    ("i" "𝗂")
    ("j" "𝗃")
    ("k" "𝗄")
    ("l" "𝗅")
    ("m" "𝗆")
    ("n" "𝗇")
    ("o" "𝗈")
    ("p" "𝗉")
    ("q" "𝗊")
    ("r" "𝗋")
    ("s" "𝗌")
    ("t" "𝗍")
    ("u" "𝗎")
    ("v" "𝗏")
    ("w" "𝗐")
    ("x" "𝗑")
    ("y" "𝗒")
    ("z" "𝗓")
    ("AA" "𝔄")
    ("BB" "𝔅")
    ("CC" "ℭ")
    ("DD" "𝔇")
    ("EE" "𝔈")
    ("FF" "𝔉")
    ("GG" "𝔊")
    ("HH" "ℌ")
    ("II" "ℑ")
    ("JJ" "𝔍")
    ("KK" "𝔎")
    ("LL" "𝔏")
    ("MM" "𝔐")
    ("NN" "𝔑")
    ("OO" "𝔒")
    ("PP" "𝔓")
    ("QQ" "𝔔")
    ("RR" "ℜ")
    ("SS" "𝔖")
    ("TT" "𝔗")
    ("UU" "𝔘")
    ("VV" "𝔙")
    ("WW" "𝔚")
    ("XX" "𝔛")
    ("YY" "𝔜")
    ("ZZ" "ℨ")
    ("aa" "𝔞")
    ("bb" "𝔟")
    ("cc" "𝔠")
    ("dd" "𝔡")
    ("ee" "𝔢")
    ("ff" "𝔣")
    ("gg" "𝔤")
    ("hh" "𝔥")
    ("ii" "𝔦")
    ("jj" "𝔧")
    ("kk" "𝔨")
    ("ll" "𝔩")
    ("mm" "𝔪")
    ("nn" "𝔫")
    ("oo" "𝔬")
    ("pp" "𝔭")
    ("qq" "𝔮")
    ("rr" "𝔯")
    ("ss" "𝔰")
    ("tt" "𝔱")
    ("uu" "𝔲")
    ("vv" "𝔳")
    ("ww" "𝔴")
    ("xx" "𝔵")
    ("yy" "𝔶")
    ("zz" "𝔷")
    ("alpha" "α")
    ("beta" "β")
    ("gamma" "γ")
    ("delta" "δ")
    ("epsilon" "ε")
    ("zeta" "ζ")
    ("eta" "η")
    ("theta" "θ")
    ("iota" "ι")
    ("kappa" "κ")
    ("lambda" "λ")
    ("mu" "μ")
    ("nu" "ν")
    ("xi" "ξ")
    ("pi" "π")
    ("rho" "ρ")
    ("sigma" "σ")
    ("tau" "τ")
    ("upsilon" "υ")
    ("phi" "φ")
    ("chi" "χ")
    ("psi" "ψ")
    ("omega" "ω")
    ("Gamma" "Γ")
    ("Delta" "Δ")
    ("Theta" "Θ")
    ("Lambda" "Λ")
    ("Xi" "Ξ")
    ("Pi" "Π")
    ("Sigma" "Σ")
    ("Upsilon" "Υ")
    ("Phi" "Φ")
    ("Psi" "Ψ")
    ("Omega" "Ω")
    ("bool" "𝔹")
    ("complex" "ℂ")
    ("nat" "ℕ")
    ("rat" "ℚ")
    ("real" "ℝ")
    ("int" "ℤ")
    ("leftarrow" "←")
    ("longleftarrow" "⟵")
    ("longlongleftarrow" "⤎")
    ("longlonglongleftarrow" "⇠")
    ("rightarrow" "→")
    ("longrightarrow" "⟶")
    ("longlongrightarrow" "⤏")
    ("longlonglongrightarrow" "⇢")
    ("Leftarrow" "⇐")
    ("Longleftarrow" "⟸")
    ("Lleftarrow" "⇚")
    ("Rightarrow" "⇒")
    ("Longrightarrow" "⟹")
    ("Rrightarrow" "⇛")
    ("leftrightarrow" "↔")
    ("longleftrightarrow" "⟷")
    ("Leftrightarrow" "⇔")
    ("Longleftrightarrow" "⟺")
    ("mapsto" "↦")
    ("longmapsto" "⟼")

    ("midarrow" "─")
    ("Midarrow" "═")
    ("hookleftarrow" "↩")
    ("hookrightarrow" "↪")
    ("leftharpoondown" "↽")
    ("rightharpoondown" "⇁")
    ("leftharpoonup" "↼")
    ("rightharpoonup" "⇀")
    ("rightleftharpoons" "⇌")
    ("leadsto" "↝")
    ("downharpoonleft" "⇃")
    ("downharpoonright" "⇂")
    ("upharpoonleft" "↿")
    ("restriction" "↾")
    ("Colon" "∷")
    ("up" "↑")
    ("Up" "⇑")
    ("down" "↓")
    ("Down" "⇓")
    ("updown" "↕")
    ("Updown" "⇕")
    ("langle" "⟨")
    ("rangle" "⟩")
    ("lceil" "⌈")
    ("rceil" "⌉")
    ("lfloor" "⌊")
    ("rfloor" "⌋")
    ("lparr" "⦇")
    ("rparr" "⦈")
    ("lbrakk" "⟦")
    ("rbrakk" "⟧")
    ("lbrace" "⦃")
    ("rbrace" "⦄")
    ("guillemotleft" "«")
    ("guillemotright" "»")
    ("bottom" "⊥")
    ("top" "⊤")
    ("and" "∧")
    ("And" "⋀")
    ("or" "∨")
    ("Or" "⋁")
    ("forall" "∀")
    ("exists" "∃")
    ("nexists" "∄")
    ("not" "¬")
    ("circle" "○")
    ("box" "□")
    ("diamond" "◇")
    ("diamondop" "⋄")
    ("turnstile" "⊢")
    ("Turnstile" "⊨")
    ("tturnstile" "⊩")
    ("TTurnstile" "⊫")
    ("stileturn" "⊣")
    ("surd" "√")
    ("le" "≤")
    ("ge" "≥")
    ("lless" "≪")
    ("ggreater" "≫")
    ("lesssim" "≲")
    ("greatersim" "≳")
    ("lessapprox" "⪅")
    ("greaterapprox" "⪆")
    ("in" "∈")
    ("notin" "∉")
    ("subset" "⊂")
    ("supset" "⊃")
    ("subseteq" "⊆")
    ("supseteq" "⊇")
    ("sqsubset" "⊏")
    ("sqsupset" "⊐")
    ("sqsubseteq" "⊑")
    ("sqsupseteq" "⊒")
    ("inter" "∩")
    ("Inter" "⋂")
    ("union" "∪")
    ("Union" "⋃")
    ("squnion" "⊔")
    ("Squnion" "⨆")
    ("sqinter" "⊓")
    ("Sqinter" "⨅")
    ("setminus" "∖")
    ("propto" "∝")
    ("uplus" "⊎")
    ("Uplus" "⨄")
    ("noteq" "≠")
    ("sim" "∼")
    ("doteq" "≐")
    ("simeq" "≃")
    ("approx" "≈")
    ("asymp" "≍")
    ("cong" "≅")
    ("smile" "⌣")
    ("equiv" "≡")
    ("frown" "⌢")
    ("Join" "⋈")
    ("bowtie" "⨝")
    ("prec" "≺")
    ("succ" "≻")
    ("preceq" "≼")
    ("succeq" "≽")
    ("parallel" "∥")
    ("bar" "¦")
    ("plusminus" "±")
    ("minusplus" "∓")
    ("times" "×")
    ("div" "÷")
    ("cdot" "⋅")
    ("star" "⋆")
    ("bullet" "∙")
    ("circ" "∘")
    ("dagger" "†")
    ("ddagger" "‡")
    ("lhd" "⊲")
    ("rhd" "⊳")
    ("unlhd" "⊴")
    ("unrhd" "⊵")
    ("triangleleft" "◃")
    ("triangleright" "▹")
    ("triangle" "△")
    ("triangleq" "≜")
    ("oplus" "⊕")
    ("Oplus" "⨁")
    ("otimes" "⊗")
    ("Otimes" "⨂")
    ("odot" "⊙")
    ("Odot" "⨀")
    ("ominus" "⊖")
    ("oslash" "⊘")
    ("dots" "…")
    ("cdots" "⋯")
    ("Sum" "∑")
    ("Prod" "∏")
    ("Coprod" "∐")
    ("infinity" "∞")
    ("integral" "∫")
    ("ointegral" "∮")
    ("clubsuit" "♣")
    ("diamondsuit" "♢")
    ("heartsuit" "♡")
    ("spadesuit" "♠")
    ("aleph" "ℵ")
    ("emptyset" "∅")
    ("nabla" "∇")
    ("partial" "∂")
    ("flat" "♭")
    ("natural" "♮")
    ("sharp" "♯")
    ("angle" "∠")
    ("copyright" "©")
    ("registered" "®")
    ("hyphen" "‐")
    ("inverse" "¯")
    ("onequarter" "¼")
    ("onehalf" "½")
    ("threequarters" "¾")
    ("ordfeminine" "ª")
    ("ordmasculine" "º")
    ("section" "§")
    ("paragraph" "¶")
    ("exclamdown" "¡")
    ("questiondown" "¿")
    ("euro" "€")
    ("pounds" "£")
    ("yen" "¥")
    ("cent" "¢")
    ("currency" "¤")
    ("degree" "°")
    ("amalg" "⨿")
    ("mho" "℧")
    ("lozenge" "◊")
    ("wp" "℘")
    ("wrong" "≀")
    ("acute" "´")
    ("index" "ı")
    ("dieresis" "¨")
    ("cedilla" "¸")
    ("hungarumlaut" "˝")
    ("bind" "⤜")
    ("then" "⪢")
    ("some" "ϵ")
    ("hole" "⌑")
    ("newline" "⏎")
    ("comment" "―")
    ("^cancel" "⌦")
    ("open" "‹")
    ("close" "›")
    ("here" "⌂")
    ("undefined" "❖")
    ("noindent" "⇤")
    ("smallskip" "┈")
    ("medskip" "┉")
    ("bigskip" "━")
    ("^item" "▪")
    ("^enum" "▸")
    ("^descr" "➧")
    ("^footnote" "⁋")
    ("^verbatim" "▩")
    ("^theory_text" "⬚")
    ("^emph" "∗")
    ("^bold" "❙")
    ("^file" "🗏")
    ("^dir" "🗀")
    ("^term" "⬣")
    ("^typ" "⚛")
    ("^text" "🖹")
    ("^latex" "🖋")
    ("^url" "🌐")
    ("^doc" "📓")
    ("^action" "☛")
    )
  "Symbol token map for Isabelle.  The standard set of Isabelle symbols."
  :group 'isabelle-tokens
  :set 'isar-set-and-restart-tokens)

(defcustom isar-extended-symbols-tokens
  '(("stareq" "≛")
    ("defeq" "≝")
    ("questioneq" "≟")
    ("vartheta" "ϑ")
    ; ("o" "ø")
    ("varpi" "ϖ")
    ("varrho" "ϱ")
    ("varsigma" "ς")
    ("varphi" "ϕ")
    ("hbar" "ℏ")
    ("ell" "ℓ")
    ("ast" "∗")

    ("bigcirc" "◯")
    ("bigtriangleup" "△")
    ("bigtriangledown" "▽")
    ("ni" "∋")
    ("mid" "∣")
    ("notlt" "≮")
    ("notle" "≰")
    ("notprec" "⊀")
    ("notpreceq" "⋠")
    ("notsubset" "⊄")
    ("notsubseteq" "⊈")
    ("notsqsubseteq" "⋢")
    ("notgt" "≯")
    ("notge" "≱")
    ("notsucc" "⊁")
    ("notsucceq" "⋡")
    ("notsupset" "⊅")
    ("notsupseteq" "⊉")
    ("notsqsupseteq" "⋣")
    ("notequiv" "≢")
    ("notsim" "≁")
    ("notsimeq" "≄")
    ("notapprox" "≉")
    ("notcong" "≇")
    ("notasymp" "≭")
    ("nearrow" "↗")
    ("searrow" "↘")
    ("swarrow" "↙")
    ("nwarrow" "↖")
    ("vdots" "⋮")
    ("ddots" "⋱")
    ("closequote" "’")
    ("openquote" "‘")
    ("opendblquote" "”")
    ("closedblquote" "“")
    ("emdash" "—")
    ("prime" "′")
    ("doubleprime" "″")
    ("tripleprime" "‴")
    ("quadrupleprime" "⁗")
    ("nbspace" " ")
    ("thinspace" " ")
    ("notni" "∌")
    ("colonequals" "≔")
    ("foursuperior" "⁴")
    ("fivesuperior" "⁵")
    ("sixsuperior" "⁶")
    ("sevensuperior" "⁷")
    ("eightsuperior" "⁸")
    ("ninesuperior" "⁹"))
  "Extended symbols token map for Isabelle.  These are not defined standardly."
  :group 'isabelle-tokens
  :set 'isar-set-and-restart-tokens)

(defun isar-try-char (charset code1 code2)
  (and (charsetp charset) ; prevent error
       (char-to-string (make-char charset code1 code2))))

(defcustom isar-symbols-tokens-fallbacks
  `(;; Faked long symbols
    ("longleftarrow" "←-")
    ("Longleftarrow" "⇐–")
    ("longrightarrow" "–→")
    ("Longrightarrow" "–⇒")
    ("longleftrightarrow" "←→")
    ("Longleftrightarrow" "⇐⇒")
    ("longmapsto" "❘→")
    ;; bracket composition alternatives
    ("lparr" "(|")
    ("rparr" "|)")
    ;; an example of using characters from another charset.
    ;; to expand this table, see output of M-x list-charset-chars
    ("lbrakk" ,(isar-try-char 'japanese-jisx0208 #x22 #x5A))
    ("rbrakk" ,(isar-try-char 'japanese-jisx0208 #x22 #x5A))
    ("lbrakk" "[|")
    ("rbrakk" "|]")
    ("lbrace" "{|")
    ("rbrace" "|}"))
  "Fallback alternatives to `isar-symbols-tokens'.
The first displayable composition will be displayed to replace the
tokens."
  :group 'isabelle-tokens
  :set 'isar-set-and-restart-tokens)

(defcustom isar-bold-nums-tokens
  '(("zero" "0" bold)
    ("one" "1" bold)
    ("two" "2" bold)
    ("three" "3" bold)
    ("four" "4" bold)
    ("five" "5" bold)
    ("six" "6" bold)
    ("seven" "7" bold)
    ("eight" "8" bold)
    ("nine" "9" bold))
  "Tokens for bold numerals."
  :group 'isabelle-tokens
  :set 'isar-set-and-restart-tokens)

(defcustom isar-modifier-symbols-tokens
  '(("^sub" "⇩")
    ("^sup" "⇧")
    ("^bsub" "⇘")
    ("^esub" "⇙")
    ("^bsup" "⇗")
    ("^esup" "⇖")
    ("zero" "𝟎")
    ("one" "𝟏")
    ("two" "𝟐")
    ("three" "𝟑")
    ("four" "𝟒")
    ("five" "𝟓")
    ("six" "𝟔")
    ("seven" "𝟕")
    ("eight" "𝟖")
    ("nine" "𝟗"))
  "Some various symbols used for Unicode printing. Most should be either invisible or styled (e.g. bold)."
  :group 'isabelle-tokens
  :set 'isar-set-and-restart-tokens)

(defun isar-map-letters (f1 f2 &rest symbs)
  (cl-loop for x below 26
        for c = (+ 65 x)
        collect
        (cons (funcall f1 c) (cons (funcall f2 c) symbs))))

(defconst isar-script-letters-tokens ; \<A> \<B> ...
  (isar-map-letters (lambda (x) (format "%c" x))
                    (lambda (x) (format "%c" x))
                    'script))

(defconst isar-roman-letters-tokens ; \<a> \<b> ...
  (isar-map-letters (lambda (x) (downcase (format "%c" x)))
                    (lambda (x) (downcase (format "%c" x)))
                    'serif))

(defconst isar-fraktur-uppercase-letters-tokens ; \<AA> \<BB> ..
  (isar-map-letters (lambda (x) (format "%c%c" x x))
                    (lambda (x) (format "%c" x))
                    'frakt))

(defconst isar-fraktur-lowercase-letters-tokens ; \<AA> \<BB> ..
  (isar-map-letters (lambda (x) (downcase (format "%c%c" x x)))
                    (lambda (x) (downcase (format "%c" x)))
                    'frakt))

(defcustom isar-token-symbol-map nil
  "Table mapping Isabelle symbol token names to Unicode strings.
See `unicode-tokens-token-symbol-map'.

You can adjust this table to change the default entries.

Each element is a list

  (TOKNAME COMPOSITION FONTSYMB ...)

COMPOSITION is usually a string, perhaps containing Unicode characters.
For Isabelle, the token TOKNAME is made into the token \\<TOKNAME>."
  :group 'isabelle
  :set 'isar-set-and-restart-tokens
  :tag "Isabelle Unicode Token Mapping")

(defcustom isar-user-tokens nil
  "User-defined additions to `isar-token-symbol-map'.

Each element is a list

  (TOKNAME COMPOSITION FONTSYMB ...)

COMPOSITION is usually a string, perhaps containing Unicode characters.
For Isabelle, the token TOKNAME is made into the token \\<TOKNAME>."
  :group 'isabelle
  :set 'isar-set-and-restart-tokens
  :tag "User extensions for Isabelle Token Mapping")

(defun isar-init-token-symbol-map ()
  "Initialise the default value for `unicode-tokens-token-symbol-map'."
  (custom-set-default 'isar-token-symbol-map
                      (append
                       isar-symbols-tokens
                       isar-extended-symbols-tokens
                       isar-user-tokens
                       isar-misc-letters-tokens
                       isar-greek-letters-tokens
                       isar-bold-nums-tokens
                       isar-script-letters-tokens
                       isar-roman-letters-tokens
                       isar-fraktur-uppercase-letters-tokens
                       isar-fraktur-lowercase-letters-tokens
                       isar-user-tokens
                       isar-symbols-tokens-fallbacks)))

(isar-init-token-symbol-map)



;;
;; Shortcuts
;;

(defcustom isar-symbol-shortcuts
  '(("\\/" . "\\<or>")
    ("/\\" . "\\<and>")
    ("+O" . "\\<oplus>")
    ("-O" . "\\<ominus>")
    ("xO" . "\\<otimes>")
    ("/O" . "\\<oslash>")
    (".O" . "\\<odot>")
    ("|+" . "\\<dagger>")
    ("|++" . "\\<ddagger>")
    ("<=" . "\\<le>")
    ("|-" . "\\<turnstile>")
    (">=" . "\\<ge>")
    ("-|" . "\\<stileturn>")
    ("||" . "\\<parallel>")
    ("==" . "\\<equiv>")
    ("~=" . "\\<noteq>")
    ("~:" . "\\<notin>")
    ("~~~" . "\\<notapprox>")
    ("~~" . "\\<approx>")
    ("~==" . "\\<cong>")
    ("|<>|" . "\\<bowtie>")
    ("|=" . "\\<Turnstile>")
    ("=." . "\\<doteq>")
    ("_|_" . "\\<bottom>")
    ("</" . "\\<notle>")
    ("~>=" . "\\<notge>")
    ("==/" . "\\<notequiv>")
    ("~/" . "\\<notsim>")
    ("~=/" . "\\<notsimeq>")
    ("~~/" . "\\<notsimeq>")
    ("<-" . "\\<leftarrow>")
    ("<=" . "\\<Leftarrow>")
    ("->" . "\\<rightarrow>")
    ("=>" . "\\<Rightarrow>")
    ("<-->" . "\\<longleftrightarrow>")
    ("<=>" . "\\<Leftrightarrow>")
    ("|->" . "\\<mapsto>")
    ("<--" . "\\<longleftarrow>")
    ("<==" . "\\<Longleftarrow>")
    ("-->" . "\\<longrightarrow>")
    ("==>" . "\\<Longrightarrow>")
    ("<==>" . "\\<Longleftrightarrow>")
    ("|-->" . "\\<longmapsto>")
    ("<->" . "\\<leftrightarrow>")
    ("<<" . "\\<langle>")
    (">>" . "\\<rangle>")
    ("<>" . "\\<diamond>")
    ("[|" . "\\<lbrakk>")
    ("|]" . "\\<rbrakk>")
    ("{|" . "\\<lbrace>")
    ("|}" . "\\<rbrace>")
    ("(|" . "\\<lparr>")
    ("|)" . "\\<rparr>")
    ;; useful for unicode-tokens-replace-shortcuts
    ("ALL" . "\\<forall>")
    ("EX"  . "\\<exists>")
    ("!!"  . "\\<And>")
    ("~"  . "\\<not>")
    ;("!"  . "\\<forall>") makes it impossible to type ! as in intro!:
    ;("?"  . "\\<exists>")
    ("(="  . "\\<subseteq>")
    ("__"  . "\\<^sub>")
    ("^^"  . "\\<^sup>")
    ("./" . "\\<Down>")
    ;; extra misc, switch them off if you don't like them
    ;("|>" . "\\<triangleright>") ; clashes with ML parsing combinator
    ("«" . "\\<open>")
    ("»" . "\\<close>")
    ("<|" . "\\<triangleleft>"))
  "Shortcut key sequence table for symbol tokens input.
See `unicode-tokens-shortcut-alist'."
    :type 'unicode-tokens-shortcut-alist
    :set 'isar-set-and-restart-tokens
    :group 'isabelle
    :tag "Isabelle symbol shortcuts")

(defcustom isar-shortcut-alist nil
  "Shortcut key sequence table for token input.
See `unicode-tokens-shortcut-alist'."
  :type 'unicode-tokens-shortcut-alist
  :set 'isar-set-and-restart-tokens
  :group 'isabelle
  :tag "Isabelle Unicode Input Shortcuts")

(defun isar-init-shortcut-alists ()
  "Set defaults for `isar-shortcut-alist' and `isar-shortcut-replacement-alist'."
  (custom-set-default 'isar-shortcut-alist
                      (append
                       isar-symbol-shortcuts
                       ;; LaTeX-like syntax for symbol names, easier to type
                       (mapcar
                        (lambda (tokentry)
                          (cons (concat "\\" (car tokentry))
                                (format isar-token-format (car tokentry))))
                        (append isar-greek-letters-tokens
                                isar-symbols-tokens)))))
  ;; todo: allow shortcuts for replacements to be a different list
  ;; (setq unicode-tokens-shortcut-replacement-alist nil))

(isar-init-shortcut-alists)

;;
;; To generate special menu entries
;;

(defconst isar-tokens-customizable-variables
  '(("Symbols" isar-symbols-tokens)
    ("Extended Symbols" isar-extended-symbols-tokens)
    ("User Tokens" isar-user-tokens)
    ("Misc Letters" isar-misc-letters-tokens)
    ("Greek Letters" isar-greek-letters-tokens)
    ("Fallbacks" isar-symbols-tokens-fallbacks)
    ("Shortcuts" isar-symbol-shortcuts)))


;;
;; prover symbol support
;;
(defvar proof-tokens-activate-command nil "Function with unknow effect")
(defvar proof-tokens-deactivate-command nil "Function with unknow effect")

(eval-after-load "isar"
  '(setq
    proof-tokens-activate-command
    (isar-markup-ml "change print_mode (insert (op =) \"xsymbols\")")
    proof-tokens-deactivate-command
    (isar-markup-ml "change print_mode (remove (op =) \"xsymbols\")")))



(provide 'isar-unicode-tokens)

;;; isar-unicode-tokens.el ends here
