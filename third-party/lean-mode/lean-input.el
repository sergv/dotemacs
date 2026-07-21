;;; lean-input.el --- The Lean input method (based/copied from Agda) -*- lexical-binding: t; -*-
;;;
;;; DISCLAIMER: This file is based on agda-input.el provided with the Agda language.
;;; We did minor modifications
;;
;;; Commentary:
;;
;;;; A highly customisable input method which can inherit from other
;; Quail input methods. By default the input method is geared towards
;; the input of mathematical and other symbols in Lean programs.
;;
;; Use M-x customize-group lean-input to customise this input method.
;; Note that the functions defined under "Functions used to tweak
;; translation pairs" below can be used to tweak both the key
;; translations inherited from other input methods as well as the
;; ones added specifically for this one.
;;
;; Use lean-input-show-translations to see all the characters which
;; can be typed using this input method (except for those
;; corresponding to ASCII characters).

;;; Code:

(require 'quail)

(eval-when-compile
  (require 'cl-lib))
;; Quail is quite stateful, so be careful when editing this code.  Note
;; that with-temp-buffer is used below whenever buffer-local state is
;; modified.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun lean-input-concat-map (f xs)
  "Concat (map F XS)."
  (apply #'append (mapcar f xs)))

(defun lean-input-to-string-list (s)
  "Convert a string S to a list of one-character strings, after
removing all space and newline characters."
  (lean-input-concat-map
   (lambda (c) (if (member c (string-to-list " \n"))
              nil
            (list (string c))))
   (string-to-list s)))

(defun lean-input-character-range (from to)
  "A string consisting of the characters from FROM to TO."
  (let (seq)
    (dotimes (i (1+ (- to from)))
      (setq seq (cons (+ from i) seq)))
    (concat (nreverse seq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to tweak translation pairs

(defun lean-input-compose (f g)
  "λx -> concatMap F (G x)"
  (let ((f1 f) (g1 g))
    (lambda (x) (lean-input-concat-map f1 (funcall g1 x)))))

(defun lean-input-or (f g)
  "λx -> F x ++ G x"
  (let ((f1 f) (g1 g))
    (lambda (x) (append (funcall f1 x) (funcall g1 x)))))

(defun lean-input-nonempty ()
  "Only keep pairs with a non-empty first component."
  (lambda (x) (if (> (length (car x)) 0) (list x))))

(defun lean-input-prepend (prefix)
  "Prepend PREFIX to all key sequences."
  (let ((prefix1 prefix))
    (lambda (x) `((,(concat prefix1 (car x)) . ,(cdr x))))))

(defun lean-input-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX."
  (let ((prefix1 prefix))
    (lambda (x)
      (if (equal (substring (car x) 0 (length prefix1)) prefix1)
          (list x)))))

(defun lean-input-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX."
  (let ((suffix1 suffix))
    (lambda (x)
      (if (equal (substring (car x)
                            (- (length (car x)) (length suffix1)))
                 suffix1)
          (list x)))))

(defun lean-input-drop (ss)
  "Drop pairs matching one of the given key sequences.
SS should be a list of strings."
  (let ((ss1 ss))
    (lambda (x) (unless (member (car x) ss1) (list x)))))

(defun lean-input-drop-beginning (n)
  "Drop N characters from the beginning of each key sequence."
  (let ((n1 n))
    (lambda (x) `((,(substring (car x) n1) . ,(cdr x))))))

(defun lean-input-drop-end (n)
  "Drop N characters from the end of each key sequence."
  (let ((n1 n))
    (lambda (x)
      `((,(substring (car x) 0 (- (length (car x)) n1)) .
         ,(cdr x))))))

(defun lean-input-drop-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX.
This prefix is dropped."
  (lean-input-compose
   (lean-input-drop-beginning (length prefix))
   (lean-input-prefix prefix)))

(defun lean-input-drop-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX.
This suffix is dropped."
  (let ((suffix1 suffix))
    (lean-input-compose
     (lean-input-drop-end (length suffix1))
     (lean-input-suffix suffix1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

;; The :set keyword is set to 'lean-input-incorporate-changed-setting
;; so that the input method gets updated immediately when users
;; customize it. However, the setup functions cannot be run before all
;; variables have been defined. Hence the :initialize keyword is set to
;; 'custom-initialize-default to ensure that the setup is not performed
;; until lean-input-setup is called at the end of this file.

(defgroup lean-input nil
  "The Lean input method.
After tweaking these settings you may want to inspect the resulting
translations using `lean-input-show-translations'."
  :group 'lean
  :group 'leim)

(defcustom lean-input-tweak-all
  '(lean-input-compose
    (lean-input-prepend "\\")
    (lean-input-nonempty))
  "An expression yielding a function which can be used to tweak
all translations before they are included in the input method.
The resulting function (if non-nil) is applied to every
\(KEY-SEQUENCE . TRANSLATION) pair and should return a list of such
pairs. (Note that the translations can be anything accepted by
`quail-defrule'.)

If you change this setting manually (without using the
customization buffer) you need to call `lean-input-setup' in
order for the change to take effect."
  :group 'lean-input
  :set 'lean-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type 'sexp)

(defcustom lean-input-inherit
  `(("TeX" . (lean-input-compose
              (lean-input-drop '("geq" "leq" "bullet" "qed" "par"))
              (lean-input-or
               (lean-input-drop-prefix "\\")
               (lean-input-or
                (lean-input-compose
                 (lean-input-drop '("^o"))
                 (lean-input-prefix "^"))
                (lean-input-prefix "_")))))
    )
  "A list of Quail input methods whose translations should be
inherited by the Lean input method (with the exception of
translations corresponding to ASCII characters).

The list consists of pairs (qp . tweak), where qp is the name of
a Quail package, and tweak is an expression of the same kind as
`lean-input-tweak-all' which is used to tweak the translation
pairs of the input method.

The inherited translation pairs are added last, after
`lean-input-user-translations' and `lean-input-translations'.

If you change this setting manually (without using the
customization buffer) you need to call `lean-input-setup' in
order for the change to take effect."
  :group 'lean-input
  :set 'lean-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Quail package")
                       (sexp :tag "Tweaking function"))))

(defcustom lean-input-translations
  (list

   ;; Negation

   `("not" . ("¬"))

   ;; Equality and similar symbols.

   `("eq"  . ,(lean-input-to-string-list "=∼∽≈≋∻∾∿≀≃⋍≂≅ ≌≊≡≣≐≑≒≓≔≕≖≗≘≙≚≛≜≝≞≟≍≎≏≬⋕"))
   `("eqn" . ,(lean-input-to-string-list "≠≁ ≉     ≄  ≇≆  ≢                 ≭    "))
   `("equiv" . ,(lean-input-to-string-list "≃⋍"))
   `("iso" . ,(lean-input-to-string-list "≅≌"))

   `("=n"  . ("≠"))
   `("~"    . ("∼"))
   `("~n"  . ("≁"))
   `("homotopy"    . ("∼"))
   `("~~"   . ("≈"))
   `("~~n" . ("≉"))
   `("~~~"  . ("≋"))
   `(":~"   . ("∻"))
   `("~-"   . ("≃"))
   `("~-n" . ("≄"))
   `("-~"   . ("≂"))
   `("~="   . ("≅"))
   `("~=n" . ("≇"))
   `("~~-"  . ("≊"))
   `("=="   . ("≡"))
   `("==n" . ("≢"))
   `("==="  . ("≣"))
   `(".="   . ("≐"))
   `(".=." . ("≑"))
   `(":="   . ("≔"))
   `("=:"  . ("≕"))
   `("=o"   . ("≗"))
   `("(="   . ("≘"))
   `("and=" . ("≙"))
   `("or=" . ("≚"))
   `("*="   . ("≛"))
   `("t="   . ("≜"))
   `("def=" . ("≝"))
   `("m="   . ("≞"))
   `("?="   . ("≟"))

   `("pr" . ("↣"))

   `("1" . ("₁"))
   `("2" . ("₂"))
   `("3" . ("₃"))
   `("4" . ("₄"))
   `("5" . ("₅"))
   `("6" . ("₆"))
   `("7" . ("₇"))
   `("8" . ("₈"))
   `("9" . ("₉"))
   `("0" . ("₀"))

   ;; Inequality and similar symbols.

   `("leq"  . ,(lean-input-to-string-list "≤≦≲<≪⋘ ≶≺≼≾⊂⊆ ⋐⊏⊑ ⊰⊲⊴⋖⋚⋜⋞"))
   `("leqn" . ,(lean-input-to-string-list "≰≨≮≴⋦   ≸⊀ ⋨⊄⊈⊊  ⋢⋤ ⋪⋬   ⋠"))
   `("geq"  . ,(lean-input-to-string-list "≥≧≳>≫⋙ ≷≻≽≿⊃⊇ ⋑⊐⊒ ⊱⊳⊵⋗⋛⋝⋟"))
   `("geqn" . ,(lean-input-to-string-list "≱≩≯≵⋧ ≹  ⊁ ⋩⊅⊉⊋  ⋣⋥ ⋫⋭   ⋡"))

   `("<="      . ("≤"))
   `(">="   . ("≥"))
   `("<=n"     . ("≰"))
   `(">=n"  . ("≱"))
   `("len"     . ("≰"))
   `("gen"  . ("≱"))
   `("<n"      . ("≮"))
   `(">n"   . ("≯"))
   `("<~"      . ("≲"))
   `(">~"   . ("≳"))
   `("<~n"     . ("⋦"))
   `(">~n"  . ("⋧"))
   `("<~nn"    . ("≴"))
   `(">~nn" . ("≵"))
   `("<:"      . ("⋖"))
   `(":>"   . ("⋗"))
   `("<=:"     . ("⩿"))
   `(":>="  . ("⪀"))
   `("covby"   . ("⋖"))
   `("covers"  . ("⋖"))
   `("wcovby"  . ("⩿"))
   `("wcovers" . ("⩿"))

   `("ssub"  . ("⊂"))
   `("ssup"  . ("⊃"))
   `("ssubn" . ("⊄"))
   `("ssupn" . ("⊅"))
   `("sub"   . ("⊆"))
   `("sup"   . ("⊇"))
   `("subn"  . ("⊈"))
   `("supn"  . ("⊉"))
   `("ssqub" . ("⊏"))
   `("ssqup" . ("⊐"))
   `("squb"  . ("⊑"))
   `("squp"  . ("⊒"))
   `("squbn" . ("⋢"))
   `("squpn" . ("⋣"))

   ;; Set membership etc.

   `("member" . ,(lean-input-to-string-list "∈∉∊∋∌∍⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿"))
   `("mem" . ("∈"))

   `("inn" . ("∉"))
   `("nin" . ("∌"))

   ;; Types

   `("T1" . ("Type₁"))
   `("T2" . ("Type₂"))
   `("T+" . ("Type₊"))

   ;; Intersections, unions etc.

   `("intersection" . ,(lean-input-to-string-list "∩⋂∧⋀⋏⨇⊓⨅⋒∏ ⊼      ⨉"))
   `("union"        . ,(lean-input-to-string-list "∪⋃∨⋁⋎⨈⊔⨆⋓∐⨿⊽⊻⊍⨃⊎⨄⊌∑⅀"))

   `("and" . ("∧"))
   `("or"  . ("∨"))
   `("And" . ("⋀"))
   `("Or"  . ("⋁"))
   `("i"   . ("∩"))
   `("un"  . ("∪"))
   `("u+" . ("⊎"))
   `("u." . ("⊍"))
   `("I"   . ("⋂"))
   `("Un"  . ("⋃"))
   `("U+" . ("⨄"))
   `("U." . ("⨃"))
   `("glb" . ("⊓"))
   `("lub" . ("⊔"))
   `("Glb" . ("⨅"))
   `("Lub" . ("⨆"))

   ;; Entailment etc.

   `("entails" . ,(lean-input-to-string-list "⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯"))

   `("|-"   . ("⊢"))
   `("|-n"  . ("⊬"))
   `("-|"   . ("⊣"))
   `("|="   . ("⊨"))
   `("|=n"  . ("⊭"))
   `("||-"  . ("⊩"))
   `("||-n" . ("⊮"))
   `("||="  . ("⊫"))
   `("||=n" . ("⊯"))
   `("|||-" . ("⊪"))

   ;; Divisibility, parallelity.

   `("|"  . ("∣"))
   `("|n"  . ("∤"))
   `("||" . ("∥"))
   `("||n" . ("∦"))

   ;; Some symbols from logic and set theory.

   `("all" . ("∀"))
   `("ex"  . ("∃"))
   `("exn" . ("∄"))
   `("0"   . ("∅"))
   `("empty"   . ("∅"))
   `("C"   . ("∁"))
   `("powerset" . ("𝒫"))

   ;; Corners, ceilings and floors.

   `("c"  . ,(lean-input-to-string-list "⌜⌝⌞⌟⌈⌉⌊⌋"))
   `("cu" . ,(lean-input-to-string-list "⌜⌝  ⌈⌉  "))
   `("cl" . ,(lean-input-to-string-list "  ⌞⌟  ⌊⌋"))

   `("cul" . ("⌜"))
   `("cuL" . ("⌈"))
   `("cur" . ("⌝"))
   `("cuR" . ("⌉"))
   `("cll" . ("⌞"))
   `("clL" . ("⌊"))
   `("clr" . ("⌟"))
   `("clR" . ("⌋"))

   ;; Various operators/symbols.
   `("tr"        . ,(lean-input-to-string-list "⬝▹"))
   `("trans"     . ,(lean-input-to-string-list "▹⬝"))
   `("transport" . ("▹"))
   `("con"       . ("⬝"))
   `("cdot"      . ("⬝"))
   `("dot"      . ("⬝"))
   `("sy"        . ("⁻¹"))
   `("inv"       . ("⁻¹"))
   `("-1"        . ("⁻¹" "₋₁"))
   `("^-1"       . ("⁻¹"))
   `("-2"        . ("⁻²" "₋₂"))
   `("-3"        . ("⁻³"))
   `("qed"       . ("∎"))
   `("x"         . ("×"))
   `("o"         . ("∘"))
   `("comp"      . ("∘"))
   `("."         . ("∙"))
   `("*"         . ("⋆"))
   `(".+"        . ("∔"))
   `(".-"        . ("∸"))
   `(":"         . ("∶"))
   `("::"        . ("∷"))
   `("::-"       . ("∺"))
   `("-:"        . ("∹"))
   `("+ "        . ("⊹"))
   `("surd3"     . ("∛"))
   `("surd4"     . ("∜"))
   `("increment" . ("∆"))
   `("inf"       . ("∞"))
   `("&"         . ("⅋"))
   `("od"        . ("ᵒᵈ"))
   `("aop"       . ("ᵃᵒᵖ"))
   `("mop"       . ("ᵐᵒᵖ"))
   `("op"        . ("ᵒᵖ"))

   ;; Circled operators.

   `("o+"  . ("⊕"))
   `("o--" . ("⊖"))
   `("ox"  . ("⊗"))
   `("o/"  . ("⊘"))
   `("o."  . ("⊙"))
   `("oo"  . ("⊚"))
   `("o*"  . ("∘*" "⊛"))
   `("o="  . ("⊜"))
   `("o-"  . ("⊝"))

   `("O+"  . ("⨁"))
   `("Ox"  . ("⨂"))
   `("O."  . ("⨀"))
   `("O*"  . ("⍟"))

   ;; Boxed operators.

   `("b+" . ("⊞"))
   `("b-" . ("⊟"))
   `("bx" . ("⊠"))
   `("b." . ("⊡"))

   ;; Various symbols.

   `("integral" . ,(lean-input-to-string-list "∫∬∭∮∯∰∱∲∳"))
   `("angle"    . ,(lean-input-to-string-list "∟∡∢⊾⊿"))
   `("join"     . ,(lean-input-to-string-list "⋈⋉⋊⋋⋌⨝⟕⟖⟗"))

   ;; Arrows.
   `("iff" . ("↔"))
   `("imp"  . ("→"))
   `("l"  . ,(lean-input-to-string-list "←⇐⇚⇇⇆↤⇦↞↼↽⇠⇺↜⇽⟵⟸↚⇍⇷ ↹     ↢↩↫⇋⇜⇤⟻⟽⤆↶↺⟲                                     "))
   `("r"  . ,(lean-input-to-string-list "→⇒⇛⇉⇄↦⇨↠⇀⇁⇢⇻↝⇾⟶⟹↛⇏⇸⇶ ↴    ↣↪↬⇌⇝⇥⟼⟾⤇↷↻⟳⇰⇴⟴⟿ ➵➸➙➔➛➜➝➞➟➠➡➢➣➤➧➨➩➪➫➬➭➮➯➱➲➳➺➻➼➽➾⊸"))
   `("u"  . ,(lean-input-to-string-list "↑⇑⟰⇈⇅↥⇧↟↿↾⇡⇞          ↰↱➦ ⇪⇫⇬⇭⇮⇯                                           "))
   `("d"  . ,(lean-input-to-string-list "↓⇓⟱⇊⇵↧⇩↡⇃⇂⇣⇟         ↵↲↳➥ ↯                                                "))
   `("ud" . ,(lean-input-to-string-list "↕⇕   ↨⇳                                                                    "))
   `("lr" . ,(lean-input-to-string-list "↔⇔         ⇼↭⇿⟷⟺↮⇎⇹                                                        "))
   `("ul" . ,(lean-input-to-string-list "↖⇖                        ⇱↸                                               "))
   `("ur" . ,(lean-input-to-string-list "↗⇗                                         ➶➹➚                             "))
   `("dr" . ,(lean-input-to-string-list "↘⇘                        ⇲                ➴➷➘                             "))
   `("dl" . ,(lean-input-to-string-list "↙⇙                                                                         "))
   `("==>" . ("⟹"))
   `("nattrans" . ("⟹"))
   `("nat_trans" . ("⟹"))

   `("l-"  . ("←"))
   `("<-"  . ("←"))
   `("l="  . ("⇐"))
   `("r-"  . ("→"))
   `("->"  . ("→"))
   `("r="  . ("⇒"))
   `("=>"  . ("⇒"))
   `("functor"  . ("⥤"))
   `("u-"  . ("↑"))
   `("u="  . ("⇑"))
   `("d-"  . ("↓"))
   `("d="  . ("⇓"))
   `("ud-" . ("↕"))
   `("ud=" . ("⇕"))
   `("lr-" . ("↔"))
   `("<->" . ("↔"))
   `("lr=" . ("⇔"))
   `("<=>" . ("⇔"))
   `("ul-" . ("↖"))
   `("ul=" . ("⇖"))
   `("ur-" . ("↗"))
   `("ur=" . ("⇗"))
   `("dr-" . ("↘"))
   `("dr=" . ("⇘"))
   `("dl-" . ("↙"))
   `("dl=" . ("⇙"))

   `("l==" . ("⇚"))
   `("l-2" . ("⇇"))
   `("l-r-" . ("⇆"))
   `("r==" . ("⇛"))
   `("r-2" . ("⇉"))
   `("r-3" . ("⇶"))
   `("r-l-" . ("⇄"))
   `("u==" . ("⟰"))
   `("u-2" . ("⇈"))
   `("u-d-" . ("⇅"))
   `("d==" . ("⟱"))
   `("d-2" . ("⇊"))
   `("d-u-" . ("⇵"))

   `("l--"  . ("⟵"))
   `("<--"  . ("⟵"))
   `("l~"  . ("↜" "⇜"))
   `("r--"  . ("⟶"))
   `("-->"  . ("⟶"))
   `("r~"  . ("↝" "⇝" "⟿"))
   `("hom"  . ("⟶"))
   `("lr--" . ("⟷"))
   `("<-->" . ("⟷"))
   `("lr~" . ("↭"))

   `("l-n"  . ("↚"))
   `("<-n"  . ("↚"))
   `("l=n"  . ("⇍"))
   `("r-n"  . ("↛"))
   `("->n"  . ("↛"))
   `("r=n"  . ("⇏"))
   `("=>n"  . ("⇏"))
   `("lr-n" . ("↮"))
   `("<->n" . ("↮"))
   `("lr=n" . ("⇎"))
   `("<=>n" . ("⇎"))

   `("l-|"  . ("↤"))
   `("ll-" . ("↞"))
   `("r-|"  . ("↦"))
   `("rr-" . ("↠"))
   `("u-|"  . ("↥"))
   `("uu-" . ("↟"))
   `("d-|"  . ("↧"))
   `("dd-" . ("↡"))
   `("ud-|" . ("↨"))

   `("l->" . ("↢"))
   `("r->" . ("↣"))

   `("r-o" . ("⊸"))
   `("-o"  . ("⊸"))

   `("dz" . ("↯"))

   ;; Ellipsis.

   `("..." . ,(lean-input-to-string-list "⋯⋮⋰⋱"))

   ;; Box-drawing characters.

   `("---" . ,(lean-input-to-string-list "─│┌┐└┘├┤┬┼┴╴╵╶╷╭╮╯╰╱╲╳"))
   `("--=" . ,(lean-input-to-string-list "═║╔╗╚╝╠╣╦╬╩     ╒╕╘╛╞╡╤╪╧ ╓╖╙╜╟╢╥╫╨"))
   `("--_" . ,(lean-input-to-string-list "━┃┏┓┗┛┣┫┳╋┻╸╹╺╻
                                        ┍┯┑┕┷┙┝┿┥┎┰┒┖┸┚┠╂┨┞╀┦┟╁┧┢╈┪┡╇┩
                                        ┮┭┶┵┾┽┲┱┺┹╊╉╆╅╄╃ ╿╽╼╾"))
   `("--." . ,(lean-input-to-string-list "╌╎┄┆┈┊
                                        ╍╏┅┇┉┋"))

   ;; Triangles.

   ;; Big/small, black/white.

   `("t"     . ,(lean-input-to-string-list "▸▹►▻◂◃◄◅▴▵▾▿◢◿◣◺◤◸◥◹"))
   `("Tr"    . ,(lean-input-to-string-list "◀◁▶▷▲△▼▽◬◭◮"))

   `("tb" . ,(lean-input-to-string-list "◂▸▴▾◄►◢◣◤◥"))
   `("tw" . ,(lean-input-to-string-list "◃▹▵▿◅▻◿◺◸◹"))

   `("Tb" . ,(lean-input-to-string-list "◀▶▲▼"))
   `("Tw" . ,(lean-input-to-string-list "◁▷△▽"))

   ;; Squares.

   `("sq"  . ,(lean-input-to-string-list "◾◽■□◼◻▣▢▤▥▦▧▨▩◧◨◩◪◫◰◱◲◳"))
   `("sqb" . ,(lean-input-to-string-list "■◼◾"))
   `("sqw" . ,(lean-input-to-string-list "□◻◽"))
   `("sq." . ("▣"))
   `("sqo" . ("▢"))

   ;; Rectangles.

   `("re"  . ,(lean-input-to-string-list "▬▭▮▯"))
   `("reb" . ,(lean-input-to-string-list "▬▮"))
   `("rew" . ,(lean-input-to-string-list "▭▯"))

   ;; Parallelograms.

   `("pa"  . ,(lean-input-to-string-list "▰▱"))
   `("pab" . ("▰"))
   `("paw" . ("▱"))

   ;; Diamonds.

   `("di"  . ,(lean-input-to-string-list "◆◇◈"))
   `("dib" . ("◆"))
   `("diw" . ("◇"))
   `("di." . ("◈"))

   ;; Circles.

   `("ci"   . ,(lean-input-to-string-list "●○◎◌◯◍◐◑◒◓◔◕◖◗◠◡◴◵◶◷⚆⚇⚈⚉"))
   `("cib"  . ("●"))
   `("ciw"  . ("○"))
   `("ci."  . ("◎"))
   `("ci.." . ("◌"))
   `("ciO"  . ("◯"))

   ;; Stars.

   `("st"   . ,(lean-input-to-string-list "⋆✦✧✶✴✹ ★☆✪✫✯✰✵✷✸"))
   `("st4"  . ,(lean-input-to-string-list "✦✧"))
   `("st6"  . ("✶"))
   `("st8"  . ("✴"))
   `("st12" . ("✹"))

   ;; Blackboard bold letters.

   `("bn"   . ("ℕ"))
   `("bz"   . ("ℤ"))
   `("bq"   . ("ℚ"))
   `("br"   . ("ℝ"))
   `("bc"   . ("ℂ"))
   `("bp"   . ("ℙ"))
   `("bb"   . ("𝔹"))
   `("bsum" . ("⅀"))
   `("bbA"  . ("𝔸"))
   `("bbB"  . ("𝔹"))
   `("bbC"  . ("ℂ"))
   `("bbD"  . ("𝔻"))
   `("bbE"  . ("𝔼"))
   `("bbF"  . ("𝔽"))
   `("bbG"  . ("𝔾"))
   `("bbH"  . ("ℍ"))
   `("bbI"  . ("𝕀"))
   `("bbJ"  . ("𝕁"))
   `("bbK"  . ("𝕂"))
   `("bbL"  . ("𝕃"))
   `("bbM"  . ("𝕄"))
   `("bbN"  . ("ℕ"))
   `("bbO"  . ("𝕆"))
   `("bbP"  . ("ℙ"))
   `("bbQ"  . ("ℚ"))
   `("bbR"  . ("ℝ"))
   `("bbS"  . ("𝕊"))
   `("bbT"  . ("𝕋"))
   `("bbU"  . ("𝕌"))
   `("bbV"  . ("𝕍"))
   `("bbW"  . ("𝕎"))
   `("bbX"  . ("𝕏"))
   `("bbY"  . ("𝕐"))
   `("bbZ"  . ("ℤ"))
   `("bba"  . ("𝕒"))
   `("bbb"  . ("𝕓"))
   `("bbc"  . ("𝕔"))
   `("bbd"  . ("𝕕"))
   `("bbe"  . ("𝕖"))
   `("bbf"  . ("𝕗"))
   `("bbg"  . ("𝕘"))
   `("bbh"  . ("𝕙"))
   `("bbi"  . ("𝕚"))
   `("bbj"  . ("𝕛"))
   `("bbk"  . ("𝕜"))
   `("bbl"  . ("𝕝"))
   `("bbm"  . ("𝕞"))
   `("bbn"  . ("𝕟"))
   `("bbo"  . ("𝕠"))
   `("bbp"  . ("𝕡"))
   `("bbq"  . ("𝕢"))
   `("bbr"  . ("𝕣"))
   `("bbs"  . ("𝕤"))
   `("bbt"  . ("𝕥"))
   `("bbu"  . ("𝕦"))
   `("bbv"  . ("𝕧"))
   `("bbw"  . ("𝕨"))
   `("bbx"  . ("𝕩"))
   `("bby"  . ("𝕪"))
   `("bbz"  . ("𝕫"))

   ;; Blackboard bold numbers.

   `("b0"   . ("𝟘"))
   `("b1"   . ("𝟙"))
   `("b2"   . ("𝟚"))
   `("b3"   . ("𝟛"))
   `("b4"   . ("𝟜"))
   `("b5"   . ("𝟝"))
   `("b6"   . ("𝟞"))
   `("b7"   . ("𝟟"))
   `("b8"   . ("𝟠"))
   `("b9"   . ("𝟡"))

   ;; Parentheses.

   `("(" . ,(lean-input-to-string-list "([{⁅⁽₍〈⎴⟅⟦⟨⟪⦃〈《‹«「『【〔〖〚︵︷︹︻︽︿﹁﹃﹙﹛﹝（［｛｢"))
   `(")" . ,(lean-input-to-string-list ")]}⁆⁾₎〉⎵⟆⟧⟩⟫⦄〉》›»」』】〕〗〛︶︸︺︼︾﹀﹂﹄﹚﹜﹞）］｝｣"))

   `("[[" . ("⟦"))
   `("]]" . ("⟧"))
   `("<"  . ("⟨"))
   `(">"  . ("⟩"))
   `("<<" . ("⟪"))
   `(">>" . ("⟫"))
   `("f<"  . ("‹"))
   `("f>"  . ("›"))
   `("f<<" . ("«"))
   `("f>>" . ("»"))
   `("{{" . ("⦃"))
   `("}}" . ("⦄"))

   `("(b" . ("⟅"))
   `(")b" . ("⟆"))

   `("lbag" . ("⟅"))
   `("rbag" . ("⟆"))

   ;; lambda

   `("fun" . ("λ"))
   `("lam" . ("λ"))

   `("X" . ("⨯"))

   ;; Primes.

   `("'" . ,(lean-input-to-string-list "′″‴⁗"))
   `("`" . ,(lean-input-to-string-list "‵‶‷"))

   ;; Fractions.

   `("frac" . ,(lean-input-to-string-list "¼½¾⅓⅔⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞⅟"))

   ;; Bullets.

   `("bu"  . ,(lean-input-to-string-list "•◦‣⁌⁍"))
   `("bub" . ("•"))
   `("buw" . ("◦"))
   `("but" . ("‣"))

   ;; Types
   `("nat"  . ("ℕ"))
   `("Nat"  . ("ℕ"))
   `("N"    . ("ℕ"))
   `("int"  . ("ℤ"))
   `("Int"  . ("ℤ"))
   `("Z"    . ("ℤ"))
   `("rat"  . ("ℚ"))
   `("Rat"  . ("ℚ"))
   `("Q"    . ("ℚ"))
   `("real" . ("ℝ"))
   `("Real" . ("ℝ"))
   `("R"    . ("ℝ"))
   `("Com"  . ("ℂ"))
   `("com"  . ("ℂ"))
   `("C"    . ("ℂ"))
   `("A"    . ("𝔸"))
   `("F"    . ("𝔽"))
   `("H"    . ("ℍ"))
   `("K"    . ("𝕂"))

   `("a" . ("α"))
   `("b" . ("β"))
   `("g" . ("γ"))

   ;; Musical symbols.

   `("note" . ,(lean-input-to-string-list "♩♪♫♬"))
   `("flat" . ("♭"))
   `("#"    . ("♯"))

   ;; Other punctuation and symbols.

   `("\\"         . ("\\"))
   `("en"         . ("–"))
   `("em"         . ("—"))
   `("^i"         . ("ⁱ"))
   `("^o"         . ("ᵒ"))
   `("!!"         . ("‼"))
   `("??"         . ("⁇"))
   `("?!"         . ("‽" "⁈"))
   `("!?"         . ("⁉"))
   `("die"        . ,(lean-input-to-string-list "⚀⚁⚂⚃⚄⚅"))
   `("asterisk"   . ,(lean-input-to-string-list "⁎⁑⁂✢✣✤✥✱✲✳✺✻✼✽❃❉❊❋"))
   `("8<"         . ("✂" "✄"))
   `("tie"        . ("⁀"))
   `("undertie"   . ("‿"))
   `("apl"        . ,(lean-input-to-string-list "⌶⌷⌸⌹⌺⌻⌼⌽⌾⌿⍀⍁⍂⍃⍄⍅⍆⍇⍈
                                               ⍉⍊⍋⍌⍍⍎⍏⍐⍑⍒⍓⍔⍕⍖⍗⍘⍙⍚⍛
                                               ⍜⍝⍞⍟⍠⍡⍢⍣⍤⍥⍦⍧⍨⍩⍪⍫⍬⍭⍮
                                               ⍯⍰⍱⍲⍳⍴⍵⍶⍷⍸⍹⍺⎕"))
   `("/"         . ("⧸"))
   `("quot"      . ("⧸"))

   ;; Some combining characters.
   ;;
   ;; The following combining characters also have (other)
   ;; translations:
   ;; ̀ ́ ̂ ̃ ̄ ̆ ̇ ̈ ̋ ̌ ̣ ̧ ̱

   `("^--" . ,(lean-input-to-string-list"̅̿"))
   `("_--" . ,(lean-input-to-string-list"̲̳"))
   `("^~"  . ,(lean-input-to-string-list"̃͌"))
   `("_~"  .  (                         "̰"))
   `("^."  . ,(lean-input-to-string-list"̇̈⃛⃜"))
   `("_."  . ,(lean-input-to-string-list"̣̤"))
   `("^l"  . ,(lean-input-to-string-list"⃖⃐⃔"))
   `("^l-" .  (                         "⃖"))
   `("^r"  . ,(lean-input-to-string-list"⃗⃑⃕"))
   `("^r-" .  (                         "⃗"))
   `("^lr" .  (                         "⃡"))
   `("_lr" .  (                         "͍"))
   `("^^"  . ,(lean-input-to-string-list"̂̑͆"))
   `("_^"  . ,(lean-input-to-string-list"̭̯̪"))
   `("^v"  . ,(lean-input-to-string-list"̌̆"))
   `("_v"  . ,(lean-input-to-string-list"̬̮̺"))

   ;; Shorter forms of many greek letters plus ƛ.

   `("Ga"  . ("α"))
   `("GA"  . ("Α"))
   `("Gb"  . ("β"))
   `("GB"  . ("Β"))
   `("Gg"  . ("γ"))
   `("GG"  . ("Γ"))
   `("Gd"  . ("δ"))
   `("GD"  . ("Δ"))
   `("Ge"  . ("ε"))
   `("GE"  . ("Ε"))
   `("eps" . ("ε"))
   `("Gz"  . ("ζ"))
   `("GZ"  . ("Ζ"))
   ;; \eta \Eta
   `("Gth" . ("θ"))
   `("GTH" . ("Θ"))
   `("th"  . ("θ"))
   `("Gi"  . ("ι"))
   `("GI"  . ("Ι"))
   `("Gk"  . ("κ"))
   `("GK"  . ("Κ"))
   `("Gl"  . ("λ"))
   `("GL"  . ("Λ"))
   `("Gl-" . ("ƛ"))
   `("Gm"  . ("μ"))
   `("GM"  . ("Μ"))
   `("Gn"  . ("ν"))
   `("GN"  . ("Ν"))
   `("Gx"  . ("ξ"))
   `("GX"  . ("Ξ"))
   ;; \omicron \Omicron
   ;; \pi \Pi
   `("Gr"  . ("ρ"))
   `("GR"  . ("Ρ"))
   `("Gs"  . ("σ"))
   `("GS"  . ("Σ"))
   `("Gt"  . ("τ"))
   `("GT"  . ("Τ"))
   `("Gu"  . ("υ"))
   `("GU"  . ("Υ"))
   `("Gf"  . ("φ"))
   `("GF"  . ("Φ"))
   `("Gc"  . ("χ"))
   `("GC"  . ("Χ"))
   `("Gp"  . ("ψ"))
   `("GP"  . ("Ψ"))
   `("Go"  . ("ω"))
   `("GO"  . ("Ω"))
   ;; even shorter versions for central type constructors
   `("S"   . ("Σ"))
   `("P"   . ("Π"))

   ;; Mathematical characters

   `("MiA" . ("𝐴"))
   `("MiB" . ("𝐵"))
   `("MiC" . ("𝐶"))
   `("MiD" . ("𝐷"))
   `("MiE" . ("𝐸"))
   `("MiF" . ("𝐹"))
   `("MiG" . ("𝐺"))
   `("MiH" . ("𝐻"))
   `("MiI" . ("𝐼"))
   `("MiJ" . ("𝐽"))
   `("MiK" . ("𝐾"))
   `("MiL" . ("𝐿"))
   `("MiM" . ("𝑀"))
   `("MiN" . ("𝑁"))
   `("MiO" . ("𝑂"))
   `("MiP" . ("𝑃"))
   `("MiQ" . ("𝑄"))
   `("MiR" . ("𝑅"))
   `("MiS" . ("𝑆"))
   `("MiT" . ("𝑇"))
   `("MiU" . ("𝑈"))
   `("MiV" . ("𝑉"))
   `("MiW" . ("𝑊"))
   `("MiX" . ("𝑋"))
   `("MiY" . ("𝑌"))
   `("MiZ" . ("𝑍"))
   `("Mia" . ("𝑎"))
   `("Mib" . ("𝑏"))
   `("Mic" . ("𝑐"))
   `("Mid" . ("𝑑"))
   `("Mie" . ("𝑒"))
   `("Mif" . ("𝑓"))
   `("Mig" . ("𝑔"))
   `("Mii" . ("𝑖"))
   `("Mij" . ("𝑗"))
   `("Mik" . ("𝑘"))
   `("Mil" . ("𝑙"))
   `("Mim" . ("𝑚"))
   `("Min" . ("𝑛"))
   `("Mio" . ("𝑜"))
   `("Mip" . ("𝑝"))
   `("Miq" . ("𝑞"))
   `("Mir" . ("𝑟"))
   `("Mis" . ("𝑠"))
   `("Mit" . ("𝑡"))
   `("Miu" . ("𝑢"))
   `("Miv" . ("𝑣"))
   `("Miw" . ("𝑤"))
   `("Mix" . ("𝑥"))
   `("Miy" . ("𝑦"))
   `("Miz" . ("𝑧"))
   `("MIA" . ("𝑨"))
   `("MIB" . ("𝑩"))
   `("MIC" . ("𝑪"))
   `("MID" . ("𝑫"))
   `("MIE" . ("𝑬"))
   `("MIF" . ("𝑭"))
   `("MIG" . ("𝑮"))
   `("MIH" . ("𝑯"))
   `("MII" . ("𝑰"))
   `("MIJ" . ("𝑱"))
   `("MIK" . ("𝑲"))
   `("MIL" . ("𝑳"))
   `("MIM" . ("𝑴"))
   `("MIN" . ("𝑵"))
   `("MIO" . ("𝑶"))
   `("MIP" . ("𝑷"))
   `("MIQ" . ("𝑸"))
   `("MIR" . ("𝑹"))
   `("MIS" . ("𝑺"))
   `("MIT" . ("𝑻"))
   `("MIU" . ("𝑼"))
   `("MIV" . ("𝑽"))
   `("MIW" . ("𝑾"))
   `("MIX" . ("𝑿"))
   `("MIY" . ("𝒀"))
   `("MIZ" . ("𝒁"))
   `("MIa" . ("𝒂"))
   `("MIb" . ("𝒃"))
   `("MIc" . ("𝒄"))
   `("MId" . ("𝒅"))
   `("MIe" . ("𝒆"))
   `("MIf" . ("𝒇"))
   `("MIg" . ("𝒈"))
   `("MIh" . ("𝒉"))
   `("MIi" . ("𝒊"))
   `("MIj" . ("𝒋"))
   `("MIk" . ("𝒌"))
   `("MIl" . ("𝒍"))
   `("MIm" . ("𝒎"))
   `("MIn" . ("𝒏"))
   `("MIo" . ("𝒐"))
   `("MIp" . ("𝒑"))
   `("MIq" . ("𝒒"))
   `("MIr" . ("𝒓"))
   `("MIs" . ("𝒔"))
   `("MIt" . ("𝒕"))
   `("MIu" . ("𝒖"))
   `("MIv" . ("𝒗"))
   `("MIw" . ("𝒘"))
   `("MIx" . ("𝒙"))
   `("MIy" . ("𝒚"))
   `("MIz" . ("𝒛"))
   `("McA" . ("𝒜"))
   `("McC" . ("𝒞"))
   `("McD" . ("𝒟"))
   `("McG" . ("𝒢"))
   `("McJ" . ("𝒥"))
   `("McK" . ("𝒦"))
   `("McN" . ("𝒩"))
   `("McO" . ("𝒪"))
   `("McP" . ("𝒫"))
   `("McQ" . ("𝒬"))
   `("McS" . ("𝒮"))
   `("McT" . ("𝒯"))
   `("McU" . ("𝒰"))
   `("McV" . ("𝒱"))
   `("McW" . ("𝒲"))
   `("McX" . ("𝒳"))
   `("McY" . ("𝒴"))
   `("McZ" . ("𝒵"))
   `("Mca" . ("𝒶"))
   `("Mcb" . ("𝒷"))
   `("Mcc" . ("𝒸"))
   `("Mcd" . ("𝒹"))
   `("Mcf" . ("𝒻"))
   `("Mch" . ("𝒽"))
   `("Mci" . ("𝒾"))
   `("Mcj" . ("𝒿"))
   `("Mck" . ("𝓀"))
   `("Mcl" . ("𝓁"))
   `("Mcm" . ("𝓂"))
   `("Mcn" . ("𝓃"))
   `("Mcp" . ("𝓅"))
   `("Mcq" . ("𝓆"))
   `("Mcr" . ("𝓇"))
   `("Mcs" . ("𝓈"))
   `("Mct" . ("𝓉"))
   `("Mcu" . ("𝓊"))
   `("Mcv" . ("𝓋"))
   `("Mcw" . ("𝓌"))
   `("Mcx" . ("𝓍"))
   `("Mcy" . ("𝓎"))
   `("Mcz" . ("𝓏"))
   `("MCA" . ("𝓐"))
   `("MCB" . ("𝓑"))
   `("MCC" . ("𝓒"))
   `("MCD" . ("𝓓"))
   `("MCE" . ("𝓔"))
   `("MCF" . ("𝓕"))
   `("MCG" . ("𝓖"))
   `("MCH" . ("𝓗"))
   `("MCI" . ("𝓘"))
   `("MCJ" . ("𝓙"))
   `("MCK" . ("𝓚"))
   `("MCL" . ("𝓛"))
   `("MCM" . ("𝓜"))
   `("MCN" . ("𝓝"))
   `("MCO" . ("𝓞"))
   `("MCP" . ("𝓟"))
   `("MCQ" . ("𝓠"))
   `("MCR" . ("𝓡"))
   `("MCS" . ("𝓢"))
   `("MCT" . ("𝓣"))
   `("MCU" . ("𝓤"))
   `("MCV" . ("𝓥"))
   `("MCW" . ("𝓦"))
   `("MCX" . ("𝓧"))
   `("MCY" . ("𝓨"))
   `("MCZ" . ("𝓩"))
   `("MCa" . ("𝓪"))
   `("MCb" . ("𝓫"))
   `("MCc" . ("𝓬"))
   `("MCd" . ("𝓭"))
   `("MCe" . ("𝓮"))
   `("MCf" . ("𝓯"))
   `("MCg" . ("𝓰"))
   `("MCh" . ("𝓱"))
   `("MCi" . ("𝓲"))
   `("MCj" . ("𝓳"))
   `("MCk" . ("𝓴"))
   `("MCl" . ("𝓵"))
   `("MCm" . ("𝓶"))
   `("MCn" . ("𝓷"))
   `("MCo" . ("𝓸"))
   `("MCp" . ("𝓹"))
   `("MCq" . ("𝓺"))
   `("MCr" . ("𝓻"))
   `("MCs" . ("𝓼"))
   `("MCt" . ("𝓽"))
   `("MCu" . ("𝓾"))
   `("MCv" . ("𝓿"))
   `("MCw" . ("𝔀"))
   `("MCx" . ("𝔁"))
   `("MCy" . ("𝔂"))
   `("MCz" . ("𝔃"))
   `("MfA" . ("𝔄"))
   `("MfB" . ("𝔅"))
   `("MfD" . ("𝔇"))
   `("MfE" . ("𝔈"))
   `("MfF" . ("𝔉"))
   `("MfG" . ("𝔊"))
   `("MfJ" . ("𝔍"))
   `("MfK" . ("𝔎"))
   `("MfL" . ("𝔏"))
   `("MfM" . ("𝔐"))
   `("MfN" . ("𝔑"))
   `("MfO" . ("𝔒"))
   `("MfP" . ("𝔓"))
   `("MfQ" . ("𝔔"))
   `("MfS" . ("𝔖"))
   `("MfT" . ("𝔗"))
   `("MfU" . ("𝔘"))
   `("MfV" . ("𝔙"))
   `("MfW" . ("𝔚"))
   `("MfX" . ("𝔛"))
   `("MfY" . ("𝔜"))
   `("Mfa" . ("𝔞"))
   `("Mfb" . ("𝔟"))
   `("Mfc" . ("𝔠"))
   `("Mfd" . ("𝔡"))
   `("Mfe" . ("𝔢"))
   `("Mff" . ("𝔣"))
   `("Mfg" . ("𝔤"))
   `("Mfh" . ("𝔥"))
   `("Mfi" . ("𝔦"))
   `("Mfj" . ("𝔧"))
   `("Mfk" . ("𝔨"))
   `("Mfl" . ("𝔩"))
   `("Mfm" . ("𝔪"))
   `("Mfn" . ("𝔫"))
   `("Mfo" . ("𝔬"))
   `("Mfp" . ("𝔭"))
   `("Mfq" . ("𝔮"))
   `("Mfr" . ("𝔯"))
   `("Mfs" . ("𝔰"))
   `("Mft" . ("𝔱"))
   `("Mfu" . ("𝔲"))
   `("Mfv" . ("𝔳"))
   `("Mfw" . ("𝔴"))
   `("Mfx" . ("𝔵"))
   `("Mfy" . ("𝔶"))
   `("Mfz" . ("𝔷"))

   ;; Some ISO8859-1 characters.

   `(" "         . (" "))
   `("!"         . ("¡"))
   `("cent"      . ("¢"))
   `("brokenbar" . ("¦"))
   `("degree"    . ("°"))
   `("?"         . ("¿"))
   `("^a_"       . ("ª"))
   `("^o_"       . ("º"))

   ;; Circled, parenthesised etc. numbers and letters.

   `( "(0)" . ,(lean-input-to-string-list " ⓪"))
   `( "(1)" . ,(lean-input-to-string-list "⑴①⒈❶➀➊"))
   `( "(2)" . ,(lean-input-to-string-list "⑵②⒉❷➁➋"))
   `( "(3)" . ,(lean-input-to-string-list "⑶③⒊❸➂➌"))
   `( "(4)" . ,(lean-input-to-string-list "⑷④⒋❹➃➍"))
   `( "(5)" . ,(lean-input-to-string-list "⑸⑤⒌❺➄➎"))
   `( "(6)" . ,(lean-input-to-string-list "⑹⑥⒍❻➅➏"))
   `( "(7)" . ,(lean-input-to-string-list "⑺⑦⒎❼➆➐"))
   `( "(8)" . ,(lean-input-to-string-list "⑻⑧⒏❽➇➑"))
   `( "(9)" . ,(lean-input-to-string-list "⑼⑨⒐❾➈➒"))
   `("(10)" . ,(lean-input-to-string-list "⑽⑩⒑❿➉➓"))
   `("(11)" . ,(lean-input-to-string-list "⑾⑪⒒"))
   `("(12)" . ,(lean-input-to-string-list "⑿⑫⒓"))
   `("(13)" . ,(lean-input-to-string-list "⒀⑬⒔"))
   `("(14)" . ,(lean-input-to-string-list "⒁⑭⒕"))
   `("(15)" . ,(lean-input-to-string-list "⒂⑮⒖"))
   `("(16)" . ,(lean-input-to-string-list "⒃⑯⒗"))
   `("(17)" . ,(lean-input-to-string-list "⒄⑰⒘"))
   `("(18)" . ,(lean-input-to-string-list "⒅⑱⒙"))
   `("(19)" . ,(lean-input-to-string-list "⒆⑲⒚"))
   `("(20)" . ,(lean-input-to-string-list "⒇⑳⒛"))

   `("(a)"  . ,(lean-input-to-string-list "⒜Ⓐⓐ"))
   `("(b)"  . ,(lean-input-to-string-list "⒝Ⓑⓑ"))
   `("(c)"  . ,(lean-input-to-string-list "⒞Ⓒⓒ"))
   `("(d)"  . ,(lean-input-to-string-list "⒟Ⓓⓓ"))
   `("(e)"  . ,(lean-input-to-string-list "⒠Ⓔⓔ"))
   `("(f)"  . ,(lean-input-to-string-list "⒡Ⓕⓕ"))
   `("(g)"  . ,(lean-input-to-string-list "⒢Ⓖⓖ"))
   `("(h)"  . ,(lean-input-to-string-list "⒣Ⓗⓗ"))
   `("(i)"  . ,(lean-input-to-string-list "⒤Ⓘⓘ"))
   `("(j)"  . ,(lean-input-to-string-list "⒥Ⓙⓙ"))
   `("(k)"  . ,(lean-input-to-string-list "⒦Ⓚⓚ"))
   `("(l)"  . ,(lean-input-to-string-list "⒧Ⓛⓛ"))
   `("(m)"  . ,(lean-input-to-string-list "⒨Ⓜⓜ"))
   `("(n)"  . ,(lean-input-to-string-list "⒩Ⓝⓝ"))
   `("(o)"  . ,(lean-input-to-string-list "⒪Ⓞⓞ"))
   `("(p)"  . ,(lean-input-to-string-list "⒫Ⓟⓟ"))
   `("(q)"  . ,(lean-input-to-string-list "⒬Ⓠⓠ"))
   `("(r)"  . ,(lean-input-to-string-list "⒭Ⓡⓡ"))
   `("(s)"  . ,(lean-input-to-string-list "⒮Ⓢⓢ"))
   `("(t)"  . ,(lean-input-to-string-list "⒯Ⓣⓣ"))
   `("(u)"  . ,(lean-input-to-string-list "⒰Ⓤⓤ"))
   `("(v)"  . ,(lean-input-to-string-list "⒱Ⓥⓥ"))
   `("(w)"  . ,(lean-input-to-string-list "⒲Ⓦⓦ"))
   `("(x)"  . ,(lean-input-to-string-list "⒳Ⓧⓧ"))
   `("(y)"  . ,(lean-input-to-string-list "⒴Ⓨⓨ"))
   `("(z)"  . ,(lean-input-to-string-list "⒵Ⓩⓩ"))
   `("y"    . ("ɏ"))

   ;; Mathlib-specific (combinations of) characters.

   `("allf" . ("∀ᶠ"))
   `("all^f" . ("∀ᶠ"))
   `("allm" . ("∀ₘ"))
   `("all_m" . ("∀ₘ"))
   `("Pi0" . ("Π₀"))
   `("P0" . ("Π₀"))
   `("Pi_0" . ("Π₀"))
   `("P_0" . ("Π₀"))
   `("to0" . ("→₀"))
   `("r0" . ("→₀"))
   `("to_0" . ("→₀"))
   `("r_0" . ("→₀"))
   `("finsupp" . ("→₀"))
   `("to1" . ("→₁"))
   `("r1" . ("→₁"))
   `("to_1" . ("→₁"))
   `("r_1" . ("→₁"))
   `("l1" . ("→₁"))
   `("to1s" . ("→₁ₛ"))
   `("r1s" . ("→₁ₛ"))
   `("to_1s" . ("→₁ₛ"))
   `("r_1s" . ("→₁ₛ"))
   `("l1simplefunc" . ("→₁ₛ"))
   `("toa" . ("→ₐ"))
   `("ra" . ("→ₐ"))
   `("to_a" . ("→ₐ"))
   `("r_a" . ("→ₐ"))
   `("alghom" . ("→ₐ"))
   `("tob" . ("→ᵇ"))
   `("rb" . ("→ᵇ"))
   `("to^b" . ("→ᵇ"))
   `("r^b" . ("→ᵇ"))
   `("boundedcontinuousfunction" . ("→ᵇ"))
   `("tol" . ("→ₗ"))
   `("rl" . ("→ₗ"))
   `("to_l" . ("→ₗ"))
   `("r_l" . ("→ₗ"))
   `("linearmap" . ("→ₗ"))
   `("tom" . ("→ₘ"))
   `("rm" . ("→ₘ"))
   `("to_m" . ("→ₘ"))
   `("r_m" . ("→ₘ"))
   `("aeeqfun" . ("→ₘ"))
   `("rp" . ("→ₚ"))
   `("to_p" . ("→ₚ"))
   `("r_p" . ("→ₚ"))
   `("dfinsupp" . ("→ₚ"))
   `("tos" . ("→ₛ"))
   `("rs" . ("→ₛ"))
   `("to_s" . ("→ₛ"))
   `("r_s" . ("→ₛ"))
   `("simplefunc" . ("→ₛ"))
   `("root" . ("√"))
   `("sqrt" . ("√"))
   `("boxmid" . ("◫"))
   `("hcomp" . ("◫"))
   `("Rge0" . ("ℝ≥0"))
   `("R>=0" . ("ℝ≥0"))
   `("nnreal" . ("ℝ≥0"))
   `("Zsqrt" . ("ℤ√"))
   `("zsqrtd" . ("ℤ√"))
   `("liel" . ("⁅"))
   `("[-" . ("⁅"))
   `("bracketl" . ("⁅"))
   `("lier" . ("⁆"))
   `("-]" . ("⁆"))
   `("bracketr" . ("⁆"))
   `("nhds" . ("𝓝"))
   `("nbhds" . ("𝓝"))
   `("X" . ("×"))
   `("vectorproduct" . ("⨯"))
   `("crossproduct" . ("⨯"))
   `("coprod" . ("⨿"))
   `("sigmaobj" . ("∐"))
   `("bigcoprod" . ("∐"))
   `("xf" . ("×ᶠ"))
   `("exf" . ("∃ᶠ"))
   `("specializes" . ("⤳"))
   )
  "A list of translations specific to the Lean input method.
Each element is a pair (KEY-SEQUENCE-STRING . LIST-OF-TRANSLATION-STRINGS).
All the translation strings are possible translations
of the given key sequence; if there is more than one you can choose
between them using the arrow keys.

Note that if you customize this setting you will not
automatically benefit (or suffer) from modifications to its
default value when the library is updated.  If you just want to
add some bindings it is probably a better idea to customize
`lean-input-user-translations'.

These translation pairs are included after those in
`lean-input-user-translations', but before the ones inherited
from other input methods (see `lean-input-inherit').

If you change this setting manually (without using the
customization buffer) you need to call `lean-input-setup' in
order for the change to take effect."
  :group 'lean-input
  :set 'lean-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

(defcustom lean-input-user-translations nil
  "Like `lean-input-translations', but more suitable for user
customizations since by default it is empty.

These translation pairs are included first, before those in
`lean-input-translations' and the ones inherited from other input
methods."
  :group 'lean-input
  :set 'lean-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspecting and modifying translation maps

(defun lean-input-get-translations (qp)
  "Return a list containing all translations from the Quail
package QP (except for those corresponding to ASCII).
Each pair in the list has the form (KEY-SEQUENCE . TRANSLATION)."
  (with-temp-buffer
    (activate-input-method qp) ; To make sure that the package is loaded.
    (unless (quail-package qp)
      (error "%s is not a Quail package." qp))
    (let ((decode-map (list 'decode-map)))
      (quail-build-decode-map (list (quail-map)) "" decode-map 0)
      (cdr decode-map))))

(defun lean-input-show-translations (qp)
  "Display all translations used by the Quail package QP (a string).
\(Except for those corresponding to ASCII)."
  (interactive (list (read-input-method-name
                      "Quail input method (default %s): " "Lean")))
  (let ((buf (concat "*" qp " input method translations*")))
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (quail-insert-decode-map
         (cons 'decode-map (lean-input-get-translations qp)))))))

(defun lean-input-add-translations (trans)
  "Add the given translations TRANS to the Lean input method.
TRANS is a list of pairs (KEY-SEQUENCE . TRANSLATION). The
translations are appended to the current translations."
  (with-temp-buffer
    (dolist (tr (lean-input-concat-map (eval lean-input-tweak-all) trans))
      (quail-defrule (car tr) (cdr tr) "Lean" t))))

(defun lean-input-inherit-package (qp &optional fun)
  "Let the Lean input method inherit the translations from the
Quail package QP (except for those corresponding to ASCII).

The optional function FUN can be used to modify the translations.
It is given a pair (KEY-SEQUENCE . TRANSLATION) and should return
a list of such pairs."
  (let ((trans (lean-input-get-translations qp)))
    (lean-input-add-translations
     (if fun (lean-input-concat-map fun trans)
       trans))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up the input method

(defun lean-input-setup ()
  "Set up the Lean input method based on the customisable
variables and underlying input methods."

  ;; Create (or reset) the input method.
  (with-temp-buffer
    (quail-define-package "Lean" "UTF-8" "∏" t ; guidance
     "Lean input method.
The purpose of this input method is to edit Lean programs, but
since it is highly customisable it can be made useful for other
tasks as well."
     nil nil nil nil nil nil t ; maximum-shortest
     ))

  (lean-input-add-translations
   (mapcar (lambda (tr) (cons (car tr) (vconcat (cdr tr))))
           (append lean-input-user-translations
                   lean-input-translations)))
  (dolist (def lean-input-inherit)
    (lean-input-inherit-package (car def)
                                (eval (cdr def)))))

(defun lean-input-incorporate-changed-setting (sym val)
  "Update the Lean input method based on the customisable
variables and underlying input methods.
Suitable for use in the :set field of `defcustom'."
  (set-default sym val)
  (lean-input-setup))

;; Set up the input method.

(lean-input-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative details

(provide 'lean-input)
;;; lean-input.el ends here

(require 'dash)

(defun lean-input-export-translations ()
  "Export the current translation, (input, output) pairs for
input-method, in a javascript format. It can be copy-pasted to
leanprover.github.io/tutorial/js/input-method.js"
  (interactive)
  (with-current-buffer
      (get-buffer-create "*lean-translations*")
    (let ((exclude-list '("\\newline")))
      (insert "var corrections = {")
      (--each
          (--filter (not (member (car it) exclude-list))
                    (lean-input-get-translations "Lean"))
        (let* ((input (substring (car it) 1))
               (outputs (cdr it)))
          (insert (format "%s:\"" (prin1-to-string input)))
          (cond ((vectorp outputs)
                 (insert (elt outputs 0)))
                (t (insert-char outputs)))
          (insert (format "\",\n" input))))
      (insert "};"))))

(defun lean-input-export-translations-to-stdout ()
  (lean-input-export-translations)
  (with-current-buffer "*lean-translations*"
    (princ (buffer-string))))
