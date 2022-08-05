;; haskell-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'abbrev+)
(require 'common)
(require 'current-column-fixed)
(require 'haskell-completions)
(require 'haskell-misc)
(require 'haskell-snippets)
(require 'v)

;; for ghc flags to OPTIONS_GHC
(require 'shell-completion)

(defun haskell-insert-followed-by-dollar? (pos)
  "Check whether text after POS is followed by $."
  (save-excursion
    (goto-char pos)
    (skip-syntax-forward " >")
    (let ((c (char-after)))
      (and (characterp c)
           (char=? c ?$)))))

(defun haskell--quote-string-for-template-insertion (str)
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defmacro haskell-abbrev+--is-function-available (function-name module-name)
  `(save-excursion
     (save-match-data
       (goto-char (point-min))
       (re-search-forward
        (rx (or ,@(when function-name (list `(seq bow ,function-name eow)))
                ,@(when module-name   (list `(seq bol "import" (* any) ,module-name)))))
        nil
        t))))

(defun haskell-abbrev+--ensure-debug-trace-available ()
  (unless (haskell-abbrev+--is-function-available nil
                                                  (or (seq "qualified" (* any) symbol-start "Debug.Trace" symbol-end)
                                                      (seq symbol-start "Debug.Trace" symbol-end (* any) "qualified")))
    (save-excursion
      (haskell-navigate-imports)
      (if (haskell-ext-tracking-have-import-qualified-post?)
          (insert "import Debug.Trace qualified\n\n")
        (insert "import qualified Debug.Trace\n\n")))))

(defun haskell-insert-general-info-template (_arg monadic? trace-func-name)
  (let* ((start-position (point))
         (insert-dollar?
          (not (haskell-insert-followed-by-dollar? start-position)))
         (start
          (if monadic?
              (lambda ()
                (insert
                 (if trace-func-name
                     trace-func-name
                   (save-match-data
                     (haskell-abbrev+--is-function-available
                      (seq "lift"
                           (or (group-n 1 "IO")
                               (group-n 2 "Base")))
                      (seq "Control.Monad."
                           (or (group-n 1 "IO.Class")
                               (group-n 2 "Base"))))
                     (let ((has-liftio? (match-beginning 1))
                           (has-liftbase? (match-beginning 2)))
                       (cond
                         (has-liftio?   "liftIO $ putStrLn")
                         (has-liftbase? "liftBase $ putStrLn")
                         (t             "putStrLn")))))
                 " $ \""))
            (lambda ()
              (insert (if trace-func-name
                          trace-func-name
                        "trace")
                      " (\""))))
         (end
          (if monadic?
              (lambda ()
                (insert ""))
            (lambda () (insert ") " (if insert-dollar? "$ " "")))))
         (quote-input
          #'haskell--quote-string-for-template-insertion)
         (insert-continuation
          (lambda (should-merge-messages?)
            (if should-merge-messages?
                (delete-char -1)
              (insert " ++ \""))))
         (insert-message
          (lambda (_is-initial-insertion? user-input)
            (insert (format "%s\"" (funcall quote-input user-input)))))
         (insert-variable
          (lambda (is-initial-insertion? user-input)
            (insert
             (format "%s%s = \" ++ show %s"
                     (if is-initial-insertion? "" ", ")
                     (funcall quote-input user-input)
                     (if (string-match-p "[ \t]" user-input)
                         (concat "(" user-input ")")
                       user-input))))))
    (insert-info-template
     :start start
     :end end
     :insert-continuation insert-continuation
     :insert-message insert-message
     :insert-variable insert-variable)))

(defun haskell-insert-trace-template (&optional arg)
  (interactive "P")
  (haskell-abbrev+--ensure-debug-trace-available)
  (haskell-insert-general-info-template arg nil "Debug.Trace.trace"))

(defun haskell-insert-tracem-template (&optional arg)
  (interactive "P")
  (haskell-abbrev+--ensure-debug-trace-available)
  (haskell-insert-general-info-template arg t "Debug.Trace.traceM"))

(defun haskell-insert-monadic-info-template (&optional arg)
  (interactive "P")
  (haskell-insert-general-info-template arg t nil))

(cl-defun haskell-insert-pp-dict-info-template--helper (&key function-name
                                                             make-print-entry
                                                             realign)
  (let ((start-column (indentation-size))
        (user-input nil)
        (make-str
         (lambda (x)
           (concat "\""
                   (haskell--quote-string-for-template-insertion x)
                   "\""))))
    (insert function-name)
    (let ((column-after-pp-dict (current-column-fixed-uncached))
          (header-message (funcall make-str
                                   (read-string-no-default "Message header: "
                                                           nil
                                                           nil
                                                           ""))))
      ;; Decide whether to put message on a separate line or on current line.
      (if (< 80 (+ 1 column-after-pp-dict (length header-message)))
          (progn
            ;; Separate line - otherwise it would go past 80 column.
            (insert "\n")
            (indent-to (+ haskell-indent-offset start-column)))
        (progn
          ;; Same line - will fit into 80 columns.
          (insert " ")))
      (insert header-message "\n")
      (indent-to (+ haskell-indent-offset start-column)))
    (let ((loop-start-position (point))
          (is-first-iteration? t))
      (while (and (setf user-input
                        (read-string-no-default "What to print: "
                                                nil
                                                nil
                                                ""))
                  (not (string= user-input "")))
        (insert (if is-first-iteration?
                    "[ "
                  ", "))
        (insert (funcall make-print-entry (funcall make-str user-input) user-input) "\n")
        (funcall realign loop-start-position (point))
        (indent-to (+ haskell-indent-offset start-column))
        (setf is-first-iteration? nil))
      (insert "]"))))

(defun haskell-insert-pp-dict-info-template ()
  (interactive)
  (haskell-insert-pp-dict-info-template--helper
   :function-name "ppDictHeader"
   :make-print-entry (lambda (x y)
                       (concat x " --> " y))
   :realign #'haskell-align-on-arrows-indent-region))

(defun haskell-insert-pp-info-template ()
  (interactive)
  (haskell-abbrev+--ensure-debug-trace-available)
  (insert "Debug.Trace.trace (renderString $ ")
  (haskell-insert-pp-dict-info-template)
  (insert ") $"))

(defun haskell-insert-monadic-pp-info-template ()
  (interactive)
  (haskell-abbrev+--ensure-debug-trace-available)
  (insert "Debug.Trace.traceM $ renderString $ ")
  (haskell-insert-pp-dict-info-template))

(defun haskell-abbrev+-extract-first-capital-char (qualified-name)
  (when qualified-name
    (if (zerop (length qualified-name))
        qualified-name
      (substring qualified-name 0 1))))

;;;###autoload
(defun haskell-abbrev+-extract-mod-name (qualified-name)
  "Extract module name from QUALIFIED-NAME, e.g. if QUALIFIED-NAME = Foo.Bar
then Bar would be the result."
  (save-match-data
    (if (string-match "\\`\\(?:[A-Z][a-zA-Z0-9_']*\\.\\)+\\([A-Z][a-zA-Z0-9_']*\\)\\'"
                      qualified-name)
        (match-string-no-properties 1 qualified-name)
      qualified-name)))

(defun haskell-abbrev+-align-language-pragmas ()
  (remove-hook
   'yas-after-exit-snippet-hook
   #'haskell-abbrev+-align-language-pragmas
   t)
  (haskell-align-language-pragmas yas-snippet-beg))

(defun haskell-abbrev+--register-alignment-of-language-pragmas ()
  (add-hook 'yas-after-exit-snippet-hook
            #'haskell-abbrev+-align-language-pragmas
            nil ;; append
            t   ;; local
            ))

(defun haskell-insert-language-pragmas ()
  (let ((start (point))
        (ext nil))
    (while (and (setf ext
                      (ivy-yas-completing-prompt "Extension: "
                                                 (get-haskell-language-extensions)))
                (not (string= "" ext)))
      (insert "{-# LANGUAGE " ext " #-}")
      (haskell-align-language-pragmas start)
      (insert "\n"))))

(defun haskell-abbrev++--import-expand-pred ()
  "Check that the point is at the start of the line."
  (let ((c (char-before (point))))
    ;; By this point we’re assured that we’re
    ;; not in a string or comment via
    ;; ‘abbrev+-do-not-expand-predicate’.
    (or (null c) ;; beginning of buffer
        ;; Expand only in 0th column, i.e. after a newline.
        (eq c ?\n)
        (eq c ?\r) ;; Old Mac newlines, anyone?
        )))

(add-to-list 'ivy-re-builders-alist
             '(haskell-abbrev+--insert-pragma . ivy--regex-fuzzy))

(defun haskell-abbrev+--insert-pragma ()
  (insert "{-# ")
  (let ((pragma (ivy-read "Pragma: "
                          haskell-completions--pragma-names
                          :predicate nil
                          :require-match t
                          :initial-input nil
                          :history nil
                          :caller 'haskell-abbrev+--insert-pragma)))
    (insert pragma)
    (cond
      ((string-match-p haskell-regexen/pragma-without-args-re pragma)
       (insert " #-}"))
      ((string-match-p haskell-regexen/language-pragma-name pragma)
       (yas-expand-snippet " $\{1:\$\$\(yas-choose-value \(get-haskell-language-extensions\)\)\} #-}\$0"))
      ((string-match-p haskell-regexen/scc-pragma-name pragma)
       (yas-expand-snippet " \"${1:cost center name}\" #-}\$0"))
      (t
       (yas-expand-snippet " $1 #-}$0")))))

(defun haskell-abbrev+--get-ghc-flags ()
  (let ((flags (-mapcat (lambda (x)
                          (cond
                            ((stringp x)
                             (list x))
                            ((and (listp x)
                                  (not (null x)))
                             (let ((head (car x)))
                               (if (listp head)
                                   head
                                 (list head))))
                            (t
                             (error "invalid ghc flag specification, string or list with first string element expected but got: %s"
                                    x))))
                        pcomplete-ghc-flags)))
    (cl-assert (-all? #'stringp flags))
    flags))

(defun haskell-insert-qualified-import ()
  (yas-expand-snippet
   (if (haskell-ext-tracking-have-import-qualified-post?)
       "import $1 qualified as ${1:$(haskell-abbrev+-extract-first-capital-char (haskell-abbrev+-extract-mod-name yas-text))}$0"
     "import qualified $1 as ${1:$(haskell-abbrev+-extract-first-capital-char (haskell-abbrev+-extract-mod-name yas-text))}$0")))

(defun haskell-insert-import-block (type-name module-name alias)
  (insert (concat (if type-name
                      (concat "import " module-name " (" type-name ")\n")
                    "")
                  (if (haskell-ext-tracking-have-import-qualified-post?)
                      (concat "import " module-name " qualified as " alias)
                    (concat "import qualified " module-name " as " alias)))))

(defun-once haskell-abbrev+-make-abbrevs
  (let* ((language-snippet "{-# LANGUAGE ${1:\$\$(yas-choose-value (get-haskell-language-extensions))} #-}$0")
         (options-snippet "{-# OPTIONS_GHC ${1:\$\$(yas-choose-value (haskell-abbrev+--get-ghc-flags))} #-}$0")
         (dump-core-snippet
          (concat
           "{-# OPTIONS_GHC "
           (mapconcat #'identity
                      '("-O2" "-ddump-simpl" "-dsuppress-uniques"
                        "-dsuppress-idinfo" "-dsuppress-module-prefixes"
                        "-dsuppress-type-applications" "-dsuppress-coercions"
                        "-dppr-cols200")
                      " ")
           " ${1:-dsuppress-type-signatures }${2:-ddump-to-file }#-}")))
    (cl-assert (-all? #'stringp haskell-completions--pragma-names))
    (let ((non-repl-abbrevs
           (list
            (cons (list "#!")
                  (make-abbrev+-abbreviation
                   :followed-by-space t
                   :action-type 'yas-snippet
                   :action-data haskell-snippets--cabal-run-header--body
                   :predicate #'bobp))
            (cons (list "##")
                  (make-abbrev+-abbreviation
                   :followed-by-space t
                   :action-type 'function-with-side-effects
                   :action-data #'haskell-abbrev+--insert-pragma))
            (cons (list "#scc"
                        "##scc"
                        "# scc"
                        "## scc")
                  (make-abbrev+-abbreviation
                   :followed-by-space t
                   :action-type 'yas-snippet
                   :action-data "{-# SCC \"${1:cost center name}\" #-}$0"))
            (cons (list "#l"
                        "#lang"
                        "##l"
                        "##lang")
                  (make-abbrev+-abbreviation
                   :followed-by-space t
                   :action-type 'yas-snippet
                   :action-data language-snippet
                   :on-successful-expansion #'haskell-abbrev+--register-alignment-of-language-pragmas))
            (cons (list "#ll"
                        "#llang"
                        "##ll"
                        "##llang")
                  (make-abbrev+-abbreviation
                   :followed-by-space t
                   :action-type 'function-with-side-effects
                   :action-data #'haskell-insert-language-pragmas))
            (cons (list "#o"
                        "#opt"
                        "#opts"
                        "##o"
                        "##opt"
                        "##opts")
                  (make-abbrev+-abbreviation
                   :followed-by-space t
                   :action-type 'yas-snippet
                   :action-data options-snippet))
            (cons (list "#d"
                        "#dump"
                        "#dump-core"
                        "##d"
                        "##dump"
                        "##dump-core")
                  (make-abbrev+-abbreviation
                   :action-type 'yas-snippet
                   :action-data dump-core-snippet))))
          (plain-abbrevs
           (append
            (list (cons (list "hpf"
                              "hprf")
                        (make-abbrev+-abbreviation
                         :action-type 'literal-string
                         :action-data "hPrintf"))
                  (cons (list "pf"
                              "prf")
                        (make-abbrev+-abbreviation
                         :action-type 'literal-string
                         :action-data "printf"))
                  (cons (list "pn"
                              "psn"
                              "pln"
                              "psln")
                        (make-abbrev+-abbreviation
                         :action-type 'literal-string
                         :action-data "putStrLn"))
                  (cons (list "hpn"
                              "hpsn"
                              "hpln"
                              "hpsln")
                        (make-abbrev+-abbreviation
                         :action-type 'literal-string
                         :action-data "hPutStrLn"))
                  (cons (list "hps")
                        (make-abbrev+-abbreviation
                         :action-type 'literal-string
                         :action-data "hPutStr"))

                  (cons (make-abbrev+-prefixes "import" 1)
                        (make-abbrev+-abbreviation
                         :action-type 'literal-string
                         :action-data "import"
                         :predicate #'haskell-abbrev++--import-expand-pred))

                  (cons (--mapcat (list (concat it "q") (concat "q" it))
                                  (make-abbrev+-prefixes "import" 1))
                        (make-abbrev+-abbreviation
                         :action-type 'function-with-side-effects
                         :action-data #'haskell-insert-qualified-import
                         :predicate #'haskell-abbrev++--import-expand-pred))

                  (cons (list "pp"
                              "ppdh"
                              "ppdict"
                              "ppDict"
                              "ppdictheader"
                              "ppdictHeader"
                              "ppDictheader"
                              "ppDictHeader")
                        (make-abbrev+-abbreviation
                         :action-type 'function-with-side-effects
                         :action-data #'haskell-insert-pp-dict-info-template))
                  (cons (list "infopp"
                              "tracepp"
                              "ppinfo"
                              "pptrace")
                        (make-abbrev+-abbreviation
                         :action-type 'function-with-side-effects
                         :action-data #'haskell-insert-pp-info-template))
                  (cons (list "ppinfom"
                              "ppinfoM"
                              "pptracem"
                              "pptraceM"
                              "infompp"
                              "infoMpp"
                              "infoppm"
                              "infoppM"
                              "tracempp"
                              "traceMpp"
                              "traceppm"
                              "traceppM")
                        (make-abbrev+-abbreviation
                         :action-type 'function-with-side-effects
                         :action-data #'haskell-insert-monadic-pp-info-template))
                  (cons (list "trace"
                              "info")
                        (make-abbrev+-abbreviation
                         :action-type 'function-with-side-effects
                         :action-data #'haskell-insert-trace-template))
                  (cons (list "tracem"
                              "traceM")
                        (make-abbrev+-abbreviation
                         :action-type 'function-with-side-effects
                         :action-data #'haskell-insert-tracem-template))
                  (cons (list "infom"
                              "infoM")
                        (make-abbrev+-abbreviation
                         :action-type 'function-with-side-effects
                         :action-data #'haskell-insert-monadic-info-template)))
            (--map
             (cl-destructuring-bind (suffix module-name type-name alias full-match?) it
               (cons (--map (concat it suffix)
                            (if full-match?
                                (list "import")
                              (make-abbrev+-prefixes "import" 2)))
                     (make-abbrev+-abbreviation
                      :action-type 'function-with-side-effects-and-args
                      :action-data (list #'haskell-insert-import-block type-name module-name alias)
                      :predicate #'haskell-abbrev++--import-expand-pred)))
             '(("m"   "Data.Map.Strict"               "Map"          "M"   nil)
               ("s"   "Data.Set"                      "Set"          "S"   nil)
               ("v"   "Data.Vector"                   "Vector"       "V"   nil)
               ("vp"  "Data.Vector.Primitive"         nil            "VP"  nil)
               ("vpm" "Data.Vector.Primitive.Mutable" nil            "VPM" nil)
               ("vs"  "Data.Vector.Storable"          nil            "VS"  nil)
               ("vsm" "Data.Vector.Storable.Mutable"  nil            "VSM" nil)
               ("u"   "Data.Vector.Unboxed"           nil            "U"   nil)
               ("vu"  "Data.Vector.Unboxed"           nil            "U"   nil)
               ("um"  "Data.Vector.Unboxed.Mutable"   nil            "UM"  nil)
               ("vum" "Data.Vector.Unboxed.Mutable"   nil            "UM"  nil)
               ("im"  "Data.IntMap"                   "IntMap"       "IM"  nil)
               ("is"  "Data.IntSet"                   "IntSet"       "IS"  nil)
               ("hm"  "Data.HashMap.Strict"           "HashMap"      "HM"  nil)
               ("hs"  "Data.HashSet"                  "HashSet"      "HS"  nil)
               ("ne"  "Data.List.NonEmpty"            "NonEmpty(..)" "NE"  nil)
               ("l"   "Data.List"                     nil            "L"   nil)
               ("dl"  "Data.DList"                    "DList"        "DL"  nil)
               ;; Without full-match? flag this would be parsed as [impor][t] and will shadow
               ;; abbrev for real import.
               ("t"   "Data.Text"                     "Text"         "T"   t)
               ("tl"  "Data.Text.Lazy"                nil            "TL"  t)
               ("bs"  "Data.ByteString"               "ByteString"   "BS"  nil)
               ("bl"  "Data.ByteString.Lazy"          nil            "BSL" nil)
               ("bsl" "Data.ByteString.Lazy"          nil            "BSL" nil)
               ("c8"  "Data.ByteString.Char8"         "ByteString"   "C8"  nil)
               ("cl8" "Data.ByteString.Lazy.Char8"    nil            "CL8" nil))))))
      (cons (abbrev+-compile-abbreviations plain-abbrevs)
            (abbrev+-compile-abbreviations
             (append non-repl-abbrevs plain-abbrevs))))))

(defun haskell-space-abbrev+ (&optional dont-expand)
  (interactive "*P")
  (when (or dont-expand
            (not (abbrev+-expand)))
    (haskell-space-with-block-indent)))

(defun haskell-abbrev+-setup (repl)
  (setf abbrev+-abbreviations (let ((abbrevs (haskell-abbrev+-make-abbrevs)))
                                (if repl
                                    (car abbrevs)
                                  (cdr abbrevs)))
        abbrev+-do-not-expand-predicate #'point-inside-string-or-comment?)
  (unless repl
    (haskell-snippets-install! major-mode))
  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" haskell-space-abbrev+)))

(provide 'haskell-abbrev+)

;; Local Variables:
;; End:

;; haskell-abbrev+.el ends here
