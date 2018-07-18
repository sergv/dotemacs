;; haskell-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(require 'abbrev+)
(require 'common)
(require 'haskell-completions)
(require 'haskell-misc)

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
  (replace-regexp-in-string "\"" "\\\"" str))

(defun haskell-insert-general-info-template (arg monadic? trace-func-name)
  (let* ((start-position (point))
         (insert-dollar?
          (not (haskell-insert-followed-by-dollar? start-position)))
         (start
          (if monadic?
              ;; (if trace-func-name
              ;;     (lambda ()
              ;;       (insert trace-func-name " $ \"")))
              (lambda ()
                (insert
                 (if trace-func-name
                     trace-func-name
                   (let ((has-liftio?
                          (save-match-data
                            (save-excursion
                              (goto-char (point-min))
                              (re-search-forward "\\<liftIO\\>\\|^import.*Control\\.Monad\\.IO\\.Class" nil t)))))
                     (if has-liftio?
                         "liftIO $ putStrLn"
                       "putStrLn")))
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
                (delete-backward-char 1)
              (insert " ++ \""))))
         (insert-message
          (lambda (is-initial-insertion? user-input)
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

(defun haskell-insert-info-template (&optional arg)
  (interactive "P")
  (haskell-insert-general-info-template arg nil "trace"))

(defun haskell-insert-tracem-template (&optional arg)
  (interactive "P")
  (haskell-insert-general-info-template arg t "traceM"))

(defun haskell-insert-monadic-info-template (&optional arg)
  (interactive "P")
  (haskell-insert-general-info-template arg t nil))

(defun* haskell-insert-pp-dict-info-template--helper
    (&key function-name
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
    (let ((column-after-pp-dict (current-column))
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
    (if (string-match "^\\(?:[A-Z][a-zA-Z0-9_']*\\.\\)+\\([A-Z][a-zA-Z0-9_']*\\)$"
                      qualified-name)
        (match-string 1 qualified-name)
      qualified-name)))

(defconst haskell-abbrev+/language-pragma-prefix
  (rx
   "{-# "
   (char ?l ?L)
   (char ?a ?A)
   (char ?n ?N)
   (char ?g ?G)
   (char ?u ?U)
   (char ?a ?A)
   (char ?g ?G)
   (char ?e ?E)))

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

;;;###autoload
(defun* haskell-abbrev+-setup (indent &key (repl nil))
  (let* ((import-expand-pred (lambda () (let ((c (char-before (point))))
                                     (and (not (point-inside-string-or-comment?))
                                          (or (null? c)
                                              (not (char=? c ?:)))))))
         (haskell-extensions haskell-language-extensions)
         (expand-qualified-import-snippet
          "import qualified $1 as ${1:$(haskell-abbrev+-extract-first-capital-char (haskell-abbrev+-extract-mod-name yas-text))}$0")
         (expand-qualified-import-snippet-action
          (lambda () (yas-expand-snippet "import qualified $1 as ${1:$(haskell-abbrev+-extract-first-capital-char (haskell-abbrev+-extract-mod-name yas-text))}$0")))
         (language-snippet (format "{-# LANGUAGE ${1:$\$(yas-choose-value '%S)} #-}$0"
                                   haskell-extensions))
         (pragma-snippet (format "{-# ${1:$\$(yas-choose-value '%S)} $2 #-}$0"
                                 (remove-duplicates-sorting
                                  (cons "SCC" haskell-completions--pragma-names)
                                  #'string=
                                  #'string<)))
         (ghc-flags (-mapcat (lambda (x)
                               (cond
                                 ((string? x)
                                  (list x))
                                 ((and (list? x)
                                       (not (null? x)))
                                  (let ((head (first x)))
                                    (if (list? head)
                                        head
                                      (list head))))
                                 (t
                                  (error "invalid ghc flag specification, string or list with first string element expected but got: %s"
                                         x))))
                             pcomplete-ghc-flags))
         (options-snippet (format "{-# OPTIONS_GHC ${1:$\$(yas-choose-value '%S)} #-}$0"
                                  ghc-flags))
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
    (cl-assert (-all? #'stringp ghc-flags))
    (setf abbrev+-skip-syntax '("w_" "^ >" (" " "w_") (" " "^ >"))
          abbrev+-abbreviations
          (append
           (unless repl
             (list
              (make-abbrev+-abbreviation
               :trigger "## *"
               :action-type 'yas-snippet
               :action-data pragma-snippet
               :predicate #'point-not-inside-string-or-comment?)
              (make-abbrev+-abbreviation
               :trigger "##?scc *"
               :action-type 'yas-snippet
               :action-data "{-# SCC \"${1:cost center name}\" #-}$0"
               :predicate #'point-not-inside-string-or-comment?)
              (make-abbrev+-abbreviation
               :trigger "##?l\\(?:ang\\)? *"
               :action-type 'yas-snippet
               :action-data language-snippet
               :predicate #'point-not-inside-string-or-comment?
               :on-successful-expansion #'haskell-abbrev+--register-alignment-of-language-pragmas)
              (make-abbrev+-abbreviation
               :trigger "##?o\\(?:pts?\\)? *"
               :action-type 'yas-snippet
               :action-data options-snippet
               :predicate #'point-not-inside-string-or-comment?)
              (make-abbrev+-abbreviation
               :trigger "##?d\\(?:ump\\(?:-core\\)?\\)? *"
               :action-type 'yas-snippet
               :action-data dump-core-snippet
               :predicate #'point-not-inside-string-or-comment?)
              (make-abbrev+-abbreviation
               :trigger "@@ *"
               :action-type 'yas-snippet
               :action-data "{-@ $1 @-}$0"
               :predicate #'point-not-inside-string-or-comment?)))
           (list
            (make-abbrev+-abbreviation
             :trigger "^hpr?f$"
             :action-type 'literal-string
             :action-data "hPrintf"
             :predicate #'point-not-inside-string-or-comment?)
            (make-abbrev+-abbreviation
             :trigger "^pr?f$"
             :action-type 'literal-string
             :action-data "printf"
             :predicate #'point-not-inside-string-or-comment?)
            (make-abbrev+-abbreviation
             :trigger "^\\(?:ps\\|p\\)l?n$"
             :action-type 'literal-string
             :action-data "putStrLn"
             :predicate #'point-not-inside-string-or-comment?)
            (make-abbrev+-abbreviation
             :trigger "^hps?l?n$"
             :action-type 'literal-string
             :action-data "hPutStrLn"
             :predicate #'point-not-inside-string-or-comment?)
            (make-abbrev+-abbreviation
             :trigger "^hp\\(?:s\\|l\\)\\{1,2\\}$"
             :action-type 'literal-string
             :action-data "hPutStr"
             :predicate #'point-not-inside-string-or-comment?)

            (make-abbrev+-abbreviation
             :trigger (concat "^" (abbrev+--make-re-with-optional-suffix "import" 2) "$")
             :action-type 'literal-string
             :action-data "import"
             :predicate import-expand-pred)

            (make-abbrev+-abbreviation
             :trigger (concat "^" (abbrev+--make-re-with-optional-suffix "import" 2) "q$")
             :action-type 'yas-snippet
             :action-data expand-qualified-import-snippet
             :predicate import-expand-pred)
            (make-abbrev+-abbreviation
             :trigger (concat "^q" (abbrev+--make-re-with-optional-suffix "import" 2) "$")
             :action-type 'yas-snippet
             :action-data expand-qualified-import-snippet
             :predicate import-expand-pred)


            (make-abbrev+-abbreviation
             :trigger "\\<\\(?:pp\\(?:[dD]ict\\(?:[hH]eader\\)?\\)?\\)\\>"
             :action-type 'function-with-side-effects
             :action-data #'haskell-insert-pp-dict-info-template
             :predicate #'point-not-inside-string-or-comment?)
            (make-abbrev+-abbreviation
             :trigger "\\<\\(?:info\\|trace\\)\\>"
             :action-type 'function-with-side-effects
             :action-data #'haskell-insert-info-template
             :predicate #'point-not-inside-string-or-comment?)
            (make-abbrev+-abbreviation
             :trigger "\\<trace[Mm]\\>"
             :action-type 'function-with-side-effects
             :action-data #'haskell-insert-tracem-template
             :predicate #'point-not-inside-string-or-comment?)
            (make-abbrev+-abbreviation
             :trigger "\\<info[Mm]\\>"
             :action-type 'function-with-side-effects
             :action-data #'haskell-insert-monadic-info-template
             :predicate #'point-not-inside-string-or-comment?))
           (--map
            (destructuring-bind (suffix module-name type-name alias) it
              (make-abbrev+-abbreviation
               :trigger (concat "^" (abbrev+--make-re-with-optional-suffix "import" 2) suffix "$")
               :action-type 'literal-string
               :action-data (concat "import " module-name " (" type-name ")\n"
                                    "import qualified " module-name " as " alias)
               :predicate import-expand-pred))
            '(("m"  "Data.Map.Strict"     "Map"     "M")
              ("s"  "Data.Set"            "Set"     "S")
              ("v"  "Data.Vector"         "Vector"  "V")
              ("im" "Data.IntMap"         "IntMap"  "IM")
              ("is" "Data.IntSet"         "IntSet"  "IS")
              ("hm" "Data.HashMap.Strict" "HashMap" "HM")
              ("hs" "Data.HashSet"        "HashSet" "HS"))))))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'haskell-abbrev+)

;; Local Variables:
;; End:

;; haskell-abbrev+.el ends here
