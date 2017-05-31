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
(require 'shm-ast)

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
          (lambda (x)
            (replace-regexp-in-string "\"" "\\\"" x)))
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
                     (if (string-match-pure? "[ \t]" user-input)
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
  (save-current-line-column
   (haskell-align-language-pragmas yas-snippet-beg)))

(defun* haskell-abbrev+-setup (&key (repl nil))
  (add-hook 'yas-after-exit-snippet-hook #'haskell-abbrev+-align-language-pragmas nil t)
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
         (ghc-flags (-map (lambda (x)
                            (cond
                              ((string? x)
                               x)
                              ((and (list? x)
                                    (not (null? x)))
                               (first x))
                              (t
                               (error "invalid ghc flag specification, string or list with first string element expected but got: %s"
                                      x))))
                          pcomplete-ghc-flags))
         (options-snippet (format "{-# OPTIONS_GHC ${1:$\$(yas-choose-value '%S)} #-}$0"
                                  ghc-flags))
         (default-options-snippet (format "{-# OPTIONS_GHC -Wall -fwarn-monomorphism-restriction ${1:$\$(yas-choose-value '%S)} #-}$0"
                                          ghc-flags)))
    (setf abbrev+-skip-syntax '("w_" "^ >" (" " "w_") (" " "^ >"))
          abbrev+-abbreviations
          (append
           (unless repl
             (list
              (make-abbrev+-abbreviation
               :trigger "main"
               :action-type 'yas-snippet
               :action-data
               (let ((indent (make-string haskell-indent-offset ?\s)))
                 (concat "main :: IO ()\nmain = do\n"
                         indent "$1\n"
                         indent "return ()"))
               :predicate
               (lambda ()
                 (and (point-not-inside-string-or-comment?)
                      ;; Only expand if start of the matched text
                      ;; is at beginning of line.
                      (= (line-beginning-position) (point)))))
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
               :trigger "##?dump *"
               :action-type 'yas-snippet
               :action-data "{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 #-}$0"
               :predicate #'point-not-inside-string-or-comment?)
              (make-abbrev+-abbreviation
               :trigger "##?lang "
               :action-type 'yas-snippet
               :action-data language-snippet
               :predicate #'point-not-inside-string-or-comment?)
              (make-abbrev+-abbreviation
               :trigger "#opts?"
               :action-type 'yas-snippet
               :action-data options-snippet
               :predicate #'point-not-inside-string-or-comment?)
              (make-abbrev+-abbreviation
               :trigger "#\\(?:opts?-def\\)"
               :action-type 'yas-snippet
               :action-data default-options-snippet
               :predicate #'point-not-inside-string-or-comment?)))
           (list
            ;; (cons "pwd" #'(lambda () (expand-file-name default-directory)))

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
             :trigger (concat "^" (abbrev+--make-re-with-optional-suffix "import" 2) "m$")
             :action-type 'literal-string
             :action-data "import Data.Map (Map)\nimport qualified Data.Map as M"
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
             :trigger "\\<\\(info\\|trace\\)\\>"
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
             :predicate #'point-not-inside-string-or-comment?)))))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'haskell-abbrev+)

;; Local Variables:
;; End:

;; haskell-abbrev+.el ends here
