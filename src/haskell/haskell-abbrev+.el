;; haskell-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(require 'abbrev+)
(require 'haskell-misc)
;; for ghc flags to OPTIONS_GHC
(require 'shell-completion)

(define-print-info-skeleton
    haskell-debug-message-skeleton
  :doc "Insert call to trace to print some variables and messages
while interactively prompting for variables/messages."
  :print-begin "trace (printf "
  :print-end ") "

  :indent-after-func nil
  :insert-newline-before-var-list nil

  :format-print-value "%s"
  :format-string-start "\""
  ;; it is indented to not end format string with \n
  ;; since trace macro will add newline per se
  :format-string-end "\" "
  :name-value-delimiter " = "

  :insert-entity-name-procedure (constantly nil)
  :make-variable-list (lambda (vars)
                        (join-lines (map (lambda (v)
                                           (concat "(show $ " v ")"))
                                         vars)
                                    " ")))

(defun haskell-abbrev+-extract-mod-name (qualified-name)
  "Extract module name from QUALIFIED-NAME, e.g. if QUALIFIED-NAME = Foo.Bar
then Bar would be the result."
  (save-match-data
    (if (string-match "^\\(?:[A-Z][a-zA-Z0-9_']*\\.\\)+\\([A-Z][a-zA-Z0-9_']*\\)$"
                      qualified-name)
      (match-string 1 qualified-name)
      qualified-name)))

(defun* haskell-abbrev+-setup (&key (repl nil))
  (let* ((import-expand-pred (lambda () (let ((c (char-before (point))))
                                     (or (null? c)
                                         (not (char=? c ?:))))))
         (haskell-extensions (append (map #'first haskell-language-extensions)
                                     (remove nil
                                             (map #'third haskell-language-extensions))))
         (expand-qualified-import-snippet-action
          (lambda () (yas-expand-snippet "import qualified $1 as ${1:$(haskell-abbrev+-extract-mod-name yas-text)}$0")))
         (language-snippet (format "{-# LANGUAGE ${1:$\$(yas-choose-value '%S)} #-}$0"
                                   haskell-extensions))
         (ghc-flags (map (lambda (x)
                           (cond
                             ((string? x)
                              x)
                             ((and (list? x)
                                   (not (null? x)))
                              (first x))
                             (else
                              (error "invalid ghc flag specification, string or list with first string element expected but got: %s"
                                     x))))
                         pcomplete-ghc-flags))
         (options-snippet (format "{-# OPTIONS_GHC ${1:$\$(yas-choose-value '%S)} #-}$0"
                                  ghc-flags))
         (default-options-snippet (format "{-# OPTIONS_GHC -Wall -fwarn-monomorphism-restriction ${1:$\$(yas-choose-value '%S)} #-}$0"
                                          ghc-flags)))
    (setf abbrev+-skip-syntax '("w_" "^ >")
          abbrev+-abbreviations
          (append
           (if (not repl)
             (list
              (list "main"
                    (list
                     (lambda () (yas-expand-snippet
                            (concat "main :: IO ()\nmain = do\n"
                                    (make-string haskell-indent-offset ?\s) "$1")))))
              (list "##"
                    (list
                     (lambda () (yas-expand-snippet "{-# $1 #-}$0"))))
              (list "\\(?:#lang\\|langext\\)"
                    (list
                     (lambda () (yas-expand-snippet language-snippet))))
              (list "#opts?"
                    (list
                     (lambda () (yas-expand-snippet options-snippet))))
              (list "#\\(?:opts?-def\\|dopts?\\)"
                    (list
                     (lambda () (yas-expand-snippet default-options-snippet)))))
             nil)
           (list
            ;; (cons "pwd" #'(lambda () (expand-file-name default-directory)))
            (list "^hpr?f$"                    "hPrintf")
            (list "^pr?f$"                     "printf")
            (list "^\\(?:ps\\|p\\)l?n$"        "putStrLn")
            (list "^hps?l?n$"                  "hPutStrLn")
            (list "^hp\\(?:s\\|l\\)\\{1,2\\}$" "hPutStr")

            (list (concat "^" (make-re-with-optional-suffix "import" 2) "$")
                  "import"
                  import-expand-pred)
            (list (concat "^" (make-re-with-optional-suffix "import" 2) "m$")
                  "import Data.Map (Map)\nimport qualified Data.Map as M"
                  import-expand-pred)
            (list (concat "^" (make-re-with-optional-suffix "import" 2) "q$")
                  (list expand-qualified-import-snippet-action)
                  import-expand-pred)
            (list (concat "^q" (make-re-with-optional-suffix "import" 2) "$")
                  (list expand-qualified-import-snippet-action)
                  import-expand-pred)
            (list "\\<info\\>"
                  (list
                   #'haskell-debug-message-skeleton)
                  (lambda () (not (point-inside-string-or-comment?))))))))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'haskell-abbrev+)

;; Local Variables:
;; End:

;; haskell-abbrev+.el ends here
