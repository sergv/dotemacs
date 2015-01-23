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
(require 'shm-ast)

(defun haskell-debug-message-skeleton-format-print-value (msg)
  (concat "\"" msg " = \" <> show "
          (if (string-match-pure? "[ \t]" msg)
            (concat "(" msg ")")
            msg)))

(define-print-info-skeleton
    haskell-pure-debug-message-skeleton
  :doc "Insert call to trace to print some variables and messages
while interactively prompting for variables/messages."
  :print-begin "trace "
  :print-end " $ "

  :indent-after-func nil
  :insert-newline-before-var-list nil

  :format-print-value haskell-debug-message-skeleton-format-print-value

  :format-string-start "("
  :format-string-end ")"
  :msg-transform (lambda (x) (concat "\"" x "\""))
  :variable-delimiter " <> \", \" <> "
  :message-delimiter " <> \"; \" <> "

  :insert-entity-name-procedure (constantly nil)
  :make-variable-list (constantly nil))

(define-print-info-skeleton
    haskell-monadic-debug-message-skeleton
  :doc "Insert call to trace to print some variables and messages
while interactively prompting for variables/messages."
  :print-begin "liftIO $ putStrLn $ "
  :print-end ""

  :indent-after-func nil
  :insert-newline-before-var-list nil

  :format-print-value haskell-debug-message-skeleton-format-print-value

  :format-string-start ""
  :format-string-end ""
  :msg-transform (lambda (x) (concat "\"" x "\""))
  :variable-delimiter " <> \", \" <> "
  :message-delimiter " <> \"; \" <> "

  :insert-entity-name-procedure (constantly nil)
  :make-variable-list (constantly nil))

(defun haskell-debug-message-skeleton ()
  (interactive)
  ;; don't insert monadic version if we're not in some
  ;; do block
  (let ((inside-immediate-do-block?
         (eq 'Do
             (shm-node-cons
              (cdr
               (shm-current-node-pair))))))
    (if inside-immediate-do-block?
      (haskell-monadic-debug-message-skeleton)
      (haskell-pure-debug-message-skeleton))))

(defun haskell-abbrev+-extract-first-capital-char (qualified-name)
  (when qualified-name
    (if (zerop (length qualified-name))
      qualified-name
      (substring qualified-name 0 1))))

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
                                     (and (not (point-inside-string-or-comment?))
                                          (or (null? c)
                                              (not (char=? c ?:)))))))
         (haskell-extensions haskell-language-extensions)
         (expand-qualified-import-snippet-action
          (lambda () (yas-expand-snippet "import qualified $1 as ${1:$(haskell-abbrev+-extract-first-capital-char (haskell-abbrev+-extract-mod-name yas-text))}$0")))
         (language-snippet (format "{-# LANGUAGE ${1:$\$(yas-choose-value '%S)} #-}$0"
                                   haskell-extensions))
         (ghc-flags (map (lambda (x)
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
    (setf abbrev+-skip-syntax '("w_" "^ >")
          abbrev+-abbreviations
          (append
           (if (not repl)
             (list
              (list "main"
                    (list
                     (lambda () (yas-expand-snippet
                                 (concat "main :: IO ()\nmain = do\n"
                                         (make-string haskell-indent-offset ?\s) "$1"))))
                    #'point-not-inside-string-or-comment?)
              (list "##"
                    (list
                     (lambda () (yas-expand-snippet "{-# $1 #-}$0")))
                    #'point-not-inside-string-or-comment?)
              (list "\\(?:#lang\\|langext\\)"
                    (list
                     (lambda () (yas-expand-snippet language-snippet)))
                    #'point-not-inside-string-or-comment?)
              (list "#opts?"
                    (list
                     (lambda () (yas-expand-snippet options-snippet)))
                    #'point-not-inside-string-or-comment?)
              (list "#\\(?:opts?-def\\|dopts?\\)"
                    (list
                     (lambda () (yas-expand-snippet default-options-snippet)))
                    #'point-not-inside-string-or-comment?))
             nil)
           (list
            ;; (cons "pwd" #'(lambda () (expand-file-name default-directory)))
            (list "^hpr?f$"                    "hPrintf"
                  #'point-not-inside-string-or-comment?)
            (list "^pr?f$"                     "printf"
                  #'point-not-inside-string-or-comment?)
            (list "^\\(?:ps\\|p\\)l?n$"        "putStrLn"
                  #'point-not-inside-string-or-comment?)
            (list "^hps?l?n$"                  "hPutStrLn"
                  #'point-not-inside-string-or-comment?)
            (list "^hp\\(?:s\\|l\\)\\{1,2\\}$" "hPutStr"
                  #'point-not-inside-string-or-comment?)

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
                  #'point-not-inside-string-or-comment?)
            (list "\\<infom\\>"
                  (list
                   #'haskell-monadic-debug-message-skeleton)
                  #'point-not-inside-string-or-comment?)))))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'haskell-abbrev+)

;; Local Variables:
;; End:

;; haskell-abbrev+.el ends here
