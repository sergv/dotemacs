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
(require 'haskell-completions)

(defun haskell-insert-followed-by-dollar? (pos)
  "Check whether text after POS is followed by $."
  (save-excursion
    (goto-char pos)
    (skip-syntax-forward " >")
    (let ((c (char-after)))
      (and (characterp c)
           (char=? c ?$)))))

(defun haskell-insert-info-template (&optional arg monadic?)
  (interactive "P")
  (let* ((start-position (point))
         (user-input nil)
         (initial-insertion t)
         (is-message?
          (lambda (x)
            (and (not (zerop (length x)))
                 (or (char= ?\s (aref x 0))
                     (char= ?\t (aref x 0))))))
         (prompt-user
          (lambda ()
            (read-string-no-default "Variable or message starting with space: "
                                    nil
                                    nil
                                    "")))
         (quote-input
          (lambda (x)
            (replace-regexp-in-string "\"" "\\\"" x)))
         (start
          (if monadic?
            (let ((has-liftio?
                   (save-match-data
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward "\\<liftIO\\>\\|^import.*Control\\.Monad\\.IO\\.Class" nil t)))))
              (if has-liftio?
                "liftIO $ putStrLn $ "
                "putStrLn $ "))
            "trace ("))
         (insert-dollar?
          (not (haskell-insert-followed-by-dollar? start-position)))
         (end
          (if monadic?
            ""
            (concat ") " (if insert-dollar? "$ " ""))))
         (prev-was-message? nil))
    (insert start)

    (while (and (setf user-input (funcall prompt-user))
                (< 0 (length user-input)))
      (let* ((current-is-message? (funcall is-message? user-input))
             (should-merge-messages? prev-was-message?))
        (if initial-insertion
          (insert "\"")
          (if should-merge-messages?
            (delete-backward-char 1)
            (insert " ++ \"")))
        (insert
         (if current-is-message?
           (format "%s\""
                   (funcall quote-input
                            (replace-regexp-in-string "^[ \t]" "" user-input)))
           (format "%s%s = \" ++ show %s"
                   (if initial-insertion "" ", ")
                   (funcall quote-input user-input)
                   (if (string-match-pure? "[ \t]" user-input)
                     (concat "(" user-input ")")
                     user-input))))
        (setf prev-was-message? current-is-message?))
      (setf initial-insertion nil))
    (insert end)))

(defun haskell-insert-complex-info-template (&optional arg monadic?)
  (interactive "P")
  (let* ((start-position (point))
         (user-input nil)
         (initial-insertion t)
         (is-message?
          (lambda (x)
            (and (not (zerop (length x)))
                 (or (char= ?\s (aref x 0))
                     (char= ?\t (aref x 0))))))
         (prompt-user
          (lambda ()
            (read-string-no-default "Variable or message starting with space: "
                                    nil
                                    nil
                                    "")))
         (has-liftio?
          (save-match-data
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "\\<liftIO\\>\\|^import.*Control\\.Monad\\.IO\\.Class" nil t))))
         (start
          (if monadic?
            (if has-liftio?
              "liftIO $ putStrLn $ "
              "putStrLn $ ")
            "trace ("))
         (insert-dollar?
          (not (haskell-insert-followed-by-dollar? start-position)))
         (end
          (if monadic?
            ""
            (concat ") " (if insert-dollar? "$ " "")))))
    (insert start
            "intercalate \", \" ["
            ;; get name of function containing point
            ;; this form evaluates to string which would be function name
            ;; (funcall ,(funcall make-func-call make-entity-name)
            ;;          start-position)
            )

    (while (and (setf user-input (funcall prompt-user))
                (< 0 (length user-input)))
      (unless initial-insertion
        (insert ", "))
      (if (funcall is-message? user-input)
        (insert "\""
                (replace-regexp-in-string "^[ \t]+" "" user-input)
                "\"")
        (insert "\""
                user-input
                " = \" ++ "
                "show "
                (if (string-match-pure? "[ \t]" user-input)
                  (concat "(" user-input ")")
                  user-input)))
      (setf initial-insertion nil))
    (insert "]"
            end)))

(defun haskell-insert-monadic-info-template (&optional arg)
  (interactive "P")
  (haskell-insert-info-template arg t))

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
         (expand-qualified-import-snippet-action
          (lambda () (yas-expand-snippet "import qualified $1 as ${1:$(haskell-abbrev+-extract-first-capital-char (haskell-abbrev+-extract-mod-name yas-text))}$0")))
         (language-snippet (format "%s ${1:$\$(yas-choose-value '%S)} #-}$0"
                                   haskell-abbrev+/language-pragma-prefix
                                   haskell-extensions))
         (pragma-snippet (format "{-# ${1:$\$(yas-choose-value '%S)} $2 #-}$0"
                                 (remove-duplicates
                                  (sort
                                   (cons "SCC" haskell-completions--pragma-names)
                                   #'string<))))
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
           (if (not repl)
             (list
              (list "main"
                    (list
                     (lambda ()
                       (let ((indent (make-string haskell-indent-offset ?\s)))
                         (yas-expand-snippet
                          (concat "main :: IO ()\nmain = do\n"
                                  indent "$1\n"
                                  indent "return ()")))))
                    #'point-not-inside-string-or-comment?)
              (list "## *"
                    (list
                     (lambda () (yas-expand-snippet pragma-snippet)))
                    #'point-not-inside-string-or-comment?)
              (list "##?scc *"
                    (list
                     (lambda () (yas-expand-snippet "{-# SCC \"${1:cost center name}\" #-}$0")))
                    #'point-not-inside-string-or-comment?)
              (list "##?dump *"
                    (list
                     (lambda () (yas-expand-snippet "{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 #-}$0")))
                    #'point-not-inside-string-or-comment?)
              (list "\\(?:#lang\\)"
                    (list
                     (lambda () (yas-expand-snippet language-snippet)))
                    #'point-not-inside-string-or-comment?)
              (list "#opts?"
                    (list
                     (lambda () (yas-expand-snippet options-snippet)))
                    #'point-not-inside-string-or-comment?)
              (list "#\\(?:opts?-def\\)"
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
            (list "\\<\\(info\\|trace\\)\\>"
                  (list
                   #'haskell-insert-info-template)
                  #'point-not-inside-string-or-comment?)
            (list "\\<info[Mm]\\>"
                  (list
                   #'haskell-insert-monadic-info-template)
                  #'point-not-inside-string-or-comment?)))))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'haskell-abbrev+)

;; Local Variables:
;; End:

;; haskell-abbrev+.el ends here
