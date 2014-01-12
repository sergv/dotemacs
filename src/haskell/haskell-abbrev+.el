;; haskell-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(require 'abbrev+)
(require 'haskell-misc)

(defun haskell-abbrev+-extract-mod-name (qualified-name)
  "Extract module name from QUALIFIED-NAME, e.g. if QUALIFIED-NAME = Foo.Bar
then Bar would be the result."
  (save-match-data
    (if (string-match "\\(?:[A-Z][a-zA-Z0-9_']+\\.\\)+\\([A-Z][a-zA-Z0-9_']+\\)"
                      qualified-name)
      (match-string 1 qualified-name)
      nil)))

(defun haskell-abbrev+-setup ()
  (let* ((import-expand-pred (lambda () (let ((c (char-before (point))))
                                     (or (null? c)
                                         (not (char=? c ?:))))))
         (haskell-extensions (append (map #'first haskell-language-extensions)
                                     (remove nil
                                             (map #'third haskell-language-extensions))))
         (expand-qualified-import-snippet-action
          (lambda () (yas-expand-snippet "import qualified $1 as ${2:$(haskell-abbrev+-extract-mod-name yas/text)}$0")))
         (language-snippet (format "{-# LANGUAGE ${1:$$(yas-choose-value '%S)} #-}$0"
                                   haskell-extensions)))
    (setf abbrev+-skip-syntax '("w_" "^ >")
          abbrev+-abbreviations
          (list
           ;; (cons "pwd" #'(lambda () (expand-file-name default-directory)))
           (list "^hpr?f$"                    "hPrintf")
           (list "^pr?f$"                     "printf")
           (list "^\\(?:ps\\|p\\)l?n$"        "putStrLn")
           (list "^hps?l?n$"                  "hPutStrLn")
           (list "^hp\\(?:s\\|l\\)\\{1,2\\}$" "hPutStr")
           (cons "main"
                 (list
                  (lambda () (yas-expand-snippet
                         (concat "main :: IO ()\nmain = do\n"
                                 (make-string haskell-indent-offset ?\s)
                                 "$1")))))

           (list (concat "^" (make-re-with-optional-suffix "import" 2) "$")
                 "import"
                 import-expand-pred)
           (list (concat "^" (make-re-with-optional-suffix "import" 2) "q$")
                 (list expand-qualified-import-snippet-action)
                 import-expand-pred)
           (list (concat "^q" (make-re-with-optional-suffix "import" 2) "$")
                 (list expand-qualified-import-snippet-action)
                 import-expand-pred)
           (list "##"
                 (list
                  (lambda () (yas-expand-snippet "{-# $1 #-}$0"))))
           (list "#lang"
                 (list
                  (lambda () (yas-expand-snippet language-snippet)))))))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'haskell-abbrev+)

;; Local Variables:
;; End:

;; haskell-abbrev+.el ends here
