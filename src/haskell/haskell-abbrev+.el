;; haskell-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 July 2012
;; Description:

(require 'abbrev+)


(defun haskell-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w_" "^ >")
        abbrev+-abbreviations
        (list
         ;; (cons "pwd" #'(lambda () (expand-file-name default-directory)))
         (list "^hpr?f$"                    "hPrintf")
         (list "^pr?f$"                     "printf")
         (list "^\\(?:ps\\|p\\)l?n$"        "putStrLn")
         (list "^hps?l?n$"                  "hPutStrLn")
         (list "^hp\\(?:s\\|l\\)\\{1,2\\}$" "hPutStr")
         ;; (cons "main"                       "main = do\n   ")



         (list (concat "^" (make-re-with-optional-suffix "import" 1) "$") "import")
         (list (concat "^" (make-re-with-optional-suffix "import" 1) "q$") "import qualified")
         (list (concat "^q" (make-re-with-optional-suffix "import" 1) "$") "import qualified")
         (list "##"
               (list
                (lambda () (yas-expand-snippet "{-# $1 #-}$0"))))
         (list "#lang"
               (list
                (lambda () (yas-expand-snippet "{-# LANGUAGE $1 #-}$0"))))))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'haskell-abbrev+)

;; Local Variables:
;; End:

;; haskell-abbrev+.el ends here
