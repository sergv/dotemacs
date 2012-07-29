;; haskell-abbrev+.el ---

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
         (list "^hpr?f$"                  "hPrintf")
         (list "^pr?f$"                   "printf")
         (list "^ps?l?n$"                 "putStrLn")
         (list "^p\\(s\\|l\\)\\{1,2\\}$"  "putStr")
         (list "^hps?l?n$"                "hPutStrLn")
         (list "^hp\\(s\\|l\\)\\{1,2\\}$" "hPutStr")
         ;; (cons "main"                   "main = do\n   ")
         (list "^imp\\(ort\\)?$"         "import")
         (list "^imp\\(ort\\)?q$"         "import qualified")
         (list "^qimp\\(ort\\)?$"         "import qualified")
         ;(cons "##"                     "{-#  #-}")
         ))
  (def-keys-for-map1 vim:insert-mode-local-keymap
    (("SPC"   abbrev+-insert-space-or-expand-abbrev))))

(provide 'haskell-abbrev+)

;; Local Variables:
;; lexical-binding: t
;; End:

;; haskell-abbrev+.el ends here
