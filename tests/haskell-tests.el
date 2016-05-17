;; haskell-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 26 November 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'ert)

(ert-deftest haskell-tests/abbrev+-extract-module-name ()
  (should (string= (haskell-abbrev+-extract-mod-name "Foo.Bar")
                   "Bar"))
  (should (string= (haskell-abbrev+-extract-mod-name "Foo.Bar.Baz")
                   "Baz"))
  (should (string= (haskell-abbrev+-extract-mod-name "Foo'.Bar2.Baz_3.Quux")
                   "Quux")))

(defmacro haskell-tests--setup-buffer (setup contents expected)
  (declare (indent 1))
  `(save-match-data
     (with-temp-buffer
       (insert ,contents)
       (goto-char (point-min))
       (re-search-forward "_|_")
       (replace-match "")
       (haskell-mode)
       (font-lock-fontify-buffer)
       ,setup
       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                      ,expected)))))

(ert-deftest haskell-tests/haskell-align-language-pragmas ()
  (haskell-tests--setup-buffer
      (haskell-align-language-pragmas (point))
    "

{-# language
             Safe, FlexibleContexts _|_ #-}

"
    "

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe             #-}

")

  (haskell-tests--setup-buffer
      (haskell-align-language-pragmas (point))
    "
-- foobar
{-# language
             Safe, FlexibleContexts _|_ #-}

"
    "
-- foobar
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe             #-}

")

  (haskell-tests--setup-buffer
      (haskell-align-language-pragmas (point))
    "

{-# language
   Safe _|_
 , FlexibleContexts
 #-}

"
    "

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe             #-}

")

  (haskell-tests--setup-buffer
      (haskell-align-language-pragmas (point))
    "
-- foo
{-# language
   Safe _|_
 , FlexibleContexts
 #-}
-- bar
"
    "
-- foo
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe             #-}
-- bar
")

  (haskell-tests--setup-buffer
      (haskell-align-language-pragmas (point))

    "{-# LANGUAGE Safe #-}
{-# LANGUAGE AlternativeLayoutRule _|_ #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}"
    "{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE AlternativeLayoutRule #-}
{-# LANGUAGE Safe                  #-}")

  )

(ert "haskell-tests/.*")

;; (setf haskell-tests/tests
;;       '(haskell-tests/abbrev+-extract-module-name))
;;
;; (let ((ert-debug-on-error nil))
;;   (eproj-reset-projects)
;;   (ert (join-lines (-map (comp #'regexp-quote #'symbol->string)
;;                          haskell-tests/tests)
;;                    "\\|")
;;        ;; "haskell-tests/.*"
;;        )
;;   nil)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; haskell-tests.el ends here
