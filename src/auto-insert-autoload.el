;; auto-insert-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 16 August 2023
;; Description:

(setf auto-insert-directory (concat +resources-path+ "/auto-insert")
      auto-insert 'other
      auto-insert-query nil
      auto-insert-alist
      (mapcar (lambda (spec)
                (pcase spec
                  (`(,filename-regexp ,insert-template)
                   (cons filename-regexp
                         (vector insert-template #'auto-insert-update)))))
              '(("\\.eproj-info\\'" "insert.eproj-info")
                ("\\.gitignore\\'"  "insert.gitignore")
                ("LICENSE\\'"       "insert.license-apache-2.0")

                ("tests?[/\\].*Test[^/\\]*\\.hs\\'"               "insert-haskell-test.hs")
                ("TestMain\\.hs\\'"                               "insert-haskell-test.hs")
                ("bench\\(?:marks\\)?[/\\].*Bench[^/\\]*\\.hs\\'" "insert-haskell-bench.hs")
                ("Bench\\(?:Main\\)?\\.hs\\'"                     "insert-haskell-bench.hs")
                ("\\(?:exes?[/\\][^/\\]+\\|[/\\]Main\\)\\.hs\\'"  "insert-haskell-exe.hs")
                ("\\.\\(?:hs\\(?:c\\)?\\|chs\\)\\'"               "insert.hs.template")
                ("\\.hsig\\'"                                     "insert.hsig.template")
                ("\\.cabal\\'"                                    "insert.cabal.template")
                ("stack\\.yaml\\'"                                "insert-stack.yaml.template")
                ("cabal\\.project\\(?:\\.local\\)?\\'"            "insert-cabal-project")

                ("\\.rs\\'"         "insert.rs")
                ("\\.awk\\'"        "insert.awk")
                ("\\.h\\'"          "insert.h")
                ("\\.html?\\'"      "insert.html")
                ("\\.org\\'"        "insert.org")
                ("\\.py\\'"         "insert.py")
                ("\\.sh\\'"         "insert.sh")
                ("\\.snip\\'"       "insert.snip")
                ("\\.tex\\'"        "insert.tex")
                ("\\.xhtml?\\'"     "insert.xhtml")

                ("AndroidManifest.xml\\'"      "insert-android-manifest.xml")
                ("/res/drawable.*/.*\\.xml\\'" "insert-android-drawable.xml")
                ("/res/layout.*/.*\\.xml\\'"   "insert-android-layout.xml")
                ("/res/menu.*/.*\\.xml\\'"     "insert-android-menu.xml")
                ("/res/values.*/.*\\.xml\\'"   "insert-android-values.xml")

                ("\\.clj\\'"        "insert.clj")
                ("\\.el\\'"         "insert.el")
                ("\\.scm\\'"        "insert.scm")
                ("\\.\\(?:c?l\\|asdf?\\|li?sp\\|clisp\\)\\'"
                 "insert.lisp"))))

(provide 'auto-insert-autoload)

;; Local Variables:
;; End:

;; auto-insert-autoload.el ends here
