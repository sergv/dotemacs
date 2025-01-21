;; ptl-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  4 December 2023
;; Description:

(defconst ptl-mode-font-lock-keywords
  `((,(rx symbol-start
          (or "true"
              "false"
              (seq (? (any ?+ ?-))
                   (+ (any (?0 . ?9)))
                   (? "."
                      (+ (any (?0 . ?9))))))
          symbol-end)
     (0 'font-lock-constant-face))
    (,(rx symbol-start
          (or "begin"
              "define"
              "do"
              "export"
              "foreach"
              "if"
              "import"
              "include"
              "lambda"
              "local"
              "match"
              "new"
              "object"
              "public"
              "return"
              "set"
              "this"
              "while")
          symbol-end)
     (0 'font-lock-keyword-face))
    (,(rx symbol-start
          (or "alias"
              "create-glyph"
              "derive-glyphs"
              "glyph-block"
              "glyph-block-export"
              "glyph-block-import"
              "glyph-module"
              "link-reduced-variant"
              "select-variant"
              "set-base-anchor"
              "set-width"
              "turned")
          symbol-end)
     (0 'font-lock-type-face))))

(defconst ptl-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)

    (modify-syntax-entry ?\( "\(\)" table)
    (modify-syntax-entry ?\) "\)\(" table)
    (modify-syntax-entry ?\[ "\(\]" table)
    (modify-syntax-entry ?\] "\)\[" table)
    (modify-syntax-entry ?\{ "\(\}" table)
    (modify-syntax-entry ?\} "\)\{" table)

    (modify-syntax-entry ?_  "w"    table)

    (modify-syntax-entry ?+  "_"    table)
    (modify-syntax-entry ?*  "_"    table)
    (modify-syntax-entry ?/  "_"    table)
    (modify-syntax-entry ?<  "_"    table)
    (modify-syntax-entry ?>  "_"    table)
    (modify-syntax-entry ?=  "_"    table)

    ;; Comments
    (modify-syntax-entry ?-  "_" table)
    (modify-syntax-entry ?#  "<"    table)
    (modify-syntax-entry ?\n ">"    table)

    table))

;;;###autoload
(define-derived-mode ptl-mode prog-mode "PTL"
  "Mode for Iosevka’s ptl files."
  (setq-local comment-start "# "
              commend-end ""
              comment-padding "")

  (setq-local font-lock-defaults
              '(ptl-mode-font-lock-keywords
                nil ;; Do perform fontification of comments and strings.
                nil ;; Do not ignore case.
                nil))

  (indent-tabs-mode +1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ptl\\'" . ptl-mode))

(provide 'ptl-mode)

;; Local Variables:
;; End:

;; ptl-mode.el ends here
