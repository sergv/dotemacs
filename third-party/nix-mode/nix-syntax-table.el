;; nix-syntax-table.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 13 February 2025
;; Description:

(defconst nix-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "'" table)
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?' "'" table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    ;; We handle strings
    (modify-syntax-entry ?\" "." table)
    ;; We handle escapes
    (modify-syntax-entry ?\\ "." table)
    table)
  "Syntax table for Nix mode.")

(defconst nix-search-fixed-syntax-table
  (let ((tbl (copy-syntax-table nix-mode-syntax-table)))
    (modify-syntax-entry ?\' "w" tbl)
    tbl)
  "Special syntax table for nix searches that will match \"\\_<foo'\\_>\" in \"foo\"")

(provide 'nix-syntax-table)

;; Local Variables:
;; End:

;; nix-syntax-table.el ends here
