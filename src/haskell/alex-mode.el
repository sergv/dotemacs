;; alex-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 12 May 2014
;; Description:

(require 'common)
(require 'mmm-auto)

(defconst alex-colon-column 16 "\
*The column in which to place a colon separating a token from its definition.")

(defconst alex-percent-column 41 "\
*The column in which to place a percent introducing a modifier (e.g. %prec).")

(defparameter alex-mode-map
  (let ((keymap (make-sparse-keymap)))
    (def-keys-for-map keymap
      ("<backspace>" backward-delete-char-untabify)
      ("<tab>"       shm/tab))
    keymap)
  "Keymap used in alex mode.")

(defparameter alex-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14"  tbl)
    (modify-syntax-entry ?* ". 23"  tbl)
    (modify-syntax-entry ?\{ "(}  " tbl)
    (modify-syntax-entry ?\} "){  " tbl)
    (modify-syntax-entry ?\\ "\\  " tbl)
    (modify-syntax-entry ?\' "."    tbl)
    (modify-syntax-entry ?\" "\""   tbl)
    (modify-syntax-entry ?\: "."    tbl)
    (modify-syntax-entry ?\; "."    tbl)
    (modify-syntax-entry ?-  ". 12" tbl)
    (modify-syntax-entry ?\n ">"    tbl)
    tbl)
  "Syntax table in use in alex-mode buffers.")

(defparameter alex-mode-font-lock-keywords
  `((,(rx (or "%wrapper"
              ":-"))
     (0 'font-lock-keyword-face))
    (,(rx bol
          "<"
          (+ (regexp "[^>]"))
          ">")
     (0 'font-lock-function-name-face))
    (,(rx (or (seq (or "$"
                       "@")
                   (* (regexp "[a-zA-Z_]")))
              "["
              "]"
              "\\"
              "^"))
     (0 'font-lock-constant-face))
    ;; regexp metacharacters
    (,(rx (or "*"
              "+"
              "."
              "|"
              "?"
              "\\"
              "("
              ")"))
     (0 'font-lock-negation-char-face)))
  "Highlight definitions of alex distinctive constructs for font-lock.")

;;;###autoload
(define-derived-mode alex-mode prog-mode "Alex"
  "Major mode for editing Alex files."
  (set (make-local-variable 'font-lock-defaults)
       '(alex-mode-font-lock-keywords))

  (setq-local paragraph-start (concat "^$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local indent-line-function 'alex-indent-line)
  (setq-local require-final-newline t)
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-column 32)
  (setq-local comment-start-skip "--+ *")
  (setq-local parse-sexp-ignore-comments t)
  ;; (setq-local selective-display t)
  ;; (setq-local selective-display-ellipses t)
  (make-local-variable 'block-indent-level)
  (make-local-variable 'auto-fill-hook))

;;;;

(provide 'alex-mode)

;; alex-mode.el ends here
