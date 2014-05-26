;; happy-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 12 May 2014
;; Description:

(require 'common)
(require 'mmm-auto)

(defconst happy-colon-column 16 "\
*The column in which to place a colon separating a token from its definition.")

(defconst happy-percent-column 41 "\
*The column in which to place a percent introducing a modifier (e.g. %prec).")

(defparameter happy-mode-map
  (let ((keymap (make-sparse-keymap)))
    (def-keys-for-map keymap
      (";"               electric-happy-semi)
      (":"               electric-happy-colon)
      ("|"               electric-happy-colon)
      ;; ("%"               electric-happy-per)
      ("<backspace>"     backward-delete-char-untabify)
      ("<tab>"           happy-indent-command)
      ("S-<tab>"         happy-dedent-command)
      ("S-<iso-lefttab>" happy-dedent-command))
    keymap)
  "Keymap used in happy mode.")

(defparameter happy-mode-syntax-table
  (let ((tbl (make-syntax-table)))
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
  "Syntax table in use in happy-mode buffers.")

(defconst happy-mode-rule-start-regexp
  (rxx ((ws (* (any ?\s ?\t)))
        (ws-nl (* (any ?\s ?\t ?\n ?\r))))
    bol
    ws
    (+ (or (syntax word)
           (syntax symbol)))
    ws
    (? "::"
       ws
       "{"
       (* (not (any ?\})))
       "}"
       ws-nl)
    ":"))
(defconst happy-mode-rule-start-or-body-regexp
  "^[ \t]*\\(?:\\(?:\\s_\\|\\sw\\)+[ \t]*:\\||\\)")

(defparameter happy-mode-font-lock-keywords
  `((,(rx (or "%name"
              "%tokentype"
              "%error"
              "%token"
              "%%"
              "%left"
              "%right"
              "%nonassoc"
              "%monad"
              "%lexer"
              "%attribute"
              "%attributetype"
              "%partial"
              "%importedidentity"
              "%expect"))
     (0 'font-lock-keyword-face))
    (,(rx bol
          (group
           (+ (regexp "[a-z0-9_]")))
          (* (any ?\s ?\t ?\n ?\r))
          ":")
     (1 'font-lock-function-name-face))
    (,(rx bow
          (group
           (+ (regexp "[A-Z0-9_]")))
          eow)
     (0 'font-lock-constant-face))
    (,(rx (or ":"
              "|"
              ";"
              (seq "'"
                   (+ (not (any ?\n ?\')))
                   "'")))
     (0 'font-lock-negation-char-face))
    (,(rx "$" (or digit "$"))
     (0 'font-lock-variable-name-face)))
  "Highlight definitions of happy distinctive constructs for font-lock.")


(define-derived-mode happy-mode prog-mode "Happy"
  ""
  (set (make-local-variable 'font-lock-defaults)
       '(happy-mode-font-lock-keywords
         nil ;; perform syntactic fontification
         nil ;; do not ignore case
         nil ;; no special syntax provided
         ))

  (setq-local paragraph-start (concat "^$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local indent-line-function 'happy-indent-line)
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

(defun happy-in-literal-context? (start end)
  "Check whether end is in literal context."
  (let* ((state (parse-partial-sexp start end))
         (inside-string? (nth 3 state))
         (inside-comment? (nth 4 state))
         (following-quote-character? (nth 5 state)))
    (or inside-string?
               inside-comment?
               following-quote-character?)))

(defun electric-happy-colon (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (cond ((happy-in-literal-context? (save-excursion
                                      (save-match-data
                                        (if (re-search-backward
                                             "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
                                             nil 'move)
                                          (- (match-end 0) 1)
                                          (point-min))))
                                    (point))
         (self-insert-command (prefix-numeric-value arg)))
        ((and (not arg)
              (eolp))
         (happy-indent-line)
         (and c-auto-newline
              (eq last-command-event ?\|)
              (save-excursion
                (beginning-of-line)
                (not (looking-at-p "[ \t]*$")))
              (newline))
         (delete-horizontal-space)
         (indent-to happy-colon-column)
         (insert last-command-event)
         (insert " "))
        (else
         (self-insert-command (prefix-numeric-value arg)))))

(defun electric-happy-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (electric-happy-terminator arg))

;; (defun electric-happy-per (arg)
;;   "Insert character and correct line's indentation."
;;   (interactive "P")
;;   (let ((state (parse-partial-sexp
;;                 (save-excursion
;;                   (save-match-data
;;                     (if (re-search-backward
;;                          "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
;;                          nil 'move)
;;                       (- (match-end 0) 1)
;;                       (point-min))))
;;                 (point))))
;;     (if (and (not arg)
;;              (eolp)
;;              (not (eq (preceding-char) ?%))
;;              (not (or (nth 3 state) (nth 4 state) (nth 5 state))))
;;       (if (not (save-excursion
;;                  (skip-chars-backward " \t")
;;                  (bolp)))
;;         (indent-to bison-percent-column)
;;         (delete-region (save-excursion
;;                          (beginning-of-line)
;;                          (point))
;;                        (point))))
;;     (self-insert-command (prefix-numeric-value arg))))

(defun electric-happy-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (happy-in-literal-context? (save-excursion
                                   (save-match-data
                                     (if (re-search-backward
                                          "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
                                          nil 'move)
                                       (- (match-end 0) 1)
                                       (point-min))))
                                 (point))
    (self-insert-command (prefix-numeric-value arg))
    (if (and (not arg) (eolp)
             (not (save-excursion
                    (beginning-of-line)
                    (skip-chars-forward " \t")
                    (= (following-char) ?%))))
      (progn
        (and c-auto-newline
             (progn
               (if (save-excursion
                     (beginning-of-line)
                     (not (looking-at-p "[ \t]*$")))
                 (newline))
               (happy-indent-line)
               (backward-delete-char-untabify 2)))
        (insert last-command-event)
        (happy-indent-line)
        (and c-auto-newline
             (progn
               (newline)
               (setq insertpos (- (point) 2))
               (happy-indent-line)))
        (save-excursion
          (if insertpos (goto-char (1+ insertpos)))
          (delete-char -1))))
    (if insertpos
      (save-excursion
        (goto-char insertpos)
        (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun happy-indent-command (&optional whole-exp)
  "Indent current line as Happy (Bison) code, or in some cases insert a tab character.
If c-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
    (let ((shift-amount (happy-indent-line))
          beg end)
      (save-excursion
        (save-match-data
          (if c-tab-always-indent
            (beginning-of-line))
          (setq beg (point))
          (re-search-forward ";\\|^%%" nil 'move)
          (when (save-excursion
                  (beginning-of-line)
                  (looking-at-p "%%"))
            (forward-line -1)
            (end-of-line))
          (setq end (point))
          (goto-char beg)
          (forward-line 1)
          (setq beg (point))))
      (when (> end beg)
        (indent-code-rigidly beg end shift-amount "%")))
    (if (and (not c-tab-always-indent)
             (save-excursion
               (skip-chars-backward " \t")
               (not (bolp))))
      (insert-tab)
      (happy-indent-line))))

(defun happy-dedent-command ()
  (interactive)
  (indent-to! (max 0 (- (current-column) 4))))

(defun happy-indent-line ()
  "Indent current line as Happy (Bison) code.
Return the amount the indentation changed by."
  ;; Lines are indented if and only if a colon is found before a semicolon
  ;; while searching backward.  String-quoted characters are ignored.
  (let (indent)
    (save-excursion
      (cond
        ((save-excursion
           (save-match-data
             (let ((limit (point))
                   state)
               (goto-char (point-min))
               (not (and (re-search-forward "^%%" limit t)
                         (happy-in-literal-context?
                          (save-excursion
                            (if (re-search-backward
                                 happy-mode-rule-start-regexp
                                 nil
                                 t)
                              (- (match-end 0) 1)
                              (point-min)))
                          (point)))))))
         (setq indent 0))
        ((save-excursion
           (beginning-of-line)
           (looking-at-p "[ \t]*%"))
         (setq indent 0))
        (t
         (beginning-of-line)
         (if (looking-at-p happy-mode-rule-start-regexp)
           (setq indent 0)
           (progn
             (forward-line -1)
             (while (not (or (bobp)
                             (looking-at-p happy-mode-rule-start-or-body-regexp)
                             (eq (following-char) ?%)))
               (forward-line -1))
             (skip-chars-forward "^:|")
             ;; (skip-chars-forward ":| \t")
             (setq indent (current-column)))))))
    (indent-to! indent)
    (skip-chars-forward " \t")
    indent))

;;;;

(provide 'happy-mode)

;; happy-mode.el ends here
