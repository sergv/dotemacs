;; comment-util.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: very long ago
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'common)
(require 'advices-util)

(defvar-local *comment-util-current-format* nil
  "Comment format for current buffer.")

(defvar *comment-util-space-count* 1
  "Amount of spaces to put after comment markers.")

(advices/auto-comment sp-newline)
(advices/auto-comment vim:cmd-insert-line-below)
(advices/auto-comment vim:cmd-insert-line-above)
(advices/auto-comment haskell-newline-with-signature-expansion)

(defvar +comment-util-comment-format-alist+
  [(rust-mode                (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (toml-mode                (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (cargo-toml-mode          (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (cuda-mode                (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (ptx-mode                 (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (c-mode                   (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (c++-mode                 (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (glsl-mode                (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (asm-mode                 (one-line ";")  (line-regexp ";+")  (comment-chars ?\;))
   (nasm-mode                (one-line ";")  (line-regexp ";+")  (comment-chars ?\;))
   (llvm-mode                (one-line ";")  (line-regexp ";+")  (comment-chars ?\;))
   (tablegen-mode            (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (makefile-mode            (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (makefile-gmake-mode      (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (makefile-automake-mode   (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (makefile-bsdmake-mode    (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (cmake-mode               (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (j-mode                   (one-line "NB.")                    (comment-chars ?N ?B ?.))
   (lean-mode                (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (latex-mode               (one-line "%")  (line-regexp "%+")  (comment-chars ?%))
   (octave-mode              (one-line "%")  (line-regexp "\\(?:%+\\|#+\\)") (comment-chars ?% ?#))
   (graphviz-dot-mode        (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (org-mode                 (one-line "#")  (line-regexp "#+")  (comment-chars ?#))

   (markdown-mode            (region-begin "<!--") (region-end "-->") (line-regexp "<!--"))
   (rst-mode                 (one-line ".. ") (line-regexp ".. "))
   (bison-mode               (region-begin "/*") (region-end "*/") (line-regexp "/[/*]"))
   (flex-mode                (region-begin "/*") (region-end "*/") (line-regexp "/[/*]"))

   (emacs-lisp-mode          (one-line ";;") (line-regexp ";+")  (comment-chars ?\;))
   (elisp-byte-code-mode     (one-line ";;") (line-regexp ";+")  (comment-chars ?\;))
   (inferior-emacs-lisp-mode (one-line ";;") (line-regexp ";+")  (comment-chars ?\;))
   (lisp-interaction-mode    (one-line ";;") (line-regexp ";+")  (comment-chars ?\;))
   (cl-mode                  (one-line ";;") (line-regexp ";+")  (comment-chars ?\;))
   (lisp-mode                (one-line ";;") (line-regexp ";+")  (comment-chars ?\;))
   (clojure-mode             (one-line ";;") (line-regexp ";+")  (comment-chars ?\;))

   (haskell-mode             (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (haskell-c-mode           (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (haskell-c2hs-mode        (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (haskell-hsc-mode         (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (haskell-literate-mode    (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (ghc-core-mode            (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (haskell-cabal-mode       (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (alex-mode                (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (uuag-mode                (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (happy-mode               (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (agda2-mode               (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (elm-mode                 (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (nix-mode                 (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (blueprint-mode           (one-line ";;") (line-regexp ";+")  (comment-chars ?\;))
   (python-mode              (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (prolog-mode              (one-line "%")  (line-regexp "%+")  (comment-chars ?%))
   (prolog-inferior-mode     (one-line "%")  (line-regexp "%+")  (comment-chars ?%))
   (erlang-mode              (one-line "%")  (line-regexp "%+")  (comment-chars ?%))
   (gnuplot-mode             (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (debsources-mode          (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (shell-script-mode        (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (awk-mode                 (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (sh-mode                  (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (sh-script-mode           (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (conf-space-mode          (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (conf-unix-mode           (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (conf-colon-mode          (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (conf-xdefaults-mode      (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (conf-mode                (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (conf-javaprop-mode       (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (conf-windows-mode        (one-line ";")  (line-regexp ";+")  (comment-chars ?\;))
   (wisent-grammar-mode      (one-line ";;") (line-regexp ";;+") (comment-chars ?\;))
   (bovine-grammar-mode      (one-line ";;") (line-regexp ";;+") (comment-chars ?\;))
   (antlr-mode               (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (snippet-mode             (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (sql-mode                 (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (idl-mode                 (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (java-mode                (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (groovy-mode              (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (js2-mode                 (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (js-mode                  (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (json-mode                (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (css-mode                 (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (rnc-mode                 (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (gitignore-mode           (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (gitconfig-mode           (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (lua-mode                 (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (xmodmap-mode             (one-line "!")  (line-regexp "!+")  (comment-chars ?!))
   (verilog-mode             (one-line "//") (line-regexp "//+") (comment-chars ?/))
   (vhdl-mode                (one-line "--") (line-regexp "--+") (comment-chars ?-))
   (tcl-mode                 (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (ucf-mode                 (one-line "#")  (line-regexp "#+")  (comment-chars ?#))
   (yaml-mode                (one-line "#")  (line-regexp "#+")  (comment-chars ?#))

   (tuareg-mode              (region-begin "(*") (region-end "*)") (line-regexp "(\\*"))
   (sml-mode                 (region-begin "(*") (region-end "*)") (line-regexp "(\\*"))
   (comint-mode              (one-line nil))

   (dos-mode                 (one-line "rem ")     (line-regexp "rem ?"))
   (texinfo-mode             (one-line "@comment") (line-regexp "@c\\(?:o\\(?:m\\(?:m\\(?:e\\(?:n\\(?:t?\\)?\\)?\\)?\\)?\\)?\\)?"))

   (nxhtml-mode              (region-begin "<!--") (region-end "-->") (line-regexp "<!--"))
   (nxml-mode                (region-begin "<!--") (region-end "-->") (line-regexp "<!--"))
   (html-mode                (region-begin "<!--") (region-end "-->") (line-regexp "<!--"))
   (sgml-mode                (region-begin "<!--") (region-end "-->") (line-regexp "<!--"))
   (web-mode                 (region-begin "<!--") (region-end "-->") (line-regexp "<!--"))]
  "List of per-mode specifications of comments.
Contains single-line and region comments.")

;;;###autoload
(define-minor-mode comment-util-mode
  "Minor mode to handle comments in various languages."
  :init-value nil
  :lighter nil
  :keymap nil ;; no keymap
  :group util
  :global nil
  (unless *comment-util-current-format*
    (let* ((cformat-list  (v-assq major-mode +comment-util-comment-format-alist+))
           (one-line      (cadr (assq 'one-line      cformat-list)))
           (region-begin  (cadr (assq 'region-begin  cformat-list)))
           (region-end    (cadr (assq 'region-end    cformat-list)))
           (line-regexp   (cadr (assq 'line-regexp   cformat-list)))
           (comment-chars (cdr  (assq 'comment-chars cformat-list))))
      (unless cformat-list
        (error "Comment util mode error: no comment format defined for mode %s"
               major-mode))
      (setf *comment-util-current-format*
            (make-comment-format :one-line     one-line
                                 :region-begin region-begin
                                 :region-end   region-end
                                 :line-regexp  (cond
                                                 ((not (null line-regexp))
                                                  line-regexp)
                                                 ((not (null one-line))
                                                  (regexp-quote one-line))
                                                 (t
                                                  nil))
                                 :comment-chars comment-chars)))))

(cl-defstruct comment-format
  one-line
  region-begin
  region-end
  ;; line regexp is special regexp that should match all types of line comments,
  ;; i.e. ;+ for lisp, used to insert comment automatically
  line-regexp
  ;; Characters that when inserted together may make commented line as recognized by
  ;; line-regexp.
  comment-chars)

;;; User-interface functions
;;;###autoload
(defun comment-util-comment-lines (lines)
  "Comment LINES lines eiter up if argument LINES is positive
or down if LINES is negative or comment whole region if region is active."
  (interactive "p")
  (if (region-active-p)
      (with-region-bounds-unadj start end
        (comment-util-comment-region start end))
    (comment-util-comment-next-n-lines lines)))

;;;###autoload
(defun comment-util-comment-region (_beg _end)
  "Comment region between BEGIN and END position inserting region comments if
they are defined for current mode or one-line comments otherwise."
  (interactive "*r")
  (with-region-bounds-unadj begin end
    (save-excursion
      (if (comment-util-region-comments-defined?)
          (comment-util-comment-chunk-region begin end)
        (comment-util--comment-lined-region begin end)))))

;;;###autoload
(defun comment-util-uncomment-region ()
  "Uncomment region at point commented either with line comments or block comments."
  (interactive "*")
  (save-excursion
    (if (or (and (comment-format-one-line *comment-util-current-format*)
                 (save-excursion
                   (skip-to-indentation)
                   (looking-at-p
                    (comment-format-one-line *comment-util-current-format*))))
            (not (comment-util-region-comments-defined?)))
        ;;if no region comments are defined then use line comments
        (comment-util-uncomment-lined-region)
      (comment-util-uncomment-chunk-region))))

(defun comment-util-region-comments-defined? ()
  (and (comment-format-region-begin *comment-util-current-format*)
       (comment-format-region-end *comment-util-current-format*)))

;;; These two functions require somewhat special threat because
;;; the're used /only/ in vim visual mode

;;;###autoload
(defun comment-util-uncomment-region-simple (begin end)
  "Uncomment region between begin and end presumably commented with
line comments. If that's not the case then do nothing. Should
be used only for vim-visual-mode of the vim-mode package."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (skip-to-indentation)
    (when (looking-at-p
           (comment-format-one-line *comment-util-current-format*))
      (comment-util--uncomment-lines (count-lines-fixed begin end)))))

(defun comment-util--uncomment-lines (lines)
  "Uncomment lines eiter up if N is positive or down if N is negative."
  (if (> lines 0)
      (while (> lines 0)
        (skip-to-indentation)
        (comment-util-delete-comment)
        (indent-for-tab-command)
        (forward-line 1)
        (setq lines (1- lines)))
    (while (<= lines 0)
      (skip-to-indentation)
      (comment-util-delete-comment)
      (indent-for-tab-command)
      (forward-line -1)
      (setq lines (1+ lines)))))

;;; "library" functions for other part of Emacs

(defun comment--util-on-commented-line-p ()
  (let ((one-line-comm (comment-format-one-line *comment-util-current-format*)))
    (when one-line-comm
      (save-excursion
        (skip-to-indentation)
        (looking-at-p one-line-comm)))))

;;;; core functionality, not for interactive use

;;;;; Mid-level functions

(defsubst comment-util--comment-lined-region (begin end)
  "Comment region between BEGIN and END with one-line comments."
  (goto-char begin)
  (comment-util-comment-next-n-lines (count-lines-fixed begin end)))

(defun comment-util-uncomment-lined-region ()
  "Uncomment region that was commented with line comments."
  (skip-to-indentation)
  (let* ((comment (comment-format-one-line *comment-util-current-format*))
         (pos (point))
         (del-comments (lambda (d) ;;d for direction
                         (skip-to-indentation)
                         (while (looking-at-p comment)
                           (comment-util-delete-comment)
                           (forward-line d)
                           (skip-to-indentation)))))
    (unless (looking-at-p comment)
      (error "not in commented region"))
    ;;delete comments on lines that are below than current line
    (funcall del-comments 1)

    ;;delete comments on lines that are above than current line
    (goto-char pos)
    (unless (bobp)
      (forward-line -1)
      (funcall del-comments -1))))


(defun comment-util-comment-chunk-region (begin end)
  (save-excursion
    (let ((has-open (progn (goto-char begin)
                           (search-forward (comment-format-region-begin
                                            *comment-util-current-format*)
                                           end
                                           t)))
          (has-close (progn (goto-char begin)
                            (search-forward (comment-format-region-end
                                             *comment-util-current-format*)
                                            end
                                            t))))
      (cond
        ((and has-open has-close)
         (error "Specified region already contains commented part"))
        (has-open
         (error "Specified region already contains open comment"))
        (has-close
         (error "Specified region already contains close comment")))))
  (goto-char end)
  (insert (make-string *comment-util-space-count* ?\s)
          (comment-format-region-end *comment-util-current-format*))
  (goto-char begin)
  (insert (comment-format-region-begin *comment-util-current-format*)
          (make-string *comment-util-space-count* ?\s)))

(defun comment-util-uncomment-chunk-region ()
  "Uncomment region around point surrounded by region begin and end markers."
  (let* ((begin-str (concat (comment-format-region-begin
                             *comment-util-current-format*)
                            (make-string *comment-util-space-count* ?\s)))
         (end-str   (concat (make-string *comment-util-space-count* ?\s)
                            (comment-format-region-end
                             *comment-util-current-format*)))
         (curr-pos  (point))
         (begin-pos (search-backward begin-str nil t))
         (end-pos   (search-forward end-str nil t)))
    (unless begin-pos
      (error "Cannot find beginning of the commented block"))
    (unless end-pos
      (error "Cannot find end of the commented block"))
    (unless (and
             (<= begin-pos curr-pos)
             (<= curr-pos end-pos))
      (error "Point is not within commented block"))
    ;; note: deletion of last comment must come first because
    ;; otherwise point should be adjusted
    (goto-char end-pos)
    (delete-char (- (length end-str)))

    (goto-char begin-pos)
    (delete-char (length begin-str))
    (indent-for-tab-command)))

;;;;; Low-level core functions

(cl-defun comment-util-comment-next-n-lines (lines)
  (unless *comment-util-current-format*
    (error "No comment format defined for current mode"))
  (setf lines (or lines 1))
  (cond
    ;; has one-line comments defined
    ((comment-format-one-line *comment-util-current-format*)
     (comment-util--comment-n-lines-starting-at-col
      (concat (comment-format-one-line *comment-util-current-format*)
              (make-string *comment-util-space-count* ?\s))
      lines
      (end-of-whitespace-prefix)))
    ((comment-util-region-comments-defined?)
     (save-excursion
       (skip-to-indentation)
       (let ((begin (point)))
         ;; somewhat hackish but we're already in special case of
         ;; dealing with region comments
         (forward-line (- lines 1))
         (comment-util-comment-chunk-region begin (line-end-position)))))))

(defun end-of-whitespace-prefix ()
  "Count length of whitespace prefix for current line."
  (save-excursion
    (beginning-of-line)
    (skip-syntax-forward " ")
    (current-column)))

(defun comment-util--comment-n-lines-starting-at-col (comment-str lines column)
  "Comment next LINES with COMMENT-STR, but insert them at COLUMN."
  (let ((skip-to-column (lambda ()
                          (beginning-of-line)
                          (while (and (< (current-column) column)
                                      (not (or (char=? ?\n (char-after))
                                               (char=? ?\r (char-after))
                                               (eob?))))
                            (forward-char 1))))
        (update-column (lambda ()
                         (let ((new-col (end-of-whitespace-prefix)))
                           (when (< new-col column)
                             (setf column new-col)))))
        (empty-line? (lambda ()
                       (= (line-beginning-position)
                          (line-end-position))
                       ;; (save-excursion
                       ;;  (beginning-of-line)
                       ;;  (looking-at-p "^$"))
                       )))
    (when (> lines 0)
      ;; this is the zeroth iteration at which we shouldn't
      ;; update column and use supplied one
      (funcall skip-to-column)
      (insert comment-str)
      (forward-line 1)
      (cl-incf lines -1)
      (while (> lines 0)
        (cond
          ((funcall empty-line?)
           (insert (make-string column ?\s)
                   (concat comment-str)))
          (t
           (funcall update-column)
           (funcall skip-to-column)
           (insert comment-str)))
        (forward-line 1)
        (cl-incf lines -1)))))

(defun comment-util-delete-comment ()
  "Delete comments (e.g. //, ;) after point if any."
  (let* ((comment (comment-format-one-line *comment-util-current-format*))
         (n (length comment)))
    (when (looking-at-p comment)
      (delete-char n)
      (let ((i 0))
        (while (and (char= ?\s (char-after))
                    (< i *comment-util-space-count*))
          (delete-char 1)
          (cl-incf i))))))

;;;;; Some lisp-specific comment functions, inspired by paredit.el

;;;###autoload
(defun lisp-comment-sexp (&optional count)
  "If point is at the beginning of the sexp then comment it, else
move one comment up using `backward-up-list' and comment resulting
sexp. With argument COUNT greater than 0 move that many sexps
up and then comment the result."
  (interactive "p")
  (setq count (or count 1))
  (when (lisp-pos-is-beginning-of-sexp? (point))
    (setf count (1- count)))
  ;; if we're at top of file then don't break execution because of that
  (condition-case nil
      (backward-up-list count)
    (error nil))
  (indent-for-tab-command)
  (indent-sexp)
  (let ((sexp-end-exclusive (save-excursion
                              (forward-sexp)
                              (point))))
    ;; skip to include commented sexp quotes, reader syntax, etc.
    (skip-syntax-backward "^ >()")
    (save-excursion
      (goto-char sexp-end-exclusive)
      (when (or (lisp-pos-is-end-of-sexp? (point))
                (progn
                  ;; do not skip newlines!
                  (skip-syntax-forward " ")
                  (lisp-pos-is-end-of-sexp? (point))))
        (reindent-then-newline-and-indent)))
    (save-excursion
      (comment-util--comment-n-lines-starting-at-col
       ";; " ;; bad hack, hard-coded lisp comment... ;; survived for a long time...
       (count-lines-fixed (point) sexp-end-exclusive)
       (current-column)))))

(defun comment-util--on-commented-line? ()
  "Return t if current line is commented out."
  (if-let (line-comment (comment-format-one-line *comment-util-current-format*))
      (save-excursion
        (beginning-of-line)
        (skip-syntax-forward " ")
        (and (looking-at-p line-comment)
             (not (point-inside-string?))))
    (save-excursion
      (let* ((state (parse-partial-sexp (line-beginning-position)
                                        (line-end-position)))
             (inside-comment? (elt state 4)))
        ;; if parse end up inside comment then current line starts with a comment
        inside-comment?))))

(defun comment-util--get-commented-region ()
  "Return begin and end of region surrounding point that has
commented parts and leave point unchanged."
  (unless (comment-util--on-commented-line?)
    (error "Not on commented line"))
  (let ((move-while-commented
         ;; return position of the beginning of the last line in direction
         ;; that is still has commented parts
         (lambda (forward?)
           (let ((dir (if forward? +1 -1)))
             (beginning-of-line)
             (while (and (comment-util--on-commented-line?)
                         (not (if forward?
                                  (eobp)
                                (bobp))))
               (forward-line dir))
             (when (not (if forward?
                            (eobp)
                          (bobp)))
               (backward-line dir)
               ;; we're returned backwards onto line with comments
               ;; which is a known fact
               (cl-assert (comment-util--on-commented-line?)
                          nil
                          "line number: %s;\nline: %s;\nprevious line: %s"
                          (count-lines-fixed (point-min) (point))
                          (current-line)
                          (save-excursion
                            (forward-line dir)
                            (current-line))))))))
    (let ((pos (line-beginning-position))
          start
          end)
      (save-excursion
        (forward-line -1)
        (funcall move-while-commented nil)
        (setf start (line-beginning-position))

        (goto-char pos)
        (funcall move-while-commented t)
        (setf end (line-end-position)))
      (values start end))))

;;;###autoload
(defun comment-util-delete-commented-part ()
  "Delete all adjacent lines that are commented by line regexps."
  (interactive)
  (cl-multiple-value-bind (start end)
      (comment-util--get-commented-region)
    (save-excursion
      (save-match-data
        (let* ((line-regexp
                (comment-format-line-regexp *comment-util-current-format*))
               (full-line-re
                (concat "^\\s-*" line-regexp))
               (clear-comment (lambda ()
                                (cond
                                  ((looking-at-p full-line-re)
                                   (delete-current-line))
                                  ;; for cases like
                                  ;; > (let (( ;; (foo 1)
                                  ;;          (bar 2))))
                                  ((re-search-forward line-regexp (line-end-position) t)
                                   (delete-region (match-beginning 0) (line-end-position)))))))
          (goto-char end)
          (while (and (<= start (point))
                      (not (bobp)))
            (beginning-of-line)
            (funcall clear-comment)
            (forward-line -1))
          (when (bobp)
            (funcall clear-comment)))))))

;;;###autoload
(defun lisp-uncomment-sexp ()
  (interactive)
  (cl-multiple-value-bind (start end)
      (comment-util--get-commented-region)
    (save-excursion
      ;; now skip all whitespace characters and see if next char
      ;; is the close paren which would mean that sexp to be
      ;; uncommented in nested in some other and end of that
      ;; other one should be combined with it
      (goto-char end)
      (skip-syntax-forward " >")
      (when (and (not (eobp))
                 (lisp-pos-is-end-of-sexp? (point)))
        (delete-whitespace-backward))

      (uncomment-region start end)

      (goto-char start)
      (skip-syntax-backward " >")
      (when (and (not (bobp))
                 (lisp-pos-is-beginning-of-sexp? (point)))
        (delete-whitespace-forward)))))

;;;###autoload
(defun haskell-comment-line (&optional count)
  "Similar to `lisp-comment-sexp' buf for current Haskell node."
  (interactive (list current-prefix-arg))
  (let ((comment-format
         (concat (comment-format-one-line *comment-util-current-format*)
                 (make-string *comment-util-space-count* ?\s))))
    (if count
        (save-excursion
          (skip-to-indentation)
          (comment-util--comment-n-lines-starting-at-col
           comment-format
           count
           (current-column)))
      (comment-util-comment-lines 1))))

(provide 'comment-util)

;; Local Variables:
;; End:

;; comment-util.el ends here
