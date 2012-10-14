;; comment-util.el --- -*- lexical-binding: nil; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: very long ago
;; Description:

(eval-when-compile (require 'cl))
(require 'custom)
(require 'advices-util)

(defadvice:auto-comment autopair-newline)
;; paredit-newline's advice is defined in
;; general-lisp-setup.el
(defadvice:auto-comment vim:cmd-insert-line-below)
(defadvice:auto-comment vim:cmd-insert-line-above)


(defconst +comment-util-comment-format-alist+
  '((cl-mode               (one-line ";;") (line-regexp ";+"))
    (lisp-mode             (one-line ";;") (line-regexp ";+"))
    (emacs-lisp-mode       (one-line ";;") (line-regexp ";+"))
    (inferior-emacs-lisp-mode (one-line ";;") (line-regexp ";+"))
    (scheme-mode           (one-line ";;") (line-regexp ";+"))
    (inferior-scheme-mode  (one-line ";;") (line-regexp ";+"))
    (lisp-interaction-mode (one-line ";;") (line-regexp ";+"))
    (org-mode              (one-line "#")  (line-regexp "#+"))
    (prolog-mode           (one-line "%")  (line-regexp "%+"))
    (prolog-inferior-mode  (one-line "%")  (line-regexp "%+"))
    (c-mode                (region-begin "/*") (region-end "*/"))
    (maxima-mode           (region-begin "/*") (region-end "*/"))
    (haskell-mode          (one-line "--") (line-regexp "--+"))
    (literate-haskell-mode (one-line "--") (line-regexp "--+"))
    (ghc-core-mode         (one-line "--") (line-regexp "--+"))
    (latex-mode            (one-line "%")  (line-regexp "%+"))
    (octave-mode           (one-line "%")  (line-regexp "%+\\|#+"))
    (slime-repl-mode       (one-line ";;") (line-regexp ";+"))
    (slime-xref-mode       (one-line ";;") (line-regexp ";+"))
    (makefile-mode         (one-line "#")  (line-regexp "#+"))
    (makefile-gmake-mode   (one-line "#")  (line-regexp "#+"))
    (cmake-mode            (one-line "#")  (line-regexp "#+"))
    (gnuplot-mode          (one-line "#")  (line-regexp "#+"))
    (debsources-mode       (one-line "#")  (line-regexp "#+"))
    (graphviz-dot-mode     (region-begin "/*") (region-end "*/"))
    (shell-script-mode     (one-line "#")  (line-regexp "#+"))
    (awk-mode              (one-line "#")  (line-regexp "#+"))
    (sh-mode               (one-line "#")  (line-regexp "#+"))
    (sh-script-mode        (one-line "#")  (line-regexp "#+"))
    (conf-space-mode       (one-line "#")  (line-regexp "#+"))
    (conf-unix-mode        (one-line "#")  (line-regexp "#+"))
    (conf-xdefaults-mode   (one-line "#")  (line-regexp "#+"))
    (conf-mode             (one-line "#")  (line-regexp "#+"))
    (wisent-grammar-mode   (one-line ";;") (line-regexp ";;+"))
    (bovine-grammar-mode   (one-line ";;") (line-regexp ";;+"))
    (snippet-mode          (one-line "#")  (line-regexp "#+"))
    (sql-mode              (one-line "--") (line-regexp "--+"))
    (c++-mode              (one-line "//") (line-regexp "//+"))
    (glsl-mode             (one-line "//") (line-regexp "//+"))
    (python-mode           (one-line "#")  (line-regexp "#+"))
    (cython-mode           (one-line "#")  (line-regexp "#+"))
    (comint-mode           (one-line nil))
    (texinfo-mode          (one-line "@comment") (line-regexp "@c\\(?:o\\(?:m\\(?:m\\(?:e\\(?:n\\(?:t?\\)?\\)?\\)?\\)?\\)?\\)?"))
    (bison-mode            (region-begin "/*") (region-end "*/"))
    (flex-mode             (region-begin "/*") (region-end "*/"))
    (nxhtml-mode           (region-begin "<!--") (region-end "-->"))
    (nxml-mode             (region-begin "<!--") (region-end "-->"))
    (html-mode             (region-begin "<!--") (region-end "-->"))
    (sgml-mode             (region-begin "<!--") (region-end "-->"))
    (rnc-mode              (one-line "#") (line-regexp "#+"))
    (gitignore-mode        (one-line "#") (line-regexp "#+"))
    (lua-mode              (one-line "--") (line-regexp "--+"))
    (xmodmap-mode          (one-line "!") (line-regexp "!+")))
  "List of per-mode specifications of comments.
Contains single-line and region comments.")


(defvar *comment-util-space-count* 1
  "Amount of spaces to put after comment markers")

(defvar-loc *comment-util-current-format*)

(define-minor-mode comment-util-mode
  "Minor mode to handle comments in various languages"
  nil
  nil
  nil ;; no keymap
  :group util
  :global nil
  (unless *comment-util-current-format*
    (let* ((cformat-list (assoc major-mode +comment-util-comment-format-alist+))
           (one-line     (cadr (assoc 'one-line     cformat-list)))
           (region-begin (cadr (assoc 'region-begin cformat-list)))
           (region-end   (cadr (assoc 'region-end   cformat-list)))
           (line-regexp  (cadr (assoc 'line-regexp  cformat-list))))
      (unless cformat-list
        (error "Comment util mode error: no comment format defined for mode %s"
               major-mode))
      (setf *comment-util-current-format*
            (make-comment-format :one-line     one-line
                                 :region-begin region-begin
                                 :region-end   region-end
                                 :line-regexp (cond
                                                ((not (null line-regexp))
                                                 line-regexp)
                                                ((not (null one-line))
                                                 (regexp-quote one-line))
                                                (t
                                                 nil)))))))

(defstruct comment-format
  one-line
  region-begin
  region-end
  ;; line regexp is special regexp that should match all types of line comments,
  ;; i.e. ;+ for lisp, used to insert comment automatically
  line-regexp)

;; User-interface functions
(defun comment-util-comment-lines (lines)
  "Comment LINES lines eiter up if argument LINES is positive
or down if LINES is negative or comment whole region if region is active"
  (interactive "P")
  (if (or (region-active-p)
          (run-if-fbound vim:visual-mode-p))
    (comment-util-comment-region (region-beginning) (region-end))
    (comment-util-comment-next-n-lines lines)))

(defun comment-util-comment-region (begin end)
  "Comment region between BEGIN and END position inserting region comments if
they are defined for current mode or one-line comments otherwise."
  (interactive "r")
  (save-excursion
   (if (comment-util-region-comments-defined?)
     (comment-util-comment-chunk-region begin end)
     (comment-util-comment-lined-region begin end))))

(defun comment-util-uncomment-region ()
  "Uncomment region at point commented either with line comments or block comments."
  (interactive)
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

;;;;; These two functions require somewhat special threat because
;; the're used /only/ in vim visual mode

(defun comment-util-uncomment-region-simple (begin end)
  "Uncomment region between begin and end presumably commented with
line comments. If that's not the case then do nothing. Should
be used only for vim-visual-mode of the vim-mode package"
  (interactive "r")
  (save-excursion
   (goto-char begin)
   (skip-to-indentation)
   (when (looking-at-p
          (comment-format-one-line *comment-util-current-format*))
     (comment-util-uncomment-lines (count-lines begin end)))))

(defun comment-util-uncomment-lines (lines)
  "Uncomment lines eiter up if N is positive or down if N is negative"
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

;;;;;

;; "library" functions for other part of Emacs

(defun comment-util-on-commented-line-p ()
  (let ((one-line-comm (comment-format-one-line *comment-util-current-format*)))
    (when one-line-comm
      (save-excursion
       (skip-to-indentation)
       (looking-at-p one-line-comm)))))

;;; core functionality, not for interactive use

;; Mid-level functions

(defsubst comment-util-comment-lined-region (begin end)
  "Comment region between BEGIN and END with one-line comments"
  (goto-char begin)
  (comment-util-comment-next-n-lines (count-lines begin end)))

(defun comment-util-uncomment-lined-region ()
  "Uncomment region that was commented with line comments"
  (skip-to-indentation)
  (let* ((comment (comment-format-one-line *comment-util-current-format*))
         (pos (point)))
    (labels ((del-comments
               (d) ;;d for direction
               (skip-to-indentation)
               (while (looking-at-p comment)
                 (comment-util-delete-comment)
                 (forward-line d)
                 (skip-to-indentation))))
      (unless (looking-at-p comment)
        (error "not in commented region"))
      ;;delete comments on lines that are below than current line
      (del-comments 1)

      ;;delete comments on lines that are above than current line
      (goto-char pos)
      (unless (bobp)
        (forward-line -1)
        (del-comments -1)))))


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

;; Low-level core functions

(defun* comment-util-comment-next-n-lines (lines)
  (unless *comment-util-current-format*
    (error "No comment format defined for current mode"))
  (setf lines (or lines 1))
  (cond
    ;; has one-line comments defined
    ((comment-format-one-line *comment-util-current-format*)
     (comment-util-comment-n-lines-starting-at-col
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
  (save-excursion
   (beginning-of-line)
   (skip-syntax-forward " ")
   (current-column)))

(defun comment-util-comment-n-lines-starting-at-col (comment-str lines column)
  (labels ((skip-to-column ()
             (beginning-of-line)
             (forward-char column))
           (update-column ()
             (let ((new-col (end-of-whitespace-prefix)))
               (when (< new-col column)
                 (setf column new-col))))
           (empty-linep ()
             (= (line-beginning-position)
                (line-end-position))
             ;; (save-excursion
             ;;  (beginning-of-line)
             ;;  (looking-at-p "^$"))
             ))

    (when (> lines 0)
      ;; this is the zeroth iteration at which we shouldn't
      ;; update column and use supplied one
      (skip-to-column)
      (insert comment-str)
      (forward-line 1)
      (incf lines -1)

      (while (> lines 0)
        (cond
          ((empty-linep)
           (insert (make-string column ?\s)
                   (concat comment-str)))
          (t
           (update-column)
           (skip-to-column)
           (insert comment-str)))
        (forward-line 1)
        (incf lines -1))))

  )


(defun comment-util-delete-comment ()
  "Delete comments( // or ; or whatever) after point if any"
  (let* ((comment (comment-format-one-line *comment-util-current-format*))
         (n (length comment)))
    (when (looking-at-p comment)
      (delete-char n)
      (let ((i 0))
        (while (and (char= ?\s (char-after))
                    (< i *comment-util-space-count*))
          (delete-char 1)
          (incf i))))))

;;; Some lisp-specific comment functions, inspired by paredit.el

(defun lisp-comment-sexp (&optional count)
  "If point is at the beginning of the sexp then comment it, else
move one comment up using `backward-up-list' and comment resulting
sexp. With argument COUNT greater than 0 move that many sexps
up and then comment the result."
  (interactive "p")
  (setq count (or count 1))
  (when (char= ?\( (char-after))
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
    ;; skip to include into commented sexp quotes, reader syntax, etc.
    (skip-syntax-backward "^ >()")
    (save-excursion
     (goto-char sexp-end-exclusive)
     (when (or (char= ?\) (char-after))
               (progn
                 ;; do not skip newlines!
                 (skip-syntax-forward " ")
                 (char= ?\) (char-after))))
       ;; (insert "\n")
       (reindent-then-newline-and-indent)))

    (save-excursion
     (comment-util-comment-n-lines-starting-at-col
      ";; " ;; bad hack, hard-coded lisp comment...
      (count-lines (point) sexp-end-exclusive)
      (current-column)))))

(defun* lisp-commented-linep ()
  "Return t if current line contains commented parts."
  (save-excursion
   (let* ((state (parse-partial-sexp (line-beginning-position)
                                     (line-end-position)))
          (inside-commentp (elt state 4)))
     ;; if parse end up inside comment then current line has ;-comments
     inside-commentp)))

(defun* lisp-get-region-with-commented-parts ()
  "Return begin and end of region surrounding point that has
commented parts and leave point unchanged."
  (unless (lisp-commented-linep)
    (error "Not on line with commented part(s)"))
  (labels (;; return position of the beginning of the last line
           ;; in direction that is still has commented parts
           (move-while-commented (dir)
             (beginning-of-line)
             (while (and (lisp-commented-linep)
                         (if (eq dir 'backward)
                           (not (bobp))
                           (not (eobp))))
               (move-by-line dir))
             (unless (if (eq dir 'backward)
                       (bobp)
                       (eobp))
               (move-by-line-backward dir)
               ;; I've returned backwards onto line with comments
               ;; which is a known fact
               (assert (lisp-commented-linep)
                       nil
                       "LINE NUMBER: %S;\nLINE: %S;\nPREVIOUS LINE: %S"
                       (count-lines1 (point-min) (point))
                       (current-line)
                       (save-excursion
                        (move-by-line dir)
                        (current-line))))))
    (let ((pos (line-beginning-position))
          start
          end)
      (save-excursion
       (forward-line -1)
       (move-while-commented 'backward)
       (setf start (line-beginning-position))

       (goto-char pos)
       (move-while-commented 'forward)
       (setf end (line-end-position)))
      (values start end))))

(defun lisp-delete-commented-part ()
  (interactive)
  (multiple-value-bind (start end)
      (lisp-get-region-with-commented-parts)
    (save-excursion
     (save-match-data
      (goto-char end)
      (flet
          ((clear-comment ()
             (cond
               ((looking-at-pure? "^\\s-*;+.*$")
                (delete-current-line))
               ((re-search-forward ";+.*$" (line-end-position) t)
                (replace-match "")))))
        (while (and (<= start (point))
                    (not (bobp)))
          (beginning-of-line)
          (clear-comment)
          (forward-line -1))
        (when (bobp)
          (clear-comment)))))))

(defun* lisp-uncomment-sexp ()
  (interactive)
  (multiple-value-bind (start end)
      (lisp-get-region-with-commented-parts)
    (save-excursion
     ;; now skip all whitespace characters and see if next char
     ;; is the close paren which would mean that sexp to be
     ;; uncommented in nested in some other and end of that
     ;; other one shoudl be combined with it
     (goto-char end)
     (skip-syntax-forward " >")
     (when (and (not (eobp))
                (char= ?\) (char-after)))
       (delete-whitespaces-backward))

     (uncomment-region start end)

     (goto-char start)
     (skip-syntax-backward " >")
     (when (and (not (bobp))
                (char= ?\( (char-before)))
       (delete-whitespaces-forward)))))


(provide 'comment-util)

;; Local Variables:
;; End:

;; comment-util.el ends here
