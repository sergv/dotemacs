;; comment-util.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: very long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'subr-x)

  (require 'macro-util))

(require 'common)
(require 'current-column-fixed)
(require 'advices-util)

(defconst comment-util--spaces-after-comment " "
  "Amount of spaces to put after comment markers.")

(defmacro comment-util--auto-commenting-action (&rest body)
  (let ((line-comment-prefix-var '#:line-comment-prefix)
        (current-format-var '#:current-format))
    `(let ((,line-comment-prefix-var
            (unless current-prefix-arg ;; Disable advice if prefix argument is supplied.
              (when-let (,current-format-var (comment-util-current-format-lax))
                (save-excursion
                  (let ((bol (line-beginning-position)))
                    (goto-char bol)
                    (when (funcall (comment-format-detect-line-comment ,current-format-var)
                                   ,current-format-var)
                      (buffer-substring-no-properties bol (point)))))))))
       ,@body
       (when ,line-comment-prefix-var
         (let ((bol (line-beginning-position)))
           (goto-char bol)
           (skip-indentation-forward)
           (delete-region bol (point))
           (insert ,line-comment-prefix-var
                   comment-util--spaces-after-comment))))))

(defmacro comment-util-auto-comment-advice (func)
  "Define advice around FUNC that will insert comments at
beginning of line if previous line was commented out.

In case of non-nil prefix-arg no comment will be inserted.

Intended to be used with comment-util-mode."
  (let ((advice-name (string->symbol (format "%s--auto-comment" func))))
    `(progn
       (defun ,advice-name (old-func &rest args)
         "Insert comments at beginning of line if previous line was commented out."
         (comment-util--auto-commenting-action
          (apply old-func args)))

       (advice-add ',func :around #',advice-name))))

(comment-util-auto-comment-advice vim:cmd-insert-line-below)
(comment-util-auto-comment-advice vim:cmd-insert-line-above)
(comment-util-auto-comment-advice haskell-newline-with-signature-expansion)
(comment-util-auto-comment-advice newline)

(vimmize-function comment-util-comment-lines :has-count t)
(vimmize-function comment-util-uncomment-region :has-count nil)
(vimmize-function comment-util-delete-commented-part :has-count nil)

(defun comment-util--detect-line-comment-with-regexp (format)
  (let ((start (point)))
    (skip-indentation-forward)
    (save-match-data
      (cl-assert (stringp (comment-format-line-regexp format)))
      (if (looking-at (comment-format-line-regexp format))
          (progn
            (goto-char (match-end 0))
            t)
        (progn
          (goto-char start)
          nil)))))

(defsubst comment-util--detect-line-comment-with-at-least-n-chars (n format)
  (let ((start (point)))
    (skip-indentation-forward)
    (cl-assert (stringp (comment-format-comment-chars-str format)))
    (let ((k (skip-chars-forward (comment-format-comment-chars-str format))))
      (if (<= n k)
          t
        (progn
          (goto-char start)
          nil)))))

(defun comment-util--detect-line-comment-with-at-least-2-chars (format)
  (comment-util--detect-line-comment-with-at-least-n-chars 2 format))

(defun comment-util--detect-line-comment-with-at-least-1-char (format)
  (comment-util--detect-line-comment-with-at-least-n-chars 1 format))

(cl-defstruct comment-format
  one-line
  region-begin
  region-end
  ;; line regexp is special regexp that should match all types of line comments,
  ;; i.e. ;+ for lisp, used to insert comment automatically
  line-regexp
  ;; Function of one arguments - current comment format structure.
  ;; Should return ‘t’ if there’s a comment after the position and
  ;; move the point past that comment.
  detect-line-comment
  comment-chars-str
  nested-ok?)

(defconst comment-util-comment-format-alist
  (mapcan (lambda (x)
            (if (listp (car x))
                (mapcar (lambda (y)
                          (cons y (cdr x)))
                        (car x))
              (list x)))
          `(((haskell-mode
              haskell-ts-mode
              haskell-c-mode
              haskell-c2hs-mode
              haskell-hsc-mode
              haskell-literate-mode
              ghc-core-mode
              haskell-cabal-mode
              haskell-cabal-config-mode
              haskell-cabal-project-mode
              alex-mode
              alex-grammar-mode
              uuag-mode
              happy-mode
              happy-grammar-mode
              agda2-mode
              lean-mode
              sql-mode
              lua-mode
              vhdl-mode)
             .
             ,(make-comment-format :one-line "--"
                                   :line-regexp "--+"
                                   :comment-chars-str "-"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-2-chars
                                   :nested-ok? nil))

            ((org-mode
              nix-mode
              makefile-mode
              makefile-gmake-mode
              makefile-automake-mode
              makefile-bsdmake-mode
              cmake-mode
              toml-mode
              cargo-toml-mode
              python-mode
              gnuplot-mode
              debsources-mode
              shell-script-mode
              awk-mode
              sh-mode
              sh-script-mode
              conf-space-mode
              conf-unix-mode
              conf-colon-mode
              conf-xdefaults-mode
              conf-mode
              conf-javaprop-mode
              rnc-mode
              gitignore-mode
              gitconfig-mode
              tcl-mode
              ucf-mode
              yaml-mode
              snippet-mode
              ptl-mode
              gdb-script-mode)
             .
             ,(make-comment-format :one-line "#"
                                   :line-regexp "#+"
                                   :comment-chars-str "#"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-1-char
                                   :nested-ok? nil))

            ((rust-mode
              cuda-mode
              ptx-mode
              c-mode
              c++-mode
              glsl-mode
              tablegen-mode
              graphviz-dot-mode
              antlr-mode
              idl-mode
              java-mode
              kotlin-mode
              kotlin-ts-mode
              groovy-mode
              js2-mode
              js-mode
              json-mode
              json-ts-mode
              css-mode
              verilog-mode
              nix-drv-mode
              cmm-mode)
             .
             ,(make-comment-format :one-line "//"
                                   :line-regexp "//+"
                                   :comment-chars-str "/"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-2-chars
                                   :nested-ok? nil))

            ((emacs-lisp-mode
              elisp-byte-code-mode
              inferior-emacs-lisp-mode
              lisp-interaction-mode
              cl-mode
              lisp-mode
              clojure-mode
              blueprint-mode
              vim-edmacro-mode
              lisp-data-mode
              git-commit-elisp-text-mode)
             .
             ,(make-comment-format :one-line ";;"
                                   :line-regexp ";+"
                                   :comment-chars-str ";"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-1-char
                                   :nested-ok? nil))

            ((wisent-grammar-mode
              bovine-grammar-mode)
             .
             ,(make-comment-format :one-line ";;"
                                   :line-regexp ";+"
                                   :comment-chars-str ";"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-2-chars
                                   :nested-ok? nil))

            ((asm-mode
              nasm-mode
              llvm-mode
              conf-windows-mode)
             .
             ,(make-comment-format :one-line ";"
                                   :line-regexp ";+"
                                   :comment-chars-str ";"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-1-char
                                   :nested-ok? nil))

            ((latex-mode
              prolog-mode
              prolog-inferior-mode
              erlang-mode)
             .
             ,(make-comment-format :one-line "%"
                                   :line-regexp "%+"
                                   :comment-chars-str "%"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-1-char
                                   :nested-ok? nil))

            (octave-mode
             .
             ,(make-comment-format :one-line "%"
                                   :line-regexp "\\(?:%+\\|#+\\)"
                                   :comment-chars-str "%#"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-1-char
                                   :nested-ok? nil))

            ((isar-mode
              sml-mode
              tuareg-mode)
             .
             ,(make-comment-format :region-begin "(*"
                                   :region-end "*)"
                                   :line-regexp "(\\*"
                                   :detect-line-comment #'ignore
                                   :nested-ok? t))

            ((bison-mode
              flex-mode)
             .
             ,(make-comment-format :region-begin "/*"
                                   :region-end "*/"
                                   :line-regexp "/[/*]"
                                   :detect-line-comment #'ignore
                                   :nested-ok? nil))

            ((markdown-mode
              nxhtml-mode
              nxml-mode
              html-mode
              sgml-mode
              web-mode)
             .
             ,(make-comment-format :region-begin "<!--"
                                   :region-end "-->"
                                   :line-regexp "<!--"
                                   :detect-line-comment #'ignore
                                   :nested-ok? nil))

            (rst-mode
             .
             ,(make-comment-format :one-line ".. "
                                   :line-regexp "\\.\\. "
                                   :detect-line-comment #'comment-util--detect-line-comment-with-regexp
                                   :nested-ok? nil))

            (j-mode
             .
             ,(make-comment-format :one-line "NB. "
                                   :line-regexp "NB\\. "
                                   :detect-line-comment #'comment-util--detect-line-comment-with-regexp
                                   :nested-ok? nil))

            (xmodmap-mode
             .
             ,(make-comment-format :one-line "!"
                                   :line-regexp "!+"
                                   :comment-chars-str "!"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-at-least-1-char
                                   :nested-ok? nil))

            (texinfo-mode
             .
             ,(make-comment-format :one-line "@comment"
                                   :line-regexp "@c\\(?:o\\(?:m\\(?:m\\(?:e\\(?:n\\(?:t?\\)?\\)?\\)?\\)?\\)?\\)?"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-regexp
                                   :nested-ok? nil))

            (dos-mode
             .
             ,(make-comment-format :one-line "rem "
                                   :line-regexp "rem ?"
                                   :detect-line-comment #'comment-util--detect-line-comment-with-regexp
                                   :nested-ok? nil))

            (comint-mode
             .
             ,(make-comment-format))))
  "List of per-mode specifications of comments.
Contains single-line and region comments.")

(defun comment-util-current-format ()
  (if-let (fmt (comment-util-current-format-lax))
      fmt
    (error "No comment format defined for current mode")))

(defsubst comment-util-current-format-lax ()
  (cdr (assq major-mode comment-util-comment-format-alist)))

;; (defun comment-util-current-format-one-line ()
;;   (aif (comment-format-one-line (comment-util-current-format))
;;       it
;;     (error "No one-line commenst defined for current mode")))

;; (defun comment-util-current-format-line-regexp ()
;;   (if-let ((fmt (comment-util-current-format)))
;;       (aif (comment-format-one-line fmt)
;;           it
;;         (error "No one-line commenst defined for current mode"))
;;     (error "No comment format defined for current mode")))

(defun comment-util-detect-line-comment (&optional format)
  "Check whether current line is commented. May move point past the leading comment."
  (let ((fmt (or format (comment-util-current-format))))
    (funcall (comment-format-detect-line-comment fmt) fmt)))

;;;###autoload
(define-minor-mode comment-util-mode
  "Minor mode to handle comments in various languages."
  :init-value nil
  :lighter nil
  :keymap nil ;; no keymap
  :group util
  :global nil)

;;; User-interface functions
;;;###autoload
(defun comment-util-comment-lines (lines)
  "Comment LINES lines eiter up if argument LINES is positive
or down if LINES is negative or comment whole region if region is active."
  (interactive "p")
  (if (region-active-p)
      (with-region-bounds-unadj start end
        (comment-util-comment-region--impl start end))
    (let ((fmt (comment-util-current-format)))
      (comment-util--comment-next-n-lines lines fmt))))

;;;###autoload
(defun comment-util-comment-region (_beg _end)
  "Comment region between BEGIN and END position inserting region comments if
they are defined for current mode or one-line comments otherwise."
  (interactive "*r")
  (with-region-bounds-unadj begin end
    (comment-util-comment-region--impl begin end)))

(defun comment-util-comment-region--impl (begin end)
  "Comment region between BEGIN and END position inserting region comments if
they are defined for current mode or one-line comments otherwise."
  (with-ignored-invisibility
   (save-excursion
     (let ((fmt (comment-util-current-format)))
       (if (comment-util-region-comments-defined? fmt)
           (comment-util--comment-chunk-region begin end fmt)
         (comment-util--comment-lined-region begin end fmt))))))

;;;###autoload
(defun comment-util-uncomment-region ()
  "Uncomment region at point commented either with line comments or block comments."
  (interactive "*")
  (save-excursion
    (let ((fmt (comment-util-current-format)))
      (if (or (save-excursion
                (save-match-data
                  (beginning-of-line)
                  (comment-util-detect-line-comment fmt)))
              (not (comment-util-region-comments-defined? fmt)))
          ;; If no region comments are defined then use line comments.
          (comment-util--uncomment-lined-region fmt)
        (comment-util-uncomment--chunk-region fmt)))))

(defun comment-util-region-comments-defined? (&optional format)
  (let ((fmt (or format (comment-util-current-format))))
    (and (comment-format-region-begin fmt)
         (comment-format-region-end fmt))))

;;; These two functions require somewhat special threat because
;;; the're used /only/ in vim visual mode

;;;###autoload
(defun comment-util-uncomment-region-simple (begin end)
  "Uncomment region between begin and end presumably commented with
line comments. If that's not the case then do nothing. Should
be used only for vim-visual-mode of the vim-mode package."
  (interactive "*r")
  (comment-util-uncomment-region-simple--impl begin end t))

(defun comment-util-uncomment-region-simple--impl (begin end indent?)
  "Uncomment region between begin and end presumably commented with
line comments. If that's not the case then do nothing. Should
be used only for vim-visual-mode of the vim-mode package."
  (save-excursion
    (goto-char begin)
    (beginning-of-line)
    (let ((format (comment-util-current-format))
          (end-line (save-excursion (goto-char end) (line-end-position))))
      (combine-change-calls
          (point)
          end
        (with-marker (end-marker (copy-marker end-line))
          (while (and (< (point) end-marker)
                      (not (eobp)))
            (comment-util--delete-comment format)
            (when indent?
              (indent-for-tab-command))
            (forward-line 1)))))))

;;;; core functionality, not for interactive use

;;;;; Mid-level functions

(defsubst comment-util--comment-lined-region (begin end fmt)
  "Comment region between BEGIN and END with one-line comments."
  (goto-char begin)
  (comment-util--comment-next-n-lines (count-lines-fixed begin end) fmt))

(defun comment-util--uncomment-lined-region (format)
  "Uncomment region that was commented with line comments."
  (skip-to-indentation)
  (let ((middle (point))
        (start (line-beginning-position))
        (end (line-end-position)))
    (unless (comment-util-detect-line-comment format)
      (error "Not in commented region"))
    ;; Go back to find the start of commented region.
    (goto-char middle)
    (let ((tmp nil)
          (continue? t))
      (while continue?
        (setf tmp (point))
        (if (save-excursion (comment-util-detect-line-comment format))
            (progn
              (setf start tmp)
              (if (bobp)
                  (setf continue? nil)
                (forward-line -1)))
          (setf continue? nil))))
    ;; Go forward to find the end of commented region.
    (goto-char middle)
    (let ((tmp nil)
          (continue? t))
      (while continue?
        (setf tmp (point))
        (if (save-excursion (comment-util-detect-line-comment format))
            (progn
              (setf end tmp)
              (if (eobp)
                  (setf continue? nil)
                (forward-line +1)))
          (setf continue? nil))))
    ;; Fix end position.
    (goto-char end)
    (setf end (line-end-position))
    ;; Do the actual uncommenting.
    (combine-change-calls
        start
        end
      (goto-char start)
      (with-marker (end-marker (copy-marker end))
        (while (and (comment-util--delete-comment format)
                    (<= (point) end-marker))
          (forward-line +1))))))

(defun comment-util--comment-chunk-region (begin end fmt)
  (save-excursion
    (unless (comment-format-nested-ok? fmt)
      (let ((has-open (progn (goto-char begin)
                             (search-forward (comment-format-region-begin fmt)
                                             end
                                             t)))
            (has-close (progn (goto-char begin)
                              (search-forward (comment-format-region-end fmt)
                                              end
                                              t))))
        (cond
          ((and has-open has-close)
           (error "Specified region already contains commented part"))
          (has-open
           (error "Specified region already contains open comment"))
          (has-close
           (error "Specified region already contains close comment"))))))
  (goto-char end)
  (insert comment-util--spaces-after-comment
          (comment-format-region-end fmt))
  (goto-char begin)
  (insert (comment-format-region-begin fmt)
          comment-util--spaces-after-comment))

(defun comment-util-uncomment--chunk-region (fmt)
  "Uncomment region around point surrounded by region begin and end markers."
  (let* ((begin-str (comment-format-region-begin fmt))
         (end-str   (comment-format-region-end fmt))
         (curr-pos  (point))
         (begin-pos (search-backward begin-str nil t))
         (end-pos   (search-forward end-str nil t)))
    (unless begin-pos
      (error "Cannot find beginning of the commented block"))
    (unless end-pos
      (error "Cannot find end of the commented block"))

    (when (comment-format-nested-ok? fmt)
      (when-let* ((state (syntax-ppss)))
        ;; Check that we’re really inside nested comment by testing depth.
        (when (numberp (parse-partial-sexp--inside-comment? state))
          (goto-char (parse-partial-sexp--comment-or-string-start state))
          (setf begin-pos (point))
          (forward-comment 1)
          (setf end-pos (point)))))

    (unless (and (<= begin-pos curr-pos)
                 (<= curr-pos end-pos))
      (error "Point is not within commented block"))

    ;; NB deletion of last comment must come first because
    ;; otherwise point must be adjusted.
    (goto-char end-pos)
    (delete-char (- (length end-str)))
    (when (text-before-matches? comment-util--spaces-after-comment)
      (delete-char (- (length comment-util--spaces-after-comment))))

    (goto-char begin-pos)
    (delete-char (length begin-str))
    (when (text-after-matches? comment-util--spaces-after-comment)
      (delete-char (length comment-util--spaces-after-comment)))))

;;;;; Low-level core functions

(cl-defun comment-util--comment-next-n-lines (lines fmt)
  (setf lines (or lines 1))
  (cond
    ;; has one-line comments defined
    ((comment-format-one-line fmt)
     (let ((col (indentation-size)))
       (save-excursion
         (dotimes (_ lines)
           ;; When not on empty line
           (when (not (eq (line-beginning-position)
                          (line-end-position)))
             (let ((new-col (indentation-size)))
               (setf col (min col new-col))))
           (forward-line 1)))
       (comment-util--comment-n-lines-starting-at-col (concat (comment-format-one-line fmt)
                                                              comment-util--spaces-after-comment)
                                                      lines
                                                      col
                                                      nil)))
    ((comment-util-region-comments-defined? fmt)
     (save-excursion
       (skip-to-indentation)
       (let ((begin (point)))
         ;; somewhat hackish but we're already in special case of
         ;; dealing with region comments
         (forward-line (- lines 1))
         (comment-util--comment-chunk-region begin (line-end-position) fmt))))
    (t
     (error "Neither single-line nor region comment formats defined"))))

(defun comment-util--comment-n-lines-starting-at-col (comment-str lines column update-column?)
  "Comment next LINES with COMMENT-STR, but insert them at COLUMN."
  (let* ((col column)
         (skip-to-column (lambda ()
                           (beginning-of-line)
                           (move-to-column col))))
    (combine-change-calls
        (line-beginning-position)
        (save-excursion
          (forward-line lines)
          (line-end-position))
      (when (< 0 lines)
        ;; This is the zeroth iteration at which we shouldn't
        ;; update column and use supplied one.
        (funcall skip-to-column)
        (insert comment-str)
        (forward-line 1)
        (cl-incf lines -1)
        (while (< 0 lines)
          ;; Is on empty line?
          (if (and (eq (char-before) ?\n)
                   (eq (char-after) ?\n))
              (progn
                (indent-to col)
                (insert comment-str))
            (progn
              (when update-column?
                (setf col (min col (indentation-size))))
              (funcall skip-to-column)
              (insert comment-str)))
          (forward-line 1)
          (cl-incf lines -1))))))

(defun comment-util--delete-comment (format)
  "Delete comments (e.g. //, ;) after point if any."
  (skip-indentation-forward)
  (let ((before-comment (point)))
    (when (comment-util-detect-line-comment format)
      (skip-chars-forward " " (+ (point)
                                 (eval-when-compile (length comment-util--spaces-after-comment))))
      (delete-region before-comment (point))
      t)))

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
       (current-column-fixed-uncached)
       t))))

(defun comment-util--on-commented-line? (fmt)
  "Return t if current line is commented out."
  (let ((detector (comment-format-detect-line-comment fmt)))
    (if (eq detector #'ignore)
        (save-excursion
          (let* ((state (parse-partial-sexp (line-beginning-position)
                                            (line-end-position)))
                 (inside-comment? (elt state 4)))
            ;; If parse end up inside comment then current line starts with a comment.
            inside-comment?))
      (save-excursion
        (beginning-of-line)
        (funcall detector fmt)))))

(defun comment-util--get-commented-region (fmt)
  "Return begin and end of region surrounding point that has
commented parts and leave point unchanged."
  (unless (comment-util--on-commented-line? fmt)
    (error "Not on commented line"))
  (let ((move-while-commented
         ;; return position of the beginning of the last line in direction
         ;; that is still has commented parts
         (lambda (forward?)
           (let ((dir (if forward? +1 -1)))
             (beginning-of-line)
             (while (and (not (if forward?
                                  (eobp)
                                (bobp)))
                         (comment-util--on-commented-line? fmt))
               (forward-line dir))
             (when (not (if forward?
                            (eobp)
                          (bobp)))
               (backward-line dir)
               ;; We’ve returned backwards onto line with comments
               ;; which is a known fact.
               (cl-assert (comment-util--on-commented-line? fmt)
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
  (let ((fmt (comment-util-current-format)))
    (cl-multiple-value-bind (start end)
        (comment-util--get-commented-region fmt)
      (save-excursion
        (save-match-data
          (let ((clear-comment (lambda ()
                                 (cond
                                   ((save-excursion
                                      (comment-util-detect-line-comment fmt))
                                    (delete-current-line))
                                   ;; for cases like
                                   ;; _|- (let (( ;; (foo 1)
                                   ;;            (bar 2))))
                                   ((awhen (comment-format-line-regexp fmt)
                                      (save-excursion
                                        (re-search-forward it (line-end-position) t)))
                                    (delete-region (match-beginning 0) (line-end-position)))))))
            (goto-char end)
            (while (and (<= start (point))
                        (not (bobp)))
              (beginning-of-line)
              (funcall clear-comment)
              (forward-line -1))
            (when (bobp)
              (funcall clear-comment)))))
      (skip-to-indentation))))

;;;###autoload
(defun lisp-uncomment-sexp ()
  (interactive)
  (let ((fmt (comment-util-current-format)))
    (cl-multiple-value-bind (start end)
        (comment-util--get-commented-region fmt)
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
          (delete-whitespace-forward))))))

;;;###autoload
(defun haskell-comment-line (&optional count)
  "Similar to `lisp-comment-sexp' buf for current Haskell node."
  (interactive (list current-prefix-arg))
  (let* ((fmt (comment-util-current-format))
         (comment-format
          (concat (comment-format-one-line fmt)
                  comment-util--spaces-after-comment)))
    (if count
        (save-excursion
          (skip-to-indentation)
          (comment-util--comment-n-lines-starting-at-col
           comment-format
           count
           (current-column-fixed)
           t))
      (comment-util-comment-lines 1))))

(provide 'comment-util)

;; Local Variables:
;; End:

;; comment-util.el ends here
