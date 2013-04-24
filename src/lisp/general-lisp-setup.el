;; general-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

;;;; Generic setups for current module

(eval-when-compile (require 'cl-lib))

(require 'all-lisp-setup)
(require 'common)
(require 'advices-util)
(require 'macro-util)
(require 'el-swank-fuzzy)
(require 'lisp-utils)

(require 'eldoc)
(require 'paredit)
(require 'align-let)
(require 'outline-headers)
(require 'rainbow-delimiters)
(require 'paredit-setup)


(eval-after-load
    'rainbow-delimiters
  '(progn
     ;; added handling of #\(, #\), etc in addition to ?\(, ?\), etc
     (redefun rainbow-delimiters-char-ineligible-p (loc)
       "Return t if char at LOC should be skipped, e.g. if inside a comment.

Returns t if char at loc meets one of the following conditions:
- Inside a string.
- Inside a comment.
- Is an escaped char, e.g. ?\)"
       (let ((parse-state (save-excursion
                            (beginning-of-defun)
                            ;; (point) is at beg-of-defun; loc is the char location
                            (parse-partial-sexp (point) loc))))
         (or (nth 3 parse-state)        ; inside string?
             (nth 4 parse-state)        ; inside comment?
             ;; check for ?\(, ?\), #\(, #\) etc
             (and (char= (char-before loc) ?\\)
                  (or (char= (char-before (- loc 1)) ?\?)
                      (char= (char-before (- loc 1)) ?\#))))))))

(eval-after-load
    'lisp-mode
  '(progn
     ;; this function contained what I'm considering a bug - and it turned out to
     ;; be so - it treated single-semicolon comments as special and tried to
     ;; indent them as comment lines (i.e. to the fill-column) which turned out
     ;; to lead to infinite loop as delegate functions would end up
     ;; calling lisp-indent-line again and again
     ;; So the single-semicolon comments are treated just as double-semicolon ones
     (defun lisp-indent-line (&optional whole-exp)
       "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
       (interactive "P")
       (let ((indent (calculate-lisp-indent)) shift-amt end
             (pos (- (point-max) (point)))
             (beg (progn (beginning-of-line) (point))))
         (skip-chars-forward " \t")
         (if (or (null indent) (looking-at-pure? "\\s<\\s<\\s<"))
           ;; Don't alter indentation of a ;;; comment line
           ;; or a line that starts in a string.
           (goto-char (- (point-max) pos))
           ;; Single-semicolon comment lines should *not* be indented
           ;; as comment lines, but should be indented as code
           (progn
             (when (listp indent)
               (setq indent (car indent)))
             (setq shift-amt (- indent (current-column)))
             (unless (zerop shift-amt)
               (delete-region beg (point))
               (indent-to indent))
             ;; If initial point was within line's indentation,
             ;; position after the indentation.  Else stay at same point in text.
             (when (> (- (point-max) pos) (point))
               (goto-char (- (point-max) pos)))
             ;; If desired, shift remaining lines of expression the same amount.
             (and whole-exp (not (zerop shift-amt))
                  (save-excursion
                    (goto-char beg)
                    (forward-sexp 1)
                    (setq end (point))
                    (goto-char beg)
                    (forward-line 1)
                    (setq beg (point))
                    (> end beg))
                  (indent-code-rigidly beg end shift-amt))))))))

(make-align-function lisp-align-on-comments ";+")


(setq-default lisp-indent-function #'lisp-indent-function)
(setf lisp-indent-function #'lisp-indent-function)

;;;; Utility functions covering broad range of topics
;;;; These may be useful in any succeeding lisp setups

(defun lisp-point-inside-form (form-re)
  "Return t if point is positioned within sexp form
whose start (including open paren) matches FORM-RE."
  (save-excursion
    ;; if point is inside string our advice will handle this case
    (condition-case nil
        (progn
          (backward-up-list)
          (looking-at-pure? form-re))
      (error nil))))

(defun lisp-point-inside-form-n (n form-re)
  "Return t if point is positioned within Nth enclosing sexp form
whose start (including open paren) matches FORM-RE."
  (save-excursion
    ;; if point is inside string our advice will handle this case
    (condition-case nil
        (progn
          ;; (dotimes (i n)
          ;;   (backward-up-list))
          ;; (looking-at-p form-re)
          (loop
            for i below n
            for result = (progn
                           (backward-up-list)
                           (looking-at-pure? form-re))
            if result
            return t))
      (error nil))))


(defun lisp-point-inside-format-or-error-stringp ()
  "Return t if point is positioned inside a format string."
  (and (lisp-point-inside-form "( *\\(?:format\\|error\\)\\_>")
       (lisp-point-inside-string?)))

(defun lisp-point-nested-inside-declaration-formp ()
  "Return t if point is nested somewhere in proclaim/declaim/declare
declaration, not necessarily in actual sexp, e.g.
not only (declare (..) _|_ ...) but (declare (... _|_ ...) ...) too.
But nesting of more than one sexp is not supported yet
(and probably isn't required at all)."
  (and (lisp-point-inside-form-n 3
                                 (rx "("
                                     (* whitespace)
                                     (or "declare"
                                         "declaim"
                                         "proclaim"
                                         "locally")
                                     symbol-end))
       (not (lisp-point-inside-string-or-comment?))))

(defun lisp-point-nested-inside-type-using-formp ()
  (and (or (lisp-point-inside-form
            (rx "("
                (* whitespace)
                (or "check-type"
                    "typep"
                    "merge"
                    "make-array"
                    "the"
                    "make-sequence"
                    "set-pprint-dispatch"
                    "concatenate"
                    "upgraded-array-element-type"
                    "subtypep"
                    "make-condition"
                    "map"
                    "coerce"
                    "typecase"
                    "ctypecase"
                    "etypecase"
                    "type"
                    "ftype")
                symbol-end))
           (lisp-point-inside-form-n 2
                                     (rx "("
                                         (* whitespace)
                                         (or "declare"
                                             "declaim"
                                             "proclaim"
                                             "locally"

                                             "defstruct")
                                         symbol-end))
           (lisp-point-inside-form-n 3
                                     (rx "("
                                         (* whitespace)
                                         "defclass"
                                         symbol-end)))
       (not (lisp-point-inside-string-or-comment?))))



(defun lisp-point-inside-string? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let* ((end (point))
           (begin
             ;; if this proves itself too slow then use line-beginning-position
             (if (beginning-of-defun)
               (point)
               (line-beginning-position)))
           (state (parse-partial-sexp begin end)))
      (elt state 3))))

(defun lisp-point-inside-string-or-comment? ()
  "Return t if point is positioned inside a string or comment."
  (save-excursion
    (let* ((end (point))
           (begin
             ;; if this proves itself too slow then use line-beginning-position
             (if (beginning-of-defun)
               (point)
               (line-beginning-position)))
           (state (parse-partial-sexp begin
                                      end))
           (inside-stringp (elt state 3))
           (inside-commentp (elt state 4)))
      (or inside-stringp inside-commentp))))

(defun lisp-point-inside-comment? ()
  "Return t if point is inside comment starting at nearest defun or beginning
of line."
  (save-excursion
    (let* ((end (point))
           (begin
             ;; if this proves itself too slow then use line-beginning-position
             (if (beginning-of-defun)
               (point)
               (line-beginning-position)))
           (state (parse-partial-sexp begin
                                      end)))
      (elt state 4))))


;; (defun lisp-point-inside-string-fastp ()
;;   "Return t if point is positioned inside a string.
;; Should be faster than `lisp-point-inside-string?'."
;;   (save-excursion
;;    (let* ((end (point))
;;           (begin
;;             ;; if this proves itself too slow then use line-beginning-position
;;             (if (glisp/backward-up-list)
;;               (point)
;;               (line-beginning-position)))
;;           (state (parse-partial-sexp begin
;;                                      end))
;;           (inside-stringp (elt state 3)))
;;      inside-stringp)))


(defun lisp-position-inside-string (p)
  "Return beginnig of a string point P is positioned in and return nil
if it's not in string."
  (save-excursion
    (goto-char p)
    (let* ((end (point))
           (begin
             ;; if this proves itself too slow then use line-beginning-position
             (if (beginning-of-defun)
               (point)
               (line-beginning-position)))
           (state (parse-partial-sexp begin
                                      end))
           (inside-stringp (elt state 3))
           (start (elt state 8)))
      (when inside-stringp
        start))))

(defun lisp-position-inside-comment (p)
  "Return beginnig of a comment point P is positioned in and return nil
if it's not in comment."
  (save-excursion
    (goto-char p)
    (let* ((end (point))
           (begin
             ;; if this proves itself too slow then use line-beginning-position
             (if (beginning-of-defun)
               (point)
               (line-beginning-position)))
           (state (parse-partial-sexp begin
                                      end))
           (inside-commentp (elt state 4))
           (start (elt state 8)))
      (when inside-commentp
        start))))

(defun lisp-position-inside-string-or-comment (p)
  "Return character address of beginning of a string or comment
point P is positioned in, and nil if not positioned in string
nor comment."
  (save-excursion
    (goto-char p)
    (let* ((end (point))
           (begin
             ;; if this proves itself too slow then use line-beginning-position
             (if (beginning-of-defun)
               (point)
               (line-beginning-position)))
           (state (parse-partial-sexp begin
                                      end))
           (inside-stringp (elt state 3))
           (inside-commentp (elt state 4))
           (start (elt state 8)))
      (when (or inside-stringp inside-commentp)
        start))))

;;; sexps

;;;###autoload
(defun lisp-pos-is-beginning-of-sexp? (&optional pos)
  "Check if there's sexp starting at POS."
  (char= ?\( (char-syntax (char-after pos))))

;;;###autoload
(defun lisp-pos-is-end-of-sexp? (&optional pos)
  "Check if there's sexp ending at POS."
  (char= ?\) (char-syntax (char-after pos))))

(defun lisp-beginning-of-sexp-at-pos (pos)
  (when (lisp-pos-is-end-of-sexp? pos)
    (save-excursion
      (goto-char (1+ pos))
      (backward-sexp 1)
      (point))))

(defun lisp-end-of-sexp-at-pos (pos)
  (when (lisp-pos-is-beginning-of-sexp? pos)
    (save-excursion
      (goto-char pos)
      (forward-sexp 1)
      (1- (point)))))


(defsubst lisp-at-beginning-of-sexp? ()
  (and (not (eobp))
       (lisp-pos-is-beginning-of-sexp? (point))))

(defsubst lisp-at-end-of-sexp? ()
  (and (not (eobp))
       (lisp-pos-is-end-of-sexp? (point))))

(defsubst lisp-beginning-of-sexp-at-point ()
  (lisp-beginning-of-sexp-at-pos (point)))

(defsubst lisp-end-of-sexp-at-point ()
  (lisp-end-of-sexp-at-pos (point)))


;;; lists, strings

(defun lisp-pos-is-beginning-of-list? (pos)
  (char= ?\( (char-after pos)))

(defun lisp-pos-is-end-of-list? (pos)
  (char= ?\) (char-after pos)))

(defun lisp-beginning-of-list-at-pos (pos)
  "Return position of \( which corresponds to \) at position POS."
  (when (lisp-pos-is-end-of-list? pos)
    (save-excursion
      (goto-char (1+ pos))
      (backward-sexp 1)
      (point))))

(defun lisp-end-of-list-at-pos (pos)
  "Return position of \) which corresponds to \( at position POS."
  (when (lisp-pos-is-beginning-of-list? pos)
    (save-excursion
      (goto-char pos)
      (forward-sexp 1)
      (1- (point)))))


(defun lisp-pos-is-beginning-of-string? (pos)
  (and (char= ?\" (char-syntax (char-after pos)))
       (save-excursion
         (goto-char pos)
         (forward-char 1)
         (lisp-point-inside-string?))))

(defun lisp-pos-is-end-of-string? (pos)
  (and (char= ?\" (char-syntax (char-after pos)))
       (not
        (save-excursion
          (goto-char pos)
          (forward-char 1)
          (lisp-point-inside-string?)))))


(defsubst lisp-at-beginning-of-list? ()
  (and (not (eobp))
       (lisp-pos-is-beginning-of-list? (point))))

(defsubst lisp-at-end-of-list? ()
  (and (not (eobp))
       (lisp-pos-is-end-of-list? (point))))

(defsubst lisp-beginning-of-list-at-point ()
  (lisp-beginning-of-list-at-pos (point)))

(defsubst lisp-end-of-list-at-point ()
  "Return position of \) which corresponds to \( at point."
  (lisp-end-of-list-at-pos (point)))

(defsubst lisp-at-beginning-of-string? ()
  (and (not (eobp))
       (lisp-pos-is-beginning-of-string? (point))))

(defsubst lisp-at-end-of-string? ()
  (and (not (eobp))
       (lisp-pos-is-end-of-string? (point))))

;;; list navigation, realign let

(eval-after-load
    "lisp"
  '(progn
     ;; once this was an advice, but it's cleaner to redefine this up-list thing
     ;; since backward-up-list uses up-list to do it's job
     ;; only one advice is necessary
     (redefun up-list (&optional arg)
       "Move forward out of one level of parentheses.
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot.
This command assumes point is not in a string or comment."
       (interactive "^p")
       (let ((str-or-comm-start (lisp-position-inside-string-or-comment (point))))
         (when str-or-comm-start
           ;; jump out of string/comment
           (goto-char (max (point-min) (1- str-or-comm-start))))
         ;; unless we're moved to correct destination, perform move
         (when (or
                ;; if we're not in string/comment then move is necessary
                (not str-or-comm-start)
                ;; if were're in string/comment then check if we
                ;; still have to do any moves
                (not (lisp-pos-is-beginning-of-sexp? (point))))
           (setf arg (or arg 1))
           (let ((inc (if (> arg 0) 1 -1))
                 pos)
             (while (/= arg 0)
               (if (null forward-sexp-function)
                 (goto-char (or (scan-lists (point) inc 1) (buffer-end arg)))
                 (condition-case err
                     (while (progn (setq pos (point))
                                   (forward-sexp inc)
                                   (/= (point) pos)))
                   (scan-error (goto-char (nth (if (> arg 0) 3 2) err))))
                 (if (= (point) pos)
                   (signal 'scan-error
                           (list "Unbalanced parentheses" (point) (point)))))
               (setq arg (- arg inc)))))))))

(defun realign-let ()
  "Realign let/setq/setf/etc form at point."
  (interactive)
  (save-excursion
    (condition-case nil
        (progn
          (align-let)
          (glisp/backward-up-list)
          (indent-sexp))
      (error nil))))

;;; navigation

(vimmize-motion (condition-case nil
                    (progn
                      (when (looking-at-start-of-sexp?)
                        (forward-char 1))
                      (up-list 1))
                  (error (error "No enclosing list found")))
                :name vim:lisp-up-list
                :exclusive t
                :do-not-adjust-point t)

(defun glisp/backward-up-list ()
  "Move out one level of parenthesis or string quotes."
  (interactive)
  (let ((start (point)))
    (condition-case err
        (aif (lisp-position-inside-string (point))
          (goto-char it)
          (backward-up-list))
      (error (goto-char start)
             (error "No enclosing list found\n%s" err)))))

(vimmize-motion glisp/backward-up-list
                :name vim:lisp-backward-up-list
                :exclusive t
                :do-not-adjust-point t)




(add-to-list 'debug-ignored-errors "\\`No enclosing list found\\'")

(defun glisp/find-beginning-of-defun (if-nothing-was-done)
  (let ((done-up-list nil))
    (condition-case nil
        (while (or (not done-up-list)
                   (not (looking-at-pure? (rx "("
                                              (* whitespace)
                                              symbol-start
                                              (or "definline"
                                                  "defmacro"
                                                  "defmacro-"
                                                  "defmethod"
                                                  "defmulti"
                                                  "defn"
                                                  "defn-"
                                                  "defonce"
                                                  "defprotocol"
                                                  "defrecord"
                                                  "defstruct"
                                                  "deftest"
                                                  "deftest-"
                                                  "deftype"
                                                  "def"
                                                  "ns"

                                                  "defclass"
                                                  "defconstant"
                                                  "defgeneric"
                                                  "define-compiler-macro"
                                                  "define-condition"
                                                  "define-method-combination"
                                                  "define-modify-macro"
                                                  "define-setf-expander"
                                                  "define-symbol-macro"
                                                  "defmacro"
                                                  "defmethod"
                                                  "defpackage"
                                                  "defparameter"
                                                  "defsetf"
                                                  "defstruct"
                                                  "deftype"
                                                  "defun"
                                                  "defvar"
                                                  "defconst"

                                                  "defadvice"
                                                  "defun*"
                                                  "defmacro*"
                                                  "defsubst"
                                                  ;; don't really use this
                                                  ;; "lambda"
                                                  "define"
                                                  "define*"
                                                  "define-macro"
                                                  "define-syntax"
                                                  "define-method"
                                                  "define-generic"
                                                  "define-constant")
                                              symbol-end))))
          (glisp/backward-up-list)
          (setf done-up-list t))
      ;; outermost list met, full stop then
      (error))
    (unless done-up-list
      (funcall if-nothing-was-done))))

(defun glisp/beginning-of-defun ()
  (interactive)
  (vim:save-in-function-position)
  (glisp/find-beginning-of-defun #'backward-sexp))

(defun glisp/end-of-defun ()
  (interactive)
  (vim:save-in-function-position)
  (glisp/find-beginning-of-defun #'ignore)
  (forward-sexp))

;;; other

(defmacro* define-lisp-print-info-skeleton (name
                                            &key
                                            (doc nil)
                                            (print-begin "(format t ")
                                            (make-variable-list
                                             #'join-lines)
                                            (msg-transform nil)
                                            (format-print-value "~a")
                                            (format-string-start "\"")
                                            (format-string-end "~%\"")
                                            (insert-entity-name-procedure nil)
                                            (print-end ")"))
  `(define-print-info-skeleton
       ,name
     :doc ,doc
     :insert-entity-name-procedure
     ,(if insert-entity-name-procedure
        insert-entity-name-procedure
        (lambda (beginning)
          (save-excursion
            (condition-case nil
                (progn (goto-char beginning)
                       (save-excursion
                         ;; this throws error if no enclosing list found
                         (backward-up-list))
                       (beginning-of-defun)
                       (forward-symbol 1)
                       (paredit-skip-whitespace t)
                       (let ((symbol (symbol-at-point)))
                         (if symbol
                           (concat ,(if msg-transform
                                      `(funcall ,msg-transform (symbol-name symbol))
                                      '(symbol-name symbol))
                                   ": ")
                           "")))
              ;; no enclosing list was found, so use no name here
              (error "")))))
     :print-begin ,print-begin
     :print-end ,print-end

     :indent-after-func prog-indent-sexp
     :make-variable-list ,make-variable-list
     :msg-transform ,msg-transform

     :format-print-value ,format-print-value
     :format-string-start ,format-string-start
     :format-string-end ,format-string-end

     :insert-newline-before-var-list t))

(defun lisp-reindent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)))

(defun lisp-indent-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (backward-sexp)
      (indent-sexp)
      (forward-sexp)
      (forward-sexp)
      (unless (eobp)
        (backward-sexp)))))

(dolist (mode +lisp-modes+)
  (add-to-list '*mode-buffer-indent-function-alist*
               (cons mode #'lisp-indent-buffer)))


;;; this is useful for all lisps

(search-def-autoexpand-advices
 (save-excursion
   (when (hs-already-hidden-p)
     (hs-show-block))
   (when (outline-invisible-p)
     (show-subtree)))
 (lisp-mode common-lisp-mode scheme-mode emacs-lisp-mode))

;;; keybindings as variables

(defvar *lisp-vim-normal-mode-keybindings*
  '(("g c c"   lisp-comment-sexp)
    ("g c u"   lisp-uncomment-sexp)
    ("g c d"   lisp-delete-commented-part)

    ("x"       vim:paredit-forward-kill)
    ("X"       vim:paredit-backward-kill)

    ("g ("     vim:splice-sexp-killing-backward)
    ("g )"     vim:splice-sexp-killing-forward)

    ;; paredit slurps/barfs
    ("( l"     vim:backward-slurp-sexp)
    ("( r"     vim:backward-barf-sexp)
    (") l"     vim:forward-barf-sexp)
    (") r"     vim:forward-slurp-sexp)

    ("( ("     vim:backward-slurp-sexp)
    ("( )"     vim:backward-barf-sexp)
    (") ("     vim:forward-barf-sexp)
    (") )"     vim:forward-slurp-sexp)

    ("g <tab>" paredit-reindent-defun)
    ("M-p"     browse-kill-ring)
    ("="       input-unicode)))

(defvar *lisp-vim-movement-keybindings*
  '(("q"        vim:lisp-up-list)
    ("Q"        vim:lisp-backward-up-list)
    ("'"        vim:lisp-backward-up-list)
    ("g n"      glisp/beginning-of-defun)
    ("g t"      glisp/end-of-defun)
    ("g <up>"   glisp/beginning-of-defun)
    ("g <down>" glisp/end-of-defun)
    ("<home>"   paredit-backward)
    ("<end>"    paredit-forward)))

(defvar *lisp-search-keybindings*
  '(("*" search-for-symbol-at-point-forward)
    ("#" search-for-symbol-at-point-backward)))

;;;; Actual setup functions

(defun* lisp-setup (&key (use-whitespace t)
                         (use-cl-indent nil))
  (init-common :use-yasnippet nil
               :use-whitespace use-whitespace
               :use-render-formula t)
  ;; (autopair-mode) ;; don't need for lisp but useful for elisp
  (rainbow-delimiters-mode 1)
  (hs-minor-mode 1)
  ;; hiding of comments is rather annoying feature when working with lisps
  (setq-local hs-hide-comments-when-hiding-all nil)
  (enable-paredit-mode)
  ;; (setq-local paredit-space-for-delimiter-predicates
  ;;             (list #'paredit-insert-space-after-reader-sharp?))

  (when use-whitespace
    (setq-local whitespace-line-column 81)
    (setq-local whitespace-style '(face lines-tail tabs)))
  ;; (whitespace-mode 1)

  (el-swank-set-completion-syntax 'lisp)

  (setq-local comment-style 'indent)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-padding " ")

  (if use-cl-indent
    (setf lisp-indent-function #'common-lisp-indent-function)
    ;; somehow setf does not work here
    (setq-local lisp-indent-function #'lisp-indent-function))
  ;; just in case someone will want to use standard #'lisp-indent-function
  ;; put information for this case
  ;; (put 'if 'lisp-indent-function nil)

  (add-hook 'after-save-hook #'make-script-file-exec nil t)

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap)
        vim:complex-command-override-local-keymap (make-sparse-keymap))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    *lisp-search-keybindings*)

  (def-keys-for-map vim:normal-mode-local-keymap
    *lisp-vim-normal-mode-keybindings*

    (", c c"    comment-util-comment-lines)
    (", c u"    comment-util-uncomment-region)

    ;; universal align, aligns everything
    ;; but currently only aligns let's, setq's etc
    ("g a"      nil)
    ("g a a"    realign-let)
    ("g a l"    realign-let)

    ("<return>" paredit-newline)

    ("S"        paredit-split-sexp)
    ("J"        paredit-join-sexps)

    ("z o"      hs-show-block)
    ("z c"      hs-hide-block)
    ("z C"      hs-hide-all)
    ("z O"      hs-show-all))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("g a"      nil)
    ("g a ;"    lisp-align-on-comments)

    ("z c"      hs-hide-sexps-in-region)
    ("z o"      hs-show-sexps-in-region)
    ("g c c"    comment-util-comment-region)
    ("g c u"    comment-util-uncomment-region-simple))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("C-="      input-unicode)
    ("<return>" paredit-newline))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:operator-pending-mode-local-keymap
                     vim:motion-mode-local-keymap)
    *lisp-vim-movement-keybindings*

    ("<up>"    previous-line)
    ("<down>"  next-line)
    ("<left>"  backward-char)
    ("<right>" forward-char))

  (def-keys-for-map (vim:motion-mode-local-keymap
                     vim:operator-pending-mode-local-keymap)
    ("w" vim:paredit-forward-word)
    ("e" vim:paredit-forward-word-end)
    ("b" vim:paredit-backward-word)
    ("W" vim:paredit-forward-WORD)
    ("E" vim:paredit-forward-WORD-end)
    ("B" vim:paredit-backward-WORD)
    ("s"   vim:paredit-inner-symbol)
    ("i s" vim:paredit-inner-symbol)
    ("a s" vim:paredit-outer-symbol)
    ("S"   vim:paredit-backward-symbol))

  ;; (def-keys-for-map vim:complex-command-override-local-keymap
  ;;   ("d w" vim:paredit-forward-kill-word)
  ;;   ("d e" vim:paredit-forward-kill-word)
  ;;   ("d b" vim:paredit-backward-kill-word))

  (setup-outline-headers :header-symbol ";"
                         :length-min 3
                         :length-max 9))

(defun lisp-repl-setup ()
  (lisp-setup :use-whitespace nil)
  (init-repl)

  (whitespace-mode -1)

  ;; don't use prompt regexp to make comint use special field property
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-regexp "^[^> \n\t\r\f\v]*\\(>+:?\\|[*?]+\\) *")

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)
    ("<tab>"    nil)
    ("C-SPC"    comint-clear-buffer-above-prompt)
    ;; ("S-SPC"    comint-clear-buffer-above-prompt)

    ("M-p"      browse-kill-ring)
    ("M-P"      browse-comint-input-history)
    ("<return>" comint-send-input)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)))


(provide 'general-lisp-setup)

;; Local Variables:
;; End:

;; general-lisp-setup.el ends here
