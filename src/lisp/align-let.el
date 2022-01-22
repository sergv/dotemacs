;; align-let.el --- align expressions in a lisp "let"

;; Copyright 2005, 2006, 2007, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 11
;; Keywords: languages
;; URL: http://user42.tuxfamily.org/align-let/index.html
;; EmacsWiki: AlignLet

;; align-let.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; align-let.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `M-x align-let' aligns the value parts of bindings in a "let" form or a
;; multi-variable "setq".  It's designed for elisp and scheme but should
;; work with other lisp variants.  Eg.
;;
;;     (let ((x       1)
;;           (counter 2)
;;           (foo     3))
;;       ...)
;;
;; or
;;
;;     (setq abc   456
;;           xyzzy 789)
;;
;; Forms are recognised and traversed using Emacs usual paren syntax and
;; sexp motion, so it should handle complicated value expressions better
;; than align.el's regexps.
;;
;; `M-x align-let-region' can act on all `let' and `setq' forms in the
;; region from point to mark (or any region if called from lisp code).  It
;; might help you clean up a whole file of code.

;;; Install:

;; Put align-let.el somewhere in one of your `load-path' directories and to
;; get M-x align-let add to your .emacs
;;
;;     (autoload 'align-let "align-let" nil t)
;;
;; The suggested keybinding is C-c C-a, which can be made from your desired
;; mode hooks with
;;
;;     (autoload 'align-let-keybinding "align-let" nil t)
;;     (add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
;;     (add-hook 'scheme-mode-hook     'align-let-keybinding)
;;
;; There's actually no point trying to autoload for `emacs-lisp-mode'
;; because the *scratch* buffer uses that mode and thus drag in align-let.el
;; at startup.  You can avoid that with an explicit `define-key' like the
;; following (and a key other than C-c C-a if desired).
;;
;;     (autoload 'align-let "align-let" nil t)
;;     (define-key emacs-lisp-mode-map [?\C-c ?\C-a] 'align-let)
;;     (eval-after-load "scheme"
;;       '(define-key scheme-mode-map [?\C-c ?\C-a] 'align-let))
;;
;; For emacs-lisp-mode an (eval-after-load "lisp-mode" ...) would be the
;; form, but it's unnecessary because emacs-lisp-mode is pre-loaded.  Such
;; an eval-after-load also doesn't work at all in XEmacs 21.4.22 due to it
;; ending up with full paths on pre-loaded .el files.

;;; Emacsen:

;; Designed for Emacs 20 up, works in XEmacs 21.

;;; History:

;; Version 1 - the first version
;; Version 2 - recognise Scheme named let
;; Version 3 - tighten up a regexp, add autoloads and keybind function
;; Version 4 - don't align a value on a new line, handle bare elisp vars
;; Version 5 - fix for comments between binding forms
;; Version 6 - more sexp moving instead of looking-at
;; Version 7 - down-list arg for emacs20 support
;; Version 8 - fixes for unclosed or incomplete bindings at end of buffer
;; Version 9 - new align-let-spaces suggested by Drew Adams
;; Version 10 - new align-let-region suggested by Drew Adams
;;            - don't be tricked by point within a comment
;; Version 11 - align multi-variable setqs too

;;; Code:

;;;###autoload
(defgroup align-let nil
  "Align values in `let' forms."
  :prefix "align-let-"
  :group  'lisp ;; is this the best place?
  :link   '(url-link
            :tag "align-let.el home page"
            "http://user42.tuxfamily.org/align-let/index.html"))

(defcustom align-let-spaces 1
  "How many spaces to put between variables and values.
The default 1 means a single space after the longest variable,
and all values aligned to that.

    (let ((x   1)
          (foo 2))

A value 3 would instead give 3 spaces after the longest variable,

    (let ((x     1)
          (foo   2))"

  :group 'align-let
  :type  'integer)

;; A file local variable might set this as a matter of style in a particular
;; file so mark as safe, and autoload the safety in case align-let.el is not
;; yet loaded.  Crib note: "(defcustom :safe 'integerp)" is only in emacs23
;; up, so just `put' for emacs22.
;;
;;;###autoload
(put 'align-let-spaces 'safe-local-variable 'integerp)


;;-----------------------------------------------------------------------------
;; vaguely generic

(defun align-let-forward-comments ()
  "Move point forward across comments and whitespace."

  ;; xemacs 21.4.22 `forward-comment' is a bit buggy at end of buffer.  It
  ;; loops for the full specified count, without noticing end of buffer.
  ;; Using "point-max - point" is enough to cover all possible comments and
  ;; avoids a huge count in a big buffer.  The slowdown as such seems
  ;; noticable only at about 10,000,000 anyway.  A loop `(while
  ;; (forward-comment 1))' is no good, since in that xemacs the return is t
  ;; at end of buffer even when no comment has been skipped or point moved
  ;; at all.
  (forward-comment (- (point-max) (point))))

(defun align-let-symbol-string-p (str)
  "Return non-nil if STR looks like a symbol.
This means STR is entirely word or symbol syntax characters."
  (string-match "\\`\\(\\sw\\|\\s_\\)+\\'" str))

(defmacro align-let-save-point (&rest body)
  "Evaluate BODY with point saved by an `unwind-protect'.
Point is held in a marker in variable
`align-let-save-point-marker'.  BODY can change it with
`set-marker' to go to a different place, for both normal and
error exits."
  `(let ((align-let-save-point-marker (point-marker)))
     (unwind-protect
         (progn ,@body)
       (goto-char align-let-save-point-marker))))

(defun align-let-looking-at-nest ()
  "Return non-nil if point is at a nested parens \"((...\".
There can be comments and whitespace between the two parens like

    ( ;; blah blah
     ( ..."

  (save-excursion
    (align-let-forward-comments)
    (and (looking-at "\\s(")
         (progn
           (goto-char (match-end 0))
           (align-let-forward-comments)
           (looking-at "\\s(")))))

(defun align-let-backward-to-code ()
  "Move point back to the start of a preceding sexp form.
This gets out of strings, comments, backslash quotes, etc, to a
place where it makes sense to start examining sexp code forms.

The preceding form is found by a `parse-partial-sexp' starting
from `beginning-of-defun'.  If it finds nothing then just go to
`beginning-of-defun'."

  (let ((old (point)))
    (beginning-of-defun)
    (let ((parse (parse-partial-sexp (point) old)))
      (cond ((nth 2 parse) ;; innermost sexp
             (goto-char (nth 2 parse))
             (forward-sexp))
            ((nth 8 parse) ;; outside of comment or string
             (goto-char (nth 8 parse)))
            ((nth 5 parse) ;; after a quote char
             (goto-char (1- (point))))))))

(defun align-let-same-line-p (point1 point2)
  "Return non-nil if positions POINT1 and POINT2 are on the same line."
  (save-excursion
    (goto-char (min point1 point2))
    (not (search-forward "\n" (max point1 point2) t))))

(defun align-let-forward-sexp-must (n)
  "Move `forward-sexp' N times or throw an error.
An error is thrown if end of buffer is reached before N sexps are
skipped.  This is stricter than plain `forward-sexp' which throws
an error for a closing paren before N forms, but not end of
buffer."
  (while (> n 0)
    (align-let-forward-comments)
    (let ((orig (point)))
      (forward-sexp)
      (if (eq (point) orig)
          (error "No further sexp")))
    (setq n (1- n))))


;;-----------------------------------------------------------------------------
;; align-let

(defun align-let-looking-at-let ()
  "Return non-nil if a \"(let\" or \"(setq\" form is at or following point.
If it is then move point into the bindings part of the let,

    (let ((foo 123) ...
          ^---point

or after the function of a setq,

    (setq x 123 y 456 ...
         ^---point

See `align-let' for how `let' and `setq' forms are identified.
The return is t for a let, symbol setq for a setq, or nil if neither."

  (align-let-save-point
   (let (end name prop count)
     (and (progn
            (align-let-forward-comments)
            (looking-at "\\s(")) ;; must be a "(..." form

          (condition-case nil
              (progn
                (goto-char (1+ (point))) ;; past opening "("
                (forward-sexp)
                (setq end (point))
                (backward-sexp) ;; first contained sexp
                t)
            (error nil))

          (progn
            ;; first of the form must be a symbol
            (setq name (buffer-substring-no-properties (point) end))
            (align-let-symbol-string-p name))

          (progn
            (setq prop (get (intern-soft name) 'align-let))
            (cond ((eq prop 'setq)
                   ;; specials `setq' from `align-let' property, look for at
                   ;; least 4 args
                   (condition-case nil
                       (progn
                         (align-let-forward-sexp-must 1)
                         (let ((args (point)))
                           (align-let-forward-sexp-must 4)
                           (set-marker align-let-save-point-marker args)
                           'setq))
                     (error
                      nil)))

                  (prop
                   ;; specials `and-let*' etc with bindings at arg number
                   ;; `count' from `align-let' property, and not necessarily
                   ;; a "((" nesting there
                   (let ((count (if (eq prop t) 1 prop)))
                     (condition-case nil
                         (progn
                           (forward-sexp count)
                           (down-list 1))
                       (error
                        (error "Less than %s argument(s) to `%s'"
                               count name))))

                   (set-marker align-let-save-point-marker (point))
                   prop) ;; return true

                  (t
                   ;; look for a "((..." argument
                   (let ((found nil))
                     (while (and (not found)
                                 (let ((old (point)))
                                   (condition-case nil
                                       (progn
                                         (forward-sexp)
                                         (/= (point) old)) ;; stop if not moved
                                     (error nil)))) ;; stop if bad paren
                       (when (setq found (align-let-looking-at-nest))
                         (down-list 1)
                         (set-marker align-let-save-point-marker (point))))
                     found))))))))

(put 'and-let* 'align-let t) ;; Scheme
(put 'setq         'align-let 'setq)
(put 'setq-default 'align-let 'setq)
(put 'psetq        'align-let 'setq) ;; from cl.el
(put 'setf         'align-let 'setq)
(put 'psetf        'align-let 'setq)

(defun align-let-find-let ()
  "Find a `let' or `setq' surrounding point.
If a `let' is found then move point into the bindings form and
return t.  If a `setq' is found then move point after the `setq'
function name and return symbol setq.  An error is thrown if
neither surrounds point (at the current or some outer parens
level)."

  (let (type)
    (align-let-save-point
     (align-let-backward-to-code)
     (while (if (setq type (align-let-looking-at-let))
                (progn
                  (set-marker align-let-save-point-marker (point))
                  nil) ;; stop loop
              (condition-case nil
                  (up-list -1)
                (error
                 (error "Not in a \"let\" form")))
              t))) ;; keep looping
    type))

(defun align-let-doelems (type func)
  "Call FUNC for each align-able element following point.
TYPE is symbol setq to traverse a `setq' form, otherwise `let'
bindings.

Currently each call is (FUNC VAR-POINT VAR-COLUMN).  VAR-POINT is
just after the \"(\" of the element VAR-COLUMN is the column
number of VAR-POINT.  Point is at the start of the value form.

There may be whitespace following VAR-POINT, but the \"(\", the
variable and the value expression (where point is now) are on the
same line."

  (save-excursion
    (while (progn
             (save-excursion
               (when (or (eq type 'setq)
                         (condition-case nil
                             (progn (down-list 1) t)
                           (error nil))) ;; ignore bare variables "var"

                 ;; allow ( and var on diff lines, and skip white for `setq'
                 (align-let-forward-comments)

                 (let ((var-point (point)) ;; after "(" of the element
                       (var-col   (current-column)))

                   (and (condition-case nil
                            (progn (forward-sexp) t)
                          (error nil)) ;; ignore empty "()"

                        ;; ignore incomplete "(var" at end of buffer
                        (progn
                          (align-let-forward-comments)
                          (not (eobp)))

                        ;; ignore var-only "(foo)" as found in scheme
                        ;; `(and-let* ((foo) ...)'
                        (condition-case nil
                            (progn
                              (forward-sexp)
                              (backward-sexp)
                              t)
                          (error nil))

                        ;; point now at start of VALUE part of (VAR VALUE)

                        ;; ignore VAR on a line of its own like
                        ;;     (let ((my-long-variable-name
                        ;;            "foo")
                        (align-let-same-line-p (point) var-point)

                        ;; found two-element "(VAR VALUE ...)"
                        (funcall func var-point var-col)))))

             ;; step to next (VAR VALUE) elem
             (align-let-forward-comments)
             (and (not (eobp))
                  (condition-case nil
                      (progn (forward-sexp (if (eq type 'setq) 2 1)) t)
                    (error nil)))))))

;;;###autoload
(defun align-let ()
  "Align the value expressions in a Lisp `let' or `setq' form.
Point should be within or immediately in front of the let form.
For example `align-let' changes

    (let ((x 1)
          (foo   2)
          (zz (blah)))
      ...)
to
    (let ((x   1)
          (foo 2)
          (zz  (blah)))
      ...)

Or

    (setq x 1
          foo 2)
to
    (setq x   1
          foo 2)

When point is somewhere in the middle of the form, possibly
nested in an expression, the beginning of a let is found by
looking for a double-paren pattern like

    (sym ... ((...

This means `align-let' works with various kinds of `let', `let*',
etc in various Lisp dialects.  But possible `setq' functions are
hard-coded, currently `setq', `setq-default' and `psetq'.

See `align-let-spaces' to have more than one space after the
longest variable.

----------------------------------------------------------------
Scheme `and-let*' is recognised too,

    (and-let* (...

Its \"(var value)\" forms can be bare symbols so may not start
with a \"((\" like above.  You can have `align-let' recognise
other such forms by setting an `align-let' property on the symbol
in Emacs,

    (put 'my-and-let* 'align-let 1)

The property value is the argument number of the bindings part.
So for example if you made

    (my-xandlet foo bar (abc
                         (def 123)
                         (ghi 456))
      ...

then the bindings are the 3rd argument and you set

    (put 'my-xandlet 'align-let 3)

----------------------------------------------------------------
The align-let.el home page is
URL `http://user42.tuxfamily.org/align-let/index.html'"

  (interactive)
  (save-excursion

    (let ((type  (align-let-find-let))
          (width 0))
      (align-let-doelems
       type
       (lambda (var-point var-col)
         (backward-sexp) ;; start of variable
         (forward-sexp)  ;; end of variable
         ;; quietly impose a minimum 1 space, so as not to run the variable
         ;; into the value (zero), nor to delete part of the variable name
         ;; (negative)
         (setq width (max width (+ (max 1 align-let-spaces)
                                   (- (current-column) var-col))))))
      (align-let-doelems
       type
       (lambda (var-point var-col)
         (let ((this-width (- (current-column) var-col)))
           (cond ((> this-width width)
                  ;; reduce space
                  (move-to-column (+ var-col width) t)
                  (let ((beg (point)))
                    (forward-sexp)
                    (backward-sexp)
                    (delete-region beg (point))))
                 ((< this-width width)
                  ;; increase space
                  (insert-char ?\s (- width this-width))))))))))

;;;###autoload
(defun align-let-keybinding ()
  "Bind C-c C-a to `align-let' in the current mode keymap.
This is designed for use from the mode hook of any lisp-like
language, eg. `emacs-lisp-mode-hook' or `scheme-mode-hook'.

In the current implementation, if you `unload-feature' to remove
`align-let' the key bindings made here are not removed.  If you
use the `autoload' recommended in the align-let.el Install
\(manually or generated) then Emacs automatically re-loads on a
later `C-c C-a'."

  (define-key (current-local-map) [?\C-c ?\C-a] 'align-let))

;;;###autoload
;; (custom-add-option 'emacs-lisp-mode-hook 'align-let-keybinding)
;;;###autoload
;; (custom-add-option 'scheme-mode-hook     'align-let-keybinding)

;;-----------------------------------------------------------------------------

;;;###autoload
(defun align-let-region (start end)
  "`align-let' all forms between START and END.
Interactively, align forms in the region between point and mark.

If a `let' form starts within the region but extends beyond END
then all its binding forms are aligned, even those past END.

One possibility for this command would be to combine it with
`indent-sexp' or `indent-pp-sexp' (\\[indent-pp-sexp]), or some sort
of \"indent defun\", for a one-key cleanup.  There's nothing
offered for that yet but it should be easy to make a combination
command according to personal preference.  It doesn't matter
whether you indent or align-let first, because align-let only
cares about variable name widths, not any indenting, and
indenting doesn't care about how much space there is after
variables!  Remember to use markers for the operative region, as
indent and align-let will both insert or delete characters."

  (interactive "r")
  (save-excursion
    (goto-char start)
    (setq end (copy-marker end t))

    ;; actually want to go forward outside a string, but backward is good
    ;; enough since if it is a string at START then it's not a `let' and
    ;; will be skipped by the first loop iteration
    (align-let-backward-to-code)

    (while (progn
             (align-let-forward-comments)
             (< (point) end))

      (cond ((looking-at "\\s)")
             ;; end of "...)" form, up forward out of it
             (goto-char (match-end 0)))

            ((looking-at "\\s(")
             ;; start of "(..." form, check for `let', and go down into it
             (condition-case nil
                 (if (save-excursion (align-let-looking-at-let))
                     (align-let))
               (error nil))
             (goto-char (1+ (point))))

            (t
             ;; something else, symbol, string, whatnot, skip forward over it
             (condition-case nil
                 (forward-sexp)
               (error (goto-char end))))))))

(provide 'align-let)

;; align-let.el ends here
