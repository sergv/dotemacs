;; general-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

;;; Generic setups for current module

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'align-util)
(require 'advices-util)
(require 'common)
(require 'el-patch)
(require 'folding-setup)
(require 'smart-operators-utils)
(require 'hydra-setup)
(require 'indentation)
(require 'macro-util)
(require 'paredit-setup)
(require 'rainbow-delimiters)
(require 'search)

;;;###autoload
(defconst +lisp-modes+
  '(emacs-lisp-mode
    clojure-mode
    lisp-mode)
  "List of modes that are considered to be lisp.")

;;;###autoload
(el-patch-feature lisp-mode)

(el-patch-defun lisp-indent-line (&optional indent)
  "Indent current line as Lisp code."
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (progn (beginning-of-line)
                       (or indent (calculate-lisp-indent (lisp-ppss))))))
    (skip-chars-forward " \t")
    (if (or (null indent)
            ((el-patch-swap looking-at looking-at-p) "\\s<\\s<\\s<"))
	;; Don't alter indentation of a ;;; comment line
	;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
	(goto-char (- (point-max) pos))
      (el-patch-splice 3 0
        (if (and (looking-at "\\s<")
                 (not (looking-at "\\s<\\s<")))
	    ;; Single-semicolon comment lines should be indented
	    ;; as comment lines, not as code.
	    (progn (indent-for-comment) (forward-char -1))
	  (if (listp indent) (setq indent (car indent)))
          (indent-line-to indent)))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defalign lisp-align-on-comments ";+")


(setq-default lisp-indent-function #'lisp-indent-function)
(setf lisp-indent-function #'lisp-indent-function)

;;; Utility functions covering broad range of topics
;;; These may be useful in any succeeding lisp setups

(defun lisp-point-inside-form (form-re)
  "Return t if point is positioned within sexp form
whose start (including open paren) matches FORM-RE."
  (save-excursion
    ;; if point is inside string our advice will handle this case
    (condition-case nil
        (progn
          (backward-up-list)
          (looking-at-p form-re))
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
          (cl-loop
            for i below n
            for result = (progn
                           (backward-up-list)
                           (looking-at-p form-re))
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

;;;; sexps

;;;###autoload
(defun lisp-pos-is-beginning-of-sexp? (&optional pos)
  "Check if there's sexp starting at POS."
  (eq ?\( (char-syntax (char-after pos))))

;;;###autoload
(defun lisp-pos-is-end-of-sexp? (&optional pos)
  "Check if there's sexp ending at POS."
  (eq ?\) (char-syntax (char-after pos))))

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


;;;; lists, strings

(defsubst lisp-prev-pos-is-beginning-of-list? (pos)
  "Check whether position POS minus one is beginning of a list"
  (lisp-pos-is-beginning-of-list? (max (1- pos) (point-min))))

(defsubst lisp-pos-is-beginning-of-list? (pos)
  (eq ?\( (char-after pos)))

(defsubst lisp-pos-is-end-of-list? (pos)
  (eq ?\) (char-after pos)))

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
  (and (eq ?\" (char-syntax (char-after pos)))
       (save-excursion
         (goto-char pos)
         (forward-char 1)
         (lisp-point-inside-string?))))

(defun lisp-pos-is-end-of-string? (pos)
  (and (eq ?\" (char-syntax (char-after pos)))
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

;;;; list navigation, realign let

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

;;;; navigation

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


(add-to-list 'debug-ignored-errors "\\`No enclosing list found\\'")

(defun glisp/find-beginning-of-defun (if-nothing-was-done)
  (let ((done-up-list nil))
    (condition-case nil
        (while (or (not done-up-list)
                   (not (looking-at-p (rx "("
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
                                              "defvar"
                                              "defsetf"
                                              "defstruct"
                                              "deftype"
                                              "defun"
                                              "defvar"
                                              "defconst"

                                              "defadvice"
                                              "defun*"
                                              "cl-defun"
                                              "defmacro*"
                                              "cl-defmacro"
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
  (vim-save-position)
  (glisp/find-beginning-of-defun #'backward-sexp))

(defun glisp/end-of-defun ()
  (interactive)
  (vim-save-position)
  (glisp/find-beginning-of-defun #'ignore)
  (forward-sexp))

;;;; other

(defun lisp-reindent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)))

(defun lisp-format-buffer ()
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
  (puthash mode
           #'lisp-format-buffer
           *mode-indent-functions-table*))

;;;; this is useful for all lisps

(search-def-autoexpand-advices
 (save-excursion
   (when (hs-already-hidden-p)
     (hs-show-block))
   (when (outline-invisible-p)
     (outline-show-subtree))
   (when (outline-invisible-p)
     (outline-show-subtree)))
 (clojure-mode lisp-mode common-lisp-mode scheme-mode emacs-lisp-mode))

;;; Actual setup functions

(defhydra-ext hydra-lisp-align (:exit t :foreign-keys nil :hint nil)
  "
_l_et
_;_: comments"
  ("l" realign-let)
  (";" lisp-align-on-comments))

(defhydra-derive hydra-lisp-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_<tab>_: reindent defun  _t_: jump to function start
                       _h_: jump to function end"
  ("<tab>" paredit-reindent-defun)

  ("t"     glisp/beginning-of-defun)
  ("h"     glisp/end-of-defun))

(defhydra-derive hydra-lisp-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign  _t_: jump to topmost node start
       _h_: jump to topmont node end"
  ("a"     hydra-lisp-align/body)

  ("t"     glisp/beginning-of-defun)
  ("h"     glisp/end-of-defun))

(vimmize-function lisp-comment-sexp :has-count t)
(vimmize-function lisp-uncomment-sexp :has-count nil)

(defhydra-derive hydra-lisp-vim-normal-j-ext hydra-vim-normal-j-ext (:exit t :foreign-keys nil :hint nil)
  "
_cl_: comment lines"
  ("cc" vim:lisp-comment-sexp:interactive)
  ("cu" vim:lisp-uncomment-sexp:interactive)
  ("cl" vim:comment-util-comment-lines:interactive))

(defhydra-derive hydra-lisp-vim-visual-j-ext hydra-vim-visual-j-ext (:exit t :foreign-keys nil :hint nil)
  ""
  ("cc" comment-util-comment-region)
  ("cu" comment-util-uncomment-region-simple))

(defhydra-derive hydra-lisp-vim-visual-z-ext hydra-vim-visual-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide sexps in region
_o_: show sexps in region"
  ("c"      hs-hide-sexps-in-region)
  ("o"      hs-show-sexps-in-region))

;;;###autoload
(cl-defun lisp-setup (&key (use-whitespace nil) (use-fci t))
  (init-common :use-yasnippet nil
               :use-whitespace 'tabs-only
               :use-render-formula t
               :use-fci use-fci)
  (paredit-mode +1)
  (setup-folding t '(:header-symbol ";" :length-min 3))
  ;; hiding of comments is rather annoying feature when working with lisps

  (when use-whitespace
    (setq-local whitespace-line-column 81
                whitespace-style '(face lines-tail tabs)))
  ;; (whitespace-mode 1)

  (setq-local hs-hide-comments-when-hiding-all nil
              comment-style 'indent
              comment-start ";"
              comment-end ""
              comment-padding " "

              ;; somehow setf does not work here
              lisp-indent-function #'lisp-indent-function)

  ;; just in case someone will want to use standard #'lisp-indent-function
  ;; put information for this case
  ;; (put 'if 'lisp-indent-function nil)

  (add-hook 'after-save-hook #'make-script-file-exec nil t)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("*"        search-for-symbol-at-point-forward)
    ("M-*"      search-for-symbol-at-point-forward-new-color)
    ("#"        search-for-symbol-at-point-backward)
    ("M-#"      search-for-symbol-at-point-backward-new-color)

    ("C-="      input-unicode)
    ("g"        hydra-lisp-vim-normal-g-ext/body)
    ("j"        hydra-lisp-vim-normal-j-ext/body))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("g"        hydra-lisp-vim-visual-g-ext/body)
    ("j"        hydra-lisp-vim-visual-j-ext/body)
    ("z"        hydra-lisp-vim-visual-z-ext/body)

    ("'"        paredit-backward-up))

  (def-keys-for-map vim-insert-mode-local-keymap
    (";"        paredit-semicolon)
    ("C-="      input-unicode)
    (")"        paredit-close-round))

  (def-keys-for-map (vim-operator-pending-mode-local-keymap
                     vim-motion-mode-local-keymap)
    ("g t"     glisp/beginning-of-defun)
    ("g h"     glisp/end-of-defun))

  (def-keys-for-map (vim-motion-mode-local-keymap
                     vim-operator-pending-mode-local-keymap)
    ;; ("w"   vim:paredit-forward-word:interactive)
    ;; ("e"   vim:paredit-forward-word-end:interactive)
    ;; ("b"   vim:paredit-backward-word:interactive)
    ;; ("W"   vim:paredit-forward-WORD:interactive)
    ;; ("E"   vim:paredit-forward-WORD-end:interactive)
    ;; ("B"   vim:paredit-backward-WORD:interactive)

    ("s"   vim:paredit-inner-symbol:interactive)
    ("i s" vim:paredit-inner-symbol:interactive)
    ("a s" vim:paredit-outer-symbol:interactive)
    ("S"   vim:paredit-backward-symbol:interactive)))

;;;###autoload
(add-hook 'lisp-mode-hook #'lisp-setup)

;;;###autoload
(defun lisp-repl-setup ()
  (lisp-setup :use-fci nil)
  (init-repl)

  (whitespace-mode -1)

  ;; don't use prompt regexp to make comint use special field property
  (setq-local comint-use-prompt-regexp nil
              comint-prompt-regexp "^[^> \n\t\r\f\v]*\\(>+:?\\|[*?]+\\) *")

  (vim-local-emap "clear" #'vim:comint-clear-buffer-above-prompt)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC"     comint-clear-prompt))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("<backspace>" paredit-backward-delete)
    ("M-;"         paredit-comment-dwim)

    ("C-SPC"       vim:comint-clear-buffer-above-prompt:interactive)
    ("C-w"         backward-delete-word)
    ("C-S-w"       backward-delete-word*)
    ("<tab>"       nil)

    ("M-p"         browse-comint-input-history)

    ("<up>"        comint-previous-input)
    ("<down>"      comint-next-input)
    ("C-<up>"      comint-previous-prompt)
    ("C-<down>"    comint-next-prompt)
    ("S-<up>"      comint-previous-prompt)
    ("S-<down>"    comint-next-prompt)))

(provide 'general-lisp-setup)

;; Local Variables:
;; End:

;; general-lisp-setup.el ends here
