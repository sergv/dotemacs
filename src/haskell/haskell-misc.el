;; haskell-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 20 September 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'macro-util)
  (require 'set-up-platform)
  (require 'trie))

(require 'align-util)
(require 'advices-util)
(require 'macro-util)
(require 'common)
(require 'configurable-compilation)
(require 'current-column-fixed)
(require 'indentation)
(require 'search)
(require 'treesit-utils)
(require 'trie)

(require 'abbrev+)
(require 'compilation-setup)
(require 'haskell-compile)
(require 'haskell-compilation-commands)
(require 'haskell-ext-tracking)
(require 'haskell-format-setup)
(require 'haskell-mode)
(require 'haskell-regexen)
(require 'haskell-smart-operators-mode)
(require 'haskell-smart-operators-utils)
(require 'haskell-sort-imports)
(require 'hydra-setup)
(require 'nix-integration)

(require 'vim-defs)
(require 'vim-macs)
(require 'vim-core)
(require 'vim-compat)
(require 'vim-motions)

(require 'flycheck)
(require 'flycheck-haskell)
(require 'flycheck-setup)

(defvar haskell-indent-offset 2
  "Haskell indentation amount used by functions written as part
of my home config.")

(defun haskell-misc--indent-line-with-treesitter ()
  (treesit-update-ranges (line-beginning-position)
                         (line-end-position))
  (pcase-let* ((`(,anchor . ,offset) (treesit--indent-1)))
    (when (and anchor offset)
      ;; Indent with treesitter
      (let ((col (+ (save-excursion
                      (goto-char anchor)
                      (current-column))
                    offset))
            (delta (- (point-max) (point))))
        (indent-line-to col)
        ;; Now point is at the end of indentation.  If we started
        ;; from within the line, go back to where we started.
        (let ((d (- (point-max) delta)))
          (when (> d (point))
            (goto-char d))
          t)))))

(defun haskell-misc--indent-line-with-treesitter-or-fallback (fallback)
  "Try to indent with treesiter if we can, otherwise fallback to haskell-indentation.el"
  (cl-assert (functionp fallback))
  (if treesit-simple-indent-rules
      (unless (haskell-misc--indent-line-with-treesitter)
        (funcall fallback))
    (funcall fallback)))

(defun haskell-misc-combined-indent-forwards ()
  "Try to indent with treesiter if we can, otherwise fallback to ‘haskell-indentation-indent-line’."
  (interactive "*")
  (haskell-misc--indent-line-with-treesitter-or-fallback #'haskell-indentation-indent-line))

(defun haskell-misc-combined-indent-backwards ()
  "Try to indent with treesiter if we can, otherwise fallback to ‘haskell-indentation-indent-backwards’."
  (interactive "*")
  (haskell-misc--indent-line-with-treesitter-or-fallback #'haskell-indentation-indent-backwards))

(defun haskell-setup-indentation (offset simpler-indentation-by-default?)
  "Set up bindings and indentation parameters using OFFSET as a
single indentation unit."

  (haskell-indentation-mode +1)

  (if simpler-indentation-by-default?
      (progn
        (bind-tab-keys #'indent-relative-forward
                       #'indent-relative-backward
                       :enable-yasnippet t)
        (def-keys-for-map (vim-normal-mode-local-keymap
                           vim-insert-mode-local-keymap)
          ("C-<tab>"                         haskell-misc-combined-indent-forwards)
          (("C-S-<tab>" "C-S-<iso-lefttab>") haskell-misc-combined-indent-backwards)))
    (progn
      (bind-tab-keys #'haskell-misc-combined-indent-forwards
                     #'haskell-misc-combined-indent-backwards
                     :enable-yasnippet t)
      (def-keys-for-map (vim-normal-mode-local-keymap
                         vim-insert-mode-local-keymap)
        ("C-<tab>"                         indent-relative-forward)
        (("C-S-<tab>" "C-S-<iso-lefttab>") indent-relative-backward))))

  (let ((real-offset (or offset 2)))
    (setq-local-if-not-eq vim-shift-width                    real-offset
                          tab-width                          real-offset
                          haskell-indent-offset              real-offset
                          haskell-indentation-layout-offset  real-offset
                          haskell-indentation-starter-offset real-offset
                          haskell-indentation-left-offset    real-offset)))

(defmacro haskell-misc--with-expanded-invisible-overlays-in-current-function (&rest body)
  `(with-expanded-invisible-overlays
       (max (save-excursion (haskell-move-to-topmost-start)
                            (point))
            (point-min))
       (save-excursion
         (haskell-move-to-topmost-end)
         (point))
     ,@body))

(defun haskell-misc--single-indent ()
  "Return a string for single indentation amount for Haskell."
-  (make-string vim-shift-width ?\s))

(defun cleanup-stg ()
  "Remove useless srt:SRT annotations of lambdas, keep only relevant arguments
and indent them as singe line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "srt:SRT:" nil t)
      (skip-chars-backward "^\\\\")
      (zap-to-char 1 ?\])
      (delete-whitespace-forward)

      (let ((line (line-number-at-pos)))
        (while (not (= line
                       (save-excursion
                         (forward-sexp 1)
                         (backward-char 1)
                         (line-number-at-pos))))
          (save-excursion
            (join-line t)))))))



(setf dante-load-flags
      '("+c"
        "-fdiagnostics-color=always"
        "-fno-diagnostics-show-caret"
        "-Wwarn=missing-home-modules"
        "-ferror-spans"
        "-O0"
        "-fdefer-typed-holes")

      ;; dante-debug '(inputs outputs responses command-line)
      )

(setf haskell-indentation-electric-flag t)

;;; Up level navigation

(defun haskell-misc--back-up-indent-level ()
  "Move up to lesser indentation level, skipping empty lines.

Returns t if indentation occured."
  (indent-back-up-indent-level #'haskell-on-blank-line?))

;;;###autoload
(defun haskell-backward-up-indentation-or-sexp ()
  "Haskell brother of ‘paredit-backward-up’ that considers both
sexps and indentation levels."
  (interactive)
  (indent-backward-up-indentation-or-sexp #'haskell-on-blank-line?))

;;;###autoload
(defun haskell-up-sexp ()
  "Haskell brother of ‘paredit-forward-up’ that considers only sexps for now."
  (interactive)
  (paredit-forward-up))

;;;; align functions

;;;###autoload (autoload 'haskell-align-on-dollars "haskell-misc" nil t)
(defalign haskell-align-on-dollars
  "[$][^$]"
  :require-one-or-more-spaces t)

;;;###autoload (autoload 'haskell-align-on-equals "haskell-misc" nil t)
(defalign haskell-align-on-equals
  (rx ?=
      (or eol (not (char ?= ?< ?>))))
  :require-one-or-more-spaces t)

;;;###autoload (autoload 'haskell-align-on-arrows "haskell-misc" nil t)
(defalign haskell-align-on-arrows
  (rx
   (or (seq
        (+ (any ?: ?| ?- ?=))
        (+ ">"))
       "→")
   (or " "
       eol)))
;;;###autoload (autoload 'haskell-align-on-left-arrows "haskell-misc" nil t)
(defalign haskell-align-on-left-arrows
  (rx
   (or (seq
        (+ "<")
        (+ (any ?: ?| ?- ?=)))
       "←")
   (or " "
       eol)))
;;;###autoload (autoload 'haskell-align-on-guards "haskell-misc" nil t)
(defalign haskell-align-on-guards
  "|\\(?:[^|]\\|$\\)"
  :require-one-or-more-spaces t)
;;;###autoload (autoload 'haskell-align-on-commas "haskell-misc" nil t)
(defalign haskell-align-on-commas
  ",\\(?:[^,\)]\\|$\\)")
;;;###autoload (autoload 'haskell-align-on-comments "haskell-misc" nil t)
(defalign haskell-align-on-comments
  "--+\\(?: \\|$\\)"
  :require-one-or-more-spaces t)

;;;###autoload (autoload 'haskell-align-on-double-colons "haskell-misc" nil t)
(defalign haskell-align-on-double-colons
  haskell-regexen/function-signature-colons)
;;;###autoload (autoload 'haskell-align-on-pragma-close "haskell-misc" nil t)
(defalign haskell-align-on-pragma-close
  "#-}")


(defun haskell-align-generic ()
  (interactive "*")
  (haskell-align-on-equals)
  (haskell-align-on-arrows)
  (haskell-align-on-left-arrows)
  (haskell-align-on-comments)
  (haskell-align-on-double-colons)
  (haskell-align-on-pragma-close))

(defvar hydra-haskell-align--empty-keymap (make-sparse-keymap))

;;;###autoload (autoload 'hydra-haskell-align/body "haskell-misc" nil t)
(defhydra-ext hydra-haskell-align (:exit t :foreign-keys nil :hint nil :base-map hydra-haskell-align--empty-keymap)
  "
_a_:   generic
_$_:   on dollars
_=_:   on equals
_->_:  on arrows
_<-_:  on left-arrows
_|_:   on guards
_,_:   on commas
_--_:  on comments
_::_:  on double colons
_#-}_: on pragma close"
  ("a"   haskell-align-generic)
  ("$"   haskell-align-on-dollars)
  ("="   haskell-align-on-equals)
  ("->"  haskell-align-on-arrows)
  ("<-"  haskell-align-on-left-arrows)
  ("|"   haskell-align-on-guards)
  (","   haskell-align-on-commas)
  ("--"  haskell-align-on-comments)
  ("::"  haskell-align-on-double-colons)
  ("#-}" haskell-align-on-pragma-close))

;;;###autoload
(defun haskell-reindent-region (&optional width)
  "Format selected region with brittany formatter."
  (interactive "p*")
  (with-region-bounds start end
    (haskell-format--format-region-preserving-position haskell-indent-offset
                                                       width
                                                       start
                                                       end)))

;;;###autoload
(defun haskell-reindent-at-point (&optional width)
  "Do some sensible reindentation depending on the current position in file."
  (interactive "p*")
  (save-match-data
    (cond
      ((or (save-excursion
             (beginning-of-line)
             (looking-at-p haskell-regexen/language-pragma-prefix))
           ;; First check that we’re inside pragma then try to locate beginning
           ;; of pragma block.
           (and (cond
                  ((derived-mode-p 'haskell-mode)
                   (eq (get-char-property (point) 'face) 'haskell-pragma-face))
                  ((derived-mode-p 'haskell-ts-mode)
                   (string= "pragma" (treesit-node-type (treesit-node-at (point))))))
                (save-excursion
                  (re-search-backward haskell-regexen/pragma-start nil t)
                  (looking-at-p haskell-regexen/language-pragma-prefix))))
       (save-current-line-column
         (haskell-align-language-pragmas (point))))
      ((or (save-excursion
             (beginning-of-line)
             (looking-at-p haskell-regexen/options-ghc-pragma-prefix))
           (and (eq (get-char-property (point) 'face) 'haskell-pragma-face)
                (save-excursion
                  (re-search-backward haskell-regexen/pragma-start nil t)
                  (looking-at-p haskell-regexen/options-ghc-pragma-prefix))))
       (save-current-line-column
         (haskell-align-options-ghc-pragmas (point))))
      ((haskell-sort-imports-at-import)
       (save-current-line-column
         (haskell-sort-imports)))
      ((save-excursion
         (beginning-of-line)
         (not (looking-at-p "^[ \t]*$")))
       (let* ((is-module-export-list? nil)
              (start (save-excursion
                       (haskell-move-to-topmost-start)
                       (setf is-module-export-list?
                             (looking-at-p haskell-regexen/module-header-start))
                       (point)))
              (end (or (and is-module-export-list?
                            (save-excursion
                              (goto-char start)
                              (re-search-forward "\\_<where\\_>" nil t)))
                       (save-excursion
                         (haskell-move-to-topmost-end)
                         (skip-chars-forward "\r\n")
                         (prog1 (point)
                           (unless (eobp)
                             (save-excursion
                               (forward-line 1)
                               (setf end-mark (point-marker)))))))))
         (haskell-format--format-region-preserving-position haskell-indent-offset
                                                            width
                                                            start
                                                            end)))
      (t
       (error "Don't know how to reindent construct at point")))))

(defun haskell-misc--point-inside-pragma? (point)
  (cond
    ((derived-mode-p 'haskell-mode)
     (save-excursion
       (save-match-data
         (when (search-forward (eval-when-compile
                                 (unless (equal (regexp-quote haskell-regexen/pragma-end)
                                                haskell-regexen/pragma-end)
                                   (error "Definition of haskell-regexen/pragma-start is not plain string anymore, amend its use in searching"))
                                 haskell-regexen/pragma-end)
                               nil
                               t)
           (let ((end (point)))
             (backward-sexp)
             (let ((start (point)))
               (and (<= start point)
                    (<= point end))))))))
    ((derived-mode-p 'haskell-ts-mode)
     (when-let ((node (treesit-node-at (point))))
       (string= "pragma" (treesit-node-type node))))
    (t
     (error "haskell-misc--point-inside-pragma?: not implemented for major mode %s" major-mode))))

(defun haskell-align-language-pragmas (start)
  (haskell-align--pragmas-impl haskell-regexen/language-pragma-prefix
                               "{-# LANGUAGE %s #-}"
                               start))

(defun haskell-align-options-ghc-pragmas (start)
  (haskell-align--pragmas-impl haskell-regexen/options-ghc-pragma-prefix
                               "{-# OPTIONS_GHC %s #-}"
                               start))

(defun haskell--parse-pragma (pragma-prefix-re start end)
  "Parse single LANGUAGE pragma within START-END region and return its
extensions as a list of strings. Leaves point at the end of pragma"
  (goto-char start)
  (when (looking-at pragma-prefix-re)
    (let ((pragma-end (min end
                           (cond
                             ((derived-mode-p 'haskell-mode)
                              (save-excursion
                                (forward-sexp)
                                (point)))
                             ((derived-mode-p 'haskell-ts-mode)
                              (let ((node (treesit-node-at (point))))
                                (cl-assert (string= "pragma" (treesit-node-type node))
                                           nil
                                           "Expected ‘pragma’ node type, but got: %s"
                                           (treesit-node-type node))
                                (treesit-node-end node)))))))
      (goto-char (match-end 0))
      (let ((contents-start (point)))
        (goto-char pragma-end)
        (skip-chars-backward "#}\-" start)
        ;; (skip-chars-forward "^#}\-" pragma-end)
        (let* ((contents (buffer-substring-no-properties contents-start
                                                         (point)))
               (exts
                (split-string contents
                              "[, \t\n\r]+"
                              t ;; omit nulls
                              )))
          (goto-char pragma-end)
          exts)))))

(defun haskell-align--pragmas-impl (pragma-prefix-re template start)
  (cl-assert (stringp pragma-prefix-re))
  (cl-assert (stringp template))
  (cl-assert (integer-or-marker-p start))
  (save-match-data
    (let ((p (point)))
      (goto-char start)
      ;; (cl-assert (looking-at-p haskell-regexen/language-pragma-prefix))
      ;; Navigate up while we're still getting LANGUAGE pragmas.
      (beginning-of-line)
      (while (and (not (bobp))
                  (or (looking-at-p pragma-prefix-re)
                      (haskell-misc--point-inside-pragma? (point))))
        ;; Go to beginning of the previous line.
        (backward-line))
      ;; Skip whitespace and possible comments to the beginning of pragma.
      (search-forward (eval-when-compile
                        (unless (equal (regexp-quote haskell-regexen/pragma-start)
                                       haskell-regexen/pragma-start)
                          (error "Definition of haskell-regexen/pragma-start is not plain string anymore, amend its use in searching"))
                        haskell-regexen/pragma-start))
      (goto-char (match-beginning 0))
      (let ((pragma-block-start (point))
            (pragma-block-end nil)
            (exts nil)
            (done nil))
        ;; Collect all extensions from all pragmas
        (while (not done)
          (aif (haskell--parse-pragma pragma-prefix-re (point) (point-max))
              (progn
                (setf exts (append it exts)
                      pragma-block-end (point))
                (forward-line 1)
                (if (eobp)
                    (setf done t)
                  (beginning-of-line))
                ;; (skip-syntax-forward " >")
                )
            (setf done t)))
        (if pragma-block-end
            (progn
              (goto-char pragma-block-start)
              (delete-region pragma-block-start pragma-block-end)
              (setf exts (sort exts #'string<))
              (when exts
                (insert (format template (first exts)))
                (dolist (e (cdr exts))
                  (insert "\n")
                  (insert (format template e))))
              (haskell-align-on-pragma-close-indent-region pragma-block-start (point))
              (delete-duplicate-lines pragma-block-start (point) nil t))
          (goto-char p))))))

;;; define ‘bounds-of-haskell-symbol’

(defconst haskell-misc--bounds-of-symbol--word-chars "[:alnum:]_'#.")

(defun haskell-misc--bounds-of-symbol-impl (qualified? offset core-mode? include-quotes?)
  "Qualified symbol may return prefix of ' before the symbol."
  (save-excursion
    (save-match-data
      (let ((word-chars
             (if core-mode?
                 (eval-when-compile (concat haskell-misc--bounds-of-symbol--word-chars "$"))
               haskell-misc--bounds-of-symbol--word-chars)))
        (when offset
          (forward-char offset))
        (if (looking-at-p
             (if core-mode?
                 (eval-when-compile (concat "[" haskell-misc--bounds-of-symbol--word-chars "$]"))
               (eval-when-compile (concat "[" haskell-misc--bounds-of-symbol--word-chars "]"))))
            ;; In the middle of a word - next character is a word one
            ;; so go back while we’re part of the word.
            (skip-chars-backward word-chars)
          ;; Not followed by a word so if also not preceded by a word then try
          ;; to detect operator.
          (when (or (string-contains? (char-after) haskell-smart-operators--operator-chars-str)
                    (zerop (skip-chars-backward word-chars)))
            (skip-chars-backward haskell-smart-operators--operator-chars-str)
            ;; To get qualified part
            (when (= (char-after) ?.)
              (skip-chars-backward word-chars))))
        (unless include-quotes?
          (skip-chars-forward "'"))
        (when (looking-at (if core-mode?
                              haskell-regexen/core/opt-q/varid-or-conid-or-operator-or-number
                            haskell-regexen/opt-q/varid-or-conid-or-operator-or-number))
          (if qualified?
              (cons (match-beginning 0) (match-end 0))
            (cons (match-beginning 1) (match-end 1))))))))

;;;###autoload
(defun bounds-of-haskell-symbol ()
  (haskell-misc--bounds-of-symbol-impl nil nil nil nil))

;;;###autoload
(put 'haskell-symbol 'bounds-of-thing-at-point #'bounds-of-haskell-symbol)

;;;###autoload
(defun bounds-of-qualified-haskell-symbol ()
  (haskell-misc--bounds-of-symbol-impl t nil nil nil))

;;;###autoload
(put 'qualified-haskell-symbol 'bounds-of-thing-at-point #'bounds-of-qualified-haskell-symbol)

;;;###autoload
(defun bounds-of-ghc-core-symbol ()
  (let ((parse-sexp-lookup-properties nil))
    (haskell-misc--bounds-of-symbol-impl nil nil t nil)))

;;;###autoload
(put 'ghc-core-symbol 'bounds-of-thing-at-point #'bounds-of-ghc-core-symbol)

(vim-defmotion vim:motion-inner-haskell-symbol (inclusive count motion-result)
  "Select `count' inner symbols."
  (vim--inner-motion (or count 1)
                     #'vim-boundary--haskell-symbol
                     #'vim-boundary--ws
                     'inclusive))

(vim-defmotion vim:motion-outer-haskell-symbol (inclusive count motion-result)
  "Select `count' outer symbols."
  (vim--outer-motion (or count 1)
                     #'vim-boundary--haskell-symbol
                     #'vim-boundary--ws
                     'inclusive))

(vim-defmotion vim:motion-inner-qualified-haskell-symbol (inclusive count motion-result)
  "Select `count' inner qualified symbols."
  (vim--inner-motion (or count 1)
                     #'vim-boundary--qualified-haskell-symbol
                     #'vim-boundary--ws
                     'inclusive))

(vim-defmotion vim:motion-outer-qualified-haskell-symbol (inclusive count motion-result)
  "Select `count' outer qualified symbols."
  (vim--outer-motion (or count 1)
                     #'vim-boundary--qualified-haskell-symbol
                     #'vim-boundary--ws
                     'inclusive))

(defun vim-boundary--haskell-symbol-impl (direction qualified?)
  "A boundary selector for haskell symbols, qualified names like Foo.Bar.baz will be treated as
containining distinct words between the dot."
  (funcall (vim--union-boundary (lambda (dir)
                                  (when-let* ((bnds (haskell-misc--bounds-of-symbol-impl qualified? nil nil t))
                                              (start (car bnds))
                                              (end (cdr bnds)))
                                    (pcase dir
                                      (`fwd
                                       (1- end))
                                      (`bwd
                                       start)
                                      (_
                                       (error "Invalid boundary direction: %s" dir)))))
                                #'vim-boundary--empty-line)
           direction))

(defun vim-boundary--haskell-symbol (direction)
  "A boundary selector for haskell symbols, qualified names like Foo.Bar.baz will be treated as
containining distinct words between the dot."
  (vim-boundary--haskell-symbol-impl direction nil))

(defun vim-boundary--qualified-haskell-symbol (direction)
  "A boundary selector for haskell symbols, qualified names like Foo.Bar.baz will be treated as
a single entity."
  (vim-boundary--haskell-symbol-impl direction t))

;; newline that detects haskell signatures

(defun haskell--simple-indent-newline-same-col ()
  "Make a newline and go to the same column as the current line."
  (interactive "*")
  (delete-horizontal-space t)
  (let ((indent-size
         (save-excursion
           (let* ((start (line-beginning-position))
                  (end (progn
                         (goto-char start)
                         ;; (search-forward-regexp
                         ;;  "[^ ]" (line-end-position) t 1)
                         (skip-to-indentation)
                         (point))))
             (when (or (eobp)
                       (not (eq ?\s (char-after))))
               (- end start))))))
    (insert-char ?\n)
    (when indent-size
      (insert-char ?\s indent-size))))

(defun haskell--simple-indent-newline-indent ()
  "Make a newline on the current column and indent one step further."
  (interactive "*")
  (haskell--simple-indent-newline-same-col)
  (insert-char ?\s haskell-indent-offset))

(defun haskell-newline-with-signature-expansion ()
  "Similar to ‘paredit-newline’ but autoexpands haskell signatures."
  (interactive "*")
  (haskell-misc--with-expanded-invisible-overlays-in-current-function
   (let* (
          ;; Our regexps distinguish between upper and lower case so it’s
          ;; importent to be case-sensitive during searches.
          (case-fold-search nil)
          (start-pos (point))
          (start-pos-no-ws (save-excursion
                             (goto-char start-pos)
                             (skip-whitespace-backward)
                             (point)))
          (function-name-column nil)
          (point-at-end-of-function-signature? nil)
          ;; Whether we already performed expansion of the function name
          ;; and no futher expansion should be attempted.
          (expanded-function-name?
           (save-match-data
             (let ((lower-bound
                    (save-excursion
                      (when (re-search-backward haskell-regexen/function-signature-colons
                                                nil
                                                t)
                        (line-beginning-position)))))
               (when lower-bound
                 (let ((found? nil)
                       (func-name nil))
                   (while (and (not found?)
                               (< lower-bound (point))
                               (not (bolp)))
                     (haskell-misc--back-up-indent-level)
                     (when (looking-at (eval-when-compile
                                         (let ((ws "[ \t\n\r]")
                                               (name-re (concat
                                                         "\\(?:"
                                                         (concat "\\(?:"
                                                                 haskell-regexen/varid
                                                                 "\\)")
                                                         "\\|"
                                                         (concat "[(]"
                                                                 "\\(?:"
                                                                 haskell-regexen/sym
                                                                 "\\)+"
                                                                 "[)]")
                                                         "\\)")))
                                           (concat "\\(?:\\_<\\(?:let\\|where\\)\\_>" ws "+\\)?"
                                                   "\\(?1:"
                                                   (concat "\\(?:" name-re "\\)"
                                                           "\\(?:," ws "*" name-re "\\)*")
                                                   "\\)"
                                                   ws "*"
                                                   haskell-regexen/function-signature-colons))))
                       (setf found? t
                             func-name (match-string-no-properties 1))))
                   (when found?
                     (goto-char (match-beginning 1))
                     (setf function-name-column (current-column-fixed-uncached))
                     (let ((indented-section-end (line-end-position)))
                       (setf point-at-end-of-function-signature?
                             (or (and (derived-mode-p 'haskell-ts-mode)
                                      (let ((sig-node (treesit-node-parent (treesit-node-at (point)))))
                                        (when (string= "signature" (treesit-node-type sig-node))
                                          (setf indented-section-end (treesit-node-end sig-node))
                                          (= start-pos-no-ws (treesit-node-end sig-node)))))
                                 (progn
                                   (forward-line 1)
                                   (beginning-of-line)
                                   (while (and (not (eobp))
                                               (< function-name-column (indentation-size)))
                                     (unless (haskell-on-blank-line?)
                                       (setf indented-section-end (line-end-position)))
                                     (forward-line 1))
                                   ;; Do not expand if we're not located at the
                                   ;; type signature's end.
                                   (= start-pos indented-section-end))))
                       (when (and point-at-end-of-function-signature?
                                  (not (save-excursion
                                         (goto-char indented-section-end)
                                         (skip-syntax-forward "->")
                                         (looking-at-p (concat (regexp-quote func-name)
                                                               "\\_>")))))
                         (goto-char start-pos)
                         (delete-horizontal-space t)
                         (insert-char ?\n)
                         (insert-char ?\s function-name-column)
                         (insert func-name)
                         (insert-char ?\s)
                         t)))))))))
     (when (null expanded-function-name?)
       (goto-char start-pos)
       (let* ((syn nil)
              (in-string? (let ((node (treesit-haskell--current-node)))
                            (cond
                              ((haskell-smart-operators--treesit--in-quasiquote-body? node)
                               ;; [Non-Haskell-QQ]
                               ;; Quasiquote’s body syntax probably
                               ;; doesn’t support Haskell’s multiline
                               ;; strings separated by backslashes.
                               nil)
                              ((and (derived-mode-p 'haskell-mode)
                                    (when-let ((prop (get-char-property (point) 'haskell-mode-quasiquote)))
                                      (not (member prop '("" "t" "e" "d")))))
                               ;; Same reasoning as for [Non-Haskell-QQ].
                               nil)
                              (t
                               (or (haskell-smart-operators--in-string-syntax?-raw node)
                                   (nth 3 (syntax-ppss-update! syn))))))))
         (cond
           (in-string?
            (let ((string-start-column (save-excursion
                                         (goto-char (nth 8 (syntax-ppss-cached syn)))
                                         (current-column-fixed-uncached))))
              (delete-horizontal-space t)
              (insert-char ?\\)
              (insert-char ?\n)
              (insert-char ?\s string-start-column)
              (insert-char ?\\)))
           ((and function-name-column
                 point-at-end-of-function-signature?)
            (delete-horizontal-space t)
            (insert-char ?\n)
            (insert-char ?\s function-name-column))
           ((save-excursion
              (skip-syntax-backward " ")
              (and (trie-matches-backwards?
                    (eval-when-compile
                      (trie-opt-recover-sharing!
                       (trie-from-list
                        (--map (cons (reverse it) t)
                               '("where"
                                 "of"
                                 "do"
                                 "="
                                 "->"
                                 "<-"
                                 "\\case")))))
                    nil)
                   (let ((before (char-before)))
                     (or (eq before ?\s)
                         (eq before ?\n)
                         (eq before ?\t)))))
            (haskell--simple-indent-newline-indent))
           ((save-excursion
              (skip-to-indentation)
              (looking-at-p "let\\_>"))
            (haskell--simple-indent-newline-same-col)
            (insert-char ?\s 4))
           (t
            (haskell--simple-indent-newline-same-col))))))))

(defun haskell--ghci-hyphen (&optional prefix)
  "Version of `haskell-smart-operators-hyphen' for ghci."
  (interactive "*p")
  (let ((entering-command?
         (save-excursion
           (beginning-of-line)
           ;; skip whitespace
           (skip-syntax-forward "-")
           (let ((c (char-after)))
             (and c
                  (char= c ?:))))))
    (if entering-command?
        (self-insert-command prefix)
      (haskell-smart-operators-hyphen))))

(defun haskell--ghci-colon (&optional prefix)
  "Version of `haskell-smart-operators-hyphen' for ghci."
  (interactive "*p")
  (let ((entering-command?
         (or (= (point) (line-beginning-position))
             (save-excursion
               ;; skip whitespace
               (skip-syntax-backward "-")
               (get-pos-property (point) 'prompt)))))
    (if entering-command?
        (self-insert-command prefix)
      (haskell-smart-operators-self-insert prefix))))

(cl-defun install-haskell-smart-operators! (keymap &key bind-colon bind-hyphen track-extensions?)
  (declare (indent 1))
  (haskell-smart-operators-mode +1)
  (when track-extensions?
    (haskell-ext-tracking-mode +1))
  (when bind-colon
    (define-key keymap
      (kbd ":")
      #'haskell-smart-operators-self-insert))
  (when bind-hyphen
    (def-keys-for-map keymap
      ("-" haskell-smart-operators-hyphen)))
  (define-key keymap
    (kbd "!")
    #'haskell-smart-operators-exclamation-mark)
  (dolist (key (list (kbd "=") (kbd "+") (kbd "*") (kbd "<") (kbd ">")
                     (kbd "%") (kbd "^") (kbd "&") (kbd "/")
                     (kbd "?") (kbd "|") (kbd "~") (kbd "@")))
    (define-key keymap
      key
      #'haskell-smart-operators-self-insert))
  (def-keys-for-map keymap
    ("$"   haskell-smart-operators-$)
    ("#"   haskell-smart-operators-hash)
    (","   haskell-smart-operators-comma)
    ("."   haskell-smart-operators-dot)
    ("C-=" input-unicode)))

(defun haskell-prof-search-column (column pred)
  (cl-assert (< 0 column))
  (save-match-data
    (let ((column-re
           (concat "^\\(?:[ \t]*[^ \t]+\\)"
                   "\\(?:[ \t]+[^ \t]+\\)\\{" (number->string (- column 1)) "\\}"
                   "[ \t]+"
                   "\\([^ \t]+\\)")))
      (while (and (re-search-forward column-re nil t)
                  (not (funcall pred
                                (match-string-no-properties 1))))))))

(defun haskell-prof-search-individual-time (minimum-fraction)
  (cl-assert (numberp minimum-fraction))
  (haskell-prof-search-column
   4
   (lambda (x)
     (and (string-match-p "[0-9.]+" x)
          (< minimum-fraction (string->number x))))))

(defun haskell-misc--cabal-indented-subsection ()
  "Similar to `haskell-cabal-subsection' but sets `:data-start-column' to the
value section should have if it is to be properly indented."
  (save-match-data
    (save-excursion
      (haskell-cabal-beginning-of-subsection)
      (when (looking-at "\\(?:\\([ \t]*\\)\\(\\w*\\):\\)[ \t]*")
        (list :name (match-string-no-properties 2)
              :beginning (match-end 0)
              :end (save-match-data (haskell-cabal-subsection-end))
              :data-start-column (+ 2 (current-column-fixed-uncached)))))))

(defun haskell-misc-cabal-align-and-sort-subsection ()
  "Sort lines of the Cabal subsection at point."
  (interactive "*")
  (save-match-data
    (haskell-cabal-save-position
     (haskell-cabal-with-subsection
      (haskell-misc--cabal-indented-subsection)
      t
      (progn
        (goto-char (point-min))
        (delete-char (- (skip-chars-forward "\r\n")))
        (haskell-cabal-with-cs-list
         (haskell-cabal-each-line
          (beginning-of-line)
          (when (looking-at "^[ \t]*\\([^ \t\r\n]\\(?:.*[^ \t\r\n]\\)?\\)[ \t]*$")
            (replace-match (match-string-no-properties 1) nil t)))
         (sort-subr nil
                    'forward-line
                    'end-of-line
                    'haskell-cabal-sort-lines-key-fun)))))))

(defun haskell-misc--add-new-import (mod-name identifier is-name-from-current-project? parent-name)
  "Go to the imports section and add MOD-NAME import."
  (cl-assert (stringp mod-name))
  (save-match-data
    (save-restriction
      (save-excursion
        (widen)
        (haskell-navigate-imports)
        (let ((positions nil)
              (add-at-end nil))
          (save-excursion
            (while (re-search-forward (eval-when-compile (concat "^" haskell-regexen/pre-post-qualified-import-line)) nil t)
              (aif (match-beginning 10)
                  (push (cons (common-string-prefix-length mod-name (match-string 10) nil)
                              (match-beginning 7))
                        positions)
                (error "Import regexps matched without matching module name!"))))
          (setf positions (sort positions
                                (lambda (a b)
                                  (let ((prefix-len-a (car-sure a))
                                        (prefix-len-b (car-sure b)))
                                    (or (> prefix-len-a prefix-len-b)
                                        (and (= prefix-len-a prefix-len-b)
                                             (< (cdr-sure a) (cdr-sure b))))))))
          (if positions
              (let* ((first-prefix-length (caar-sure positions))
                     (candidate-imports (--take-while (= first-prefix-length (car-sure it)) positions)))
                (goto-char
                 (cdr-sure (if is-name-from-current-project?
                               (-last-item candidate-imports)
                             (-first-item candidate-imports)))))
            (setf add-at-end "\n"))
          (insert "import " mod-name)
          (when identifier
            (insert " ("
                    (if parent-name
                        (concat parent-name "(")
                      "")
                    (if (haskel-misc--is-operator? identifier)
                        (concat "(" identifier ")")
                      identifier)
                    (if parent-name
                        ")"
                      "")
                    ")"))
          (insert-char ?\n)
          (when add-at-end
            (save-excursion
              (insert add-at-end)))
          (haskell-sort-imports))))))

(defun haskel-misc--is-operator? (str)
  (cl-assert (stringp str))
  (let ((len (length str)))
    (if (zerop len)
        nil
      (let ((result t)
            (i 0))
        (while (and result
                    (< i len))
          (setf result (gethash (aref str i) haskell-smart-operators--operator-chars)
                i (+ i 1)))
        result))))

;;;###autoload
(defun haskell-misc--file-name-to-module-name (path)
  "Infer name of Haskell main module from file name."
  (cl-assert (stringp path))
  (s-join
   "."
   (nreverse
    (--take-while
     (is-uppercase? (string-to-char it))
     (nreverse (f-split (file-name-sans-extension path)))))))

(defsubst haskell-cabal--yasnippet--main-module-from-main-file (str)
  (haskell-misc--file-name-to-module-name str))

(defun haskell-cabal--yasnippet--main-module-from-executable-name (str)
  (let ((upcase-first-character
         (lambda (str)
           (if (s-blank? str)
               str
             (concat (upcase (substring str 0 1)) (substring str 1))))))
    (apply #'s-concat
           (-map upcase-first-character
                 (s-split "[-]+"
                          (file-name-nondirectory (file-name-sans-extension str))
                          t ;; omit nulls
                          )))))

;;; Utilities salvaged from structured-haskell-mode.

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive "*")
  (let ((start (point)))
    (when (and (looking-back "[^\[\(\{;, ]" (line-beginning-position))
               (not (bolp)))
      (insert ?\s)
      (setq start (1+ start)))
    (when (and (looking-at-p "[^\]\)\},; ]+_*")
               (not (eolp)))
      (insert ?\s)
      (forward-char -1))
    (insert "undefined")
    (evaporate-region start (point))
    (goto-char start)))

(defun haskell-move-to-topmost-start-impl (&optional _count)
  "Move to start of the topmost node, similar to `glisp/beginning-of-defun'."
  (haskell-indentation-goto-zero-column)

  ;; (beginning-of-line)
  ;; (let ((c (char-after)))
  ;;   (while (and (not (bobp))
  ;;               (or (and c
  ;;                        (whitespace-char? c))
  ;;                   (haskell-on-blank-line?)))
  ;;     (forward-line -1)
  ;;     (setf c (char-after))))
  )

(defun haskell-on-blank-line? ()
  "Assumes point is at 0th column."
  (or
   ;; Skip preprocessor lines
   (eq (char-after) ?#)
   (indent-on-blank-line?)))

(defun haskell-on-nonindented-line? ()
  "Assumes point is at 0th column."
  (= 0 (indentation-size)))

(defun haskell-on-indented-line? ()
  "Assumes point is at 0th column."
  (/= 0 (indentation-size)))

(defun haskell-move-to-topmost-start (&optional count)
  "Move to start of the topmost node, similar to `glisp/beginning-of-defun'."
  (interactive "p")
  (vim-save-position)
  (haskell-move-to-topmost-start-impl count))

(defun haskell-move-to-topmost-end ()
  "Move to end of the topmost node, similar to `glisp/end-of-defun'."
  (interactive)
  (vim-save-position)
  (beginning-of-line)
  (while (and (not (eobp))
              (and (not (haskell-on-blank-line?))
                   (haskell-on-nonindented-line?)))
    (forward-line 1))
  (while (and (not (eobp))
              (or (haskell-on-indented-line?)
                  (haskell-on-blank-line?)))
    (forward-line 1))
  (forward-line -1)
  (while (and (not (bobp))
              (haskell-on-blank-line?))
    (forward-line -1))
  (end-of-line))

(defun haskell-qualify-import ()
  "Turn ‘import X’ -> ‘import qualified X’."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (let ((qstr "qualified"))
        (if (looking-at haskell-regexen/pre-post-qualified-import-line)
            (if (haskell-ext-tracking-have-import-qualified-post?)
                (cond
                  ((match-beginning 8)
                   ;; (goto-char (match-end 8))
                   ;; (delete-whitespace-forward)
                   (replace-match "" nil nil nil 8))
                  ((match-beginning 9)
                   (replace-match "" nil nil nil 9))
                  ((match-beginning 4)
                   (let ((start (match-beginning 6)))
                     (replace-match " " nil nil nil 6)
                     (goto-char start)
                     (insert-char ?\s)
                     (insert qstr)))
                  (t
                   (goto-char (match-end 10))
                   (insert-char ?\s)
                   (insert qstr)))
              (cond
                ((match-beginning 9)
                 (replace-match "" nil nil nil 9))
                ((match-beginning 8)
                 ;; (goto-char (match-end 8))
                 ;; (delete-whitespace-forward)
                 (replace-match "" nil nil nil 8))
                (t
                 (goto-char (match-end 7))
                 (delete-whitespace-backward)
                 (insert-char ?\s)
                 (insert qstr)
                 (insert-char ?\s))))
          (error "Not on a line with import"))))))

(defun haskell-export-ident-at-point ()
  "Add Haskell symbol at point to the export list."
  (interactive)
  (haskell--export-ident (thing-at-point 'haskell-symbol t)))

(defun haskell--export-ident (identifier)
  "Add IDENTIFIER to the end of the module’s export list."
  (cl-assert (stringp identifier))
  (save-match-data
    (save-restriction
      (save-excursion
        (widen)
        (goto-char (point-min))
        (rx-let ((ws (any ?\n ?\r ?\s ?\t))
                 (module-name (+ (any "_." alphanumeric))))
          (let ((case-fold-search nil))
            ;; Do case-sensitive search for "module" declaration.
            (if (re-search-forward
                 (rx bol
                     "module"
                     symbol-end
                     (* ws)
                     module-name
                     (* ws)
                     "(")
                 nil
                 t)
                (let ((start (1- (match-end 0))))
                  (goto-char start)
                  (cl-assert (eq (char-after) ?\())
                  (forward-sexp)
                  (cl-assert (eq (char-after (1- (point))) ?\)))
                  (let* ((end (point))
                         (parsed (haskell-sort-imports--parse-import-list-in-buffer start end)))
                    (goto-char (1- end))
                    (skip-chars-backward " \t\n\r")
                    (pcase (length (haskell-import-list-entries parsed))
                      (0
                       (insert identifier))
                      (1
                       (insert ", " identifier))
                      (_
                       (insert (haskell-import-list-sep parsed) identifier)))))
              ;; Nothing to do: either no module keyword or no export
              ;; list - in both cses everything is exported.
              nil)))))))

(defadvice haskell-indentation-indent-line (around
                                            haskell-indentation-indent-line-expand-yafolding
                                            activate
                                            compile)
  (haskell-misc--with-expanded-invisible-overlays-in-current-function
   ad-do-it))

(defadvice haskell-indentation-indent-backwards (around
                                                 haskell-indentation-indent-backwards-expand-yafolding
                                                 activate
                                                 compile)
  (haskell-misc--with-expanded-invisible-overlays-in-current-function
   ad-do-it))

(defvar-local haskell-misc--dante-configured? nil
  "Whether ‘haskell-misc--configure-dante!’ was called once.")

;;;###autoload
(defun haskell-misc-cabal-script-buf? (buf)
  "Non-nil if BUF is a cabal-style script which has no extra configuration."
  (and (with-current-buffer buf
         (save-excursion
           (save-match-data
             (goto-char (point-min))
             ;; {- cabal:
             ;; build-depends:
             ;;   , base
             ;;   , containers ^>= 0.6
             ;; -}
             (when-let ((fname (buffer-file-name buf)))
               (and (looking-at-p "^#!.*cabal")
                    (re-search-forward "^{-[ \t]*cabal:" nil t))))))
       t))

;;;###autoload
(defun haskell-misc--configure-dante-if-needed! ()
  "Call ‘haskell-misc--configure-dante!’ if it has not been called before."
  (unless (haskell-misc-cabal-script-buf? (current-buffer))
    (unless haskell-misc--dante-configured?
      (setf haskell-misc--dante-configured?
            (haskell-misc--configure-dante!)))))

(defun haskell-misc--configure-dante! ()
  "Set up vital variables for operation of ‘dante-mode’.

Returns ‘t’ on success, otherwise returns ‘nil’."
  (let* ((buf (current-buffer))
         (proj (eproj-get-project-for-buf-lax buf))
         (vars (and proj
                    (eproj-query/local-variables proj major-mode nil)))
         (val-dante-target (cadr-safe (assq 'dante-target vars)))
         (all-warnings nil))
    (if val-dante-target
        (progn
          (setq-local dante-target it)
          t)
      (if (buffer-file-name)
          (if (file-directory-p default-directory)
              (if-let ((cabal-files (haskell-misc--find-potential-cabal-files (file-name-directory (buffer-file-name buf)))))
                  (let ((component nil)
                        (pkg-name nil)
                        (tmp cabal-files))
                    (while (and (not component)
                                tmp)
                      (when-let ((config (flycheck-haskell-get-configuration (car tmp) proj)))
                        (let-alist-static config (package-name components)
                          (let* ((result
                                  (haskell-misc--configure-dante--find-cabal-component-for-file
                                   components
                                   (buffer-file-name)))
                                 (candidate-component (car result))
                                 (warnings (cdr result)))
                            (when candidate-component
                              (setf component candidate-component
                                    pkg-name (car package-name))
                              (cl-assert (stringp pkg-name) nil
                                         "Expected package name to be a string but got %s" pkg-name))
                            (setf all-warnings (nconc warnings all-warnings)))))

                      (setf tmp (cdr tmp)))
                    (if component
                        (progn
                          (unless val-dante-target
                            (setq-local dante-target (concat pkg-name ":" component)))
                          t)
                      (error "Couldn’t determine cabal component for %s from cabal file%s%s"
                             (file-name-nondirectory (buffer-file-name))
                             (if (null (cdr cabal-files))
                                 (concat " " (car cabal-files))
                               (concat "s "
                                       (mapconcat #'file-name-nondirectory cabal-files ", ")))
                             (if all-warnings
                                 (format "\nFound problems:\n%s"
                                         (mapconcat (lambda (x) (concat "- " x)) all-warnings "\n"))
                                 ""))))
                (error "No cabal files"))
            (error "Buffer’s directory doesn’t exist: %s" default-directory))
        (error "Buffer has no file: %s" (current-buffer))))))

(defun haskell-misc--configure-dante--find-cabal-component-for-file (components filename)
  "Get components dumped by get-cabal-configuration.hs for current package and attempt
to find which component the FILENAME belongs to.

COMPONENTS is a list of
(<component type> <component name> <main file> <module list> <source dirs>)

Returns (<component name or nil> . <list of warnings>)"
  (when filename
    (let* ((case-fold-search (fold-platform-os-type nil t))
           (components-with-main-is-with-slash-and-no-dot-in-src-dirs nil)
           (entry
            (-find (lambda (component-descr)
                     (let* ((main-file (cl-third component-descr))
                            (modules (cl-fourth component-descr))
                            (src-dirs-res (filter-elem (lambda (x) (not (equal x ".")))
                                                       (-map #'strip-trailing-slash
                                                             (cl-fifth component-descr))))
                            (src-dirs (car src-dirs-res))
                            (src-dirs-contained-dot? (cdr src-dirs-res)))
                       (when (and (or (string-contains? ?/ main-file)
                                      (fold-platform-os-type nil
                                                             (string-contains? ?\\ main-file)))
                                  (not src-dirs-contained-dot?)
                                  (string-match-p (concat (regexp-quote main-file) "\\'")
                                                  filename))
                         (push component-descr components-with-main-is-with-slash-and-no-dot-in-src-dirs))
                       (when (or main-file modules)
                         (let* ((mod-regexps
                                 (when modules
                                   (mapconcat (lambda (x)
                                                (concat "\\(?:"
                                                        (mapconcat #'identity x "/")
                                                        "\\)"))
                                              modules
                                              "\\|")))
                                (re
                                 (concat (when main-file
                                           (concat (when src-dirs
                                                     (concat "\\(?:"
                                                             (mapconcat #'regexp-quote src-dirs "\\|")
                                                             "\\)"
                                                             (when src-dirs-contained-dot?
                                                               "?")
                                                             "/"))
                                                   "\\(?:" (regexp-quote main-file) "\\)"))
                                         (when (and main-file mod-regexps)
                                           "\\|")
                                         (when mod-regexps
                                           (concat
                                            "\\(?:"
                                            mod-regexps
                                            "\\)\\."
                                            (eval-when-compile (regexp-opt +haskell-extensions+))))
                                         "\\'")))
                           (and re
                                (string-match-p re filename))))))
                   components)))
      (if entry
          (let ((typ (car entry))
                (name (cadr entry)))
            (cons (concat typ ":" name)
                  nil))
        (progn
          (cons nil
                ;; Report possible error to the user
                (-map (lambda (component-descr)
                        (format "Component ‘%s:%s’ specifies main file with slash (%s) but doesn’t put ‘.’ in source dirs: %s. Possible fix: remove slash or put ‘.’ into source dirs."
                                (cl-first component-descr)
                                (cl-second component-descr)
                                (cl-third component-descr)
                                (mapconcat (lambda (x) (concat "‘" x "’"))
                                           (cl-fifth component-descr)
                                           ", ")))
                      components-with-main-is-with-slash-and-no-dot-in-src-dirs)))))))

(defun haskell-misc--find-potential-cabal-files (start-dir)
  (let ((continue? t)
        (dir start-dir)
        (result nil))
    (while continue?
      (let ((interesting-files (directory-files dir
                                                nil ;; Relative names.
                                                (rx (seq bos
                                                         (or (seq "cabal" (* nonl) ".project" (? ".local"))
                                                             (seq "stack" (* nonl) ".yaml")
                                                             (seq (+ nonl) ".cabal"))
                                                         eos))
                                                t ;; Do not sort - faster this way.
                                                ))
            (have-project? nil)
            (have-stack? nil)
            (cabal-files nil))
        (dolist (file interesting-files)
          (cond
            ((or (string-suffix-p ".project.local" file)
                 (string-suffix-p ".project" file))
             (setf have-project? t))
            ((string-suffix-p ".cabal" file)
             (push (expand-file-name file dir) result))
            ((string-suffix-p ".yaml" file)
             (setf have-stack? t))))
        (let ((new-dir (file-name-directory (strip-trailing-slash dir))))
          (setf continue? (and (not (string-match-p locate-dominating-stop-dir-regexp dir))
                               (not have-stack?)
                               (not have-project?)
                               (not (equal dir new-dir)))
                dir new-dir))))
    (nreverse result)))

;; Cheap approximation for the real thing: the closest .cabal file may
;; be some utils bundled together and won’t include the file we’re
;; actually in - the project we’re part of will have a cabal file
;; above us.
(defun haskell-misc--get-project-root-for-path (start-dir)
  "Obtain root of a Haskell project that FILE is part of."
  (cl-assert (file-directory-p start-dir))
  (let ((regexp
         (rx (seq bos
                  (or (seq "cabal" (* nonl) ".project" (? ".local"))
                      ".cabal.sandbox"
                      (seq "stack" (* nonl) ".yaml")
                      (seq (+ nonl) ".cabal"))
                  eos))))
    (locate-dominating-file start-dir
                            (lambda (dir-name)
                              (directory-files dir-name
                                               nil ;; Relative names.
                                               regexp
                                               t ;; Do not sort - faster this way.
                                               )))))

(defvar-local haskell-misc--project-root nil)

(defun haskell-misc-get-project-root ()
  (if haskell-misc--project-root
      haskell-misc--project-root
    (setf haskell-misc--project-root
          (haskell-misc--get-project-root-for-path (or (and buffer-file-name
                                                            (file-name-directory buffer-file-name))
                                                       default-directory)))))

(defun haskell-cabal-find-related-file (&optional other-window)
  "Quickly switch to e.g. ‘cabal.project.local’ if currently visiting ‘cabal.project’ and back."
  (interactive "P")
  (save-match-data
    (let ((curr-file buffer-file-name))
      (when (and curr-file
                 (string-match "cabal.*\\.project\\(?:\\..*?\\)?\\(?1:\\.local\\)?\\'"
                               curr-file))
        (let ((related-file
               (if (match-beginning 1)
                   (substring curr-file 0 (- (length curr-file)
                                             (eval-when-compile (length ".local"))))
                 (concat curr-file ".local"))))
          (if (file-exists-p related-file)
              (if other-window
                  (find-file-other-window related-file)
                (find-file related-file))
            (error "Related file doesn’t exist: %s" related-file)))))))

(defun dante-project-root ()
  "Get the root directory for the project.
If the variable `dante-project-root' is non-nil, return that,
otherwise search for project root using
`dante-initialize-method'."
  (or dante-project-root
      (progn (dante-initialize-method) dante-project-root)))

(defun haskell-misc-find-tag-default ()
  (when-let ((bnds (bounds-of-haskell-symbol)))
    (buffer-substring-no-properties (car bnds) (cdr bnds))))

(provide 'haskell-misc)

;; Local Variables:
;; End:

;; haskell-misc.el ends here
