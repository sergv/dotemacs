;; haskell-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 20 September 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'macro-util)
(require 'advices-util)
(require 'common)
(require 'search)

(require 'abbrev+)
(require 'haskell-compile)
(require 'haskell-compilation-commands)
(require 'haskell-format-setup)
(require 'haskell-mode)
(require 'haskell-regexen)
(require 'hydra-setup)
(require 'compilation-setup)

(require 'flycheck)
(require 'flycheck-haskell)
(require 'flycheck-setup)

(defvar-local haskell-indent-offset 2
  "Haskell indentation amount used by functions written as part
of my home config.")

(defun* haskell-setup-indentation (&key offset simpler-indentation-by-default)
  "Set up bindings and indentation parameters using OFFSET as a
single indentation unit."
  (if simpler-indentation-by-default
      (progn
        (bind-tab-keys #'indent-relative-forward
                       #'indent-relative-backward
                       :enable-yasnippet t)
        (def-keys-for-map (vim:normal-mode-local-keymap
                           vim:insert-mode-local-keymap)
          ("C-<tab>"           haskell-indentation-indent-line)
          ("C-S-<tab>"         haskell-indentation-indent-backwards)
          ("C-S-<iso-lefttab>" haskell-indentation-indent-backwards)))
    (progn
      (bind-tab-keys #'haskell-indentation-indent-line
                     #'haskell-indentation-indent-backwards
                     :enable-yasnippet t)
      (def-keys-for-map (vim:normal-mode-local-keymap
                         vim:insert-mode-local-keymap)
        ("C-<tab>"           indent-relative-forward)
        ("C-S-<tab>"         indent-relative-backward)
        ("C-S-<iso-lefttab>" indent-relative-backward))))

  (let ((real-offset (or offset 2)))
    (setq-local vim:shift-width                    real-offset
                tab-width                          real-offset
                haskell-indent-offset              real-offset
                haskell-indentation-layout-offset  real-offset
                haskell-indentation-starter-offset real-offset
                haskell-indentation-left-offset    real-offset)
    (haskell-abbrev+-setup nil)))

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
  (make-string vim:shift-width ?\s))

;;;###autoload
(defconst +haskell-syntax-modes+
  '(haskell-mode haskell-c-mode haskell-c2hs-mode alex-mode happy-mode uuag-mode)
  "List of modes that use haskell syntax.")

(defun cleanup-stg ()
  "Remove useless srt:SRT annotations of lambdas, keep only relevant arguments
and indent them as singe line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "srt:SRT:" nil t)
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



(setf ;; Don't kill any associated buffers when issuing `haskell-session-kill'.
      haskell-ask-also-kill-buffers nil

      ghc-core-program-args
      `("-O2"
        "-dsuppress-uniques"
        "-dsuppress-idinfo"
        "-dsuppress-module-prefixes"
        ;; "-dsuppress-type-signatures"
        "-dsuppress-type-applications"
        "-dsuppress-coercions"
        "-dppr-cols200"
        "-hidir" ,small-temporary-file-directory
        "-odir" ,small-temporary-file-directory)

      ;; Flycheck
      flycheck-ghc-args
      '("-O0"
        "-fno-warn-name-shadowing"
        "-fno-warn-type-defaults"
        "-Wincomplete-uni-patterns"
        "-Wincomplete-record-updates"
        "-Wcompat")
      flycheck-ghc-no-user-package-database t

      dante-load-flags
      '("+c"
        "-fno-diagnostics-show-caret"
        "-Wwarn=missing-home-modules"
        "-O0"
        "-fdefer-typed-holes"
        "-fdefer-type-errors")

      dante-methods-alist
      (eval-when-compile
        (let* ((ghci-options
                '("-fbyte-code"
                  "-Wall"
                  "-fwarn-name-shadowing"
                  "-Wincomplete-uni-patterns"
                  "-Wincomplete-record-updates"
                  "-Wcompat"
                  "-fdiagnostics-color=always"
                  "-Wno-missing-home-modules"
                  "-dsuppress-module-prefixes"
                  "-fshow-loaded-modules"
                  "-fprint-potential-instances"))
               (build-dir (list "--builddir"
                                (fold-platform-os-type '(concat "/tmp/dist/dante"
                                                                (awhen (eproj-sha1-of-project-root-for-buf (current-buffer))
                                                                  (concat "-" it)))
                                                       "dist/dante")))
               (repl-options (--mapcat (list "--repl-option" it) ghci-options))
               (stack-ghci-options (--mapcat (list "--ghci-options" it) ghci-options)))
          `((new-impure-nix dante-cabal-new-nix
                            `("nix-shell" "--run" (s-join " " (list "cabal" "new-repl" (or dante-target (dante-package-name) "") ,@build-dir)))
                            `("nix-shell" "--run" (s-join " " (list "cabal" "new-repl" (or dante-target (dante-package-name) "") ,@build-dir ,@repl-options))))
            (new-nix dante-cabal-new-nix
                     ("nix-shell" "--pure" "--run" (s-join " " (list "cabal" "new-repl" (or dante-target (dante-package-name) "") ,@build-dir)))
                     ("nix-shell" "--pure" "--run" (s-join " " (list "cabal" "new-repl" (or dante-target (dante-package-name) "") ,@build-dir ,@repl-options))))
            (nix dante-cabal-nix
                 ("nix-shell" "--pure" "--run" (s-join " " (list "cabal" "repl" (or dante-target "") ,@build-dir)))
                 ("nix-shell" "--pure" "--run" (s-join " " (list "cabal" "repl" (or dante-target "") ,@build-dir ,@repl-options))))
            (impure-nix dante-cabal-nix
                        ("nix-shell" "--run" (s-join " " (list "cabal" "repl" (or dante-target "") ,@build-dir)))
                        ("nix-shell" "--run" (s-join " " (list "cabal" "repl" (or dante-target "") ,@build-dir ,@repl-options))))
            (new-build "cabal.project"
                       ("cabal" "new-repl" (or dante-target (dante-package-name) nil) ,@build-dir)
                       ("cabal" "new-repl" (or dante-target (dante-package-name) nil) ,@build-dir ,@repl-options))
            (stack "stack.yaml" ("stack" "repl" dante-target) ("stack" "repl" dante-target ,@stack-ghci-options))
            (bare-ghci ,(lambda (_) t) ("ghci"))))))

(defconst +haskell-compile-error-or-warning-navigation-regexp+
  (mk-regexp-from-alts
   (list
    (default-value '*compilation-jump-error-regexp*)
    ;; Tasty errors.
    "\\<error, called at \\(.*\\.hs\\):\\([0-9]+\\):\\([0-9]+\\) in\\>"
    ))
  "Regexp matching both errors and warnings. Used to navigate between errors
in haskell compilation buffer.")

;;; up level navigation

(defun haskell-back-up-indent-level ()
  "Move up to lesser indentation level, skipping empty lines.

Returns t if indentation occured."
  (let ((start-indent (indentation-size))
        (col (current-column)))
    (cond
      ((> col start-indent)
       (back-to-indentation)
       t)
      ;; Do not move past 0th column in order to not skip to the
      ;; beginning of file.
      ((/= 0 start-indent)
       ;;(= col start-indent)
       (while (and (not (bobp))
                   (let ((curr-indent (indentation-size)))
                     (or (>= curr-indent start-indent)
                         (> curr-indent col))))
         (forward-line -1)
         (while (looking-at-p haskell-regexen/preprocessor-or-empty-line)
           (forward-line -1)))
       (back-to-indentation)
       t)
      (t
       nil))))

;;;###autoload
(defun haskell-backward-up-indentation-or-sexp ()
  "Haskell brother of `sp-backward-up-sexp' that considers both
sexps and indentation levels."
  (interactive)
  (let* ((start (point))
         (with-indentation
          (with-demoted-errors
              (save-excursion
                (haskell-back-up-indent-level)
                (let ((p (point)))
                  (when (/= p start)
                    p)))))
         (with-sp
          (when (/= 0 (syntax-ppss-depth (syntax-ppss start)))
            (with-demoted-errors
                (save-excursion
                  (sp-backward-up-sexp)
                  (let ((p (point)))
                    (when (/= p start)
                      p)))))))
    (if (and with-indentation
             with-sp)
        (goto-char (max with-indentation with-sp))
      (goto-char (or with-indentation
                     with-sp
                     (error "Both indentation-based and sexp-based navigations failed"))))))

;;;###autoload
(defun haskell-up-sexp ()
  "Haskell brother of `sp-up-sexp' that considers only sexps for now."
  (interactive)
  (sp-up-sexp))

;;;; align functions

(defalign haskell-align-on-dollars
  "[$][^$]"
  :require-one-or-more-spaces t)
(defalign haskell-align-on-equals
  "=[^=]"
  :require-one-or-more-spaces t)
(defalign haskell-align-on-arrows
  (rx
   (or (seq
        (+ (any ?: ?| ?- ?=))
        (+ ">"))
       "→")
   (or " "
       eol)))
(defalign haskell-align-on-left-arrows
  (rx
   (or (seq
        (+ "<")
        (+ (any ?: ?| ?- ?=)))
       "←")
   (or " "
       eol)))
(defalign haskell-align-on-guards
  "|\\(?:[^|]\\|$\\)"
  :require-one-or-more-spaces t)
(defalign haskell-align-on-commas
  ",\\(?:[^,\)]\\|$\\)")
(defalign haskell-align-on-comments
  "--+\\(?: \\|$\\)"
  :require-one-or-more-spaces t)

(defalign haskell-align-on-double-colons
  haskell-regexen/function-signature-colons)
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
  ("a"     haskell-align-generic)
  ("$"     haskell-align-on-dollars)
  ("="     haskell-align-on-equals)
  ("->"    haskell-align-on-arrows)
  ("<-"    haskell-align-on-left-arrows)
  ("|"     haskell-align-on-guards)
  (","     haskell-align-on-commas)
  ("--"    haskell-align-on-comments)
  (": :"   haskell-align-on-double-colons)
  ("# - }" haskell-align-on-pragma-close))

(defun haskell-reindent-at-point (&optional width)
  "Do some sensible reindentation depending on the current position in file."
  (interactive "p*")
  (save-match-data
    (cond
      ((save-excursion
         (beginning-of-line)
         (looking-at-p haskell-abbrev+/language-pragma-prefix))
       (save-current-line-column
         (haskell-align-language-pragmas (point))))
      ((and (eq (get-char-property (point) 'face) 'haskell-pragma-face)
            (save-excursion
              (re-search-backward haskell-regexen/pragma-start nil t)
              (looking-at-p haskell-abbrev+/language-pragma-prefix)))
       (save-current-line-column
         (haskell-align-language-pragmas (point))))
      ((save-excursion
         (beginning-of-line)
         (looking-at-p "import "))
       (save-current-line-column
         (haskell-sort-imports)))
      ((save-excursion
         (beginning-of-line)
         (not (looking-at-p "^[ \t]*$")))
       (let* ((p (point))
              (fingerprint-re (haskell-misc--fingerprint-re (current-line)))
              (end-mark nil)
              (end (save-excursion
                     (haskell-move-to-topmost-end)
                     (skip-chars-forward "\r\n")
                     (prog1 (point)
                       (unless (eobp)
                         (save-excursion
                           (forward-line 1)
                           (setf end-mark (point-marker)))))))
              (start (save-excursion
                       (haskell-move-to-topmost-start)
                       (point))))
         (haskell-format--format-with-brittany haskell-indent-offset
                                               (if (and width
                                                        (< 1 width))
                                                   width
                                                 haskell-format-default-width)
                                               start
                                               end)
         (goto-char start)
         (if (re-search-forward fingerprint-re end-mark t)
             (goto-char (match-beginning 0))
           (goto-char p))))
      (t
       (error "Don't know how to reindent construct at point")))))

(defun haskell-misc--fingerprint-re (str)
  "Take current line and come up with a fingerprint
regexp that will find this line after the indentation was
applied.

E.g. given a line like

>      foo = bar $ baz (quux fizz) frob

the regex should look like

foo\\w*=\\w*bar\\w*[$]\\w*baz\\w*[(]\\w*quux\\w*fizz\\w*[)]\\w*frob

where \\w matches any whitespace including newlines"
  (s-join "[ \t\r\n]*"
          (--map (regexp-quote it)
                 (--filter (not (s-blank-str? it))
                           (--map (list->string it)
                                  (-partition-by #'char-syntax
                                                 (string->list
                                                  (s-collapse-whitespace
                                                   (s-trim
                                                    str)))))))))

(defun haskell-align-language-pragmas--point-inside-pragma (point)
  (save-excursion
    (save-match-data
      (when (re-search-forward haskell-regexen/pragma-end nil t)
        (let ((end (point)))
          (backward-sexp)
          (let ((start (point)))
            (and (<= start point)
                 (<= point end))))))))

(defun haskell-align-language-pragmas (start)
  (save-match-data
    (goto-char start)
    ;; (cl-assert (looking-at-p haskell-abbrev+/language-pragma-prefix))
    ;; Navigate up while we're still getting LANGUAGE pragmas.
    (beginning-of-line)
    (while (and (not (bob?))
                (or (looking-at-p haskell-abbrev+/language-pragma-prefix)
                    (haskell-align-language-pragmas--point-inside-pragma (point))))
      ;; Go to beginning of the previous line.
      (backward-line))
    ;; Skip whitespace and possible comments to the beginning of pragma.
    (re-search-forward haskell-regexen/pragma-start)
    (goto-char (match-beginning 0))
    (let ((pragma-block-start (point))
          (pragma-block-end nil)
          (exts nil)
          (done nil))
      ;; Collect all extensions from all pragmas
      (while (not done)
        (aif (haskell--parse-language-pragma (point) (point-max))
            (progn
              (setf exts (append it exts)
                    pragma-block-end (point))
              (forward-line 1)
              (if (eob?)
                  (setf done t)
                (beginning-of-line))
              ;; (skip-syntax-forward " >")
              )
          (setf done t)))
      (goto-char pragma-block-start)
      (delete-region pragma-block-start pragma-block-end)
      (setf exts (sort exts #'string<))
      (when exts
        (insert (format "{-# LANGUAGE %s #-}" (first exts)))
        (dolist (e (cdr exts))
          (insert "\n")
          (insert (format "{-# LANGUAGE %s #-}" e))))
      (haskell-align-on-pragma-close-indent-region pragma-block-start (point)))))

(defun haskell--parse-language-pragma (start end)
  "Parse single LANGUAGE pragma within START-END region and return its
extensions as a list of strings. Leaves point at the end of pragma"
  (goto-char start)
  (when (looking-at haskell-abbrev+/language-pragma-prefix)
    (let ((pragma-end (min
                       end
                       (save-excursion
                         (forward-sexp)
                         (point)))))
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

;;; define forward-haskell-symbol

(defparameter haskell-symbol--identifier-syntax-table
  (let ((tbl (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?#  "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    (modify-syntax-entry ?\' "w" tbl)
    (modify-syntax-entry ?,  "/" tbl) ;; Disable , since it's part of syntax
    (modify-syntax-entry ?.  "_" tbl) ;; So that we match qualified names.
    tbl)
  "Special syntax table for haskell that allows to recognize symbols that contain
both unicode and ascii characters.")

;;;###autoload
(defun bounds-of-haskell-symbol ()
    (save-excursion
    (save-match-data
      (with-syntax-table haskell-symbol--identifier-syntax-table
        (forward-char 1)
        (let ((start nil)
              (end nil)
              (beginning-quotes "'"))
          (if (zerop (skip-syntax-backward "w_"))
              (progn
                (skip-syntax-backward "._")
                ;; To get qualified part
                (skip-syntax-backward "w_")
                (skip-chars-forward beginning-quotes))
            (progn
              (skip-chars-forward beginning-quotes)))
          (setf start (point))
          (when (looking-at (rx (+ (char upper) (* (char alnum ?_)) ".")))
            (goto-char (match-end 0)))
          (when (zerop (skip-syntax-forward "w_"))
            (skip-syntax-forward "._"))
          (setf end (point))
          (cons start end))))))

;;;###autoload
(put 'haskell-symbol 'bounds-of-thing-at-point #'bounds-of-haskell-symbol)

;; newline that detects haskell signatures

(defun haskell--simple-indent-newline-same-col ()
  "Make a newline and go to the same column as the current line."
  (interactive "*")
  (delete-horizontal-space t)
  (let ((indentation-size
         (save-excursion
           (let* ((start (line-beginning-position))
                  (end (progn
                         (goto-char start)
                         ;; (search-forward-regexp
                         ;;  "[^ ]" (line-end-position) t 1)
                         (skip-to-indentation)
                         (point))))
             (when (or (eobp) (/= ?\s (char-after)))
               (- end start))))))
    (insert "\n")
    (when indentation-size
      (insert (make-string indentation-size ?\s)))))

(defun haskell--simple-indent-newline-indent ()
  "Make a newline on the current column and indent on step."
  (interactive "*")
  (haskell--simple-indent-newline-same-col)
  (insert (make-string haskell-indent-offset ?\s)))

(defun haskell-newline-with-signature-expansion ()
  "Similar to `sp-newline' but autoexpands haskell signatures."
  (interactive "*")
  (haskell-misc--with-expanded-invisible-overlays-in-current-function
   (let* ((start-pos (point))
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
                     (haskell-back-up-indent-level)
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
                                                   (concat "\\(?2:" name-re "\\)"
                                                           "\\(?:," ws "*" name-re "\\)*")
                                                   "\\)"
                                                   ws "*"
                                                   haskell-regexen/function-signature-colons))))
                       (setf found? t
                             func-name (match-string 1))))
                   (when found?
                     (goto-char (match-beginning 1))
                     (setf function-name-column (current-column))
                     (let ((indented-section-end (line-end-position)))
                       (forward-line 1)
                       (while (and (not (eobp))
                                   (< function-name-column (indentation-size)))
                         (setf indented-section-end (line-end-position))
                         (forward-line 1))
                       ;; Do not expand if we're not located at the
                       ;; type signature's end.
                       (setf point-at-end-of-function-signature?
                             (= start-pos indented-section-end))
                       (when (and
                              point-at-end-of-function-signature?
                              (not
                               (save-excursion
                                 (goto-char indented-section-end)
                                 (skip-syntax-forward "->")
                                 (looking-at-p (concat (regexp-quote func-name)
                                                       "\\_>")))))
                         (goto-char start-pos)
                         (delete-horizontal-space t)
                         (insert "\n")
                         (insert (make-string function-name-column ?\s)
                                 func-name
                                 " ")
                         t)))))))))
     (when (null expanded-function-name?)
       (goto-char start-pos)
       (let* ((syn (syntax-ppss))
              (in-string? (nth 3 syn)))
         (cond
           (in-string?
            (let ((string-start-column (save-excursion
                                         (goto-char (nth 8 syn))
                                         (current-column))))
              (delete-horizontal-space t)
              (insert "\\\n" (make-string string-start-column ?\s) "\\")))
           ((and function-name-column
                 point-at-end-of-function-signature?)
            (delete-horizontal-space t)
            (insert "\n" (make-string function-name-column ?\s)))
           ((save-excursion
              (skip-syntax-backward " ")
              (skip-syntax-backward "w_.")
              (looking-at-p
               (rx (or (seq
                        symbol-start
                        (or "where"
                            "of"
                            "do")
                        symbol-end)
                       (or "="
                           "->"
                           "<-")))))
            (haskell--simple-indent-newline-indent))
           ((save-excursion
              (skip-to-indentation)
              (looking-at-p "let\\_>"))
            (haskell--simple-indent-newline-same-col)
            (insert (make-string 4 ?\s)))
           (t
            (haskell--simple-indent-newline-same-col))))))))

(defsubst haskell-abbrev+-fallback-space ()
  (interactive "*")
  (haskell-space-with-block-indent))

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

(defun* install-haskell-smart-operators! (keymap &key bind-colon bind-hyphen)
  (declare (indent 1))
  (haskell-smart-operators-mode +1)
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

(defconst haskell-compilation-buffer "*haskell-compilation*")

(defun haskell-compilation-next-error-other-window ()
  "Select next error in `haskell-compilation-buffer' buffer and jump to
it's position in current window."
  (interactive)
  (aif (get-buffer haskell-compilation-buffer)
      (compilation-navigation-next-error-in-buffer-other-window it)
    (error "No Haskell compilation started")))

(defun haskell-compilation-prev-error-other-window ()
  "Select previous error in `haskell-compilation-buffer' buffer and jump to
it's position in current window."
  (interactive)
  (aif (get-buffer haskell-compilation-buffer)
      (compilation-navigation-prev-error-in-buffer-other-window it)
    (error "No Haskell compilation started")))

(defun haskell-misc--get-potential-project-roots ()
  (haskell-watch-get-project-root))

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
              :data-start-column (+ 2 (current-column)))))))

(defun haskell-misc-cabal-align-and-sort-subsection ()
  "Sort lines of the Cabal subsection at point."
  (interactive "*")
  (save-match-data
    (haskell-cabal-save-position
     (haskell-cabal-with-subsection
      (haskell-misc--cabal-indented-subsection)
      t
      (haskell-cabal-with-cs-list
       (haskell-cabal-each-line
        (beginning-of-line)
        (when (looking-at "^[ \t]*\\([^ \t\r\n]\\(?:.*[^ \t\r\n]\\)?\\)[ \t]*$")
          (replace-match (match-string 1) nil t)))
       (sort-subr nil
                  'forward-line
                  'end-of-line
                  'haskell-cabal-sort-lines-key-fun))))))

(defun haskell-cabal--yasnippet--main-module-from-main-file (str)
  "Infer name of Haskell main module from file name."
  (s-join
   "."
   (nreverse
    (--take-while
     (is-uppercase? (string-to-char it))
     (nreverse (f-split (file-name-sans-extension str)))))))

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

(defface haskell-evaporate-face
  '((t :foreground "#666666"))
  "Face for text that will evaporate when modified/overwritten.")

(defun haskell-evaporate (beg end &optional disable-cycling?)
  "Make the region evaporate when typed over."
  (interactive "r")
  (let ((o (make-overlay beg end nil nil nil)))
    (overlay-put o 'face 'haskell-evaporate-face)
    (overlay-put o 'priority 2)
    (overlay-put o 'modification-hooks '(haskell-evaporate-modification-hook))
    (overlay-put o 'insert-in-front-hooks '(haskell-evaporate-insert-before-hook))
    (overlay-put o 'insert-behind-hooks '(haskell-evaporate-insert-behind-hook))))

(defun haskell-evaporate-modification-hook (o changed beg end &optional len)
  "Remove the overlay after a modification occurs."
  (let ((inhibit-modification-hooks t))
    (when (and changed
                (overlay-start o))
      (haskell-evaporate-delete-text o beg end)
      (delete-overlay o))))

(defun haskell-evaporate-insert-before-hook (o changed beg end &optional len)
  "Remove the overlay before inserting something at the start."
  (let ((inhibit-modification-hooks t))
    (when (and (not changed)
               (overlay-start o))
      (haskell-evaporate-delete-text o beg end)
      (delete-overlay o))))

(defun haskell-evaporate-insert-behind-hook (o changed beg end &optional len)
  "Remove the overlay when calling backspace at the end.."
  (let ((inhibit-modification-hooks t))
    (when (and (not changed)
               (overlay-start o))
      (haskell-evaporate-delete-text o beg end)
      (delete-overlay o))))

(defun haskell-evaporate-delete-text (o beg end)
  "Delete the text associated with the evaporating slot."
  (unless (eq this-command 'undo)
    (delete-region (overlay-start o)
                   (overlay-end o))))


(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive "*")
  (let ((start (point)))
    (when (and (looking-back "[^\[\(\{;, ]")
               (not (bolp)))
      (insert " ")
      (setq start (1+ start)))
    (when (and (looking-at-p "[^\]\)\},; ]+_*")
               (not (eolp)))
      (insert " ")
      (forward-char -1))
    (insert "undefined")
    (haskell-evaporate start (point) nil)
    (goto-char start)))


(defun haskell-move-to-topmost-start ()
  "Move to start of the topmost node, similar to `glisp/beginning-of-defun'."
  (interactive)
  (vim:save-position)
  (save-match-data
   (re-search-backward "^[^ \t\v\f\n\r#]" nil t))
  ;; (beginning-of-line)
  ;; (while (and (not (bobp))
  ;;             (or (/= 0 (indentation-size))
  ;;                 (looking-at-p haskell-regexen/preprocessor-or-empty-line)))
  ;;   (forward-line -1))
  )

(defun haskell-move-to-topmost-end ()
  "Move to end of the topmost node, similar to `glisp/end-of-defun'."
  (interactive)
  (vim:save-position)
  (beginning-of-line)
  (while (and (not (eobp))
              (= 0 (indentation-size)))
    (forward-line 1))
  (while (and (not (eobp))
              (or (/= 0 (indentation-size))
                  (looking-at-p haskell-regexen/preprocessor-or-empty-line)))
    (forward-line 1))
  (forward-line -1)
  (while (and (not (bobp))
              (looking-at-p haskell-regexen/preprocessor-or-empty-line))
    (forward-line -1))
  (end-of-line))

(defun haskell-qualify-import ()
  (interactive "*")
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (if (looking-at haskell-regexen/import-line)
          (progn
            (goto-char (match-end 0))
            (delete-whitespace-backward)
            (if (looking-at "qualified[ \t\r\n]+")
                (replace-match " ")
              (insert " qualified ")))
        (error "Not on a line with import")))))

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

(defun haskell-misc--configure-dante ()
  (let* ((proj (eproj-get-project-for-buf-lax (current-buffer)))
         (vars (and proj
                    (eproj-query/local-variables proj major-mode nil)))
         (val-dante-package-name (cadr-safe (assq 'dante-package-name vars)))
         (val-dante-target (cadr-safe (assq 'dante-target vars))))
    (when val-dante-package-name
      (setq-local dante-package-name val-dante-package-name))
    (when val-dante-target
      (setq-local dante-target val-dante-target))
    (when (and (or (not val-dante-package-name)
                   (not val-dante-target))
               (buffer-file-name)
               (file-directory-p default-directory))
      (when-let ((config-file (flycheck-haskell--find-config-file))
                 (config (flycheck-haskell-get-configuration config-file)))
        (let ((package-name (cadr-safe (assq 'package-name config)))
              (components (cdr-safe (assq 'components config))))
          (when (not val-dante-package-name)
            (setq-local dante-package-name package-name))
          (when (not val-dante-target)
            (when-let ((component
                        (haskell-misc--configure-dante--find-cabal-component-for-file
                         components
                         (buffer-file-name))))
              (cl-assert (stringp package-name) nil
                         "Expected package name to be as tring but got %s" package-name)
              (setq-local dante-target
                          (concat package-name ":" component)))))))))

(defun haskell-misc--configure-dante--find-cabal-component-for-file (components filename)
  (when filename
    (let ((entry
           (-find (lambda (component-descr)
                    (let ((main-file (cl-third component-descr))
                          (modules (cl-fourth component-descr)))
                      (when (or main-file modules)
                        (let* ((mod-regexps
                                (when modules
                                  (mapconcat (lambda (x)
                                               (concat "\\(?:"
                                                       (mapconcat #'identity x ".")
                                                       "\\)"))
                                             modules
                                             "\\|")))
                               (re
                                (concat (when main-file
                                          (concat "\\(?:" main-file "\\)"))
                                        (when (and main-file mod-regexps)
                                          "\\|")
                                        (when mod-regexps
                                          (concat
                                           "\\(?:"
                                           mod-regexps
                                           "\\)"
                                           "[.]"
                                           (regexp-opt +haskell-extensions+)))
                                        "\\'")))
                          (and re
                               (string-match-p re filename))))))
                  components)))
      (when entry
        (let ((typ (car entry))
              (name (cadr entry)))
          (concat typ ":" name))))))

(provide 'haskell-misc)

;; Local Variables:
;; End:

;; haskell-misc.el ends here
