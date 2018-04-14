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
(require 'haskell-mode)
(require 'haskell-regexen)
(require 'compilation-setup)

(require 'flycheck)
(require 'flycheck-haskell)
(require 'flycheck-setup)
(require 'intero)

(require 'f)
(require 'dash)

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
          ("C-S-<iso-lefttab>" haskell-indentation-indent-backwards)))
    (progn
      (bind-tab-keys #'haskell-indentation-indent-line
                     #'haskell-indentation-indent-backwards
                     :enable-yasnippet t)
      (def-keys-for-map (vim:normal-mode-local-keymap
                         vim:insert-mode-local-keymap)
        ("C-<tab>"           indent-relative-forward)
        ("C-S-<iso-lefttab>" indent-relative-backward))))

  (let ((real-offset (or offset 2)))
    (setq-local vim:shift-width                    real-offset)
    (setq-local tab-width                          real-offset)
    (setq-local haskell-indent-offset              real-offset)
    (setq-local haskell-indentation-layout-offset  real-offset)
    (setq-local haskell-indentation-starter-offset real-offset)
    (setq-local haskell-indentation-left-offset    real-offset)
    (haskell-abbrev+-setup real-offset)))

(defun haskell-misc--single-indent ()
  "Return a string for single indentation amount for Haskell."
  (make-string vim:shift-width ?\s))

;;;###autoload
(defconst +haskell-syntax-modes+ '(haskell-mode haskell-c-mode haskell-c2hs-mode)
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

(let* ((build-dir
        (fold-platform-os-type
         "/tmp/dist"
         nil))
       (mk-build-dir-arg
        (lambda (custom-build-dir)
          (if custom-build-dir
              (concat "--builddir " custom-build-dir " ")
            "")
          ;;""
          ))
       (common-conf-opts
        (lambda (custom-build-dir)
          (concat (funcall mk-build-dir-arg custom-build-dir)
                  "--enable-tests")))
       (build-command
        (lambda (custom-build-dir)
          (concat
           "cabal build " (funcall mk-build-dir-arg custom-build-dir))))
       (test-command
        (lambda (custom-build-dir)
          (concat
           "cabal test " (funcall mk-build-dir-arg custom-build-dir) "--show-details=always")))
       (sep " && \\\n")

       (stack-command
        (lambda (cmd)
          (format "cd \"%%s\" && stack %s --ghc-options=\"-j4 +RTS -A128m -H256m -RTS\"" cmd)))
       (cd-command "cd \"%s\""))
  (setf haskell-compile-cabal-build-command-presets
        (-mapcat (lambda (entry) (if (listp (car entry))
                                (-map (lambda (cmd) (list cmd (cadr entry))) (car entry))
                              (list entry)))
                 `(((vanilla stack default)
                    ,(funcall stack-command "build"))
                   (prof
                    ,(funcall stack-command "build --profile --test --no-run-tests"))
                   (clean
                    ,(funcall stack-command "clean"))
                   (test
                    ,(funcall stack-command "test"))
                   ((test-norun build-tests)
                    ,(funcall stack-command "test --no-run-tests"))
                   (bench
                    ,(funcall stack-command "bench"))
                   (cabal-vanilla
                    ,(concat
                      cd-command
                      sep
                      (concat "cabal "
                              "configure "
                              "--disable-library-profiling "
                              "--disable-profiling "
                              (funcall common-conf-opts build-dir))
                      sep
                      (funcall build-command build-dir)
                      sep
                      (funcall test-command build-dir)))
                   (cabal-build
                    ,(concat
                      cd-command
                      sep
                      (funcall build-command build-dir)))
                   (cabal-build-inlpace
                    ,(concat
                      cd-command
                      sep
                      (funcall build-command nil)))
                   (cabal-test
                    ,(concat
                      cd-command
                      sep
                      (funcall test-command build-dir)))
                   (cabal-clean
                    ,(concat
                      cd-command
                      sep
                      "cabal clean --builddir " build-dir))
                   (cabal-prof
                    ,(concat
                      cd-command
                      sep
                      (concat "cabal "
                              "configure "
                              "--enable-profiling "
                              (funcall common-conf-opts build-dir))
                      sep
                      (funcall build-command build-dir)
                      sep
                      (funcall test-command build-dir)))))))

(setf haskell-compile-cabal-build-command
      (or (cadr-safe (assoc 'vanilla haskell-compile-cabal-build-command-presets))
          (error "Failed to set up haskell-compile-cabal-build-command"))
      ;; 'cabal-repl is good as well

      ;; Don't kill any associated buffers when issuing `haskell-session-kill'.
      haskell-ask-also-kill-buffers nil

      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      ;; haskell-process-suggest-hoogle-imports t ;; may be cool
      haskell-interactive-mode-eval-mode #'haskell-mode

      ;; haskell-process
      haskell-interactive-prompt "λ> "
      haskell-process-type 'auto
      ;; haskell-process-type 'stack-ghci
      haskell-process-log t
      ;; don't prompt on starting repl
      haskell-process-load-or-reload-prompt nil
      haskell-process-show-debug-tips nil

      haskell-process-suggest-haskell-docs-imports nil
      haskell-process-suggest-hayoo-imports nil
      haskell-process-suggest-hoogle-imports nil

      haskell-process-suggest-add-package nil
      haskell-process-suggest-remove-import-lines t
      haskell-process-suggest-overloaded-strings nil

      haskell-process-check-cabal-config-on-load t
      haskell-process-auto-import-loaded-modules nil
      haskell-interactive-popup-errors nil
      ;; Don't show types for data without Show instance>
      haskell-interactive-types-for-show-ambiguous nil
      ;; unsure
      ;; haskell-interactive-mode-delete-superseded-errors nil

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

      inferior-haskell-find-project-root nil

      haskell-interactive-prompt-read-only t
      haskell-interactive-mode-read-only t

      ;; Flycheck
      flycheck-ghc-args
      '("-O0"
        "-fno-warn-name-shadowing"
        "-fno-warn-type-defaults"
        "-Wincomplete-uni-patterns"
        "-Wincomplete-record-updates"
        "-Wcompat")
      flycheck-ghc-no-user-package-database t

      intero-extra-ghc-options
      '("-O0"
        "-Wall"
        "-fwarn-name-shadowing"
        "-fno-warn-type-defaults"
        "-Wincomplete-uni-patterns"
        "-Wincomplete-record-updates"
        "-Wcompat"
        "-dsuppress-module-prefixes")
      intero-extra-ghci-options
      '("-XOverloadedStrings"))

(def-keys-for-map intero-multiswitch-keymap
  ("<escape>" abort-recursive-edit)
  ("q"        abort-recursive-edit)
  ("SPC"      widget-button-press)
  ("h"        widget-forward)
  ("<down>"   widget-forward)
  ("t"        widget-backward)
  ("<up>"     widget-backward))

;; Ghci flags
(let* ((extensions '("-XLambdaCase" "-XOverloadedStrings" "-XTemplateHaskell" "-XQuasiQuotes"))
       (ghc-options (append
                     '("-ferror-spans"
                       "-Wwarn")
                     extensions)))
  (setf haskell-process-path-ghci
        (fold-platform-os-type
         "ghci"
         "ghc")
        haskell-process-args-ghci
        (let ((opts (append
                     '("-fbyte-code" "-Wwarn")
                     (fold-platform-os-type
                      '("-i/tmp/dist/build"
                        "-odir" "/tmp/ghc"
                        "-hidir" "/tmp/ghc")
                      nil)))
              (rts-opts '("+RTS" "-M8G" "-RTS")))
          (append (fold-platform-os-type
                   nil
                   '("--interactive"))
                  extensions
                  opts
                  rts-opts))
        haskell-process-args-cabal-repl (--map (concat "--ghc-option=" it) ghc-options)
        haskell-process-args-stack-ghci (--map (concat "--ghci-options=" it) ghc-options)))

(defconst +haskell-compile-error-or-warning-navigation-regexp+
  (mapconcat
   (lambda (entry) (concat "\\(?:" entry "\\)"))
   (-map #'car
         (--filter (let* ((type-field (car (cddddr it)))
                          (error-or-warning?
                           (cond
                             ((null type-field)
                              t)
                             ((numberp type-field)
                              (<= 1 type-field))
                             ((and (consp type-field)
                                   type-field
                                   (numberp (car type-field)))
                              (<= 1 (car type-field)))
                             (t
                              (message "Invalid entry type in haskell-compilation-error-regexp-alist: %s, entry: %s"
                                       type-field
                                       it)
                              nil))))
                     error-or-warning?)
                   (append
                    haskell-compilation-error-regexp-alist
                    ;; Add C compilers into the mix so that their
                    ;; errors will also be colorised when compiling
                    ;; Haskell packages with foreign code.
                    (-map #'cdr
                          (--filter (memq (car it) haskell-compilation-extra-error-modes)
                                    compilation-error-regexp-alist-alist)))))
   "\\|")
  "Regexp matching both errors and warnings. Used to navigate between errors
in haskell compilation buffer.")

(defconst haskell-module-quantification-regexp
  (let ((conid "\\b[[:upper:]][[:alnum:]'_]*\\b"))
    (concat "\\b\\(?:" conid "\\.\\)+")))

;;;###autoload
(defun haskell-remove-module-qualification (name)
  "Removes hierarchihal modules qualification (e.g. Data.Map.null -> null,
 Prelude.++ -> ++, etc)"
  (save-match-data
    (if (string-match (eval-when-compile
                        (concat "^\\("
                                haskell-module-quantification-regexp
                                "\\)"))
                      name)
        (replace-match "" t t name 1)
      name)))

(defun inf-haskell-send-input-or-jump-to-error ()
  (interactive)
  (if (looking-at-p *compilation-jump-error-regexp*)
      (compile-goto-error)
    (comint-send-input)))

;;; simple documentation system

(defconst haskell-language-extensions
  ;; make this list from documentation, e.g.
  ;; http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/flag-reference.html
  ;; command: '<,'>s/^-X\([^\t]+\)\t\([^\t]+\)\t[^\t]+\t-\(?:X\(.*\)\)?/("\1" "\2" "\3")/
  (when-let (ghc-exec (executable-find "ghc"))
    (with-temp-buffer
      (call-process ghc-exec
                    nil
                    (current-buffer)
                    nil
                    "--supported-extensions")
      (sort
       (split-string (buffer-substring-no-properties (point-min) (point-max))
                     "[\n\r]+"
                     t
                     "[ \t]+")
       #'string<)))
  "List of Haskell extensions for current GHC in the PATH.

See http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/flag-reference.html
for more information.")

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

(make-align-function haskell-align-on-equals
                     "=[^=]"
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-arrows
                     (rx
                      (or (seq
                           (+ (any ?: ?| ?- ?=))
                           (+ ">"))
                          "→")
                      (or " "
                          eol)))
(make-align-function haskell-align-on-left-arrows
                     (rx
                      (or (seq
                           (+ "<")
                           (+ (any ?: ?| ?- ?=)))
                          "←")
                      (or " "
                          eol)))
(make-align-function haskell-align-on-guards
                     "|\\(?:[^|]\\|$\\)"
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-commas
                     ",\\(?:[^,\)]\\|$\\)")
(make-align-function haskell-align-on-comments
                     "--+\\(?: \\|$\\)"
                     :require-one-or-more-spaces t)

(make-align-function haskell-align-on-double-colons
                     haskell-regexen/function-signature-colons)
(make-align-function haskell-align-on-pragma-close
                     "#-}")


(defun haskell-align-generic ()
  (interactive "*")
  (haskell-align-on-equals)
  (haskell-align-on-arrows)
  (haskell-align-on-left-arrows)
  (haskell-align-on-comments)
  (haskell-align-on-double-colons)
  (haskell-align-on-pragma-close))

(defun haskell-define-align-bindings! (keymap)
  (def-keys-for-map keymap
    ("g a"       nil)
    ("g a a"     haskell-align-generic)
    ("g a ="     haskell-align-on-equals)
    ("g a - >"   haskell-align-on-arrows)
    ("g a < -"   haskell-align-on-left-arrows)
    ("g a |"     haskell-align-on-guards)
    ("g a ,"     haskell-align-on-commas)
    ("g a - -"   haskell-align-on-comments)
    ("g a : :"   haskell-align-on-double-colons)
    ("g a # - }" haskell-align-on-pragma-close)))

(defun haskell-reindent-at-point ()
  "Do some sensible reindentation depending on the current position in file."
  (interactive "*")
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
      (t
       (error "Don't know how to reindent construct at point")))))

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

(defparameter haskell-symbol-re
  (rx (or (group (+ ;; (regexp "[-!#$%&*+./<=>?@^|~:\\]")
                  (any ?\- ?\! ?\# ?\$ ?\% ?\& ?\* ?\+ ?\. ?\/ ?\< ?\= ?\> ?\? ?\@ ?^ ?\| ?\~ ?\: ?\\ )))
          (group
           (seq ;; allow _ as a first char to fit GHC
            (or (regexp "\\<[_a-z]")
                ;; allow ' preceding conids because of DataKinds/PolyKinds
                (regexp "'*[A-Z]")
                (syntax word))
            (group
             (* (regexp "\\(?:['a-zA-Z_0-9#]\\|\\sw\\)")))))))

  "Regexp to recognize haskell symbols as generic entities for search
(with e.g. \"*\" in vim).")

;;;###autoload
(put 'haskell-symbol 'forward-op #'forward-haskell-symbol)

(defparameter forward-haskell-symbol-syntax-table
  (let ((tbl (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?#  "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    (modify-syntax-entry ?\' "w" tbl)
    tbl)
  "Special syntax table for haskell that allows to recognize symbols that contain
both unicode and ascii characters.")

;;;###autoload
(defun forward-haskell-symbol (arg)
  "Like `forward-symbol' but for generic Haskell symbols (either operators,
uppercase or lowercase names)."
  (interactive "p")
  (let (;; (name-chars "a-zA-Z0-9_#'")

        ;; NB constructs like "''Foobar" we'd like to mach "Foobar"
        ;; via `bounds-of-thing-at-point', not the "''Foobar".
        (beginning-quotes "'")
        (operator-chars "\\-!#$%&*+./<=>?@\\^|~:\\\\"))
    (with-syntax-table forward-haskell-symbol-syntax-table
      (if (natnump arg)
          (re-search-forward haskell-symbol-re nil t arg)
        (while (< arg 0)
          (when (re-search-backward haskell-symbol-re nil t)
            (cond ((not (null? (match-beginning 1)))
                   (skip-chars-backward operator-chars)
                   ;; we may have matched # thas ends a name
                   (skip-syntax-backward "w")
                   (skip-chars-forward beginning-quotes))
                  ((not (null? (match-beginning 2)))
                   ;; (goto-char (match-beginning 2))
                   (when (not (null? (match-beginning 3)))
                     (skip-syntax-backward "w")
                     (skip-chars-forward beginning-quotes)))
                  (t
                   (error "No group of haskell-symbol-re matched, should not happen"))))
          (setf arg (1+ arg)))))))

;; newline that detects haskell signatures

(defun haskell--simple-indent-newline-same-col ()
  "Make a newline and go to the same column as the current line."
  (interactive "*")
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
  (insert (make-string vim:shift-width ?\s)))

(defun haskell-newline-with-signature-expansion ()
  "Similar to `sp-newline' but autoexpands haskell signatures."
  (interactive "*")
  (let* ((start-pos (point))
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
                                        (let ((ws "[ \t\n\r]"))
                                          (concat "\\(?:\\_<\\(?:let\\|where\\)\\_>" ws "+\\)?"
                                                  "\\(?1:"
                                                  (concat "\\(?2:" haskell-regexen/varid "\\)"
                                                          "\\(?:," ws "*" haskell-regexen/varid "\\)*")
                                                  "\\)"
                                                  ws "*"
                                                  haskell-regexen/function-signature-colons))))
                      (setf found? t
                            func-name (match-string 1))))
                  (when found?
                    (goto-char (match-beginning 1))
                    (let ((function-name-column (current-column))
                          (indented-section-end (line-end-position)))
                      (forward-line 1)
                      (while (< function-name-column (indentation-size))
                        (setf indented-section-end (line-end-position))
                        (forward-line 1))
                      (when (and
                             (= start-pos indented-section-end)
                             (not
                              (save-excursion
                                (forward-line)
                                (skip-syntax-forward "->")
                                (looking-at-p (concat (regexp-quote func-name)
                                                      "\\_>")))))
                        (goto-char start-pos)
                        (insert "\n")
                        (insert (make-string function-name-column ?\s)
                                func-name
                                " ")
                        t)))))))))
    (when (null expanded-function-name?)
      (goto-char start-pos)
      (let* ((syn (syntax-ppss))
             (in-string? (nth 3 syn)))
        (if in-string?
            (let ((string-start-column (save-excursion
                                         (goto-char (nth 8 syn))
                                         (current-column))))
              (insert "\\\n" (make-string string-start-column ?\s) "\\"))
          (haskell--simple-indent-newline-same-col))))))

(defun haskell-abbrev+-fallback-space ()
  (interactive "*")
  (if (and fci-mode
           (= 0 (current-column))
           (= (point) (line-end-position)))
      (progn
        (haskell-space-with-block-indent)
        (fci-redraw-region (line-beginning-position)
                           (line-end-position)
                           nil))
    (haskell-space-with-block-indent)))

(defun haskell-interactive-clear-prompt ()
  "Clear haskell prompt from input."
  (interactive "*")
  (goto-char haskell-interactive-mode-prompt-start)
  (when (not (equal (point)
                    (line-end-position)))
    (delete-region (point) (line-end-position))))

(defun haskell-interactive-clear-buffer-above-prompt ()
  (interactive "*")
  (let ((session (haskell-session)))
    (with-current-buffer (haskell-session-interactive-buffer session)
      (save-excursion
        (forward-line -1)
        (let ((inhibit-read-only t))
          (set-text-properties (point-min) (point) nil))
        (remove-overlays (point-min) (line-end-position))
        (delete-region (point-min) (line-end-position))
        (haskell-session-set session 'next-error-region nil)
        (haskell-session-set session 'next-error-locus nil)))))

(define-circular-jumps
    haskell-interactive-jump-to-next-prompt
    haskell-interactive-jump-to-prev-prompt
  (haskell-interactive-prompt-regex)
  :jump-to-end t)

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
      (haskell-smart-operators-hyphen prefix))))

(defun* install-haskell-smart-operators! (keymap &key bind-colon bind-hyphen)
  (declare (indent 1))
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

(defparameter haskell-compilation-buffer "*haskell-compilation*")

(defun haskell-compilation-use-selected-error-or-jump-to-next (win jump-to-next-err-func)
  "Either return error currently selected in the haskell compilation buffer, if
point is not located on it, or return the next error if current position argees
with the position of the selected error."
  (let ((buf (current-buffer))
        (line (line-number-at-pos)))
    (with-selected-window win
      (with-current-buffer haskell-compilation-buffer
        (let ((selected-err (compilation/get-selected-error)))
          (if selected-err
              (if (and
                   (eq (compilation/find-buffer
                        (compilation-error/filename selected-err)
                        (compilation-error/compilation-root-directory selected-err))
                       buf)
                   (equal (compilation-error/line-number selected-err)
                          line))
                  ;; If we're already on the selected error then jump to next error.
                  (progn
                    (funcall jump-to-next-err-func)
                    (compilation/get-selected-error))
                selected-err)
            (progn
              (funcall jump-to-next-err-func)
              (compilation/get-selected-error))))))))

(defun haskell-compilation-go-navigate-errors (jump-to-next-err-func fallback)
  "Navigate errors in `haskell-compilation-buffer'."
  (if (buffer-live-p (get-buffer haskell-compilation-buffer))
      (let ((win (get-buffer-window haskell-compilation-buffer
                                    t ;; all-frames
                                    )))
        (if (and win
                 (window-live-p win))
            (if-let (err (haskell-compilation-use-selected-error-or-jump-to-next
                          win
                          jump-to-next-err-func))
                (compilation/jump-to-error err nil)
              (funcall fallback))
          (funcall fallback)))
    (funcall fallback)))

(defun haskell-compilation-next-error-other-window ()
  "Select next error in `haskell-compilation-buffer' buffer and jump to
it's position in current window."
  (interactive)
  (haskell-compilation-go-navigate-errors
   #'compilation-jump-to-next-error
   #'flycheck-next-error))

(defun haskell-compilation-prev-error-other-window ()
  "Select previous error in `haskell-compilation-buffer' buffer and jump to
it's position in current window."
  (interactive)
  (haskell-compilation-go-navigate-errors
   #'compilation-jump-to-prev-error
   #'flycheck-previous-error))

(defvar haskell-misc--switch-to-haskell-process-type-history nil)
(defvar haskell-misc--switch-to-haskell-ghci-command-history nil)

(defun haskell-misc-switch-to-haskell (&optional query-for-process-type)
  (interactive "P")
  (let* ((process-type
          (if query-for-process-type
              (string->symbol
               (ivy-completing-read "Process type: "
                                    '("auto"
                                      "cabal-repl"
                                      "stack-ghci"
                                      "ghci"
                                      "ghci-custom-command")
                                    nil ;; predicate
                                    t   ;; require match
                                    (symbol->string haskell-process-type) ;; initial-input
                                    'haskell-misc--switch-to-haskell-process-type-history
                                    ))
            haskell-process-type))
         (edit-command? (eq process-type 'ghci-custom-command))
         (haskell-process-type
          (if edit-command?
              'ghci
            process-type))
         (old-wrapper haskell-process-wrapper-function))
    (let ((haskell-process-wrapper-function
           (lambda (args)
             (let ((transformed-args (funcall old-wrapper args))
                   (enable-recursive-minibuffers t))
               (if edit-command?
                   (split-shell-command-into-arguments
                    (read-shell-command
                     "Ghci command: "
                     (join-lines transformed-args " ")
                     'haskell-misc--switch-to-haskell-ghci-command-history))
                 transformed-args)))))
      (when (and buffer-file-name
                 (member (file-name-extension buffer-file-name) +haskell-extensions+))
        (haskell-process-load-file))
      (haskell-interactive-bring))))

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
  "Sort lines of the subsection at point."
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
  (let ((p (point)))
    (with-expanded-invisible-overlays
        (max (save-excursion (haskell-move-to-topmost-start)
                             (point))
             (point-min))
        (save-excursion
          (haskell-move-to-topmost-end)
          (point))
      ad-do-it)))

(defadvice haskell-indentation-indent-backwards (around
                                                 haskell-indentation-indent-backwards-expand-yafolding
                                                 activate
                                                 compile)
  (let ((p (point)))
    (with-expanded-invisible-overlays
        (max (save-excursion (haskell-move-to-topmost-start)
                             (point))
             (point-min))
        (save-excursion
          (haskell-move-to-topmost-end)
          (point))
      ad-do-it)))

(provide 'haskell-misc)

;; Local Variables:
;; End:

;; haskell-misc.el ends here
