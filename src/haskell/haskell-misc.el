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
(require 'haskell-regexen)
(require 'compilation-setup)

(require 'flycheck)
(require 'flycheck-haskell)
(require 'flycheck-setup)

;;; definitions

;;;###autoload
(defconst +haskell-syntax-modes+ '(haskell-mode haskell-c-mode haskell-c2hs-mode)
  "List of modes that use haskell syntax.")

(defconst +haskell-tmp-path+ (concat +tmp-path+ "/haskell-tmp"))

(make-directory +haskell-tmp-path+ t)

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
          (format "cd \"%%s\" && stack %s --ghc-options=\"-j4 +RTS -A64m -H256m -n2m -RTS\"" cmd)))
       (cd-command "cd \"%s\""))
  (setf haskell-compile-cabal-build-command-presets
        `((vanilla
           ,(funcall stack-command "build"))
          (prof
           ,(funcall stack-command "build --profile --test --no-run-tests"))
          (clean
           ,(funcall stack-command "clean"))
          (stack
           ,(funcall stack-command "build"))
          (test
           ,(funcall stack-command "test"))
          (build-tests
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
          (cabal-vanilla-noopt
           ,(concat
             cd-command
             sep
             (concat "cabal "
                     "configure "
                     "--disable-optimization "
                     "--disable-profiling "
                     (funcall common-conf-opts build-dir))
             sep
             (funcall build-command build-dir)
             sep
             (funcall test-command build-dir)))
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
             (funcall test-command build-dir)))
          ;; hpc command must use local dist build directory, it won't
          ;; work with absolute paths.
          (cabal-hpc
           ,(concat
             cd-command
             sep
             (concat "cabal "
                     "configure "
                     "--enable-library-coverage "
                     "--disable-profiling "
                     "--disable-split-objs "
                     (funcall common-conf-opts nil))
             sep
             (funcall build-command nil)
             sep
             (funcall test-command nil))))))

(defun haskell-doc-mode-setup ()
  ;; (haskell-doc-mode +1)
  (turn-on-haskell-doc-mode)
  (setq-default haskell-doc-show-global-types t)
  (setq-default haskell-doc-show-reserved t)
  (setq-default haskell-doc-show-prelude t)
  (setq-default haskell-doc-show-strategy t)
  (setq-default haskell-doc-show-user-defined t)
  (setf haskell-doc-chop-off-context nil
        haskell-doc-chop-off-fctname nil))


(setf haskell-compile-cabal-build-command
      (or (cadr-safe (assoc 'vanilla haskell-compile-cabal-build-command-presets))
          (error "failed to set up haskell-compile-cabal-build-command"))
      ;; "cd %s && cabal build --ghc-option=-ferror-spans && cabal test --show-details=always"
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
      shm-insert-space-after-comma t
      shm-indent-point-after-adding-where-clause t
      shm-colon-enabled t
      shm-indent-use-chris-done-if-indent-style nil
      inferior-haskell-find-project-root nil

      haskell-interactive-prompt-read-only t
      haskell-interactive-mode-read-only t

      ;; Flycheck
      flycheck-ghc-args '("-O0" "-fno-warn-name-shadowing" "-fno-warn-type-defaults")
      flycheck-ghc-no-user-package-database t)

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

(defconst +haskell-compile-error-or-warning-regexp+
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
                    (-map #'cdr
                          (--filter (memq (car it) '(4bssd watcom sun msft lcc gnu java ibm epc edg-1 edg-2 borland aix absoft))
                                    compilation-error-regexp-alist-alist)))))
   "\\|")
  "Regexp matching both errors and warnings.")

(defconst haskell-module-quantification-regexp
  (let ((conid "\\b[[:upper:]][[:alnum:]'_]*\\b"))
    (concat "\\b\\(?:" conid "\\.\\)+")))

(defun haskell-remove-module-qualification (name)
  "Removes hierarchihal modules qualification (e.g. Data.Map.null -> null,
 Prelude.++ -> ++, etc)"
  (save-match-data
    (if (string-match (concat "^\\("
                              haskell-module-quantification-regexp
                              "\\)")
                      name)
        (replace-match "" t t name 1)
      name)))

(defun inf-haskell-send-input-or-jump-to-error ()
  (interactive)
  (if (looking-at-pure? *compilation-jump-error-regexp*)
      (compile-goto-error)
    (comint-send-input)))


(put 'haskell-program-name 'safe-local-variable (lambda (x) (or (string? x) (list? x))))
(put 'haskell-compile-command 'safe-local-variable #'string?)
(put 'haskell-compile-cabal-build-command 'safe-local-variable #'string?)
(put 'haskell-compile-cabal-build-alt-command 'safe-local-variable #'string?)

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

;;; haddock for modules

(defun inferior-haskell-haddock-module (name)
  "Find and open the Haddock documentation of module NAME.
Only works for module in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it."
  (interactive
   (let ((name (haskell-ident-at-point)))
     (list (read-string (if (> (length name) 0)
                            (format "Find documentation of module (default %s): " name)
                          "Find documentation of module: ")
                        nil nil name))))
  (setq name (inferior-haskell-map-internal-ghc-ident name))
  (let ( ;; Find the module and look it up in the alist
        (alist-record (assoc name (inferior-haskell-module-alist))))

    (if alist-record
        (progn ;; if documentation for such module exists at all
          (let* ((package (nth 1 alist-record))
                 (file-name (concat (subst-char-in-string ?. ?- name) ".html"))
                 (local-path (concat (nth 2 alist-record) "/" file-name))
                 (url (if (or (eq inferior-haskell-use-web-docs 'always)
                              (and (not (file-exists-p local-path))
                                   (eq inferior-haskell-use-web-docs 'fallback)))
                          (concat inferior-haskell-web-docs-base package "/" file-name
                                  ;; no haddock anchor for module names
                                  )
                        (and (file-exists-p local-path)
                             ;; no haddock anchor for module names
                             (concat "file://" local-path)))))
            (if url (browse-url url) (error "Local file doesn't exist"))))
      (error "No documentation for module %s found" name))))

;;; up level navigation

(defun haskell-back-up-indent-level ()
  "Move up to lesser indentation level, skipping empty lines."
  (let* ((get-whitespace-level
          (lambda ()
            (save-excursion
              (back-to-indentation)
              (current-column))))
         (current-level (funcall get-whitespace-level)))
    (while (and (not (bob?))
                (<= current-level
                    (funcall get-whitespace-level)))
      (backward-line 1)
      (while (looking-at-pure? "^$")
        (backward-line 1)))
    (back-to-indentation)))

(defun haskell-move-up ()
  (interactive)
  (or (sp-backward-up-sexp)
      (haskell-back-up-indent-level)))

;;; miscellany

(defalias 'inferior-haskell-haddock-identifier 'inferior-haskell-find-haddock)

;;; Automatized definitions using advices-util and macro-util

;;;; align functions

(make-align-function haskell-align-on-equals
                     "=[^=]"
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-arrows
                     "\\(?:->\\|→\\)\\(?: \\|$\\)")
(make-align-function haskell-align-on-left-arrows
                     "\\(?:<-\\|←\\)\\(?: \\|$\\)")
(make-align-function haskell-align-on-guards
                     "|\\(?:[^|]\\|$\\)"
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-commas
                     ",\\(?:[^,\)]\\|$\\)")
(make-align-function haskell-align-on-comments
                     "--+\\(?: \\|$\\)"
                     :require-one-or-more-spaces t)

(make-align-function haskell-align-on-double-colons
                     "\\(?:::[^:]\\|∷\\)")
(make-align-function haskell-align-on-pragma-close
                     "#-}")


(defun haskell-align-generic ()
  (interactive)
  (haskell-align-on-equals)
  (haskell-align-on-arrows)
  (haskell-align-on-left-arrows)
  (haskell-align-on-comments)
  (haskell-align-on-double-colons)
  (haskell-align-on-pragma-close))

(defun haskell-define-align-bindings (keymap)
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
  (interactive)
  (save-match-data
    (cond
      ((save-excursion
         (beginning-of-line)
         (looking-at-pure? haskell-abbrev+/language-pragma-prefix))
       (save-current-line-column
        (haskell-align-language-pragmas (point))))
      ((and (eq (get-char-property (point) 'face) 'haskell-pragma-face)
            (save-excursion
              (re-search-backward haskell-regexen/pragma-start nil t)
              (looking-at-pure? haskell-abbrev+/language-pragma-prefix)))
       (save-current-line-column
        (haskell-align-language-pragmas (point))))
      ((save-excursion
         (beginning-of-line)
         (looking-at-pure? "import "))
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
      (haskell-align-on-pragma-close/impl pragma-block-start (point)))))

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
                (regexp "'*[A-Z]"))
            (group
             (* (regexp "['a-zA-Z_0-9#]")))))))
  "Regexp to recognize haskell symbols as generic entities for search
(with e.g. \"*\" in vim).")

;;;###autoload
(put 'haskell-symbol 'forward-op #'forward-haskell-symbol)

(defun forward-haskell-symbol (arg)
  "Like `forward-symbol' but for generic Haskell symbols (either operators,
uppercase or lowercase names)."
  (interactive "p")
  (let (
        (name-chars "a-zA-Z0-9_#'")
        ;; NB constructs like "''Foobar" we'd like to mach "Foobar"
        ;; via `bounds-of-thing-at-point', not the "''Foobar".
        (beginning-quotes "'")
        (operator-chars "\\-!#$%&*+./<=>?@\\^|~:\\\\"))
    (if (natnump arg)
        (re-search-forward haskell-symbol-re nil t arg)
      (while (< arg 0)
        (when (re-search-backward haskell-symbol-re nil t)
          (cond ((not (null? (match-beginning 1)))
                 (skip-chars-backward operator-chars)
                 ;; we may have matched # thas ends a name
                 (skip-chars-backward name-chars)
                 (skip-chars-forward beginning-quotes))
                ((not (null? (match-beginning 2)))
                 ;; (goto-char (match-beginning 2))
                 (when (not (null? (match-beginning 3)))
                   (skip-chars-backward name-chars)
                   (skip-chars-forward beginning-quotes)))
                (t
                 (error "No group of haskell-symbol-re matched, should not happen"))))
        (setf arg (1+ arg))))))

;; newline that detects haskell signatures

(defun shm-node-top-parent (node-pair)
  "Get latest parent of given NODE-PAIR."
  (let ((parent (shm-node-parent node-pair)))
    (while parent
      (setf node-pair parent
            parent (shm-node-parent node-pair)))
    node-pair))

(defun shm-search-node-upwards (predicate node-pair)
  "Searh for node matching PREDICATE starting from NODE-PAIR."
  (let ((tmp node-pair))
    (while (and tmp
                (not (funcall predicate tmp)))
      (setf tmp (shm-node-parent tmp)))
    tmp))

;; (defun shm-node-parent-chain (node-pair)
;;   "Get latest parent of given NODE-PAIR."
;;   (let* ((parent (shm-node-parent node-pair))
;;          (chain nil))
;;     (while parent
;;       (push (list parent
;;                   (buffer-substring-no-properties (shm-node-start (cdr parent))
;;                                                   (shm-node-end (cdr parent))))
;;             chain)
;;       (setf node-pair parent
;;             parent (shm-node-parent node-pair)))
;;     chain))

(defun haskell-enclosing-TypeSig-node ()
  (cdr-safe
   (shm-search-node-upwards
    (comp (partial #'eq? 'TypeSig) #'shm-node-cons #'cdr)
    (shm-current-node-pair))))

(defun haskell-TypeSig-function-name-node (typesig-node)
  "Extract function name from TypeSig node if point is currently in one or
return nil otherwise."
  (save-excursion
    (goto-char (shm-node-start typesig-node))
    (if-let (curr-node (cdr-safe (shm-current-node-pair)))
        (cond ((eq? 'Ident (shm-node-cons curr-node))
               curr-node)
              ((eq? 'Symbol (shm-node-cons curr-node))
               curr-node)
              (t
               nil
               ;; (error "node constructor is not Ident: %s" (shm-node-cons curr-node))
               ))
      ;; (error "no current node found starting at %s"
      ;;        (buffer-substring-no-properties (point)
      ;;                                        (line-end-position)))
      )))

(defun haskell-newline ()
  "Similar to `sp-newline' but autoexpands haskell signatures."
  (interactive)
  (let ((indent
         (lambda ()
           (if (shm-current-node-pair)
               (shm/newline-indent)
             (shm/simple-indent-newline-same-col)))))
    (when (memq major-mode +haskell-syntax-modes+)
      (if-let ((enclosing-sig-node (haskell-enclosing-TypeSig-node))
               (func-name-node (haskell-TypeSig-function-name-node enclosing-sig-node)))
          (let ((func-name (buffer-substring-no-properties
                            (shm-node-start func-name-node)
                            (shm-node-end func-name-node)))
                (indentation (save-excursion
                               (goto-char (shm-node-start func-name-node))
                               (current-column)))
                (p (point))
                (sig-end (shm-node-end enclosing-sig-node)))
            (funcall indent)
            ;; Maybe consider using this function instead?
            ;; (shm/simple-indent-newline-same-col)
            (when (and
                   (= p sig-end)
                   (not
                    (save-excursion
                      (forward-line)
                      (skip-syntax-forward "->")
                      (looking-at-pure? (concat (regexp-quote func-name)
                                                "\\_>")))))
              (delete-region (line-beginning-position) (point))
              (insert (make-string indentation ?\s)
                      func-name
                      " ")))
        ;; indent in either case, the key is to indent
        ;; *after* parsing signature on current line
        (funcall indent)))))

(defun haskell-abbrev+-fallback-space ()
  (interactive)
  (if structured-haskell-mode
      (shm/space)
    (insert " ")))

;; (search-def-autoexpand-advices (show-subtree) (haskell-mode))

(defun haskell-interactive-clear-prompt ()
  "Clear haskell prompt from input."
  (interactive)
  (goto-char haskell-interactive-mode-prompt-start)
  (when (not (equal (point)
                    (line-end-position)))
    (delete-region (point) (line-end-position))))

(defun haskell-interactive-clear-buffer-above-prompt ()
  (interactive)
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

(defun haskell/smart-$ ()
  "Swap parens with a dollar."
  (interactive)
  (let ((start-pos nil))
    (when (save-excursion
            (skip-syntax-forward " ")
            (when (and (char-after)
                       (char= (char-after) ?\())
              (setf start-pos (point))
              t))
      (goto-char start-pos)
      ;; delete parenthesized sexp
      (save-excursion
        (forward-sexp)
        (delete-char -1))
      (delete-char 1))
    (shm-insert-char-surrounding-with-spaces ?\$)))

(defun haskell--ghci-shm/hyphen (&optional prefix)
  "Version of `shm/hyphen' for ghci."
  (interactive "p")
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
      (shm/hyphen prefix))))

(defun* install-haskell-smart-operators (keymap &key bind-colon bind-hyphen use-shm)
  (declare (indent 1))
  (when bind-colon
    (define-key keymap
      (kbd ":")
      (if use-shm #'shm/: #'haskell-smart-operators-self-insert)))
  (when bind-hyphen
    (def-keys-for-map keymap
      ("-" haskell-smart-operators-hyphen)))
  (define-key keymap
    (kbd "@")
    (if use-shm #'shm/@ #'haskell-smart-operators-self-insert))
  (define-key keymap
    (kbd "!")
    (if use-shm
        #'shm/!
      #'haskell-smart-operators-exclamation-mark))
  (dolist (key (list (kbd "=") (kbd "+") (kbd "*") (kbd "<") (kbd ">")
                     (kbd "%") (kbd "^") (kbd "&") (kbd "/")
                     (kbd "?") (kbd "|") (kbd "~")))
    (define-key keymap
      key
      #'haskell-smart-operators-self-insert))
  (def-keys-for-map keymap
    ("$"   haskell-smart-operators-$)
    ("#"   haskell-smart-operators-hash)
    (","   haskell-smart-operators-comma)
    ("C-=" input-unicode)
    ;; ("\\"  self-insert-command)
    ))

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
     (and (string-match-pure? "[0-9.]+" x)
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
               (ido-completing-read "Process type: "
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
                 (member (file-name-extension buffer-file-name) *haskell-extensions*))
        (haskell-process-load-file))
      (haskell-interactive-bring))))

(defun haskell-shm-tab-or-indent-relative-forward ()
  (interactive)
  (if structured-haskell-mode
      (shm/tab)
    (indent-relative-forward)))

(defun haskell-shm-backtab-or-indent-relative-backward ()
  (interactive)
  (if structured-haskell-mode
      (shm/backtab)
    (indent-relative-backward)))


(defun haskell-misc--cabal-indented-subsection ()
  "Similar to `haskell-cabal-subsection' but sets `:data-start-column' to the
value section should have if it is to be properly indented."
  (save-excursion
    (haskell-cabal-beginning-of-subsection)
    (when (looking-at "\\(?:\\([ \t]*\\)\\(\\w*\\):\\)[ \t]*")
      (list :name (match-string-no-properties 2)
            :beginning (match-end 0)
            :end (save-match-data (haskell-cabal-subsection-end))
            :data-start-column (+ 2 (current-column))))))

(defun haskell-misc-cabal-align-and-sort-subsection ()
  "Sort lines of the subsection at point."
  (interactive)
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

(provide 'haskell-misc)

;; Local Variables:
;; End:

;; haskell-misc.el ends here
