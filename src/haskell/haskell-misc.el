;; haskell-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 20 September 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(require 'macro-util)
(require 'advices-util)
(require 'common)
(require 'peg)

(require 'abbrev+)
(require 'haskell-compile)
(require 'compilation-setup)

;;; definitions

(defconst +haskell-syntax-modes+ '(haskell-mode haskell-c-mode c2hs-mode)
  "List of modes that use haskell syntax.")

(defconst +haskell-tmp-path+ (concat +tmp-path+ "/haskell-tmp"))

(make-directory +haskell-tmp-path+ t)


(setf shm-insert-space-after-comma t
      shm-indent-point-after-adding-where-clause t
      shm-colon-enabled t
      shm-indent-use-chris-done-if-indent-style nil
      inferior-haskell-find-project-root nil
      ghc-core-program-args '("-O2"
                              "-dsuppress-uniques"
                              "-dsuppress-idinfo"
                              "-dsuppress-module-prefixes"
                              ;; "-dsuppress-type-signatures"
                              "-dsuppress-type-applications"
                              "-dsuppress-coercions"))

(let* ((build-dir
        (cond
          ((platform-os-type? 'linux)
           "/tmp/dist")
          ((platform-os-type? 'windows)
           nil)
          (t
           nil)))
       (mk-build-dir-arg
        (lambda (custom-build-dir)
          (if custom-build-dir
            (concat "--builddir " custom-build-dir " ")
            "")))
       (common-conf-opts
        (lambda (custom-build-dir)
          (concat (funcall mk-build-dir-arg custom-build-dir)
                  "--enable-tests")))
       (build-command
        (lambda (custom-build-dir)
          (concat
           "cabal build " (funcall mk-build-dir-arg custom-build-dir) "--ghc-options=\"-j4 -ferror-spans\"")))
       (test-command
        (lambda (custom-build-dir)
          (concat
           "cabal test " (funcall mk-build-dir-arg custom-build-dir) "--show-details=always")))
       (sep " && \\\n"))
  (setf haskell-compile-cabal-build-command-presets
        `((vanilla
           ,(concat
             "cd %s"
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
          (test
           ,(concat
             "cd %s"
             sep
             (funcall test-command build-dir)))
          (clean
           ,(concat
             "cd %s"
             sep
             "cabal clean --builddir " build-dir))
          (prof
           ,(concat
             "cd %s"
             sep
             (concat "cabal "
                     "configure "
                     "--enable-library-profiling "
                     "--enable-profiling "
                     (funcall common-conf-opts build-dir))
             sep
             (funcall build-command build-dir)
             sep
             (funcall test-command build-dir)))
          ;; hpc command must use local dist build directory, it won't
          ;; work with absolute paths.
          (hpc
           ,(concat
             "cd %s"
             sep
             (concat "cabal "
                     "configure "
                     "--enable-library-coverage "
                     "--disable-library-profiling "
                     "--disable-profiling "
                     "--disable-split-objs "
                     (funcall common-conf-opts nil))
             sep
             (funcall build-command nil)
             sep
             (funcall test-command nil))))))

(setf haskell-compile-command
      (or (getenv "HASKELL_COMPILE_COMMAND")
          (concat "ghc -W -Wall -fwarn-monomorphism-restriction "
                  "-ferror-spans -fforce-recomp "
                  (when (platform-os-type? 'linux)
                    ;; needed for ghc 7.4 and gold linker
                    "-rtsopts -pgml /usr/bin/gcc ")
                  (format "-hidir %s " +haskell-tmp-path+)
                  (format "-odir %s " +haskell-tmp-path+)
                  (format "-tmpdir %s " +haskell-tmp-path+)
                  ;; llvm
                  ;; "-fllvm -optlc-O3 -optlo-O3 "
                  "-c \"%s\""))
      haskell-compile-cabal-build-command
      (or (cadr-safe (assoc 'vanilla haskell-compile-cabal-build-command-presets))
          (error "failed to set up haskell-compile-cabal-build-command"))
      ;; "cd %s && cabal build --ghc-option=-ferror-spans && cabal test --show-details=always"
      ;; 'cabal-repl is good as well
      haskell-process-type 'ghci
      haskell-process-path-ghci
      (if (platform-os-type? 'windows)
        "ghc"
        "ghci")
      haskell-process-args-ghci
      (let ((extensions '("-XLambdaCase" "-XTemplateHaskell" "-XOverloadedStrings"))
            ;; (opts "-fobject-code")
            (opts '("-fbyte-code" "-odir" "/tmp/ghc" "-hidir" "/tmp/ghc"))
            (rts-opts '("+RTS" "-M1G")))
        (append (if (platform-os-type? 'windows)
                  "--interactive"
                  nil)
                extensions
                opts
                rts-opts))

      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      ;; haskell-process-suggest-hoogle-imports t ;; may be cool
      haskell-process-show-debug-tips nil
      haskell-interactive-popup-errors nil
      haskell-interactive-mode-eval-mode #'haskell-mode
      ;; Propertize so that later haskell-interactive-mode.el will catch
      ;; these properties up when inserting prompt.
      haskell-interactive-prompt "λ> "

      ;; haskell-program-name
      ;; (let ((extensions "-XLambdaCase -XTemplateHaskell -XOverloadedStrings")
      ;;       ;; (opts "-fobject-code")
      ;;       (opts "-fbyte-code -odir /tmp/ghc -hidir /tmp/ghc")
      ;;       (rts-opts "+RTS -M1G"))
      ;;   (cond ((platform-os-type? 'windows)
      ;;          (join-lines (list "ghc" "--interactive" extensions opts rts-opts)
      ;;                      " "))
      ;;         ((executable-find "ghci")
      ;;          (join-lines (list "ghci" extensions opts rts-opts)
      ;;                      " "))
      ;;         ((executable-find "ghc")
      ;;          (join-lines (list "ghc" "--interactive" extensions opts rts-opts)
      ;;                      " "))
      ;;         (t
      ;;          (message "GHC not found")
      ;;          nil)))
      )

(redefun haskell-interactive-prompt-regex ()
  "λ?> +")


(defconst +haskell-compile-error-or-warning-regexp+
  (join-lines (map (comp (partial #'concat "\\(?:")
                         (partial-first #'concat "\\)")
                         #'car)
                   haskell-compilation-error-regexp-alist)
              "\\|")
  "Regexp matching both errors and warnings.")


;; for outline
(defconst haskell-type-signature-regexp (rx (not (any ?: ?\n))
                                            "::"
                                            (group (or
                                                    (not (any ?: ?\n))
                                                    eol))))
(defconst haskell-toplevel-signature-regexp (rx bol
                                                (not (any ?\s))
                                                (* nonl)
                                                (or (not (any ?: ?\n))
                                                    (seq (* whitespace)
                                                         "\n"
                                                         (+ whitespace)))
                                                "::"
                                                (group (or
                                                        (not (any ?: ?\n))
                                                        eol))))
(defconst haskell-toplevel-data-declaration-regexp "^[ \t]*data[ \t]+\\(?:.\\|\n\\)+?=")
(defconst haskell-toplevel-class-declaration-regexp "^[ \t]*class[ \t]+\\(?:.\\|\n\\)+?where")
(defconst haskell-toplevel-instance-declaration-regexp "^[ \t]*instance[ \t]+\\(?:.\\|\n\\)+?where")
(defconst haskell-main-function-regexp "^main[ \t]*=[ \t\n\r]*\\(?:do\\)?")
(defconst haskell-commented-line-regexp "^[ \t]*-- ")


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


(defun haskell-yas-completing-prompt (prompt choices &optional display-fn)
  "Call `yas-completing-prompt' with ignoring case during completion."
  (let ((completion-ignore-case t))
    (yas-completing-prompt prompt
                           choices
                           display-fn)))

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

(defun haskell-help-for-symbol-at-point ()
  "Show help for entity at point, if any.

Currently only language extensions are supported."
  (interactive)
  (let ((name (haskell-ident-at-point)))
    (aif (assoc name haskell-language-extensions)
      (destructuring-bind (ext-name doc inverse) it
        (message "%s (%s)\n%s"
                 name
                 (if (eq? inverse "") "no inverse" inverse)
                 doc))
      (error "No documentation for %s" name))))

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

;;; define forward-haskell-symbol

(defparameter forward-haskell-symbol-re
  (rx (or (group (+ ;; (regexp "[-!#$%&*+./<=>?@^|~:\\]")
                  (any ?\- ?\! ?\# ?\$ ?\% ?\& ?\* ?\+ ?\. ?\/ ?\< ?\= ?\> ?\? ?\@ ?^ ?\| ?\~ ?\: ?\\ )))
          (group
           (seq bow
                ;; allow _ as a first char to fit GHC
                (or (regexp "[_a-z]")
                    ;; allow ' preceding conids because of DataKinds/PolyKinds
                    (regexp "'?[A-Z]"))
                (group
                 (* (regexp "['a-zA-Z_0-9#]")))))))
  "Regexp to recognize haskell symbols as generic entities for search
(with e.g. \"*\" in vim).")

(put 'haskell-symbol 'forward-op #'forward-haskell-symbol)

(defun forward-haskell-symbol (arg)
  "Like `forward-symbol' but for generic Haskell symbols (either operators,
uppercase or lowercase names)."
  (interactive "p")
  (let ((name-chars "a-zA-Z0-9'_#")
        (operator-chars "\\-!#$%&*+./<=>?@\\^|~:\\\\"))
    (if (natnump arg)
      (re-search-forward forward-haskell-symbol-re nil t arg)
      (while (< arg 0)
        (when (re-search-backward forward-haskell-symbol-re nil t)
          (cond ((not (null? (match-beginning 1)))
                 (skip-chars-backward operator-chars)
                 ;; we may have matched # thas ends a name
                 (skip-chars-backward name-chars))
                ((not (null? (match-beginning 2)))
                 ;; (goto-char (match-beginning 2))
                 (when (not (null? (match-beginning 3)))
                   (skip-chars-backward name-chars)))
                (t
                 (error "No group of forward-haskell-symbol-re matched, should not happen"))))
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
      (if-let* (enclosing-sig-node (haskell-enclosing-TypeSig-node)
                func-name-node (haskell-TypeSig-function-name-node enclosing-sig-node))
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

(defun haskell-clear-buffer-and-load-file ()
  "Switch to ghci, clear it and load current file."
  (interactive)
  (when (buffer-live-p inferior-haskell-buffer)
    (with-current-buffer inferior-haskell-buffer
      (goto-char (point-max))
      (comint-clear-buffer-above-prompt)))
  (inferior-haskell-load-file))

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
        (goto-char (point-max))
        (forward-line -1)
        (let ((inhibit-read-only t))
          (set-text-properties (point-min) (point-max) nil))
        (remove-overlays (point-min) (line-end-position))
        (delete-region (point-min) (line-end-position))
        (haskell-session-set session 'next-error-region nil)
        (haskell-session-set session 'next-error-locus nil)))))

(define-circular-jumps
    haskell-interactive-jump-to-next-prompt
    haskell-interactive-jump-to-prev-prompt
  (haskell-interactive-prompt-regex))

(defun haskell-bind-shm-bindings ()
  (def-keys-for-map vim:insert-mode-local-keymap
    ("-"       shm/hyphen)
    ("#"       shm/hash)
    (","       shm/comma)
    (":"       shm/:)
    ("="       shm/=)

    ("C-="     input-unicode)

    ("+"       shm/+)
    ("*"       shm/*)
    ("="       shm/=)
    ("<"       shm/<)
    (">"       shm/>)
    ("!"       shm/!)
    ("@"       shm/@)
    ("$"       shm/$)
    ("%"       shm/%)
    ("^"       shm/^)
    ("&"       shm/&)

    ("/"       shm//)
    ("?"       shm/?)
    ("|"       shm/|)
    ("\\"      shm/\\)
    ("~"       shm/~)))

(provide 'haskell-misc)

;; Local Variables:
;; End:

;; haskell-misc.el ends here
