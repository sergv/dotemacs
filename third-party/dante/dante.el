;;; dante.el --- Development mode for Haskell -*- lexical-binding: t -*-

;; Copyright (c) 2016 Jean-Philippe Bernardy
;; Copyright (c) 2016 Chris Done
;; Copyright (c) 2015 Athur Fayzrakhmanov
;; Copyright (c) 2013 Herbert Valerio Riedel
;; Copyright (c) 2007 Stefan Monnier

;; Author: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; Maintainer: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; URL: https://github.com/jyp/dante
;; Created: October 2016
;; Keywords: haskell, tools
;; Package-Requires: ((dash "2.12.0") (emacs "27.1") (f "0.19.0") (flycheck "0.30") (company "0.9") (flymake "1.0") (s "1.11.0") (lcr "1.5"))
;; Version: 0-pre

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; DANTE: Do Aim Not To Expand.

;; This is a mode to provide Emacs interface for GHCi.  The mode
;; depends on GHCi only, keeping the logic simple.  Additionally it
;; aims to be minimal as far as possible.

;;; Code:

(eval-when-compile
  (defvar dante-repl--command-line-to-use)
  (require 'cl-lib))

(require 'dash)
(require 'f)
(require 'flycheck)
(require 'flymake)
(require 'haskell-mode)
(require 's)
(require 'xref)
(require 'lcr)
(eval-when-compile (require 'company))

(require 'common)
(require 'common-whitespace)
(require 'haskell-regexen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defgroup dante nil
  "Interactive development mode for Haskell."
  :group 'haskell)


(defcustom dante-debug nil
  "Show debug output."
  :group 'dante :safe t
  :type '(set (const inputs) (const outputs) (const responses) (const command-line)))

(defcustom dante-repl-command-line nil
  "Command line to start GHCi, as a list: the executable and its arguments.
When nil, dante will guess the value depending on
the variable `dante-project-root'  This should usually be customized
as a file or directory variable.  Each element of the list is a
sexp which is evaluated to a string before being passed to the
shell."
  :group 'dante
  :type '(repeat sexp))

(defcustom dante-project-root nil
  "The project root, as a string or nil.
When nil, dante will guess the value by looking for a cabal file.
Customize as a file or directory variable."
  :group 'dante :safe #'stringp
  :type '(choice (const nil) string))

(defcustom dante-target nil
  "The target to demand from cabal repl, as a string or nil.
Customize as a file or directory variable.  Different targets
will be in loaded in different GHCi sessions."
  :group 'dante :safe #'stringp
  :type '(choice (const nil) string))

(defun dante-nix-available? (_buf)
  "Non-nil iff ‘nix’ executable is avaliable."
  (and (cached-executable-find "nix")
       t))

(defun dante-nix-shell-available? (_buf)
  "Non-nil iff ‘nix-shell’ executable is avaliable."
  (and (cached-executable-find "nix-shell")
       t))

(defun dante-stack-available? (_buf)
  "Non-nil iff ‘stack’ executable is avaliable."
  (and (cached-executable-find "stack")
       t))

(defun dante-nix-cabal-script-buf? (buf)
  "Non-nil if BUF is a cabal-style script which has no extra configuration."
  (and (dante-nix-available? buf)
       (dante-cabal-script-buf? buf)))

(defun dante-vanilla-cabal-script-buf? (buf)
  "Non-nil if BUF is a cabal-style script which has no extra configuration."
  (and (cached-executable-find "cabal")
       (dante-cabal-script-buf? buf)))

;;;###autoload
(defun dante-cabal-script-buf? (buf)
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

(defun dante-directory-regular-files (dir re)
  (--map (car it)
         (--filter (eq nil (cadr it))
                   (directory-files-and-attributes dir nil re t))))

(defun dante-cabal-new (d)
  "non-nil iff D contains a cabal project file or a cabal file."
  (and (dante-directory-regular-files d
                                      (rx (or "cabal.project"
                                              "cabal.project.local"
                                              ".cabal")
                                          eos))
       t))

(defun dante-cabal-vanilla (d)
  "non-nil iff D contains a cabal project file or a cabal file."
  (and (dante-directory-regular-files d (rx ".cabal" eos))
       t))

(defun dante-flake-nix (d)
  "non-nil iff D contains a nix flake file and a cabal file."
  (rx-let ((nix "flake.nix"))
    (let ((files (dante-directory-regular-files d (rx nix))))
      (and files
           t))))

(defun dante-cabal-flake-nix (d)
  "non-nil iff D contains a nix flake file and a cabal file."
  (rx-let ((nix "flake.nix")
           (cabal (or "cabal.project" "cabal.project.local" ".cabal")))
    (let ((files (dante-directory-regular-files d (rx (or nix cabal)))))
      ;; Both files must be present.
      (and (--any? (string-match-p (rx nix eos) it) files)
           (--any? (string-match-p (rx cabal eos) it) files)
           t))))

(defun dante-cabal-new-nix (d)
  "Non-nil iff directory D hosts a nix file and a cabal file."
  (rx-let ((nix (or "shell.nix" "default.nix"))
           (cabal (or "cabal.project" "cabal.project.local" ".cabal")))
    (let ((files (dante-directory-regular-files d (rx (or nix cabal)))))
      ;; Both files must be present.
      (and (--any? (string-match-p (rx nix eos) it) files)
           (--any? (string-match-p (rx cabal eos) it) files)
           t))))

(defun dante-cabal-nix (d)
  "Non-nil iff directory D hosts a nix file and a cabal file."
  (rx-let ((nix (or "shell.nix" "default.nix"))
           (cabal (or ".cabal")))
    (let ((files (dante-directory-regular-files d (rx (or nix cabal) eos))))
      ;; Both files must be present.
      (and (--any? (string-match-p (rx nix eos) it) files)
           (--any? (string-match-p (rx cabal eos) it) files)
           t))))

(defsubst dante--mk-repl-cmdline (a b)
  (cons 'dante-repl-cmdline (cons a b)))

(defsubst dante--repl-cmdline-p (x)
  (and (consp x)
       (eq 'dante-repl-cmdline (car x))))

(defsubst dante--repl-cmdline-loading-all-modules (x)
  (cadr x))

(defsubst dante--repl-cmdline-loading-no-modules (x)
  (cddr x))

(defconst dante--methods-tag 'dante-methods)

(defsubst dante--mk-methods (defs)
  (let ((tbl (make-hash-table :test #'eq)))
    (dolist (def defs)
      (puthash (dante-method-name def)
               def
               tbl))
    (list dante--methods-tag
          tbl
          (-map #'dante-method-name defs))))

(defsubst dante--methods-p (x)
  (eq (car x) dante--methods-tag))

(defsubst dante--methods-defs (x)
  (cl-assert (dante--methods-p x))
  (cadr x))

(defsubst dante--methods-lookup (key methods)
  (cl-assert (dante--methods-p methods))
  (gethash key (dante--methods-defs methods)))

(defsubst dante--methods-names (x)
  (cl-assert (dante--methods-p x))
  (caddr x))

(cl-defstruct dante-method
  name
  is-enabled-pred
  find-root-pred     ;; Can be nil in which case default directory will be used.
  check-command-line ;; List of strings.
  repl-command-line  ;; Whatever ‘dante--mk-repl-cmdline’ returns.
  repl-buf-name-func ;; Function of 0 arguments that returns buffer name for dante repl
  )

(defun dante--make-methods (tmp)
  (let* ((ghci-options
          '("-fbyte-code"
            "-Wall"
            "-Wcompat"
            "-Wname-shadowing"
            "-Wincomplete-uni-patterns"
            "-Wincomplete-record-updates"
            "-Wno-missing-home-modules"
            "-Wno-type-defaults"
            "-fdiagnostics-color=always"
            "-dsuppress-module-prefixes"
            "-fshow-loaded-modules"
            "-fprint-potential-instances"
            "-fdefer-typed-holes"))
         (build (when tmp
                  `("--builddir"
                    (concat ,tmp
                            "/dante"
                            (awhen (eproj-sha1-of-project-root-for-buf (current-buffer))
                              (concat "-" it))))))
         (repl (when tmp
                 `("--builddir"
                   (concat ,tmp
                           "/dante-repl"
                           (awhen (eproj-sha1-of-project-root-for-buf (current-buffer))
                             (concat "-" it))))))
         (repl-options (--mapcat (list "--repl-option" it) ghci-options))
         (mk-dante-method
          (cl-function
           (lambda
             (&key name is-enabled-pred find-root-pred repl-buf-name-func template)
             (make-dante-method
              :name name
              :is-enabled-pred is-enabled-pred
              :find-root-pred find-root-pred
              :check-command-line
              (lambda (flake-root)
                (funcall template :flake-root flake-root :flags build))
              :repl-command-line
              (lambda (flake-root)
                (dante--mk-repl-cmdline
                 (funcall template :flake-root flake-root :flags (append repl repl-options))
                 (funcall template :flake-root flake-root :flags (append repl (cons "--repl-no-load" repl-options)))))
              :repl-buf-name-func
              repl-buf-name-func)))))
    (dante--mk-methods
     (list
      (funcall mk-dante-method
               :name 'nix-flakes-build-script
               :is-enabled-pred #'dante-nix-cabal-script-buf?
               :find-root-pred #'dante-flake-nix
               :repl-buf-name-func #'dante-buffer-name--default
               :template
               (cl-function
                (lambda (&key flake-root flags)
                  (nix-call-via-flakes `("cabal" "repl" buffer-file-name ,@flags) flake-root))))

      (funcall mk-dante-method
               :name 'nix-flakes-build
               :is-enabled-pred #'dante-nix-available?
               :find-root-pred #'dante-cabal-flake-nix
               :repl-buf-name-func #'dante-buffer-name--default
               :template
               (cl-function
                (lambda (&key flake-root flags)
                  (nix-call-via-flakes `("cabal" "repl" dante-target ,@flags) flake-root))))

      (funcall mk-dante-method
               :name 'build-script
               :is-enabled-pred #'dante-vanilla-cabal-script-buf?
               :find-root-pred nil
               :repl-buf-name-func #'dante-buffer-name--default
               :template
               (cl-function
                (lambda (&key flake-root flags)
                  (declare (ignore flake-root))
                  `("cabal" "repl" buffer-file-name ,@flags))))

      (funcall mk-dante-method
               :name 'build
               :is-enabled-pred nil
               :find-root-pred #'dante-cabal-new
               :repl-buf-name-func #'dante-buffer-name--default
               :template
               (cl-function
                (lambda (&key flake-root flags)
                  (declare (ignore flake-root))
                  `("cabal" "repl" dante-target ,@flags))))

      (funcall mk-dante-method
               :name 'bare-ghci
               :is-enabled-pred nil
               :find-root-pred (lambda (_) t)
               :repl-buf-name-func #'dante-buffer-name--default
               :template
               (cl-function
                (lambda (&key flake-root flags)
                  (declare (ignore flake-root flags))
                  '("ghci"))))))))

(defvar dante--default-methods
  (dante--make-methods (fold-platform-os-type "/tmp/dist/dante" "dist/dante")))

(defvar dante-methods-defs dante--default-methods)

(defcustom dante-methods (dante--methods-names dante--default-methods)
  "Keys in `dante-methods-alist' to try, in order.
Consider setting this variable as a directory variable."
   :group 'dante :safe t :type '(repeat symbol))

(put 'dante-methods 'safe-local-variable #'listp)

(defun dante-initialize-method ()
  "Initialize the method used to run GHCi.
Set `dante-project-root', `dante-repl-command-line' and
`dante-target'.  Do so according to `dante-methods' and previous
values of the above variables."
  (haskell-misc--configure-dante-if-needed!)
  (or (-first (lambda (method)
                (let ((pred (dante-method-is-enabled-pred method)))
                  (when (or (null pred)
                            (funcall pred (current-buffer)))
                    (let ((proj-root (awhen (eproj-get-project-for-buf-lax (current-buffer))
                                       (f-full (eproj-project/root it)))))
                      (when-let ((root (if-let ((pred (dante-method-find-root-pred method)))
                                           (locate-dominating-file default-directory
                                                                   (lambda (dir)
                                                                     (and (if proj-root
                                                                              ;; If there’s a project then don’t ascend past it.
                                                                              (string-prefix-p proj-root
                                                                                               (f-full dir))
                                                                            t)
                                                                          (funcall pred dir))))
                                         default-directory)))
                        (setf root (expand-file-name root))
                        (let ((flake-root
                               (when (dante-nix-available? (current-buffer))
                                 (let ((eproj (eproj-get-project-for-buf-lax (current-buffer))))
                                   (cond
                                     ((file-exists-p (concat root "/flake.nix"))
                                      root)
                                     (eproj
                                      (let ((eproj-root (eproj-project/root eproj)))
                                        (when (file-exists-p (concat eproj-root "/flake.nix"))
                                          eproj-root)))
                                     (t
                                      nil))))))

                          (setq-local
                           dante-project-root (or dante-project-root root)
                           dante-repl-command-line (or dante-repl-command-line
                                                       (funcall (dante-method-check-command-line method) flake-root))
                           dante-repl--command-line-to-use (or (when (boundp 'dante-repl--command-line-to-use)
                                                                 dante-repl--command-line-to-use)
                                                               (funcall (dante-method-repl-command-line method) flake-root)
                                                               ;; Fall back to command used by dante for checking else
                                                               (dante--mk-repl-cmdline dante-repl-command-line dante-repl-command-line))
                           dante--selected-method method)))))))
              (-non-nil (--map (dante--methods-lookup it dante-methods-defs)
                               dante-methods)))
      (error "No GHCi loading method applies.  Customize `dante-methods' or (`dante-repl-command-line' and `dante-project-root')")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session-local variables. These are set *IN THE GHCi INTERACTION BUFFER*

(defvar-local dante-ghci-path nil "Path where GHCi runs.
Variable `dante-project-root' can be different because of cabal behaviour.")
(defvar-local dante-flymake-token 1000)
(defvar-local dante-command-line nil "Command line used to start GHCi.")
(defvar-local dante-load-message nil "Load messages.")
(defvar-local dante-loaded-file "<DANTE:NO-FILE-LOADED>")
(defvar-local dante-queue nil "List of ready GHCi queries.")
(defvar-local dante-state nil
  "The current state.
- nil: initial state
- deleting: The process of the buffer is being deleted.
- dead: GHCi died on its own.  Do not try restarting
automatically.  The user will have to manually run `dante-restart'
to destroy the buffer and create a fresh one without this variable enabled.
- other value: informative value for the user about what GHCi is doing.")

(defmacro dante-get-var (var)
  "Return the value of SYMBOL in the GHCi process buffer."
  `(when-let ((bp (dante-buffer-p))) (buffer-local-value ',var bp)))

(add-hook
 'lcr-context-switch-hook
 (defun dante-schedule-next ()
   "If no green thread is running, run the next queued one, if any."
   ;; when whatever green thread was running is over, we're back in
   ;; the original source buffer. It's time to check if anything
   ;; queued should be run.
   (if-let ((buffer (dante-buffer-p)))
       (with-current-buffer buffer
         (unless lcr-process-callback
           ;; Note that dante green threads are not interleaved,
           ;; because they only yield by placing a callback.
           (let ((req (pop dante-queue)))
             (when req (funcall req buffer))))))
   ;; we're about to yield back to emacs main loop. Inform the user of status.
   (force-mode-line-update t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defvar dante-mode-map (make-sparse-keymap) "Dante minor mode's map.")

(defun dante-status ()
  "Return dante's status for the current source buffer."
  (let ((buf (dante-buffer-p))
        (fname (buffer-file-name (current-buffer))))
    (if (not buf) "stopped"
      (with-current-buffer buf
        (concat
         (if lcr-process-callback "busy " "")
         (pcase dante-state
           (`(ghc-err (compiling ,mod)) (format "error(%s)" mod))
           (`(loaded ,_loaded-mods) (if (s-equals? dante-loaded-file fname) "loaded" (format "loaded(%s)" (file-name-base dante-loaded-file))))
           ;; (`(,hd . ,_tl) (format "%s" hd))
           (_ (format "%s" dante-state)))
         (if dante-queue (format "+%s" (length dante-queue)) ""))))))

(defcustom dante-modeline-prefix " Danté:" "Modeline prefix." :group 'dante :type 'string)

;;;###autoload
(define-minor-mode dante-mode
  "Minor mode for Dante.

`dante-mode' takes one optional (prefix) argument.
Interactively with no prefix argument, it toggles dante.
A prefix argument enables dante if the argument is positive,
and disables it otherwise.

When called from Lisp, the `dante-mode' toggles dante if the
argument is `toggle', disables dante if the argument is a
non-positive integer, and enables dante otherwise (including
if the argument is omitted or nil or a positive integer).

\\{dante-mode-map}"
  :lighter (:eval (concat dante-modeline-prefix (dante-status)))
  :keymap dante-mode-map
  :group 'dante
  (if dante-mode
      (add-hook 'flymake-diagnostic-functions 'dante-flymake nil t)
    (remove-hook 'flymake-diagnostic-functions 'dante-flymake t)))

(define-key dante-mode-map (kbd "C-c .") 'dante-type-at)
(define-key dante-mode-map (kbd "C-c ,") 'dante-info)
(define-key dante-mode-map (kbd "C-c /") 'attrap-attrap) ;; deprecated keybinding
(define-key dante-mode-map (kbd "C-c \"") 'dante-eval-block)
(define-key dante-mode-map (kbd "C-c C-c") 'dante-exec)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive utils

(defun dante-fontify-expression (expression)
  "Return a haskell-fontified version of EXPRESSION.
If `haskell-mode' is not loaded, just return EXPRESSION."
  (if (fboundp 'haskell-mode)
      ;; From https://github.com/lunaryorn/ansible-doc.el/blob/master/ansible-doc.el#L211
      ;; See also http://emacs.stackexchange.com/a/5408/227
      (with-temp-buffer
        (insert expression)
        (delay-mode-hooks
          (haskell-mode)
          (font-lock-mode))
        (font-lock-ensure)
        (buffer-string))
    expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type, info and doc at point

(defun dante-doc (ident)
  "Get the haddock about IDENT at point."
  (interactive (list (dante-ident-at-point)))
  (lcr-spawn
    (let ((info (lcr-call dante-async-call (format ":doc %s" ident))))
      (with-help-window (help-buffer)
        (princ info)))))

(defun dante-type-at (insert)
  "Get the type of the thing or selection at point.
When the universal argument INSERT is non-nil, insert the type in the buffer."
  (interactive "P")
  (let ((tap (dante--ghc-subexp (dante-thing-at-point t))))
    (lcr-spawn
      (lcr-call dante-async-load-current-buffer nil nil)
      (let ((ty (lcr-call dante-async-call (concat ":type-at " tap))))
        (dante--insert-or-show-fontified ty insert)))))

(defun dante--insert-or-show-fontified (expr insert?)
  (if insert?
      (dante--insert-type expr)
    (message "%s" (dante-fontify-expression expr))))

(defun dante--bwd-word ()
  (skip-chars-backward " \t")
  (skip-chars-backward "^ \t")
  nil)

(defun dante--insert-type (ty)
  (with-marker (p (point-marker))
    (set-marker-insertion-type p t)
    (let ((let-is-prev? (lambda ()
                          (save-excursion
                            (dante--bwd-word)
                            (looking-at-p (rx symbol-start "let" symbol-end)))))
          (insert-for-let (lambda (indent)
                            (insert (dante-fontify-expression ty) "\n")
                            (insert-char ?\s indent))))
      (if (funcall let-is-prev?)
          ;; let _|_foo = bar
          (funcall insert-for-let (current-column))
        (progn
          (dante--bwd-word)
          (let ((col (current-column)))
            (if (funcall let-is-prev?)
                (funcall insert-for-let col)
              (progn
                (goto-char p)
                (goto-char (line-beginning-position))
                (insert-char ?\s (current-indentation))
                (insert (dante-fontify-expression ty) "\n")))))))
    (goto-char p)))

(defun dante-info (ident)
  "Get the info about the IDENT at point."
  (interactive (list (dante-ident-at-point)))
  (lcr-spawn
    (lcr-call dante-async-load-current-buffer t nil)
    (let ((help-xref-following nil)
          (origin (buffer-name))
          (info (lcr-call dante-async-call (format ":info %s" ident))))
      (help-setup-xref (list #'dante-call-in-buffer (current-buffer) #'dante-info ident)
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (princ (concat (dante-fontify-expression ident)
                       " in `" origin "'"
                       "\n\n"
                       (dante-fontify-expression info)))))))

(defvar-local dante-temp-fingerprint -1
  "The value of `sha1' of source buffer’s contents when the contents were last loaded.")

(defvar-local dante-interpreted nil)

(defvar dante-original-buffer-map (make-hash-table :test 'equal)
  "Hash table from (local) temp file names to the file they originate.")

(lcr-def dante-async-load-current-buffer (interpret err-fn)
  "Load and maybe INTERPRET the temp file for current buffer.
Interpreting puts all symbols from the current module in
scope.  Compiling to avoids re-interpreting the dependencies over
and over."
  (let* ((fingerprint (sha1 (current-buffer)))
         (unchanged (equal fingerprint dante-temp-fingerprint))
         (src-fname (buffer-file-name (current-buffer)))
         (fname (dante-temp-file-name (current-buffer)))
         (buffer (lcr-call dante-session))
         (same-target (and (or dante-interpreted (not interpret))
                           (s-equals? (buffer-local-value 'dante-loaded-file buffer) src-fname))))
    (if (and unchanged same-target) ; see #52
        (buffer-local-value 'dante-load-message buffer)
      (setq dante-temp-fingerprint fingerprint)
      (setq dante-interpreted interpret)
      (puthash (dante-local-name fname) src-fname dante-original-buffer-map)
      (unless (s-equals? src-fname fname)
        ;; Set `noninteractive' to suppress messages from `write-region'.
        (let ((noninteractive t))
          (write-region nil nil fname nil 0)))
      ;; GHCi will interpret the buffer if both -fbyte-code and :l * are used.
      (lcr-call dante-async-call (if interpret ":set -fbyte-code" ":set -fobject-code"))
      (with-current-buffer buffer
        (setq-local dante-status 'loading)
        (dante-async-write (if same-target
                               ":r!"
                             (concat ":l! "
                                     (if interpret "*" "")
                                     (dante-local-name fname))))
        (cl-destructuring-bind (_status err-messages _loaded-modules)
            (lcr-call dante-load-loop "" nil err-fn)
          (setq dante-loaded-file src-fname)
          (setq dante-load-message
                (--map (-map #'ansi-color-apply it) err-messages)))))))

(defun dante-local-name (fname)
  "Local name of FNAME on the remote host."
  (string-remove-prefix (or (file-remote-p fname) "") fname))

;;;;;;;;;;;;;;;;;;;;;
;; Flycheck checker

(defun dante-check (checker cont)
  "Run a check with CHECKER and pass the status onto CONT."
  (if (eq (dante-get-var dante-state) 'dead) (funcall cont 'interrupted)
    (lcr-spawn
      (let* ((messages (lcr-call dante-async-load-current-buffer nil nil))
             (temp-file (dante-local-name (dante-temp-file-name (current-buffer)))))
        (funcall cont
                 'finished
                 (let ((errs (-non-nil (--map (dante-fly-message it checker (current-buffer) temp-file) messages))
                             ;; (--remove (eq 'splice (flycheck-error-level it))
                             ;;           (--map (dante-fly-message it checker (current-buffer) temp-file) messages))
                             ))
                   (remove-duplicates-by-hashing-projections
                    (lambda (err)
                      (list (flycheck-error-level err)
                            (flycheck-error-message err)
                            (flycheck-error-filename err)
                            (flycheck-error-line err)
                            (flycheck-error-column err)))
                    #'equal
                    errs)))))))

(flycheck-define-generic-checker 'haskell-dante
  "A syntax and type checker for Haskell using a Dante worker
process."
  :start 'dante-check
  :predicate (lambda () dante-mode)
  :modes '(haskell-mode haskell-literate-mode)
  :working-directory (lambda (_checker)
                       (unless dante-project-root (dante-initialize-method))
                       dante-project-root))

(add-to-list 'flycheck-checkers 'haskell-dante)

(defcustom dante-flycheck-types
  '(("^warning:\\(?: \\[GHC-[0-9]+\\]\\)? \\[-W\\(?:typed-holes\\|deferred-\\(?:type-errors\\|out-of-scope-variables\\)\\)\\]" . error)
    ("^warning" . warning)
    ("^splicing " . nil)
    ("" . error))
  "Map of regular expressions to flycheck error types, ordered by priority."
  :group 'dante :type '(repeat cons (regex symbol)))

(defun dante-fly-message (matched checker buffer temp-file)
  "Convert the MATCHED message to flycheck format.
CHECKER and BUFFER are added if the error is in TEMP-FILE."
  (save-match-data
    (cl-destructuring-bind (file location-raw err-type msg) matched
      (let* ((type (cdr (--first (string-match (car it) err-type) dante-flycheck-types)))
             (fixed-err-type (if (eq type 'error)
                                 err-type
                               (replace-match (symbol->string type) nil nil err-type)))
             (location (dante-parse-error-location location-raw)))
        ;; FIXME: sometimes the "error type" contains the actual error too.
        (when type
          (let ((buf-name (dante-buffer-file-name-for-error-message buffer)))
            (flycheck-error-new-at (car location) (cadr location) type
                                   (replace-regexp-in-string (regexp-quote temp-file)
                                                             (car buf-name)
                                                             (concat fixed-err-type
                                                                     "\n"
                                                                     (trim-whitespace-right msg)))
                                   :checker checker
                                   :buffer buffer
                                   :filename (if (string= (dante-canonicalize-path temp-file)
                                                          (dante-canonicalize-path file))
                                                 (cdr buf-name)
                                               file))))))))

(defun dante-parse-error-location (string)
  "Parse the line/col numbers from the error in STRING."
  (--map (when it (string-to-number it))
         (cdr (s-match (concat
                        "\\(?1:[0-9]+\\):\\(?2:[0-9]+\\)\\(?:-\\(?4:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
                        "\\|"
                        "(\\(?1:[0-9]+\\),\\(?2:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?4:[0-9]+\\))") ;; "(289,5)-(291,36)"
                       string))))

(defun dante-call-in-buffer (buffer func &rest args)
  "In BUFFER, call FUNC with ARGS."
  (with-current-buffer buffer (apply func args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company integration (auto-completion)

(lcr-def dante-complete (prefix)
  (lcr-call dante-async-load-current-buffer nil nil)
  (let* ((reply (lcr-call dante-async-call (format ":complete repl %S" prefix)))
         (lines (s-lines reply))
         (common (nth 2 (read (concat "(" (car lines) ")")))))
    (--map (concat common (read it)) (cdr lines))))

(defun dante--in-a-comment ()
  "Return non-nil if point is in a comment."
  (nth 4 (syntax-ppss)))

(declare-function company-begin-backend 'company)

(defun dante-company (command &optional arg &rest _ignored)
  "Company backend for dante.
See ``company-backends'' for the meaning of COMMAND, ARG and _IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'dante-company))
    (sorted t)
    (prefix
     (let ((bounds (dante-ident-pos-at-point -1)))
       (when (and dante-mode (not (dante--in-a-comment)) bounds)
         (let* ((id-start (car bounds))
                (_ (save-excursion (re-search-backward "import[\t ]*" (line-beginning-position) t)))
                (import-end (match-end 0))
                (import-start (match-beginning 0))
                (is-import (eq import-end id-start)))
           (buffer-substring-no-properties (if is-import import-start id-start) (point))))))
    (candidates
     (unless (eq (dante-get-var dante-state) 'dead)
       (cons :async (lambda (callback) (lcr-spawn (lcr-halt callback (lcr-call dante-complete arg)))))))))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'dante-company))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source buffer operations

(defun dante-thing-at-point (&optional include-parens)
  "Return (START . END) the indent at point, or the region if it is active."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let ((bounds (dante--bounds-of-haskell-symbol)))
      (if (and include-parens
               bounds)
          (let ((start (car bounds))
                (end (cdr bounds)))
            (cons (if (char-equal ?\( (char-before start))
                      (- start 1)
                    start)
                  (if (char-equal ?\) (char-after end))
                      (+ end 1)
                    end)))
        bounds))))

(defun dante-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (when-let ((reg (or (dante-ident-pos-at-point)
                      (dante--bounds-of-haskell-symbol))))
    (buffer-substring-no-properties (car reg) (cdr reg))))

(defun dante-ident-pos-at-point (&optional offset)
  "Return the span of the (qualified) identifier at point+OFFSET.
Nil if none found."
  (with-syntax-table dante--identifier-syntax-table
    (let* ((qualifier-regex "\\([[:upper:]][[:alnum:]]*\\.\\)")
           (ident-regex (concat qualifier-regex "*\\(\\s.+\\|\\(\\sw\\|\\s_\\)+\\)"))) ; note * for many qualifiers
      (save-excursion
        (goto-char (+ (point) (or offset 0)))
        (when (looking-at ident-regex)
          (let ((end (match-end 0)))
            (skip-syntax-backward (if (looking-at "\\s.") "." "w_")) ;; find start of operator/variable
            (while (save-excursion
                     (and (re-search-backward (concat "\\b" qualifier-regex) (line-beginning-position) t)
                          (s-matches? (concat "^" ident-regex "$") (buffer-substring-no-properties (point) end))))
              (goto-char (match-beginning 0)))
            (cons (point) end)))))))

(defvar dante--identifier-syntax-table
  (let ((tbl (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?#  "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    (modify-syntax-entry ?\' "w" tbl)
    (modify-syntax-entry ?,  "/" tbl) ;; Disable , since it's part of syntax
    (modify-syntax-entry ?.  "_" tbl) ;; So that we match qualified names.
    tbl)
  "Special syntax table for haskell that allows to recognize symbols that contain
both unicode and ascii characters.")

(defun dante--bounds-of-haskell-symbol ()
  "Like `forward-symbol' but for generic Haskell symbols (either operators,
uppercase or lowercase names)."
  (save-excursion
    (save-match-data
      (with-syntax-table dante--identifier-syntax-table
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

(defun dante-buffer-file-name-for-error-message (&optional buffer)
  "Call function `buffer-file-name' for BUFFER and clean its result.
The path returned is canonicalized and stripped of any text properties."
  (if-let (name (buffer-file-name buffer))
      (let ((path (dante-canonicalize-path (substring-no-properties name))))
        (cons path path))
    (cons (buffer-name buffer) nil)))

(defvar-local dante-temp-file-name nil
  "The name of a temporary file to which the current buffer's content is copied.")

(require 'tramp)
(defun dante-tramp-make-tramp-temp-file (buffer)
  "Create a temporary file for BUFFER, perhaps on a remote host."
  (if-let (fname (buffer-file-name buffer))
      (let ((suffix (file-name-extension fname t)))
        (if (file-remote-p fname)
            (with-parsed-tramp-file-name (buffer-file-name buffer) vec
              (let ((prefix (concat
                             (expand-file-name
                              tramp-temp-name-prefix (tramp-get-remote-tmpdir vec))
                             "dante"))
                    result)
                (while (not result)
                  (setq result (concat (make-temp-name prefix) suffix))
                  (if (file-exists-p result)
                      (setq result nil)))
                ;; This creates the file by side effect.
                (set-file-times result)
                (set-file-modes result #o700)
                result))
          fname))
    (make-temp-file "dante" nil ".hs")))

(defun dante-temp-file-name (buffer)
  "Return a (possibly remote) filename suitable to store BUFFER's contents."
  (with-current-buffer buffer
    (or dante-temp-file-name
        (setq dante-temp-file-name
              (dante-tramp-make-tramp-temp-file buffer)))))

(defun dante-canonicalize-path (path)
  "Return a standardized version of PATH.
On Windows, forward slashes are changed to backslashes and the
drive letter is capitalized."
  (let ((standard-path (convert-standard-filename path)))
    (if (eval-when-compile (eq system-type 'windows-nt))
        (dante-capitalize-drive-letter (s-replace "/" "\\" standard-path))
      standard-path)))

(defun dante-capitalize-drive-letter (path)
  "Ensures the drive letter is capitalized in PATH.
This applies to paths of the form x:\\foo\\bar"
  (save-match-data
    (let ((drive-path (split-string path ":\\\\")))
      (if (or (null (car drive-path)) (null (cdr drive-path)))
          path
        (concat (upcase (car drive-path)) ":\\" (cadr drive-path))))))

;;;;;;;;;;;;;;;;;;;;
;; GHCi formatting

(defsubst dante--ghc-column-number-at-pos (pos)
  "Format the point POS as a column number as expected by GHCi."
  (1+ (character-column-at-pos pos)))

(defun dante--ghc-subexp (reg)
  "Format the subexpression denoted by REG for GHCi commands."
  (let ((beg (car reg))
        (end (cdr reg)))
    (format "%S %d %d %d %d %s"
            (dante-local-name (dante-temp-file-name (current-buffer)))
            (line-number-at-pos beg)
            (dante--ghc-column-number-at-pos beg)
            (line-number-at-pos end)
            (dante--ghc-column-number-at-pos end)
            (buffer-substring-no-properties beg end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GHCi process communication

(defun dante-destroy ()
  "Stop GHCi and kill its associated process buffer."
  (interactive)
  (when-let ((buf (dante-buffer-p)))
    (dante-set-state 'deleting)
    (when-let ((process (get-buffer-process buf)))
      (kill-process process)
      (delete-process process))
    (kill-buffer buf)))

(defun dante-restart ()
  "Restart GHCi with the same configuration (root, command line, …) as before."
  (interactive)
  (when (dante-buffer-p)
    (dante-destroy))
  (lcr-spawn (lcr-call dante-start))
  (when flymake-mode (flymake-start t t))) ; re-enable backend

(defun dante-session (cont)
  "Get the session or create one if none exists.
If WAIT is nil, abort if Dante is busy.  Pass the dante buffer to CONT"
  (if-let* ((buf (dante-buffer-p)))
      (if (buffer-local-value 'lcr-process-callback buf)
          (lcr-context-switch
              (with-current-buffer buf
                (when dante-queue
                  (message "Overriding previously queued GHCi request."))
                (setq dante-queue (cons (lambda (x) (lcr-resume cont x)) nil))))
        (funcall cont buf))
    (dante-start cont)))

(defcustom dante-load-flags '("+c" "-fdiagnostics-color=never" "-fno-diagnostics-show-caret" "-Wwarn=missing-home-modules" "-ferror-spans")
  "Flags to set whenever GHCi is started."
  :type (cons 'set (--map (list 'const :tag (concat (car it) ": " (cadr it)) (car it))
                          '(("+c" "Gather type information (necessary for `dante-type-at')")
                            ("-Wall" "Report all warnings")
                            ("-ferror-spans" "Report span in error messages (used in flymake only)")
                            ("-fdefer-typed-holes" "Accept typed holes, so that completion/type-at continues to work then.")
                            ("-Wwarn=missing-home-modules" "Do not error-out if a module is missing in .cabal file")
                            ("-fdiagnostics-color=never" "No color codes in error messages (color codes will trigger bugs in Dante)")
                            ("-fno-diagnostics-show-caret" "Cleaner error messages for GHC >=8.2 (ignored by earlier versions)")))))

(defun dante--make-file-process (name buffer program args filter sentinel)
  (let ((fh (find-file-name-handler default-directory 'dante--make-file-process)))
    (if fh
        (apply fh #'make-process :name name :buffer buffer :command (cons program program-args) :filter filter :sentinel sentinel :file-handler t)
      (apply #'make-process :name name :buffer buffer :command (cons program program-args) :filter filter :sentinel sentinel :file-handler t))))

(lcr-def dante-start ()
  "Start a GHCi process and return its buffer."
  (let* ((buffer (dante-buffer-create))
         (args (-non-nil (-map #'eval dante-repl-command-line)))
         (ghc-initialising? t)
         (initial-ghc-messages nil)
         (vanilla-filter (lcr-process-make-filter buffer))
         (process (with-current-buffer buffer
                    (message "Dante: Starting GHCi: %s" (combine-and-quote-strings args))
                    (make-process :name "dante"
                                  :buffer buffer
                                  :command args
                                  :noquery t
                                  :filter (lambda (process str)
                                            (if ghc-initialising?
                                                (progn
                                                  (push str initial-ghc-messages)
                                                  (funcall vanilla-filter process str))
                                              (progn
                                                ;; Discharge ourselves
                                                (set-process-filter process vanilla-filter)
                                                (funcall vanilla-filter process str))))
                                  :sentinel (lambda (process change)
                                              (dante-sentinel process
                                                              change
                                                              (and ghc-initialising?
                                                                   (join-lines (reverse (--drop-while (string= "" it) initial-ghc-messages))))))
                                  :file-handler t))))
    (with-current-buffer buffer
      (erase-buffer)
      (setq-local dante-command-line (process-command process)))
    (dante-set-state 'starting)
    (lcr-call dante-async-call
              (s-join "\n" (--map (concat ":set " it)
                                  (append dante-load-flags
                                          (list "prompt \"\\4%s|\""
                                                ;; Empty continuation prompt so that output
                                                ;; of :{ will be correctly identified.
                                                "prompt-cont \"\"")))))
    (let ((dir (lcr-call dante-async-call ":!pwd")))
      (with-current-buffer buffer (setq dante-ghci-path dir)))
    (dante-set-state 'started)
    (setf ghc-initialising? nil
          initial-ghc-messages nil)
    buffer))

(defun dante-debug (category msg &rest objects)
  "Append a debug message MSG to the current buffer.
Do so iff CATEGORY is enabled in variable `dante-debug'."
  (when (memq category dante-debug)
    (goto-char (1- (point-max)))
    (insert (apply #'format msg objects))))

(lcr-def dante-async-read ()
  "Read input from GHCi.
Must be called from GHCi process buffer."
  (let* ((input (lcr-call lcr-process-read (current-buffer))))
    (dante-debug 'inputs "%s" input)
    (s-replace "\r" "" input)))

(defconst dante-ghci-prompt "\4\\(.*\\)|")

(defun dante-regexp-disjoin (&rest regexps)
  "Return a regexp matching any of REGEXPS."
  (s-join "\\|" regexps))

(defconst dante-error-regexp
  "^\\([A-Z]?:?[^ \n:][^:\n\r]+\\):\\([0-9()-:]+\\): \\(.*\\)\n\\(\\([ ]+.*\n\\)*\\)")

(lcr-def dante-load-loop (acc err-msgs err-fn)
  "Parse the output of load command.
ACC umulate input and ERR-MSGS."
  (save-match-data
    (let ((success (dante-regexp-disjoin
                    "^Ok, modules loaded:[ ]*\\([^\n ]*\\)\\( (.*)\\)?\."
                    "^Ok, .*modules loaded." ;; .* stands for a number in english (two, three, ...) (GHC 8.2)
                    "^Ok, one module loaded."))
          (progress "^\\[\\([0-9]*\\) of \\([0-9]*\\)\\] Compiling \\([^ \n]*\\).*")
          result cur-file)
      (while (not result)
        (let* ((i (string-match (dante-regexp-disjoin dante-ghci-prompt success dante-error-regexp progress) acc))
               (m (when i (match-string 0 acc)))
               (rest (when i (substring acc (match-end 0)))))
          (cond ((and m (string-match dante-ghci-prompt m))
                 (setq dante-state (list 'ghc-err (pcase dante-state
                                                    (`(compiling ,module) (ansi-color-apply module))
                                                    (_ cur-file)))) ; when the module name is wrong, ghc does not output any "Compiling ..." message
                 (setq result (list 'failed (nreverse err-msgs) (match-string 1 m))))
                ((and m (string-match progress m))
                 (setq dante-state (list 'compiling (match-string 3 m))))
                ((and m (string-match success m))
                 ;; With the +c setting, GHC (8.2) prints: 1. error
                 ;; messages+warnings, if compiling only 2. if successful,
                 ;; repeat the warnings
                 (setq dante-state 'process-warnings)
                 (cl-destructuring-bind (_status warning-msgs loaded-mods) (lcr-call dante-load-loop rest nil nil)
                   (setq dante-state (list 'loaded loaded-mods))
                   (setq result (list 'ok (or (nreverse err-msgs) warning-msgs) loaded-mods))))
                ((and m (> (length rest) 0) (/= (elt rest 0) ?\s)) ;; make sure we're matching a full error message
                 (when (string-match dante-error-regexp m)
                   (let* ((file (match-string 1 m))
                          (err-msg (list file
                                         (match-string 2 m)
                                         (match-string 3 m)
                                         (match-string 4 m))))
                     (setq cur-file file)
                     (push err-msg err-msgs)
                     (when err-fn (funcall err-fn (list err-msg))))))
                (t (setq rest (concat acc (lcr-call dante-async-read)))))
          (setq acc rest)))
      result)))

(defun dante-async-write (cmd)
  "Write to GHCi associated with current buffer the CMD."
  (dante-debug 'outputs "\n[Dante] -> %s\n" cmd)
  (process-send-string (get-buffer-process (current-buffer)) (concat cmd "\n")))

(lcr-def dante-async-call (cmd)
  "Send GHCi the command string CMD and return the answer."
  (with-current-buffer (dante-buffer-p)
    (dante-async-write cmd)
    (let ((acc "")
          (matched nil))
      (while (not matched)
        (setq acc (concat acc (lcr-call dante-async-read)))
        (setq matched (string-match dante-ghci-prompt acc)))
      (ansi-color-apply (trim-whitespace-right (substring acc 0 (1- (match-beginning 1))))))))

(defun dante-sentinel (process change init-msg)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (let ((buffer (process-buffer process)))
    (when (and (buffer-live-p buffer) (not (process-live-p process)))
      (if (eq (buffer-local-value 'dante-state buffer) 'deleting)
          (message "GHCi process deleted.")
        (with-current-buffer buffer (setq dante-state 'dead))
        (dante-show-process-problem process change init-msg)))))

(defun dante-diagnose ()
  "Show all state info in a help buffer."
  (interactive)
  (let ((info (dante-debug-info (dante-buffer-p) nil)))
    (with-help-window (help-buffer)
      (princ info))))

(defun dante-debug-info (buffer init-msg)
  "Show debug info for dante buffer BUFFER."
  (if buffer
      (with-current-buffer buffer
        (s-join "\n"
                (cons (and init-msg
                           (concat "GHCi output\n" init-msg))
                      (cons (concat "dante-command-line " (s-join " " dante-command-line))
                            (--map (format "%s %S" it (eval it))
                                   '(default-directory dante-ghci-path dante-state dante-queue dante-loaded-file dante-load-message lcr-process-callback))))))
    "No GHCi interaction buffer"))

(defun dante-show-process-problem (process change init-msg)
  "Report to the user that PROCESS reported CHANGE, causing it to end."
  (message "Dante GHCi process failed! Further details in '%s'.\n--------------------------------\n%s%s"
           (process-buffer process)
           (join-lines (process-command process) " ")
           (if init-msg
               (concat "\n--------------------------------\n" init-msg)
             ""))
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert "\n---\n\n"
            "Process state change: " change "\n"
            (dante-debug-info (current-buffer) init-msg))))

(defvar-local dante--selected-method nil)

(defun dante-buffer-name ()
  "Create a dante process buffer name."
  (unless dante--selected-method (dante-initialize-method))
  (funcall (dante-method-repl-buf-name-func dante--selected-method)))

(defun dante-buffer-name--default ()
  "Default implementation for getting name of the GHCi interaction buffer.
First, if needed, initialize the GHCi invokation method.  Then construct the
appropriate buffer name on this basis."
  (unless dante-project-root (dante-initialize-method))
  (concat " *dante#" dante-target "#" dante-project-root "*"))

(cl-defstruct dante-initialize-state
  (project-root    nil :read-only t)
  (selected-method nil :read-only t)
  (target          nil :read-only t))

(defun dante-store-initialize-state ()
  (make-dante-initialize-state
   :project-root    dante-project-root
   :selected-method dante--selected-method
   :target          dante-target))

(defun dante-restore-initialize-state (state)
  ;; Important to transfer all variables confnigured by ‘dante-initialize-method’
  ;; to this new buffer so that functions like ‘dante-buffer-name’ will pick them up
  ;; instead of trying to initialize method once again from within GHCI buffer, which
  ;; would fail since it doesn’t have file associated.
  ;;
  ;; Most of these settings are included into buffer name anyway so different
  ;; buffers obviously have different values of the following parameters.

  (setq-local dante-project-root     (dante-initialize-state-project-root state)
              dante--selected-method (dante-initialize-state-selected-method state)
              dante-target           (dante-initialize-state-target state)))

(defun dante-buffer-create ()
  "Create the buffer for GHCi."
  (unless dante-project-root (dante-initialize-method))
  (let ((state (dante-store-initialize-state)))
    (with-current-buffer (get-buffer-create (dante-buffer-name))
      (cd (dante-initialize-state-project-root state))
      (fundamental-mode) ;; this has several effects, including resetting the local variables
      (buffer-disable-undo)
      (dante-restore-initialize-state state)
      (current-buffer))))

(defun dante-set-state (state)
  "Set the `dante-state' to STATE."
  (with-current-buffer (dante-buffer-p) (setq-local dante-state state)))

(defun dante-buffer-p ()
  "Return the GHCi buffer if it exists, nil otherwise."
  (get-buffer (dante-buffer-name)))

(defun dante-cabal-find-file (&optional file)
  "Search for directory of cabal file.
Search upwards in the directory structure, starting from FILE (or
`default-directory' if nil)."
  (let ((dir (locate-dominating-file (or file default-directory)
                                     (lambda (d) (directory-files d t ".\\.cabal\\'")))))
    (when dir (car (directory-files dir t ".\\.cabal\\'")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref support

(defun dante--xref-backend () "Dante xref backend." (when dante-mode 'dante))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql dante)))
  (dante--ghc-subexp (dante-thing-at-point)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql dante)))
  nil)

(defun dante-expand-filename (filename)
  "Prepend FILENAME with the dante running directory."
  (concat (with-current-buffer (dante-buffer-p) default-directory) filename))

(defun dante--match-src-span (string)
  "Extract a location from a ghc span STRING."
  ;; On external symbols, GHC may return a location such as integer-gmp-1.0.0.1:integer-gmp-1.0.0.1:GHC.Integer.Type
  (when (string-match haskell-regexen/ghci-src-span string)
    (let ((file (match-string 1 string))
          (line (string-to-number (match-string 2 string)))
          (col (string-to-number (match-string 3 string))))
      (xref-make-file-location
       (or (gethash file dante-original-buffer-map)
           (expand-file-name file (dante-get-var dante-ghci-path)))
       line (1- col)))))

(defun dante--summarize-src-spans (spans file)
  "Add summary strings to a list of source SPANS in FILE."
  (if (not (and file (file-readable-p file)))
      (--map (xref-make "<unreadable>" it) spans)
    (let* ((lines (s-lines (f-read file)))
           (wanted (--map (1- (xref-file-location-line it)) spans))
           (lines (-select-by-indices wanted lines)))
      (-zip-with #'xref-make lines spans))))

(defun dante--make-xrefs (string)
  "Make xref objects for the source spans in STRING."
  (--mapcat (funcall #'dante--summarize-src-spans (cdr it) (car it))
            (--group-by (xref-file-location-file it)
                        (-non-nil (-map #'dante--match-src-span
                                        (s-lines string))))))

(cl-defmethod xref-backend-definitions ((_backend (eql dante)) symbol)
  (lcr-spawn-and-wait
   (lcr-call dante-async-load-current-buffer nil nil)
   (dante--make-xrefs (lcr-call dante-async-call (concat ":loc-at " symbol)))))

(cl-defmethod xref-backend-references ((_backend (eql dante)) symbol)
  (lcr-spawn-and-wait
   (lcr-call dante-async-load-current-buffer nil nil)
   (dante--make-xrefs (lcr-call dante-async-call (concat ":uses " symbol)))))

(add-hook 'xref-backend-functions 'dante--xref-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reploid

(defun dante-eval-block ()
  "Evaluate the GHCi command(s) found at point and insert the results.
The command block is indicated by the >>> symbol."
  (interactive)
  (push-mark)
  (beginning-of-line)
  (let ((block-end (save-excursion
                     (while (looking-at-p " *--") (forward-line))
                     ;; ensure that there is a newline at the end of buffer
                     (when (eq (point) (point-max)) (newline))
                     (point-marker))))
    (while (looking-at-p " *--") (forward-line -1))
    (forward-line)
    (lcr-spawn
      (lcr-call dante-async-load-current-buffer t nil)
      (while (search-forward-regexp " *-- +>>>" (line-end-position) t 1)
        ;; found a command; execute it and replace the result.
        (let ((cmd-start (match-end 0))
              (prefix (string-replace ">>>" "   " (match-string 0)))
              (cmd-start-col (current-column))
              (cmd-lines (list (buffer-substring-no-properties (point) (line-end-position)) ":{")))
          (cl-assert (equal (point) cmd-start))
          (beginning-of-line)
          (forward-line)
          (while (looking-at-p prefix)
            (move-to-column cmd-start-col)
            (push (buffer-substring-no-properties (point) (line-end-position)) cmd-lines)
            (beginning-of-line)
            (forward-line))
          (let ((cmd-end (1- (point))))
            (let* ((cmd (join-lines (nreverse (cons ":}" cmd-lines))))
                   (res (lcr-call dante-async-call cmd))))
            (save-excursion
              (delete-region (point)
                             ;; look for: empty comment line, next command or end of block.
                             (or (and (search-forward-regexp " *-- *\\( >>>\\|$\\)" block-end t 1)
                                      (match-beginning 0))
                                 block-end)))
            (insert (apply #'concat (--map (concat "-- " it "\n") (--remove (s-blank? it) (s-lines res)))))
            (beginning-of-line)
            ;; skip any non-executable comment
            (while (and (looking-at " *--")
                        (not (looking-at-p " *-- +>>>")))
              (forward-line))))))))

(defcustom dante-exec-default "main"
  (substitute-command-keys "Default command to run by `dante-exec'.")
  :group 'dante :safe t :type 'string)

(defun dante-exec (command)
  "Execute COMMAND in GHCi and show the result in the echo area."
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       dante-exec-default)))
  (dante-set-state 'running)
  (lcr-spawn
    (lcr-call dante-async-load-current-buffer nil nil)
    (message "%s" (lcr-call dante-async-call command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake

(defun dante-flymake (report-fn &rest _args)
  "Run a check and pass the status onto REPORT-FN."
  (let* ((src-buffer (current-buffer))
         (buf0 (dante-buffer-p))
         (temp-file (dante-local-name (dante-temp-file-name src-buffer)))
         (nothing-done t)
         ;; flymake raises errors when any report is made using an
         ;; "old" call to the backend. However, we must deal with all
         ;; GHCi output, so we must let the loop run to completion. So
         ;; we simply disable messages if another call to this
         ;; function is detected.  This token must be set before any
         ;; context switch occurs, otherwise we cannot detect if we
         ;; got another call from flymake in between.
         (local-token (if buf0 (with-current-buffer buf0 (setq dante-flymake-token (1+ dante-flymake-token)))
                        dante-flymake-token)))
    (if (eq (dante-get-var dante-state) 'dead) (funcall report-fn :panic :explanation "Ghci is dead")
      (lcr-spawn
        (let* ((buf (lcr-call dante-session)) ; yield until GHCi is ready to process the request
               (token-guard (lambda () (eq (buffer-local-value 'dante-flymake-token buf) local-token)))
               (msg-fn (lambda (messages)
                         (when (funcall token-guard)
                           (setq nothing-done nil)
                           (funcall report-fn
                                    (-non-nil
                                     (--map (dante-fm-message it src-buffer temp-file) messages)))))))
          (when (funcall token-guard) ; don't try to load if we're too late.
            (let ((messages (lcr-call dante-async-load-current-buffer nil msg-fn)))
              (when nothing-done ; clears previous messages and deals with #52
                ;; this can happen when the file did not change
                (funcall msg-fn messages)))))))))

(defun dante-pos-at-line-col (buf l c)
  "Translate line L and column C into a position within BUF."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- l))
      (move-to-character-column (1- c))
      (point))))

(defun dante-fm-message (matched buffer temp-file)
  "Convert the MATCHED message to flymake format.
Or nil if BUFFER / TEMP-FILE are not relevant to the message."
  (cl-destructuring-bind (file location-raw first-line msg) matched
    ;; Flymake bug: in fact, we would want to report all errors,
    ;; with buffer = (if (string= temp-file file) buffer (find-buffer-visiting file)),
    ;; but flymake actually ignores the buffer argument of flymake-make-diagnostic (?!).
    (when (string= temp-file file)
      (let* ((type-analysis
              (cl-destructuring-bind (typ msg-start) (s-split-up-to ":" first-line 1)
                (cond ((string-equal typ "warning")
                       (if (s-matches? "\\[-W\\(typed-holes\\|deferred-\\(type-errors\\|out-of-scope-variables\\)\\)\\]" msg-start)
                           (list :error "")
                         (list :warning "")))
                      ((string-equal typ "splicing") (list :info ""))
                      (t (list :error msg-start)))))
             (location (dante-parse-error-location location-raw))
             (r (pcase location
                  (`(,l1 ,c1 ,l2 ,c2) (cons (dante-pos-at-line-col buffer l1 c1) (dante-pos-at-line-col buffer (or l2 l1) (1+ c2))))
                  (`(,l ,c) (flymake-diag-region buffer l c)))))
        (when r
          (cl-destructuring-bind (type msg-first-line) type-analysis
            (let* ((final-msg (trim-whitespace (concat msg-first-line "\n" (replace-regexp-in-string "^    " "" msg)))))
              (flymake-make-diagnostic buffer (car r) (cdr r) type final-msg))))))))

(provide 'dante)

;;; dante.el ends here
