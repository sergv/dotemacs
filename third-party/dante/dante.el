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
  (require 'cl-lib)
  (require 'company)
  (require 'dash)
  (require 'set-up-platform))

(require 'dash)
(require 'f)
(require 'flycheck)
(require 'flymake)
(require 'haskell-mode)
(require 's)
(require 'xref)
(require 'lcr)
(when-windows
 (require 'windows-setup))

(require 'common)
(require 'common-whitespace)
(require 'haskell-cabal-components)
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

;; (setf dante-debug '(inputs outputs responses command-line))
;; (setf dante-debug nil)

(defun dante-nix-available? (_buf)
  "Non-nil iff ‘nix’ executable is avaliable."
  (and (cached-executable-find "nix")
       t))

(defun dante-nix-cabal-script-buf? (buf)
  "Non-nil if BUF is a cabal-style script which has no extra configuration."
  (and (dante-nix-available? buf)
       (haskell-misc-cabal-script-buf? buf)))

(defun dante-vanilla-cabal-script-buf? (buf)
  "Non-nil if BUF is a cabal-style script which has no extra configuration."
  (and (cached-executable-find "cabal")
       (haskell-misc-cabal-script-buf? buf)))

(defun dante-directory-regular-files (dir re)
  (--map (car it)
         (--filter (eq nil (cadr it))
                   (directory-files-and-attributes dir nil re t))))

(defun dante-cabal-new (d)
  "non-nil iff D contains a cabal project file or a cabal file."
  (and (or (dante-directory-regular-files d
                                          (rx (or "cabal.project"
                                                  "cabal.project.local")
                                              eos))
           (dante-directory-regular-files d
                                          (rx ".cabal"
                                              eos)))
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

(cl-defstruct (dante-repl-cmdline
               (:conc-name dante-repl-cmdline/))
  ;; Command for a repl session that should load all project modules into new repl.
  (cmdline-loading-all-modules nil :read-only t)
  ;; Command to get a “fresh” repl with no modules loaded.
  (cmdline-loading-no-modules nil :read-only t))

(defconst dante--methods-tag 'dante-methods)

(defsubst dante--mk-methods (defs)
  (let ((tbl (make-hash-table :test #'eq)))
    (dolist (def defs)
      (puthash (dante-method/name def)
               def
               tbl))
    (list dante--methods-tag
          tbl
          (-map #'dante-method/name defs))))

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

(defvar-local dante--config nil)

(defun dante-get-config (&optional buf)
  "Get config for current buffer that specifies all the necessary paths,
targets and components about current buffer’s ghci session."
  (let ((b (or buf (current-buffer))))
    (let ((dante-config (or (buffer-local-value 'dante--config b)
                            (setf (buffer-local-value 'dante--config b) (dante--make-config)))))
      (cl-assert (not (null dante-config)))
      dante-config)))

;; Already set up command lines for use in current buffer.
;; Will be typically populated twice, once for checking and
;; second time for the ‘dante-repl’.
(cl-defstruct (dante-config
               (:conc-name dante-config/))

  ;; String path with the project root - working directory for the
  ;; flycheck checker.
  (project-root nil :read-only t)

  ;; String with path thas is guaranteed to resolve to eproj project
  ;; for this buffer (via ‘eproj-get-project-for-path’) or nil.
  (eproj-root nil :read-only t)

  ;; Nil or string with a directory where flake.nix for current project
  ;; resides.
  (flake-root nil :read-only t)

  ;; String or nil, e.g. "foo:lib:foo". Nil is for e.g. cabal script buffers with #!
  (cabal-target nil :read-only t)

  ;; Value of type ‘cabal-component’ struct or nil. Nil is for e.g. cabal script buffers with #!
  (cabal-component nil :read-only t)

  ;; Cabal build directory for checking session, e.g. /tmp/dist/dante-xxxxx
  (build-dir nil :read-only t)

  ;; Cabal build directory for the dante-repl session, e.g. /tmp/dist/dante-repl-xxxxx
  (repl-dir nil :read-only t)

  ;; Value of type ‘dante-method’
  (method nil :read-only t))

;; Describes how to make various command lines depending on found flakes,
;; presence of eproj project, etc.
(cl-defstruct (dante-method
               (:conc-name dante-method/))
  (name nil :read-only t)
  ;; Function that takes buffer and returns boolean whether this method
  ;; should be considered for enabling in this buffer.
  (is-enabled-pred nil :read-only t)
  ;; Function of one directory string argument.
  ;; Can be nil in which case default directory will be used.
  (find-root-pred nil :read-only t)
  ;; Function of one argument that takes value of type
  ;; ‘dante-config’ and returns list of strings.
  (make-check-command-line nil :read-only t)
  ;; Function of one argument that takes value of type
  ;; ‘dante-config’ and returns value of type ‘dante-repl-cmdline’.
  (make-repl-command-line nil :read-only t)

  ;; Function of one argument of type ‘dante-config’ that
  ;; returns string buffer name for dante repl buffer
  (repl-buf-name-func nil :read-only t)

  ;; Function of two arguments, project root and build directory that returns
  ;; subdirectory of build directory where build artifacts for the checking
  ;; will be stored.
  ;;
  ;; Build directory may be nil, in which case may return either absolute or
  ;; relative path. Relative path would be relative to the project root.
  (get-check-build-dir nil :read-only t)
  ;; Function of two arguments, project root and build directory that returns
  ;; subdirectory of build directory where build artifacts for the repl session
  ;; will be stored.
  ;;
  ;; Build directory may be nil, in which case may return either absolute or
  ;; relative path. Relative path would be relative to the project root.
  (get-repl-build-dir nil :read-only t)

  ;; Nil or function of two arguments:
  ;; 1 Value of type ‘dante-config’
  ;; 2 Build directory to use for preprocessing
  ;;
  ;; Returns command line to run for preprocessing current project via
  ;; cabal so that it will put latest results of running e.g. alex,
  ;; happy or hsc2hs into the specified build directory where cabal
  ;; usually puts them.
  ;;
  ;; May be nil for cases when we don’t need preprocessing.
  (make-preprocess-command-line nil :read-only t))

(defun dante--get-build-dir (name proj-root root)
  (cl-assert (stringp name))
  (cl-assert (or (null proj-root)
                 (and (stringp proj-root)
                      (file-directory-p proj-root))))
  (cl-assert (stringp root))
  (cl-assert (or (not (file-exists-p root)) (file-directory-p root)))
  (if root
      (concat root
              "/"
              name
              (awhen proj-root
                (concat "-" (sha1 it))))
    (concat "dist-newstyle/" name)))

(defconst dante-methods-defs
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

         (get-check-build-dir (lambda (proj-root build-dir)
                                (cl-assert (stringp proj-root))
                                (cl-assert (file-directory-p proj-root))
                                (cl-assert (stringp build-dir))
                                (dante--get-build-dir "dante" proj-root build-dir)))
         (get-repl-build-dir (lambda (proj-root build-dir)
                               (cl-assert (file-directory-p proj-root))
                               (cl-assert (stringp build-dir))
                               (dante--get-build-dir "dante-repl" proj-root build-dir)))

         (repl-options (--mapcat (list "--repl-option" it) ghci-options))
         (mk-dante-method
          (cl-function
           (lambda
             (&key name is-enabled-pred find-root-pred repl-buf-name-func template disable-preprocess)
             (make-dante-method
              :name name
              :is-enabled-pred is-enabled-pred
              :find-root-pred find-root-pred
              :make-check-command-line
              (lambda (cfg)
                (let ((result
                       (funcall template
                                :flake-root (dante-config/flake-root cfg)
                                :flags (list "--builddir"
                                             (dante-config/build-dir cfg))
                                :target (dante-config/cabal-target cfg))))
                  (cl-assert (-all? #'stringp result)
                             nil
                             "Dante method command line must contain only strings, but it’s: %s"
                             result)
                  result))
              :make-repl-command-line
              (lambda (cfg)
                (let ((load-all-mods
                       (funcall template
                                :flake-root (dante-config/flake-root cfg)
                                :flags (append (list "--builddir"
                                                     (dante-config/repl-dir cfg))
                                               repl-options)
                                :target (dante-config/cabal-target cfg)))
                      (load-no-mods
                       (funcall template
                                :flake-root (dante-config/flake-root cfg)
                                :flags (append (list "--builddir"
                                                     (dante-config/repl-dir cfg))
                                               (cons "--repl-no-load" repl-options))
                                :target (dante-config/cabal-target cfg))))
                  (cl-assert (-all? #'stringp load-all-mods))
                  (cl-assert (-all? #'stringp load-no-mods))
                  (make-dante-repl-cmdline
                   :cmdline-loading-all-modules
                   load-all-mods
                   :cmdline-loading-no-modules
                   load-no-mods)))
              :repl-buf-name-func
              repl-buf-name-func
              :get-check-build-dir
              get-check-build-dir
              :get-repl-build-dir
              get-repl-build-dir
              :make-preprocess-command-line
              (unless disable-preprocess
                (lambda (cfg build-dir)
                  (cl-assert (stringp build-dir))
                  (let ((result
                         (funcall template
                                  :flake-root (dante-config/flake-root cfg)
                                  :flags (append (list "--builddir" build-dir)
                                                 (list "--repl-no-load" "--with-repl=echo"))
                                  :target (dante-config/cabal-target cfg))))
                    (cl-assert (-all? #'stringp result)
                               nil
                               "Dante method command line must contain only strings, but it’s: %s"
                               result)
                    result))))))))
    (dante--mk-methods
     (list
      (funcall mk-dante-method
               :name 'nix-flakes-build-script
               :is-enabled-pred #'dante-nix-cabal-script-buf?
               :find-root-pred #'dante-flake-nix
               :repl-buf-name-func #'dante-buffer-name--default
               :disable-preprocess t
               :template
               (cl-function
                (lambda (&key flake-root flags target)
                  (declare (ignore target))
                  (nix-call-via-flakes `("cabal" "repl" ,buffer-file-name ,@flags) flake-root))))

      (funcall mk-dante-method
               :name 'nix-flakes-build
               :is-enabled-pred #'dante-nix-available?
               :find-root-pred #'dante-cabal-flake-nix
               :repl-buf-name-func #'dante-buffer-name--default
               :template
               (cl-function
                (lambda (&key flake-root flags target)
                  (cl-assert (stringp target))
                  (nix-call-via-flakes `("cabal" "repl" ,target ,@flags) flake-root))))

      (funcall mk-dante-method
               :name 'build-script
               :is-enabled-pred #'dante-vanilla-cabal-script-buf?
               :find-root-pred nil
               :repl-buf-name-func #'dante-buffer-name--default
               :disable-preprocess t
               :template
               (cl-function
                (lambda (&key flake-root flags target)
                  (declare (ignore flake-root target))
                  `("cabal" "repl" ,buffer-file-name ,@flags))))

      (funcall mk-dante-method
               :name 'build
               :is-enabled-pred nil
               :find-root-pred #'dante-cabal-new
               :repl-buf-name-func #'dante-buffer-name--default
               :template
               (cl-function
                (lambda (&key flake-root flags target)
                  (declare (ignore flake-root))
                  (cl-assert (stringp target))
                  `("cabal" "repl" ,target ,@flags))))

      (funcall mk-dante-method
               :name 'bare-ghci
               :is-enabled-pred nil
               :find-root-pred (lambda (_) t)
               :repl-buf-name-func #'dante-buffer-name--default
               :template
               (cl-function
                (lambda (&key flake-root flags target)
                  (declare (ignore flake-root flags target))
                  '("ghci"))))))))

(defcustom dante-methods (dante--methods-names dante-methods-defs)
  "Keys in `dante-methods-alist' to try, in order.
Consider setting this variable as a directory variable."
   :group 'dante :safe t :type '(repeat symbol))

(put 'dante-methods 'safe-local-variable #'listp)

(defun dante--make-config ()
  "Initialize the method used to run GHCi according to ‘dante-methods’."
  (let ((cabal-cfg (haskell-misc-configure-dante)))
    ;; (haskell-misc--configure-dante-if-needed!)
    (when (and cabal-cfg
               (not (stringp (dante-configuration-result/target cabal-cfg))))
      (error "dante target not configured"))
    (let ((result nil)
          (ms dante-methods))
      (dolist (method-name dante-methods)
        (while (and (not result)
                    ms)
          (let ((method-name (car ms)))
            (setf ms (cdr ms))
            (when-let ((method (dante--methods-lookup method-name dante-methods-defs)))
              (let ((pred (dante-method/is-enabled-pred method)))
                (when (or (null pred)
                          (funcall pred (current-buffer)))
                  (let* ((proj (eproj-get-project-for-buf-lax (current-buffer)))
                         (proj-root (awhen proj
                                      (f-full (eproj-project/root it)))))
                    (when-let ((root (if-let ((find-root-pred (dante-method/find-root-pred method)))
                                         (locate-dominating-file default-directory
                                                                 (lambda (dir)
                                                                   (and (if proj-root
                                                                            ;; If there’s a project then don’t ascend past it.
                                                                            (string-prefix-p proj-root
                                                                                             (f-full dir))
                                                                          t)
                                                                        (funcall find-root-pred dir))))
                                       default-directory)))
                      (setf root (expand-file-name root))
                      (let ((flake-root
                             (when (dante-nix-available? (current-buffer))
                               (cond
                                 ((file-exists-p (concat root "/flake.nix"))
                                  root)
                                 (proj
                                  (let ((eproj-root (eproj-project/root proj)))
                                    (when (file-exists-p (concat eproj-root "/flake.nix"))
                                      eproj-root)))
                                 (t
                                  nil))))
                            (build-dir
                             (eproj-query/fold-build-dir
                              proj
                              ;; if not defined
                              (lambda ()
                                (fold-platform-os-type "/tmp/dist/dante" "dist-newstyle/dante"))
                              ;; if defined
                              #'identity)))

                        (cl-assert (stringp root))
                        (cl-assert (file-directory-p root))
                        (cl-assert (or (null proj-root)
                                       (and (stringp proj-root)
                                            (file-directory-p proj-root))))
                        (cl-assert (or (null flake-root)
                                       (and (stringp flake-root)
                                            (file-directory-p flake-root))))
                        (cl-assert (dante-method-p method))
                        (cl-assert (or (null cabal-cfg)
                                       (dante-configuration-result-p cabal-cfg)))
                        (setf result
                              (make-dante-config
                               :project-root root
                               :eproj-root proj-root
                               :flake-root flake-root
                               :cabal-target (and cabal-cfg
                                                  (dante-configuration-result/target cabal-cfg))
                               :cabal-component (and cabal-cfg
                                                     (dante-configuration-result/component cabal-cfg))
                               :build-dir (funcall (dante-method/get-check-build-dir method) root build-dir)
                               :repl-dir (funcall (dante-method/get-repl-build-dir method) root build-dir)
                               :method method)))))))))))
      (if result
          result
        (error "Dante not configured - no method applies")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session-local variables. These are set *IN THE GHCi INTERACTION BUFFER*

(defstruct (dante-check-ghci-state
            (:conc-name dante-check-ghci-state/))
  ;; Path where GHCi runs.
  ;;
  ;; May be different to what ‘dante-config/project-root’ returns
  ;; because of cabal behaviour.
  ghci-path

  ;; Command line used to start GHCi.
  command-line

  ;; Load messages from GHCi before actual repl starts.
  load-message

  loaded-file

  ;; List of ready GHCi queries.
  queue

  ;;  The current checker state.
  ;; - nil: initial state
  ;; - deleting: The process of the buffer is being deleted.
  ;; - dead: GHCi died on its own.  Do not try restarting
  ;; automatically.  The user will have to manually run `dante-restart'
  ;; to destroy the buffer and create a fresh one without this variable enabled.
  ;; - other value: informative value for the user about what GHCi is doing.
  checker-state)

(defvar-local dante--ghci-state nil
  "Value of type ‘dante-check-ghci-state’ in the GHCi buffer.")

(defun dante-get-ghci-state (&optional buf)
  (when-let ((ghci-buf (or buf (dante-buffer-p))))
    (buffer-local-value 'dante--ghci-state ghci-buf)))

(defvar-local dante-flymake-token 1000)

(add-hook
 'lcr-context-switch-hook
 (defun dante-schedule-next ()
   "If no green thread is running, run the next queued one, if any."
   ;; when whatever green thread was running is over, we're back in
   ;; the original source buffer. It's time to check if anything
   ;; queued should be run.
   (when-let ((ghci-buf (dante-buffer-p)))
     (with-current-buffer ghci-buf
       (unless lcr-process-callback
         ;; Note that dante green threads are not interleaved,
         ;; because they only yield by placing a callback.
         (let ((req (pop (dante-check-ghci-state/queue (dante-get-ghci-state ghci-buf)))))
           (when req (funcall req ghci-buf))))))
   ;; we're about to yield back to emacs main loop. Inform the user of status.
   (force-mode-line-update t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defconst dante-mode-map (make-sparse-keymap) "Dante minor mode's map.")

(defun dante-status ()
  "Return dante's status for the current source buffer."
  (condition-case err
      (let ((ghci-buf (dante-buffer-p))
            (fname (buffer-file-name (current-buffer))))
        (if (not ghci-buf)
            "stopped"
          (let ((ghci-state (dante-get-ghci-state ghci-buf)))
            (concat
             (if (buffer-local-value 'lcr-process-callback ghci-buf) "busy " "")
             (pcase (dante-check-ghci-state/checker-state ghci-state)
               (`(ghc-err (compiling ,mod)) (format "error(%s)" mod))
               (`(loaded ,_loaded-mods)
                (let ((loaded-file (dante-check-ghci-state/loaded-file ghci-state)))
                  (if (s-equals? loaded-file fname) "loaded" (format "loaded(%s)" (file-name-base loaded-file)))))
               ;; (`(,hd . ,_tl) (format "%s" hd))
               (other (format "%s" other)))
             (if-let ((queue (dante-check-ghci-state/queue ghci-state)))
                 (format "+%s" (length queue))
               "")))))
    (error (format "error %s" err))))

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
  (cond
    ((and (fboundp #'haskell-ts-mode)
          (fboundp #'treesit-language-available-p)
          (treesit-language-available-p 'haskell))
     (with-temp-buffer
       (insert expression)
       (delay-mode-hooks
         (haskell-ts-mode)
         (font-lock-mode))
       (font-lock-ensure)
       (buffer-string)))
    ((fboundp #'haskell-mode)
     ;; From https://github.com/lunaryorn/ansible-doc.el/blob/master/ansible-doc.el#L211
     ;; See also http://emacs.stackexchange.com/a/5408/227
     (with-temp-buffer
       (insert expression)
       (delay-mode-hooks
         (haskell-mode)
         (font-lock-mode))
       (font-lock-ensure)
       (buffer-string)))
    (t
     expression)))

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
  (dante-type-at--with-type-at-point
   (lambda (ty)
     (dante--insert-or-show-fontified ty insert))))

(defun dante-type-at--with-type-at-point (consume)
  (let ((tap (if-let ((thing (dante-thing-at-point t)))
                 (dante--ghc-subexp thing)
               (error "No thing at point"))))
    (lcr-spawn
      (lcr-call dante-async-load-current-buffer nil nil)
      (let ((ty (lcr-call dante-async-call (concat ":type-at " tap))))
        (funcall consume ty)))))

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

(defvar-local dante-temp-fingerprint nil
  "The value of `sha1' of source buffer’s contents when the contents were last loaded.")

(defvar-local dante-interpreted nil)

(defsubst dante-reset-temp-fingerprint! ()
  (setf dante-temp-fingerprint nil))

(defvar dante-original-buffer-map (make-hash-table :test 'equal)
  "Hash table from (local) temp file names to the file they originate.")

(lcr-def dante-async-load-current-buffer (interpret err-fn)
  "Load and maybe INTERPRET the temp file for current buffer.
Interpreting puts all symbols from the current module in
scope.  Compiling avoids re-interpreting the dependencies over
and over."
  (let* ((curr-buf (current-buffer))
         (resolved-buf (resolve-to-base-buffer curr-buf))
         (fingerprint (sha1 curr-buf))
         (unchanged (equal fingerprint dante-temp-fingerprint))
         ;; Make sure to not use indirect buffer’s filename (which is typically nil)
         ;; or we’ll overwrite the base buffer and trigger its revert!
         (src-fname (buffer-file-name resolved-buf))
         (ghci-buf (lcr-call dante-session))
         (ghci-state (dante-get-ghci-state ghci-buf))
         (same-target (and (or dante-interpreted (not interpret))
                           (s-equals? (dante-check-ghci-state/loaded-file ghci-state)
                                      src-fname))))
    (if (and unchanged same-target) ; see #52
        (dante-check-ghci-state/load-message ghci-state)
      (let ((fname (lcr-call dante-check--get-file-to-load src-fname curr-buf)))
        (setq dante-temp-fingerprint fingerprint)
        (setq dante-interpreted interpret)
        (puthash (dante-local-name fname) src-fname dante-original-buffer-map)
        ;; GHCi will interpret the buffer if both -fbyte-code and :l * are used.
        (lcr-call dante-async-call (if interpret ":set -fbyte-code" ":set -fobject-code"))
        (with-current-buffer ghci-buf
          (setq-local dante-status 'loading)
          (dante-async-write (if same-target
                                 ":r!"
                               (concat ":l! "
                                       (if interpret "*" "")
                                       (dante-local-name fname))))
          (cl-destructuring-bind (_status err-messages _loaded-modules)
              (lcr-call dante-load-loop ghci-buf "" err-fn)
            (setf (dante-check-ghci-state/loaded-file ghci-state)
                  src-fname
                  (dante-check-ghci-state/load-message ghci-state)
                  (let ((ansi-color-context nil))
                    (--map (-map #'ansi-color-apply it) err-messages)))))))))

(defun dante-local-name (fname)
  "Local name of FNAME on the remote host."
  (string-remove-prefix (or (file-remote-p fname) "") fname))

;;;;;;;;;;;;;;;;;;;;;
;; Flycheck checker

(defvar dante-check-force-interpret nil
  "Make dante load any module into ghci as bytecode. Typically has to be used if
module was already loaded as object code but hasn’t changed since so subsequent loads
won’t have any effect (ghci’s recompilation avoidance will make it skip doing extra work).")

(defun dante-check (checker cont)
  "Run a check with CHECKER and pass the status onto CONT."
  (if (eq (awhen (dante-get-ghci-state) (dante-check-ghci-state/checker-state it)) 'dead)
      (funcall cont 'interrupted)
    (lcr-spawn
      (let* ((messages (lcr-call dante-async-load-current-buffer dante-check-force-interpret nil))
             ;; todo: map current buffer to base buffer if it’s an indirect one?
             (temp-file (dante-local-name (dante-get-filename-to-load (current-buffer)))))
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
  :start #'dante-check
  :predicate (lambda () dante-mode)
  :modes '(haskell-mode haskell-ts-mode haskell-hsc-mode haskell-literate-mode)
  :working-directory (lambda (_checker)
                       (dante-config/project-root (dante-get-config))))

(add-to-list 'flycheck-checkers 'haskell-dante)

(defcustom dante-flycheck-types
  '(("^warning:\\(?: \\[GHC-[0-9]+\\]\\)? \\[-W\\(?:typed-holes\\|deferred-\\(?:type-errors\\|out-of-scope-variables\\)\\)\\]" . error)
    ("^warning" . warning)
    ("^splicing " . nil)
    ("" . error))
  "Map of regular expressions to flycheck error types, ordered by priority."
  :group 'dante :type '(repeat cons (regex symbol)))

(defun dante-fly-message (matched checker buf temp-file)
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
          (let ((buf-name (dante-buffer-file-name-for-error-message buf)))
            (flycheck-error-new-at (car location) (cadr location) type
                                   (replace-regexp-in-string (regexp-quote temp-file)
                                                             (car buf-name)
                                                             (concat fixed-err-type
                                                                     "\n"
                                                                     (trim-whitespace-right msg)))
                                   :checker checker
                                   :buffer buf
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

(defun dante-call-in-buffer (buf func &rest args)
  "In BUFFER, call FUNC with ARGS."
  (with-current-buffer buf (apply func args)))

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
     (let ((bounds (haskell-misc--bounds-of-symbol-impl t -1 nil nil)))
       (when (and dante-mode (not (dante--in-a-comment)) bounds)
         (let* ((id-start (car bounds))
                (_ (save-excursion (re-search-backward "import[\t ]*" (line-beginning-position) t)))
                (import-end (match-end 0))
                (import-start (match-beginning 0))
                (is-import (eq import-end id-start)))
           (buffer-substring-no-properties (if is-import import-start id-start) (point))))))
    (candidates
     (unless (eq (awhen (dante-get-ghci-state) (dante-check-ghci-state/checker-state it)) 'dead)
       (cons :async (lambda (callback) (lcr-spawn (lcr-halt callback (lcr-call dante-complete arg)))))))))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'dante-company))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source buffer operations

(defun dante-thing-at-point (&optional include-parens)
  "Return (START . END) the indent at point, or the region if it is active."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let ((bounds (bounds-of-qualified-haskell-symbol)))
      (if (and include-parens
               bounds)
          (let* ((start (car bounds))
                 (end (cdr bounds))
                 (has-parens? (and (eq ?\( (char-before start))
                                   (eq ?\) (char-after end)))))
            (cons (if has-parens?
                      (- start 1)
                    start)
                  (if has-parens?
                      (+ end 1)
                    end)))
        bounds))))

(defun dante-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (thing-at-point 'qualified-haskell-symbol))

(defun dante-buffer-file-name-for-error-message (&optional buf)
  "Call function `buffer-file-name' for BUFFER and clean its result.
The path returned is canonicalized and stripped of any text properties."
  (if-let (name (buffer-file-name buf))
      (let ((path (dante-canonicalize-path (substring-no-properties name))))
        (cons path path))
    (cons (buffer-name buf) nil)))

(defvar dante-check--get-file-to-load--impl #'dante-check--get-file-to-load--regular-file)

(lcr-def dante-check--get-file-to-load (src-fname buf)
  "Prepare filename SRC-FNAME, visited in buffer BUF, to load into
underlying ghci session. Return the filename that should actually be loaded,
which may be different from SRC-FNAME if e.g. preprocessing was performed."
  (lcr-funcall dante-check--get-file-to-load--impl src-fname buf))

(lcr-def dante-check--get-file-to-load--regular-file (src-fname buf)
  "Prepare to load into GHCi a regular Haskell file."
  (let ((fname (dante-get-filename-to-load--default-impl buf))
        (cfg (dante-get-config)))

    (lcr-call dante--preprocess-project-if-needed
              cfg
              (dante-config/build-dir cfg))

    ;; Take care not to overwrite original buffer needlessly if we’re
    ;; calling this function from a remote buffer or base buffer will be
    ;; auto-reverted and, for example, will lose all its flycheck error overlays.
    (unless (s-equals? src-fname fname)
      ;; Set `noninteractive' to suppress messages from `write-region'.
      (let ((noninteractive t))
        (with-current-buffer buf
          (write-region nil nil fname nil 0))))
    fname))

(lcr-def dante--preprocess-project-if-needed (cfg package-build-dir)
  (when (dante-method/make-preprocess-command-line (dante-config/method cfg))
    (let* ((src-dirs (let* ((component
                             (dante-config/cabal-component cfg))
                            (cabal-file-dir
                             (strip-trailing-slash
                              (file-name-directory
                               (cabal-component/cabal-file component)))))
                       (--map (concat cabal-file-dir "/" it)
                              (cabal-component/source-dirs component))))

           (component-build-dir (dante-get-component-build-dir cfg package-build-dir))

           (needs-preprocessing? (null (file-directory-p component-build-dir)))
           (already-preprocessed-files (unless needs-preprocessing?
                                         (find-rec* :root component-build-dir
                                                    :globs-to-find '("*.hs")
                                                    :relative-paths t)))
           (already-preprocessed-trie
            (let ((tr (make-empty-trie)))
              (dolist (x already-preprocessed-files)
                (trie-insert! (reverse (file-name-sans-extension x)) x tr))
              tr))
           (sources (find-rec-multi* :roots src-dirs
                                     :globs-to-find (eval-when-compile
                                                      (--map (concat "*." it)
                                                             +haskell-preprocessing-extensions+)))))

      (unless needs-preprocessing?
        (setf needs-preprocessing?
              (catch 'found
                (dolist (src sources)
                  (when-let ((preprocessed (trie-matches-string-suffix? already-preprocessed-trie
                                                                        (file-name-sans-extension src))))
                    (when (dante--is-file-newer-than? src (concat component-build-dir "/" preprocessed))
                      (throw 'found t))))
                nil)))

      (when needs-preprocessing?
        (lcr-call dante--preprocess-current-project!
                  cfg
                  package-build-dir)))))

(lcr-def dante--preprocess-current-project! (cfg build-dir)
  (cl-assert (dante-method/make-preprocess-command-line (dante-config/method cfg)))
  (let* ((default-directory (dante-config/project-root cfg))
         (cmdline (funcall (dante-method/make-preprocess-command-line (dante-config/method cfg))
                           cfg
                           build-dir)))
    (with-fresh-buffer-no-switch tmp-buf (get-buffer-create " *dante-cabal-preprocessing*")
      (let ((exit-code (lcr-call lcr-call-process-async
                                 (list
                                  :name "dante-preprocessing"
                                  :buffer tmp-buf
                                  :command cmdline
                                  :noquery t))))
        (unless (zerop exit-code)
          (let ((sep "\n--------------------------------\n"))
            (error "Preprocessing of current project failed with exit code %s:%s%s%s%s"
                   exit-code
                   sep
                   cmdline
                   sep
                   (with-current-buffer tmp-buf
                     (buffer-substring-no-properties (point-min) (point-max))))))))))

(lcr-def dante-check--get-file-to-load--hsc2hs (current-file buf)
  "Prepare to load into GHCi a file that requires preprocessing."
  (let ((preprocessed-file (dante-get-filename-to-load--hsc2hs buf))
        (cfg (dante-get-config)))
    (lcr-call dante--preprocess-project-if-needed cfg (dante-config/build-dir cfg))
    ;; Only check current buffer’s file if it’s stale, faster but
    ;; may miss some situations where we should run preprocessing.
    ;; (when (or (not (file-exists-p preprocessed-file))
    ;;           (dante--is-file-newer-than? current-file preprocessed-file))
    ;;   (lcr-call dante--preprocess-current-project!
    ;;             cfg
    ;;             (dante-config/build-dir cfg)))
    preprocessed-file))

(defun dante--is-file-newer-than? (file-to-test file-to-test-against)
  (let ((to-test-modtime (nth 5 (file-attributes file-to-test)))
        (to-test-against-modtime (nth 5 (file-attributes file-to-test-against))))
    (and (not (equal to-test-against-modtime to-test-modtime))
         (time-less-p to-test-against-modtime to-test-modtime))))

(require 'tramp)
(defun dante-tramp-make-tramp-temp-file (buf)
  "Create a temporary file for BUFFER, perhaps on a remote host."
  (if-let (fname (buffer-file-name buf))
      (let ((suffix (file-name-extension fname t)))
        (if (file-remote-p fname)
            (with-parsed-tramp-file-name (buffer-file-name buf) vec
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

(defvar dante-get-filename-to-load--impl #'dante-get-filename-to-load--default-impl)

(defvar-local dante-get-filename-to-load nil
  "The name of a file whish should actually be loaded instead of buffer’s filename.")

(defun dante-get-filename-to-load (buf)
  (funcall dante-get-filename-to-load--impl buf))

(defun dante-get-filename-to-load--default-impl (buf)
  "Return a (possibly remote) filename suitable to store BUFFER's contents."
  (with-current-buffer buf
    (or dante-get-filename-to-load
        (setq dante-get-filename-to-load
              (dante-tramp-make-tramp-temp-file buf)))))

(defun dante-get-component-build-dir (cfg package-build-dir)
  (let ((component (dante-config/cabal-component cfg)))
    (cl-assert (or (cabal-component-p component)
                   (null component)))
    (when component
      (let ((build-dir (cabal-component/build-dir component)))
        (concat (aif package-build-dir
                    it
                  (concat
                   (aif (dante-config/eproj-root cfg)
                       (concat (strip-trailing-slash it) "/")
                     "")
                   "dist-newstyle"))
                "/"
                build-dir)))))

(defun dante-get-filename-to-load--hsc2hs (buf)
  "Return filename where cabal would put result of preprocessing BUFFER’s file."
  (with-current-buffer buf
    (or dante-get-filename-to-load
        (let* ((cfg (dante-get-config buf))
               (component-build-dir
                (dante-get-component-build-dir cfg (dante-config/build-dir cfg))))
          (if component-build-dir
              (setq-local dante-get-filename-to-load
                          (concat
                           component-build-dir
                           "/"
                           (replace-regexp-in-string "[.]"
                                                     "/"
                                                     (treesit-haskell-get-buffer-module-name))
                           ".hs"))
            (error "hsc2hs does not support #! scripts"))))))

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
  (cl-assert (not (null reg)))
  (save-restriction
    (widen)
    (let ((beg (car reg))
          (end (cdr reg)))
      (format "%S %d %d %d %d %s"
              (dante-local-name (dante-get-filename-to-load (current-buffer)))
              (line-number-at-pos beg)
              (dante--ghc-column-number-at-pos beg)
              (line-number-at-pos end)
              (dante--ghc-column-number-at-pos end)
              (buffer-substring-no-properties beg end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GHCi process communication

(defun dante-destroy ()
  "Stop GHCi and kill its associated process buffer."
  (interactive)
  (when-let ((ghci-buf (dante-buffer-p)))
    (dante--set-checker-state! 'deleting ghci-buf)
    (when-let ((process (get-buffer-process ghci-buf)))
      (kill-process process)
      (delete-process process))
    (kill-buffer ghci-buf)))

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
  (if-let* ((ghci-buf (dante-buffer-p)))
      (if (buffer-local-value 'lcr-process-callback ghci-buf)
          (lcr-context-switch
              (let ((ghci-state (dante-get-ghci-state ghci-buf)))
                (when (dante-check-ghci-state/queue ghci-state)
                  (message "Overriding previously queued GHCi request."))
                (setf (dante-check-ghci-state/queue ghci-state)
                      (cons (lambda (x) (lcr-resume cont x)) nil))))
        (funcall cont ghci-buf))
    (dante-start cont)))

(defcustom dante-load-flags '("+c" "-fdiagnostics-color=never" "-fno-diagnostics-show-caret" "-Wwarn=missing-home-modules" "-ferror-spans" "-Wwarn")
  "Flags to set whenever GHCi is started."
  :type (cons 'set (--map (list 'const :tag (concat (car it) ": " (cadr it)) (car it))
                          '(("+c" "Gather type information (necessary for `dante-type-at')")
                            ("-Wall" "Report all warnings")
                            ("-ferror-spans" "Report span in error messages (used in flymake only)")
                            ("-fdefer-typed-holes" "Accept typed holes, so that completion/type-at continues to work then.")
                            ("-Wwarn=missing-home-modules" "Do not error-out if a module is missing in .cabal file")
                            ("-fdiagnostics-color=never" "No color codes in error messages (color codes will trigger bugs in Dante)")
                            ("-fno-diagnostics-show-caret" "Cleaner error messages for GHC >=8.2 (ignored by earlier versions)")))))

(defun dante--make-file-process (name buf program args filter sentinel)
  (let ((fh (find-file-name-handler default-directory 'dante--make-file-process)))
    (if fh
        (apply fh #'make-process :name name :buffer buf :command (cons program args) :filter filter :sentinel sentinel :file-handler t)
      (apply #'make-process :name name :buffer buf :command (cons program args) :filter filter :sentinel sentinel :file-handler t))))

(lcr-def dante-start ()
  "Start a GHCi process and return its buffer."
  (let* ((ghci-buf (dante--create-ghci-interaction-buffer))
         (cfg (dante-get-config))
         (args (funcall (dante-method/make-check-command-line (dante-config/method cfg))
                        cfg))
         (ghc-initialising? t)
         (initial-ghc-messages nil)
         (vanilla-filter (lcr-process-make-filter ghci-buf))
         (process (with-current-buffer ghci-buf
                    (message "Dante: Starting GHCi: %s" (combine-and-quote-strings args))
                    (make-process :name "dante"
                                  :buffer ghci-buf
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
                                                                   initial-ghc-messages)))
                                  :file-handler t))))
    (with-current-buffer ghci-buf
      (erase-buffer)
      (setf (dante-check-ghci-state/command-line dante--ghci-state)
            (process-command process)))
    (dante--set-checker-state! 'starting ghci-buf)
    (lcr-call dante-async-call
              (s-join "\n" (--map (concat ":set " it)
                                  (append dante-load-flags
                                          (list "prompt \"\\4%s|\""
                                                ;; Empty continuation prompt so that output
                                                ;; of :{ will be correctly identified.
                                                "prompt-cont \"\""
                                                "-ignore-dot-ghci")))))
    (let ((dir (fold-platform-os-type (lcr-call dante-async-call ":!pwd")
                                      (lcr-call dante-async-call "System.IO.putStrLn =<< System.Directory.getCurrentDirectory"))))
      (with-current-buffer ghci-buf
        (setf (dante-check-ghci-state/ghci-path dante--ghci-state)
              (fold-platform-os-type
               dir
               (msys-or-cygwin-file-name-to-emacs dir)))))
    (dante--set-checker-state! 'started ghci-buf)
    (setf ghc-initialising? nil
          initial-ghc-messages nil)
    ghci-buf))

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

(defun dante-regexp-disjoin (&rest regexps)
  "Return a regexp matching any of REGEXPS."
  (mapconcat (lambda (x) (concat "\\(?:" x "\\)")) regexps "\\|"))

(defconst dante-ghci-prompt "\4\\(?1:.*\\)|")

(defconst dante-progress-regexp
  "^\\[[0-9]* of [0-9]*\\] Compiling \\(?2:[^ \n]*\\).*")

(defconst dante-success-regexp
  (eval-when-compile
    (concat
     "^\\(?3:"
     (dante-regexp-disjoin
      "Ok, modules \\(?:re\\)?loaded:[ ]*[^\n ]*\\(?: (.*)\\)?"
      "Ok, .*modules \\(?:re\\)?loaded" ;; .* stands for a number in english (two, three, ...) (GHC 8.2)
      "Ok, one module \\(?:re\\)?loaded")
     "\\.\\)")))

(defconst dante-error-regexp
  "^\\(?:\e\\\[[0-9;]*m\\)?\\(?4:\\(?:[a-zA-Z]:\\)?[^ \n:][^:\n\r]+\\):\\(?5:[0-9()-:]+\\): \\(?6:.*\\)\n\\(?7:\\(?:[ ]+.*\r?\n\\)*\\)")

(lcr-def dante-load-loop (ghci-buf str err-fn)
  "Parse the output of load command.
ACC umulate input and ERR-MSGS."
  (save-match-data
    (let (result
          cur-file
          err-msgs
          (acc str)
          (ghci-state (dante-get-ghci-state ghci-buf)))
      (while (not result)
        (let* ((i (string-match (eval-when-compile
                                  (dante-regexp-disjoin dante-ghci-prompt
                                                        dante-progress-regexp
                                                        dante-success-regexp
                                                        dante-error-regexp))
                                acc))
               (rest (when i (substring acc (match-end 0)))))
          (cond ((and i
                      (match-beginning 1))
                 ;; (and m
                 ;;      (string-match dante-ghci-prompt m))
                 (setf (dante-check-ghci-state/checker-state ghci-state)
                       (list 'ghc-err (pcase (dante-check-ghci-state/checker-state ghci-state)
                                        (`(compiling ,module) (ansi-color-apply module))
                                        (_ cur-file)))) ; when the module name is wrong, ghc does not output any "Compiling ..." message
                 (setq result (list 'failed (nreverse err-msgs) (match-string 1 acc))))

                ((and i
                      (match-beginning 2))
                 (setf (dante-check-ghci-state/checker-state ghci-state)
                       (list 'compiling (match-string 2 acc))))

                ((and i
                      (match-beginning 3))
                 ;; With the +c setting, GHC (8.2) prints:
                 ;; 1. error messages+warnings, if compiling only
                 ;; 2. if successful, repeat the warnings
                 (setf (dante-check-ghci-state/checker-state ghci-state)
                       'process-warnings)
                 (cl-destructuring-bind (_status warning-msgs loaded-mods) (lcr-call dante-load-loop ghci-buf rest nil)
                   (setf (dante-check-ghci-state/checker-state ghci-state)
                         (list 'loaded loaded-mods)
                         result
                         (list 'ok (or (nreverse err-msgs) warning-msgs) loaded-mods))))

                ((and i
                      (not (zerop (length rest)))
                      (/= (aref rest 0) ?\s)) ;; make sure we're matching a full error message
                 (when (match-beginning 4)
                   (let* ((file (match-string 4 acc))
                          (err-msg (list file
                                         (match-string 5 acc)
                                         (match-string 6 acc)
                                         (match-string 7 acc))))
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
          (matched nil)
          (ansi-color-context nil))
      (save-match-data
        (while (not matched)
          (setq acc (concat acc (lcr-call dante-async-read)))
          (setq matched (string-match dante-ghci-prompt acc)))
        (ansi-color-apply (trim-whitespace-right (substring acc 0 (1- (match-beginning 1)))))))))

(defun dante-sentinel (process change initial-ghc-messages)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (let ((ghci-buf (process-buffer process)))
    (when (and (buffer-live-p ghci-buf)
               (not (process-live-p process)))
      (let ((ghci-state (dante-get-ghci-state ghci-buf)))
        (if (eq (dante-check-ghci-state/checker-state ghci-state) 'deleting)
            (message "GHCi process deleted.")
          (progn
            (setf (dante-check-ghci-state/checker-state ghci-state) 'dead)
            (let ((init-msg (join-lines (reverse (--drop-while (string= "" it) initial-ghc-messages)))))
              (dante-show-process-problem process change init-msg))))))))

(defun dante-diagnose ()
  "Show all state info in a help buffer."
  (interactive)
  (let ((info (dante-debug-info (dante-buffer-p) nil)))
    (with-help-window (help-buffer)
      (princ info))))

(defun dante-debug-info (buf init-msg)
  "Show debug info for dante buffer BUFFER."
  (if buf
      (with-current-buffer buf
        (s-join "\n"
                (cons (and init-msg
                           (concat "GHCi output\n" init-msg))
                      (cons (concat "dante-command-line " (s-join " " (dante-check-ghci-state/command-line (dante-get-ghci-state))))
                            (--map (format "%s %S" it (eval it))
                                   '(default-directory (dante-get-config) (dante-get-ghci-state) lcr-process-callback))))))
    "No GHCi interaction buffer"))

(defun dante-show-process-problem (process change init-msg)
  "Report to the user that PROCESS reported CHANGE, causing it to end."
  (let ((sep "\n--------------------------------\n"))
    (message "Dante GHCi process failed! Further details in ‘%s’.%sDirectory: ‘%s’%s%s%s"
             (process-buffer process)
             sep
             (buffer-local-value 'default-directory (process-buffer process))
             sep
             (join-lines (process-command process) " ")
             (if init-msg
                 (concat sep (trim-whitespace-right init-msg))
               "")))
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert "\n---\n\n"
            "Process state change: " change "\n"
            (dante-debug-info (current-buffer) init-msg))))

(defun dante-buffer-name (cfg)
  "Create a dante process buffer name."
  (funcall (dante-method/repl-buf-name-func (dante-config/method cfg)) cfg))

(defun dante-buffer-name--default (cfg)
  "Default implementation for getting name of the GHCi interaction buffer.
First, if needed, initialize the GHCi invokation method.  Then construct the
appropriate buffer name on this basis."
  (concat " *dante#" (dante-config/cabal-target cfg) "#" (dante-config/project-root cfg) "*"))

(defun dante--create-ghci-interaction-buffer ()
  "Create the buffer for GHCi interaction."
  (let ((cfg (dante-get-config)))
    (with-current-buffer (get-buffer-create (dante-buffer-name cfg))
      (cd (dante-config/project-root cfg))
      (fundamental-mode) ;; this has several effects, including resetting the local variables
      (buffer-disable-undo)
      ;; Important to transfer all variables confnigured by ‘dante--make-config’
      ;; to this new buffer so that functions like ‘dante-buffer-name’ will pick them up
      ;; instead of trying to initialize method once again from within GHCI buffer, which
      ;; would fail since it doesn’t have file associated.
      ;;
      ;; Most of these settings are included into buffer name anyway so different
      ;; buffers obviously have different values of the following parameters.
      (setq-local dante--config cfg
                  dante--ghci-state
                  (make-dante-check-ghci-state
                   :ghci-path nil
                   :command-line nil
                   :load-message nil
                   :loaded-file "<DANTE:NO-FILE-LOADED>"
                   :queue nil
                   :checker-state nil))
      (current-buffer))))

(defun dante--set-checker-state! (state &optional ghci-buf)
  "Set the `dante--config' to STATE."
  (setf (dante-check-ghci-state/checker-state (dante-get-ghci-state ghci-buf))
        state))

(defun dante-buffer-p ()
  "Return the GHCi buffer if it exists, nil otherwise."
  (get-buffer (dante-buffer-name (dante-get-config))))

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
  (if-let ((thing (dante-thing-at-point)))
      (dante--ghc-subexp thing)
    (error "No thing at point")))

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
           (expand-file-name file (dante-check-ghci-state/ghci-path (dante-get-ghci-state))))
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
                   (res (lcr-call dante-async-call cmd)))
              (save-excursion
                (delete-region (point)
                               ;; look for: empty comment line, next command or end of block.
                               (or (and (search-forward-regexp " *-- *\\( >>>\\|$\\)" block-end t 1)
                                        (match-beginning 0))
                                   block-end)))
              (insert (apply #'concat (--map (concat "-- " it "\n") (--remove (s-blank? it) (s-lines res))))))
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
  (dante--set-checker-state! 'running)
  (lcr-spawn
    (lcr-call dante-async-load-current-buffer nil nil)
    (message "%s" (lcr-call dante-async-call command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake

(defun dante-flymake (report-fn &rest _args)
  "Run a check and pass the status onto REPORT-FN."
  (let* ((src-buffer (current-buffer))
         (ghci-buf (dante-buffer-p))
         (temp-file (dante-local-name (dante-get-filename-to-load src-buffer)))
         (nothing-done t)
         ;; flymake raises errors when any report is made using an
         ;; "old" call to the backend. However, we must deal with all
         ;; GHCi output, so we must let the loop run to completion. So
         ;; we simply disable messages if another call to this
         ;; function is detected.  This token must be set before any
         ;; context switch occurs, otherwise we cannot detect if we
         ;; got another call from flymake in between.
         (local-token (if ghci-buf
                          (with-current-buffer ghci-buf
                            (setq dante-flymake-token (1+ dante-flymake-token)))
                        dante-flymake-token)))
    (if (eq (awhen (dante-get-ghci-state ghci-buf) (dante-check-ghci-state/checker-state it)) 'dead)
        (funcall report-fn :panic :explanation "Ghci is dead")
      (lcr-spawn
        (let* ((ghci-buf (lcr-call dante-session)) ; yield until GHCi is ready to process the request
               (token-guard (lambda ()
                              (eq (buffer-local-value 'dante-flymake-token ghci-buf) local-token)))
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

(defun dante-fm-message (matched buf temp-file)
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
                  (`(,l1 ,c1 ,l2 ,c2) (cons (dante-pos-at-line-col buf l1 c1) (dante-pos-at-line-col buf (or l2 l1) (1+ c2))))
                  (`(,l ,c) (flymake-diag-region buf l c)))))
        (when r
          (cl-destructuring-bind (type msg-first-line) type-analysis
            (let* ((final-msg (trim-whitespace (concat msg-first-line "\n" (replace-regexp-in-string "^    " "" msg)))))
              (flymake-make-diagnostic buf (car r) (cdr r) type final-msg))))))))

(provide 'dante)

;;; dante.el ends here
