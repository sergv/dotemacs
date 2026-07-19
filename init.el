;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Added by Package.el. Keep commented, don’t delete.
;; (package-initialize)

;; 1e4   recursion depth is quite safe since:
;; 1.6e4 still works
;; 2e4   makes emacs crash
(setq max-lisp-eval-depth 1000)

;; speeds up startup time considerably, worth to use
(setq gc-cons-threshold (* 4 1024 1024)
      gc-cons-percentage 0.001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Customizations done by emacs customize routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables)

(provide 'custom-variables-defined)

(defalias 'strip-trailing-slash 'directory-file-name)

;; Invariant: init.el is always located at the root of emacs
;; configuration directory.
;;
;; This is the same as ‘user-emacs-directory’ when we’re running
;; but may be different when we’re compiling so use the init.el’s location
;; as the final source of truth.
(defconst +emacs-config-path+
  (strip-trailing-slash (file-name-directory load-file-name))
  "Path to root for my emacs configuration. For things than are read
but typically not written.

Usually either ~/.emacs.d or unique path under /nix/store.")

(defvar +platform+
  (let ((sys-type-env (getenv "EMACS_SYSTEM_TYPE")))
    (cond
      (sys-type-env
       (read sys-type-env))
      ((eq system-type 'windows-nt)
       '(windows work))
      ((memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
       '(linux home))
      (t
       '(linux home))))
  "List of the form (<os> <use>), <os> may be 'linux or 'windows.")

(unless (and (listp +platform+)
             (memq (car +platform+)
                   '(linux windows)))
  (error "+platform+'s os %s should be one of 'linux or 'windows"
         (car +platform+)))

(defmacro when-windows (&rest body)
  (let ((os-type (car +platform+)))
    (when (eq os-type 'windows)
      `(progn
         ,@body))))

;; Recursively find directories containing elisp files starting at
;; ROOT. Omit directories whose absolute path matches IGNORED-DIR-RE.
(defun find-elisp-dirs (root &optional ignored-dirs-re)
  (let ((standard-ignored-dirs-re
         (rx
          (or ".git"
              ".github"
              ".cask"
              "test"
              "tests"))))
    (let ((dirs nil))
      (letrec ((collect-dirs
                (lambda (path rel-path)
                  (when
                      (and (file-directory-p path)
                           (not
                            (string-match-p standard-ignored-dirs-re
                                            (file-name-nondirectory (strip-trailing-slash path)))))
                    (let ((has-elisp-files? nil))
                      (dolist (entry (directory-files path
                                                      nil ;; produce relative names
                                                      directory-files-no-dot-files-regexp
                                                      t ;; don't sort
                                                      ))
                        (let ((p (concat path "/" entry)))
                          (if (file-regular-p p)
                              (when (string-suffix-p ".el" entry)
                                (setf has-elisp-files? t))
                            (funcall collect-dirs p (concat rel-path "/" entry)))))
                      (when (and has-elisp-files?
                                 (or (null ignored-dirs-re)
                                     (not (string-match-p ignored-dirs-re path))))
                        (push rel-path dirs)))))))
        (funcall collect-dirs root "")
        dirs))))

(let ((skip-elc? (equal (getenv "EMACS_SKIP_ELC") "1")))

  (dolist (dir
           (list "compiled"
                 (unless skip-elc?
                   "compiled/elc")
                 "lib"
                 "src"
                 "src/custom"))
    (when dir
      (let ((full-dir (concat +emacs-config-path+ "/" dir)))
        (unless (file-directory-p full-dir)
          (error "Important config directory does not exist: %s" full-dir)))))

  (setf
   load-path
   (nconc
    (append
     (mapcar
      (lambda (x) (directory-file-name (concat +emacs-config-path+ "/" x)))
      (delq nil
            (list "compiled"
                  (unless skip-elc?
                    "compiled/elc")
                  "lib")))
     (mapcar
      (lambda (x) (directory-file-name (concat +emacs-config-path+ "/src" x)))
      (eval-when-compile
        (find-elisp-dirs
         (concat (file-name-directory (or load-file-name
                                          (and (boundp 'byte-compile-current-file)
                                               byte-compile-current-file)
                                          buffer-file-name))
                 ;; +emacs-config-path+
                 "/src"))))
     (mapcar
      (lambda (x) (directory-file-name (concat +emacs-config-path+ "/third-party" x)))
      (eval-when-compile
        (find-elisp-dirs
         (concat (file-name-directory (or load-file-name
                                          (and (boundp 'byte-compile-current-file)
                                               byte-compile-current-file)
                                          buffer-file-name))
                 ;; +emacs-config-path+
                 "/third-party")
         ;; Ignored third-party dirs.
         (rx
          (or
           (seq bow (or "tests" "doc" "examples" ".cask" ".stack-work.*") eol)
           (seq (* anything) "/test" (? "s"))
           "auctex/tests"
           "auctex/style"
           "clojure-mode/test"
           "company-mode/test"
           "dash.el/dev"
           (seq "f.el/" (or "bin" "test"))
           "flycheck-haskell/test"
           (seq "flycheck/" (or ".cask" "maint" "test"))
           "ivy/targets"
           "groovy-mode/test"
           (seq "haskell-mode/" (or "doc/gifcasts" "tests" "tests/compat"))
           "ht/test"
           "js2-mode/tests"
           "kotlin-ts-mode/test"
           "lua-mode/test"
           "lsp-mode/test"
           "magit/t"
           "markdown-mode/tests"
           "markdown-mode/scripts"
           "nix-ts-mode/test"
           (seq "org-mode/" (or "mk" "testing"))
           "pkg-info/test"
           "s.el/dev"
           "treepy.el/test"
           "transient/test"
           "yafolding.el/features")))))
     (when-windows
      (list (directory-file-name (concat +emacs-config-path+ "/native/fakecygpty")))))
    load-path))

  ;; By default Emacs will use (concat +emacs-config-path+ "/tree-sitter") here
  ;; so no extra configuration is needed.
  ;; (add-to-list 'treesit-extra-load-path (concat +emacs-config-path+ "/lib"))

  ;; (let ((compiled-dir (concat emacs-dir "/compiled")))
  ;;   (unless (equal (car native-comp-eln-load-path) compiled-dir)
  ;;     (startup-redirect-eln-cache compiled-dir)))

  ;; Sometimes there are extra entries, remove them all except the last one.
  (when (boundp 'native-comp-eln-load-path)
    (while (cdr native-comp-eln-load-path)
      (setf native-comp-eln-load-path (cdr native-comp-eln-load-path)))
    (push (directory-file-name (concat +emacs-config-path+ "/compiled/eln"))
          native-comp-eln-load-path)))

;; Must load this every time so that e.g. temporary directory
;; will get reinitialized anew. Or fresh ~/.bashrc will be picked up.
(load-library "start")
