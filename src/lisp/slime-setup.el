;;; slime-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'common)
(require 'more-scheme)
(require 'general-lisp-setup)

;; slime path
(add-to-list 'load-path +slime-path+)
;; chicken slime
(add-to-list 'load-path (concat +slime-path+
                                "/chicken-scheme"))
(require 'chicken-slime)
(add-to-list 'load-path (concat +slime-path+
                                "/mit-scheme"))
(require 'mit-scheme-slime)

;; currently broken
;; (add-to-list 'load-path (concat +slime-path+
;;                                 "/gambit-scheme"))
;; (require 'gambit-scheme-slime)

(setq inferior-lisp-program "sbcl")

(defvar *slime-registered-lisps*
  `((sbcl
     (type common-lisp)
     (run-command
      ("sbcl" "--noinform"
              ;; reserve some stack for plain recursive functions
              "--control-stack-size"
              "100"
              ;; limit heap usage to 2Gb
              "--dynamic-space-size"
              ,(if (eq +platform+ 'home-linux)
                 "2048"
                 "1024")
              "--core"
              ,(concat +slime-path+
                       "/sbcl-swank-core")))
     (init
      (lambda (port-file _)
        (format "(swank:start-server %S)\n" port-file))))

    (sbcl-full
     (type common-lisp)
     (run-command
      ("sbcl" "--noinform"
              ;; reserve some stack for plain recursive functions
              "--control-stack-size"
              "100"
              ;; limit heap usage to 2Gb
              "--dynamic-space-size"
              ,(if (eq +platform+ 'home-linux)
                 "2048"
                 "1024")
              "--core"
              ,(concat +slime-path+
                       "/sbcl-swank-core-full")))
     (init
      (lambda (port-file _)
        (format "(swank:start-server %S)\n" port-file))))

    (clisp
     (type common-lisp)
     (run-command
      ("clisp"
       ;; "-m" "100Mb"
       ;; "--verbose"
       ;; "-disable-readline"
       "-ansi"
       "-M"
       ,(concat +slime-path+
                "/clisp-swank-core.mem"))))

    (ecl
     (type common-lisp)
     (run-command
      ("ecl"
       "--frame-stack"
       "100000" ;; amount of frames
       "--c-stack"
       "53687091200")))

    (ccl
     (type common-lisp)
     (run-command
      ("/home/sergey/projects/lisp/implementations/cl/ccl-1.7/lx86cl64"
       "--image-name"
       ,(concat +slime-path+
                "/ccl-swank-core"))))

    (chicken
     (type scheme)
     (run-command
      ("csi"
       "-q"
       "-R"
       "numbers"
       "-r5rs-syntax"
       "-include-path"
       ,(concat +slime-path+
                "/chicken-slime")
       "-:c"))
     (init
      chicken-slime-init))

    (mit-scheme
     (type scheme)
     (run-command
      ("mit-scheme"
       ;; "--emacs"
       ;; "--interactive"
       ))
     (init
      mit-scheme-init))

    ;; currently broken
    ;; (gambit-scheme
    ;;  (type scheme)
    ;;  (run-command
    ;;   ("gsc"
    ;;    "-:d-"))
    ;;  (init
    ;;   gambit-scheme-init))
    ))

;; for other SLIME do following:
;;  shell$ sbcl
;; * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
;; * (load ".../slime/swank-loader.lisp")
;; * (swank-loader:dump-image "sbcl.core-with-swank")
;; OR:
;; shell$ cd "/home/sergey/emacs/slime-*"
;; shell$ ./sbcl-make-image.lisp

(setf slime-lisp-implementations
      (mapcar
       (lambda (entry)
         (append (list (car entry)
                       (assoc-value 'run-command (cdr entry)))
                 (let ((init (assoc-value 'init (cdr entry))))
                   (if init
                     (list ':init init)
                     nil))))
       *slime-registered-lisps*)

      slime-use-autodoc-mode nil)

(require 'slime)

(slime-setup '(slime-repl
               slime-parse
               slime-fontifying-fu
               slime-indentation

               ;; slime-fancy-inspector
               slime-fuzzy
               slime-banner))

;; ********************************

(defun default-lisp-indent (path state indent-point sexp-column normal-indent)
  (lisp-indent-function indent-point state))

(define-common-lisp-style "my-style"
    "My custom indentation style, very similar to modern one."
  (:inherit "modern")
  (:variables
   (lisp-indent-maximum-backtracking 10)
   (lisp-align-keywords-in-calls t)
   (lisp-loop-indent-subclauses nil)
   (lisp-lambda-list-keyword-parameter-indentation 0)

   (indent-tabs-mode nil)
   (comment-fill-column nil))

  (:indentation
   (if (4 2 2))
   (setf (nil))
   (setq (as setf))
   (defpackage (4 &rest (&whole 2 &rest nil)))
   (bind (as let))
   (bind* (as let*))
   (define-foreign-library (4 &rest 2))
   (begin (as progn))
   (labels ((&whole 2 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))))

(define-common-lisp-style "clisp"
    "Indentation style used in CLISP sources."
  (:inherit "modern")
  (:variables
   (lisp-indent-maximum-backtracking 10)
   (lisp-align-keywords-in-calls t)
   (lisp-loop-indent-subclauses nil)
   (lisp-lambda-list-keyword-parameter-indentation 0)

   (indent-tabs-mode nil)
   (comment-fill-column nil))

  (:indentation
   (and (&rest 2))
   (appease-cerrors (&rest 2))
   (assert (&rest 2))
   (block (4 &rest 2))                                    ;; default
   (case (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (catch (4 &rest 2))                                    ;; default
   (ccase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (check-type (2 2 &rest 2))
   (compiler-let ((&whole 4 &rest (&whole 1 1 2)) &body)) ;; default
   (cond (&rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (ctypecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (decf (2 2))
   (declaim (&rest 2))
   (declare (&rest 2))
   (def-c-enum (4 &rest 2))
   (def-c-struct (4 &rest 2))
   (defclass (10 (&whole 10 1 &rest 1) &rest (&whole 2 &rest 1)))
   (defconstant (4 2 2))                                  ;; default
   (defgeneric (4 (&whole 4 1 &rest 1) &body))
   (define-condition (18 (&whole 18 1 &rest 1) &rest (&whole 2 &rest 1)))
   (define-modify-macro (4 (&whole 4 1 &rest 1) 4 &body))
   (define-setf-expander (4 (&whole 4 1 &rest 1) &body))
   (define-setf-method (4 (&whole 4 1 &rest 1) &body))
   (define-symbol-macro (4 &body))
   (definternational (4 &body))
   (deflanguage (4))
   (deflocalized (4 4 &body))
   (defmacro (4 (&whole 4 1 &rest 1) &body))
   (defmethod lisp-indent-defmethod)                      ;; default
   (defpackage (4 &rest 2))
   (defparameter (4 2 2))                                 ;; default
   ;; FIXME: How to deal with both short and long forms of defsetf?
   ;;(defsetf (4 (&whole 4 1 &rest 1) 2 &body))
   ;;(defsetf (14 (&whole 14 1 &rest 1) (&whole 14 1 &rest 1) &body))
   (defstruct ((&whole 4 &rest (&whole 2 &rest 1)) &rest (&whole 2 &rest 1))) ;; default
   (deftype (9 (&whole 9 1 &rest 1) &body))
   (defun (7 (&whole 7 1 &rest 1) &body))
   (defvar (4 2 2))                                       ;; default
   (destructuring-bind ((&whole 6 1 &rest 1) 4 &body))
   (deutsch (2 1 2 1 2))
   (do lisp-indent-do)                                    ;; default
   (do* lisp-indent-do)                                   ;; default
   (do-all-symbols ((&whole 4 1 &rest 1) &body))
   (do-external-symbols ((&whole 4 1 &rest 1) &body))
   (do-symbols ((&whole 4 1 &rest 1) &body))
   (dohash ((&whole 4 1 &rest 1) (&whole 4 1 &rest 1) &body))
   (dolist ((&whole 4 1 1) &body))
   (doseq ((&whole 4 1 1) &body))
   (dotimes ((&whole 4 1 1) &body))
   (ecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (english (2 1 2 1 2))
   (etypecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (eval-when (4 &body))                                  ;; default
   (exit-on-error (&body))
   (fcase '(6 4 &rest (&whole 2 &rest 1)))
   (flet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (formatter (&body))
   (francais (2 1 2 1 2))
   (function (&body))
   (generic-flet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (generic-function ((&whole 4 1 &rest 1) &body))
   (generic-labels ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (go (2))
   (handler-bind (2 &body))
   (handler-case (2 &rest (&whole 2 (&whole 4 1 &rest 1) &body)))
   ;; CLISP source indents the two branchs of an 'if' form equally.
   (if (4 2 2))
   (ignore-errors (&body))
   (in-package (&body))
   (incf (2 2))
   (labels ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (lambda ((&whole 4 1 &rest 1) &body))
   (let ((&whole 4 &rest (&whole 1 2 2)) &body))
   (let* ((&whole 4 &rest (&whole 1 2 2)) &body))
   (load-time-value (&body))
   (locally (2 &body))
   ;; CLISP sources don't use the "big" LOOP - its semantics is too unreliable.
   (loop (&body))
   (loop-finish ())
   (macrolet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (:method ((&whole 4 1 &rest 1) &body))          ; for defgeneric etc.
   (muffle-cerrors (&rest 2))
   (multiple-value-bind ((&whole 6 &rest 1) 4 2 &rest 2))
   (multiple-value-call (4 2 &rest 2))
   (multiple-value-list (2))
   (multiple-value-prog1 (2 &rest 2))
   (multiple-value-setq (4 2))                            ;; default
   (nth-value (2 2))
   (optimize (&rest 2))
   (or (&rest 2))
   (pop (2))
   (print-unreadable-object ((&whole 4 1 &rest 1) &body))
   (prog ((&whole 4 1 &rest 1) &rest lisp-indent-tagbody))
   (prog* ((&whole 4 1 &rest 1) &rest lisp-indent-tagbody))
   (prog1 (2 &body))
   (prog2 (2 2 &body))
   (progn (&body))                                        ;; default
   (progv (4 4 &body))                                    ;; default
   (psetf (7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7))
   (psetq (7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7))
   (push (&body))
   (pushnew (&body))
   (quote (2))
   (remf (2 2))
   (restart-bind ((&whole 4 &rest 1) &body))
   (restart-case (4 &rest (&whole 2 (&whole 4 1 &rest 1) &body)))
   (return (&body))                                       ;; default
   (return-from (2 &body))
   (rotatef (&body))
   (setf (6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6))
   (setq (6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6))
   (:shadowing-import-from (4 &rest 2))
   (shiftf (&body))
   (space (2))
   (step (2))
   (symbol-macrolet ((&whole 4 &rest (&whole 1 2 &rest 2)) &body))
   (tagbody lisp-indent-tagbody)                          ;; default
   (the (4 2))
   (the-environment ())
   (throw (4 &body))                                      ;; default
   (time (2))
   (trace (&body))
   (typecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (unless (4 &body))                                     ;; default
   (untrace (&body))
   (unwind-protect (2 &body))
   (when (4 &body))                                       ;; default
   (with-accessors ((&whole 4 1 &rest 1) 4 &body))
   (with-condition-restarts (4 4 &body))
   (with-hash-table-iterator (4 &body))
   (with-input-from-string ((&whole 4 1 &rest 1) &body))
   (with-keyboard (&body))
   (with-open-file ((&whole 4 1 &rest 1) &body))
   (with-open-stream (4 &body))
   (with-output-to-printer ((&whole 4 1 &rest 1) &body))
   (with-output-to-string ((&whole 4 1 &rest 1) &body))
   (with-package-iterator ((&whole 4 1 &rest 1) &body))
   (with-restarts ((&whole 4 &rest (&whole 2 (&whole 4 1 &rest 1) &body)) &body))
   (with-simple-restart ((&whole 4 1 &rest 1) &body))
   (with-slots ((&whole 4 1 &rest 1) 4 &body))
   (with-standard-io-syntax (&body))
   (without-floating-point-underflow (&body))))

;; if we're using netbook linux then it's desirable to use harddrive less,
;; so store our tmp contet it /tmp dir
(when (eq +platform+ 'netbook-linux)
  (make-directory "/tmp/slime-fasls/" t)
  (setf slime-compile-file-options
        '(:fasl-directory "/tmp/slime-fasls/")))

(setf common-lisp-style-default "my-style"
      slime-net-coding-system 'utf-8-unix
      common-lisp-hyperspec-root
      (concat +emacs-documentation-path+ "/HyperSpec/")
      cltl2-root-url
      (concat +emacs-documentation-path+ "/cltl/"))

;;;; once these were common lisp-only, now this incorporates scheme too

;; SLIME is a minor mode and is enabled along with major lisp mode.
;; setting up repl buffer should not interfere with common lisp bindings
(defun eval-with-slime-setup-hook (func)
  (add-hook 'slime-mode-hook
            #'slime-setup)
  (save-window-excursion
   (save-excursion
    (funcall func)))
  (remove-hook 'slime-mode-hook
               #'slime-setup))



(define-switch-to-interpreter
    switch-to-slime
  ("*slime-repl chicken*"
   "*slime-repl mit-scheme*"

   "*slime-repl sbcl*"
   "*slime-repl sbcl-full*"
   "*slime-repl clisp*"
   "*slime-repl ecl*"
   "*slime-repl ccl*"

   "*inferior-lisp*"

   "*slime-repl abcl*"
   "*slime-repl acl*"
   "*slime-repl lispworks*"
   "*slime-repl cmucl*"
   "*slime-repl bigloo*"
   "*slime-repl guile*"
   "*slime-repl lisp*"
   "*slime-repl scheme*")
  (eval-with-slime-setup-hook #'slime)
  :test-if-already-running (slime-connected-p)
  :doc "Pop to slime repl."
  :save-buffer t
  :error-msg "Can't switch to SLIME")

(defun reload-slime ()
  "Reload SLIME process with confirmation from user."
  (interactive)
  (if (y-or-n-p "Reload SLIME? ")
    (eval-with-slime-setup-hook #'slime-restart-inferior-lisp)
    (message "SLIME not reloaded.")))

(vimmize-motion
 slime-repl-bol
 :doc "Move the cursor to the first character after prompt on current line.")

(defun slime-repl-implementation-specific-setup ()
  "Set up syntax highlighting in current buffer depending on
currently chosen lisp implementation."
  ;; WARNING: do not use this function in slime-interpreter-setup
  ;; because call (slime-current-connection) when connection is not
  ;; fully established makes SLIME ignore it and try again ->
  ;; effect similar to infinite loop
  (let* ((current-implementation (string->symbol
                                  (slime-connection-name
                                   (slime-current-connection))))
         (impl-entry (cdr (assoc current-implementation
                                 *slime-registered-lisps*))))
    (when impl-entry
      (case (assoc-value 'type impl-entry)
        (common-lisp
         (slime-redirect-inferior-output)
         (ansi-lisp-highlight-keywords)
         ;; cl has packages and it's useful to track them in prompt
         (set (make-local-variable 'slime-repl-track-package-in-prompt) t))
        (scheme
         ;; copied from scheme.el
         (set (make-local-variable 'font-lock-defaults)
              '((scheme-font-lock-keywords
                 scheme-font-lock-keywords-1 scheme-font-lock-keywords-2)
                nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
                beginning-of-defun
                (font-lock-mark-block-function . mark-defun)
                (font-lock-syntactic-face-function
                 . scheme-font-lock-syntactic-face-function)
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props syntax-table)))
         (set-syntax-table scheme-mode-syntax-table)

         ;; new things, not in scheme.el
         (scheme-highlight)
         ;; scheme has no packages and nothing at the moment that may
         ;; take their place here, so do not track
         (set (make-local-variable 'slime-repl-track-package-in-prompt) nil)
         (setf mode-name "Scheme REPL")
         (when (eq? current-implementation 'mit-scheme)
           (setq slime-find-buffer-package-function
                 'find-mit-scheme-package))
         ;; plain gambit (e.g. not gambit-scheme) is not supposed to
         ;; print anything meaningful to it's standard output
         (unless (eq? current-implementation 'gambit)
           (slime-redirect-inferior-output)))))))

(defun slime-interpreter-setup ()
  "Set up *inferior-lisp* buffer, still usually used by SLIME."
  ;; this is very similar to lisp-interaction setup, but I prefer
  ;; to keep things separate
  (lisp-repl-setup)

  ;; (turn-on-cldoc-mode)
  ;;
  ;; (setq slime-use-autodoc-mode nil)

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap)
    ("<f1>" reload-slime)))

(define-circular-jumps
    slime-repl-next-prompt+
    slime-repl-prev-prompt+
  "^[^ \n\t\r\f\v]*>+ *")

(defun slime-repl-setup ()
  "Set up SLIME REPL buffer."
  ;; this is very similar to slime-setup-interpreter setup, but I prefer
  ;; to keep things separate
  (lisp-repl-setup)
  (slime-repl-implementation-specific-setup)

  ;; (turn-on-cldoc-mode)
  ;;
  ;; (setq slime-use-autodoc-mode nil)

  (def-keys-for-map2 vim:insert-mode-local-keymap
    ("SPC"      slime-space)
    (","        slime-handle-repl-shortcut))

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ("SPC SPC"  slime-repl-clear-prompt)
    ("'"        slime-selector)
    ("S"        slime-selector)

    (", d"      slime-describe-symbol)
    (", h"      slime-hyperspec-lookup)
    (", c"      cltl2-lookup))

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap)
    ("<f1>"        reload-slime)
    ("<return>"    slime-repl-return)
    ("M-."         slime-edit-definition)
    ("M-/"         slime-complete-symbol)
    ("M-:"         slime-interactive-eval)

    ("M-P"         browse-slime-repl-input-history)

    ("<backspace>" paredit-backward-delete)
    ("C-SPC"       slime-repl-clear-buffer)
    ("<down>"      slime-repl-forward-input)
    ("<up>"        slime-repl-backward-input)
    ("C-<down>"    slime-repl-next-prompt+)
    ("S-<down>"    slime-repl-next-prompt+)
    ("C-<up>"      slime-repl-prev-prompt+)
    ("S-<up>"      slime-repl-prev-prompt+))

  ;; (def-keys-for-map2 (vim:normal-mode-local-keymap
  ;;                     vim:operator-pending-mode-local-keymap
  ;;                     vim:motion-mode-local-keymap)
  ;;   ("^" vim:slime-repl-bol))
  )

(defun slime-repl-clear-prompt ()
  "Replace all input at latest prompt with one space."
  (interactive)
  (slime-repl-replace-input " "))



(defun slime-sldb-setup ()
  ;; wrap lines at screen end so that continuation is placed below
  ;; in short: do as my default emacs does
  (setf truncate-lines nil)

  ;; vim-mode is disabled in sldb
  (def-keys-for-map1 sldb-mode-map
    +control-x-prefix+)

  (def-keys-for-map1 sldb-mode-map
    +vim-special-keys+)

  (use-repl-modeline :show-column nil)

  ;; (def-keys-for-map1 sldb-mode-map
  ;;   +vim-word-motion-keys+)

  (def-keys-for-map2 sldb-mode-map
    ("t"        sldb-down)
    ("n"        sldb-up)

    ("<return>" sldb-default-action)
    ("RET"      sldb-default-action)
    ("M-."      sldb-show-source)
    ("C-v"      set-mark-command)
    ("C-y"      copy-region-as-kill)

    ("<f0>"     sldb-invoke-restart-0)
    ("<f1>"     sldb-invoke-restart-1)
    ("<f2>"     sldb-invoke-restart-2)
    ("<f3>"     sldb-invoke-restart-3)
    ("<f4>"     sldb-invoke-restart-4)
    ("<f5>"     sldb-invoke-restart-5)
    ("<f6>"     sldb-invoke-restart-6)))

(defun slime-inspector-setup ()
  ;; vim mode is disabled here
  (def-keys-for-map1 slime-inspector-mode-map
    +control-x-prefix+)
  (def-keys-for-map1 slime-inspector-mode-map
    +vim-special-keys+)

  (use-repl-modeline :show-column nil)

  (def-keys-for-map2 slime-inspector-mode-map
    ("h"       left-char)
    ("t"       next-line)
    ("n"       previous-line)
    ("s"       right-char)

    ("<down>"  slime-inspector-next)
    ("<up>"    slime-inspector-pop)
    ("<left>"  nil)
    ("<right>" nil)
    ("<next>"  scroll-up-commend)
    ("<prior>" scroll-down-commend)

    ("M-."     slime-inspector-show-source)

    ("/"       search-start-forward)
    ("?"       search-start-backward)
    ("k"       search-next)
    ("K"       search-prev)
    ("*"       search-for-symbol-at-point-forward)
    ("#"       search-for-symbol-at-point-backward)
    ("C-h"     search-toggle-highlighting)

    ("r"       slime-inspector-reinspect)
    ("SPC"     slime-inspector-pop)))

(defun slime-fuzzy-completions-setup ()
  (def-keys-for-map1 slime-fuzzy-completions-map
    +control-x-prefix+)
  (def-keys-for-map1 slime-fuzzy-completions-map
    +vim-special-keys+)

  (use-repl-modeline :show-column nil)

  (def-keys-for-map2 slime-fuzzy-completions-map
    ("<escape>" slime-fuzzy-abort)
    ("t"        slime-fuzzy-next)
    ("n"        slime-fuzzy-prev)
    ("["        slime-fuzzy-next)
    ("]"        slime-fuzzy-prev))

  (def-keys-for-map2 slime-target-buffer-fuzzy-completions-map
    ("<return>" slime-fuzzy-select)
    ("RET"      slime-fuzzy-select)
    ("<escape>" slime-fuzzy-abort)
    ("ESC"      slime-fuzzy-abort))

  (dolist (item '((vim:normal-mode-exit slime-fuzzy-abort)
                  (vim:viluas-mode-exit slime-fuzzy-abort)
                  (vim:insert-mode-exit slime-fuzzy-abort)
                  (paredit-newline
                   slime-fuzzy-select-and-process-event-in-target-buffer)
                  (abbrev+-insert-space-or-expand-abbrev
                   slime-fuzzy-select-and-process-event-in-target-buffer)

                  (scroll-up-command slime-fuzzy-next)
                  (scroll-down-command slime-fuzzy-prev)

                  ;; repl-specific bindings
                  (slime-repl-return
                   slime-fuzzy-select-and-process-event-in-target-buffer)
                  (slime-space
                   slime-fuzzy-select-and-process-event-in-target-buffer)
                  (slime-repl-backward-input slime-fuzzy-prev)
                  (slime-repl-forward-input slime-fuzzy-next)))
    (define-key slime-target-buffer-fuzzy-completions-map
        (vector 'remap (first item))
      (second item)))

  (slime-fuzzy-update-keymap-override))

(defun slime-xref-setup ()
  ;; vim-mode is disabled in xref
  (def-keys-for-map1 slime-xref-mode-map
    +control-x-prefix+)

  (def-keys-for-map1 slime-xref-mode-map
    +vim-special-keys+)

  (use-repl-modeline :show-column nil)

  ;; (def-keys-for-map1 sldb-mode-map
  ;;   +vim-word-motion-keys+)

  (def-keys-for-map2 slime-xref-mode-map
    ("ESC"      nil)
    ("M-."      slime-showxrev)
    ("C-v"      set-mark-command)
    ("C-y"      copy-region-as-kill)

    ("t"        slime-xref-next-line)
    ("n"        slime-xref-prev-line)
    ("DEL"      nil)
    ("ESC"      slime-popup-buffer-quit-function)
    ("C-SPC"    slime-popup-buffer-quit-function)))

;;;;

;; (defvar *slime-compilation-invoker* nil
;;   "A buffer where latest `common-lisp-compile-and-load-file' was invoked.")

;; (defun slime-remember-compilation-invoker (begin end)
;;   "Store buffer in variable `*slime-compilation-initiator*' from
;; which `common-lisp-compile-and-load-file' was invoked."
;;   (setf *slime-compilation-invoker* (current-buffer)))


(defvar *slime-before-compilation-window-config* nil
  "Window configuration before the start of compilation.")

(defun slime-remember-window-config (begin end)
  "Store current window configuration in
`*slime-before-compilation-window-config*'."
  (setf *slime-before-compilation-window-config*
        (current-window-configuration)))


(defun slime-kill-compilation-result (notes)
  "Close *slime-compilation* buffer if no compiler notes were produced."
  (unless (= 0 (length notes))
    (let ((buffer (get-buffer (slime-buffer-name :compilation))))
      (when buffer
        ;; don't query about removal
        (remove-buffer buffer)))))

(defun slime-restore-window-config (notes)
  "Restore window configuration before compilation start if no notes
were produced."
  (when (and (= 0 (length notes))
             *slime-before-compilation-window-config*)
    (set-window-configuration *slime-before-compilation-window-config*)
    (setf *slime-before-compiln-window-config* nil)))

;; (push #'slime-remember-compilation-invoker slime-before-compile-functions)
(push #'slime-remember-window-config slime-before-compile-functions)

;; it's not necessary to kill compile buffer, hide is just enough
;; (add-hook 'slime-compilation-finished-hook #'slime-kill-compilation-result t)
(add-hook 'slime-compilation-finished-hook #'slime-restore-window-config t)


;;;; prompt package tracking

(defcustom slime-repl-track-package-in-prompt nil
  "If T then repl prompt will be of the form \"PACKAGE>\",
and \">\" otherwise."
  :type 'boolean
  :group 'slime-repl)

(redefun slime-repl-insert-prompt ()
  "Insert the prompt (before markers!).
Set point after the prompt.
Return the position of the prompt beginning.

If `slime-repl-suppress-prompt' is true, does nothing and returns nil."
  (goto-char slime-repl-input-start-mark)
  (slime-save-marker
   slime-output-start
   (slime-save-marker
    slime-output-end
    (unless (bolp) (insert-before-markers "\n"))
    (let ((prompt-start (point))
          (prompt (if slime-repl-track-package-in-prompt
                    (format "%s> " (slime-lisp-package-prompt-string))
                    "> ")))
      (slime-propertize-region
       '(face slime-repl-prompt-face
         font-lock-face slime-repl-prompt-face
         read-only t
         slime-repl-prompt t
         ;; these three properties transform this
         ;; piece of text into real prompt
         ;; which is plainly convenient to use
         intangible t
         field 'prompt
         rear-nonsticky t
         ;; emacs stuff
         ;; rear-nonsticky was here

         ;; xemacs stuff
         ;; start-open t
         ;; end-open t
         )
       (insert-before-markers prompt))
      ;; (insert-before-markers " ")
      (set-marker slime-repl-prompt-start-mark prompt-start)
      prompt-start))))

;;;; boring redefinitions and "optimizations"

(redefun slime-read-interactive-args ()
  "This is fixed version of `slime-read-interactive-args'."
  (let ((table slime-lisp-implementations))
    (cond ((not current-prefix-arg) (slime-lisp-options))
          (t
           (let ((key (completing-read-vanilla
                       "Choose Lisp: " (mapcar (lambda (x)
                                                 (symbol-name (car x)))
                                               table)
                       nil t)))
             (slime-lookup-lisp-implementation table (intern key)))))))

;; slime-fuzzy keymap override fixes

(defun slime-fuzzy-update-keymap-override ()
  "Basically put entry for `slime-fuzzy-target-buffer-completions-mode' at
the head of `emulation-mode-map-alists'."
  (setf emulation-mode-map-alists
        (remove-if (lambda (x)
                     (and (consp x)
                          (eq (car x)
                              'slime-fuzzy-target-buffer-completions-mode)))
                   emulation-mode-map-alists))
  (add-to-list 'emulation-mode-map-alists
               (cons 'slime-fuzzy-target-buffer-completions-mode
                     slime-target-buffer-fuzzy-completions-map)))

(slime-fuzzy-update-keymap-override)

(setf slime-fuzzy-explanation
      "Flags: boundp fboundp generic-function class macro special-operator package\n\n")

(redefun slime-fuzzy-next ()
  "Moves point directly to the next completion in the completions
buffer."
  (interactive)
  (with-current-buffer (slime-get-fuzzy-buffer)
    (let ((point (next-single-char-property-change (point)
                                                   'completion
                                                   nil
                                                   (1+ slime-fuzzy-last))))
      (set-window-point (get-buffer-window (current-buffer)) point)
      (if (= point (1+ slime-fuzzy-last))
        (goto-char slime-fuzzy-first)
        (goto-char point)))
    (slime-fuzzy-highlight-current-completion)))

(redefun slime-fuzzy-prev ()
  "Moves point directly to the previous completion in the
completions buffer."
  (interactive)
  (assert (< 0 slime-fuzzy-first))
  (with-current-buffer (slime-get-fuzzy-buffer)
    (let ((point (previous-single-char-property-change (point)
                                                       'completion
                                                       nil
                                                       (1- slime-fuzzy-first))))
      (set-window-point (get-buffer-window (current-buffer)) point)
      (if (= point (1- slime-fuzzy-first))
        (goto-char slime-fuzzy-last)
        (goto-char point)))
    (slime-fuzzy-highlight-current-completion)))



;; netbook optimizations to make use of /tmp mapped to RAM memory
;; for loading and compiling files

(when (eq +platform+ 'netbook-linux)

  (defvar *cl-tmp-file* (format "/tmp/cl%s.lisp" (emacs-pid))
    "Temporary file to save Common Lisp buffers for futher loading.")

  (defvar *slime-compilation-invoker* nil
    "Buffer from which `slime-compile-file+' was invoked.")


  (redefun slime-compile-file (&optional load policy)
    (unless buffer-file-name
      (error "Buffer %s is not associated with a file." (buffer-name)))
    (check-parens)
    (setf *slime-compilation-invoker* (current-buffer))
    (write-region (point-min) (point-max) *cl-tmp-file*)
    (message "")
    (run-hook-with-args 'slime-before-compile-functions (point-min) (point-max))
    (let ((file (slime-to-lisp-filename *cl-tmp-file*))
          (options (slime-simplify-plist `(,@slime-compile-file-options
                                           :policy ,policy))))
      (slime-eval-async
       `(swank:compile-file-for-emacs ,file ,(if load t nil)
                                      . ,(slime-hack-quotes options))
       #'slime-compilation-finished)
      (message "Compiling %s..." (buffer-file-name *slime-compilation-invoker*))))


  (redefun slime-change-note-location (loc)
    (subst (buffer-file-name *slime-compilation-invoker*)
           *cl-tmp-file*
           loc
           :test #'(lambda (a b)
                     (when (and (stringp a)
                                (stringp b))
                       (string= a b)))))

  (redefun slime-change-compilation-buffer ()
    (with-current-buffer (slime-buffer-name :compilation)
      (let ((inhibit-read-only t))
        (save-excursion
         (goto-char (point-min))
         (while (search-forward *cl-tmp-file* nil t)
           (replace-match (buffer-file-name *slime-compilation-invoker*)
                          t
                          t))
         (goto-char (point-min))
         (while (search-forward (file-name-nondirectory *cl-tmp-file*) nil t)
           (replace-match (buffer-file-name *slime-compilation-invoker*)
                          t
                          t))))))

  (redefun slime-compilation-finished (result)
    ;; fix file entry in notes list
    (setf (slime-compilation-result.notes result)
          (mapcar #'(lambda (note)
                      (plist-put note
                                 :location
                                 (slime-change-note-location
                                  (plist-get note :location)))
                      note)
                  (slime-compilation-result.notes result)))
    (with-struct (slime-compilation-result. notes duration successp
                                            loadp faslfile)
      result
      (setf slime-last-compilation-result result)
      (slime-show-note-counts notes duration (cond ((not loadp) successp)
                                                   (t (and faslfile successp))))
      (when slime-highlight-compiler-notes
        (slime-highlight-notes notes))
      (run-hook-with-args 'slime-compilation-finished-hook notes)
      (when (and loadp faslfile
                 (or successp
                     (slime-load-failed-fasl-p)))
        (slime-eval-async `(swank:load-file ,faslfile)))
      (unless successp
        (slime-change-compilation-buffer)))))


;;;; misc stuff

(setf *elisp-do-not-move-files*
      (append *elisp-do-not-move-files*
              '("slime-autoloads.el"
                "slime.el"
                "slime-highlight-edits.el"
                "slime-presentation-streams.el"
                "slime-autodoc.el"
                "slime-editing-commands.el"
                "slime-snapshot.el"
                "slime-mrepl.el"
                "slime-mdot-fu.el"
                "slime-banner.el"
                "slime-motd.el"
                "slime-presentations.el"
                "slime-cl-indent.el"
                "slime-scratch.el"
                "slime-compiler-notes-tree.el"
                "slime-parse.el"
                "slime-package-fu.el"
                "slime-fuzzy.el"
                "slime-hyperdoc.el"
                "slime-sprof.el"
                "slime-typeout-frame.el"
                "slime-repl.el"
                "slime-fancy.el"
                "slime-asdf.el"
                "slime-fontifying-fu.el"
                "slime-references.el"
                "slime-xref-browser.el"
                "slime-clipboard.el"
                "slime-c-p-c.el"
                "slime-indentation.el"
                "slime-scheme.el"
                "slime-tramp.el"
                "slime-fancy-inspector.el"
                "inferior-slime.el"
                "bridge.el"
                "slime-enclosing-context.el"
                "slime-sbcl-exts.el"
                "slime-media.el"
                "hyperspec.el")))

(provide 'slime-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; slime-setup.el ends here
