;;; scheme-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'general-lisp-setup)
(require 'slime-setup) ;; for indentation
(require 'scheme-highlight)
(require 'scheme-abbrev+)
(require 'comint-setup)
(require 'more-scheme)
(require 'common)

(define-common-lisp-style "scheme"
  "Custom indent style for scheme."
  (:inherit "modern")
  (:variables
   (lisp-indent-maximum-backtracking 10)
   (lisp-align-keywords-in-calls t)
   (lisp-loop-indent-subclauses nil)
   (lisp-lambda-list-keyword-parameter-indentation 0)

   (indent-tabs-mode nil)
   (comment-fill-column nil))

  (:indentation
   (if             (4 4 4))
   (aif            (as if))
   (define         (nil &body))
   (define-macro   (as define))
   (define-syntax  (as define))
   (define-method  (as define))
   (define-generic (as define))
   (module         (nil nil 0))
   (syntax-rules   (as define))
   ;; guile-specific
   (lambda*        (as lambda))
   (define*        (as define))
   (letrec         (as let))
   (let-values     (as let))
   (let*-values    (as let))))


(defconst +scheme-implementations+
  (remove nil
          (list
           (when (executable-find "csi") ;; Chicken Scheme
             `(chicken
               (command
                ;; case-sensitive, no banner,
                ;; full numeric tower,
                ;; suffix: keywords:
                ,(concat "csi -q -R numbers -keyword-style suffix " ;; "-r5rs-syntax "
                         +prog-data-path+
                         "/chicken-init.scm"
                         " -:c "))))

           (when (executable-find "bigloo") ;; Bigloo
             `(bigloo
               (command
                ,(concat "bigloo -glines -gerror-localization -Wall -load "
                         +prog-data-path+
                         "/bigloo-init.scm"))))

           (when (executable-find "scheme48")
             `(scheme48
               (command "scheme48")))

           ;; gauche
           (when (executable-find "gosh")
             `(gauche
               ;; -i - interactive mode
               (command
                ,(concat "gosh -i -l "
                         +prog-data-path+
                         "/gauche-init.scm"))))

           (when (executable-find "mit-scheme")
             `(mit-scheme
               (command
                ,(concat "mit-scheme "
                         "--interactive "))))

           ;; gambit
           ;; -:<options>
           ;; hN - maximum heap size in N kilobytes, set to 1024 Mb
           ;; s  - select standart scheme mode - case-insensetevie + no keywords
           ;; S  - select gambit scheme mode   - case sensetive + keywords
           ;; -d<debugging options>
           ;;     a     - uncaught exceptions will be treated as errors
           ;;             in all threads
           ;;     R     - when a user interrupt occurs a new REPL will be started
           ;;     [0-9] - verbosity level
           ;;     -     - the REPL interaction channel will be standard
           ;;             input and standard output
           (let ((command-args
                   (format " -:h1048576,S,daR1- -e \"(load \\\"%s\\\")\" -"
                           (concat +prog-data-path+
                                   "/gambit-init.scm"))))
             (cond
               ((executable-find "gsc")
                `(gambit
                  (command
                   ,(concat "gsc"
                            command-args))))
               ((executable-find "gsi")
                `(gambit
                  (command
                   ,(concat "gsi"
                            command-args))))
               (else
                nil)))

           (when (executable-find "guile")
             `(guile
               (command
                ,(concat "guile -l "
                         +prog-data-path+
                         "/guile-init.scm"))))

           (when (executable-find "racket")
             `(racket
               (command
                ,(mapconcat
                  'identity
                  (list "racket"
                        "--repl")
                  " "))))))
  "List of scheme implementation records providing name, command to run etc")


(setf scheme-program-name
      (if (null? +scheme-implementations+)
        ""
        (assoc-value 'command (cdr (car +scheme-implementations+)))))
(setf quack-default-program scheme-program-name)

(require 'quack)
(require 'scheme-complete)

(eval-after-load
 'quack
 '(begin
   (redefun quack-run-scheme-prompt ()
     (let* ((last    (car quack-run-scheme-prompt-history))
            (default-implementation (caar +scheme-implementations+))
            (default-name (symbol->string
                           default-implementation))
            (default (assoc-value 'command
                                  (assq default-implementation
                                        +scheme-implementations+))
                     ;; (or (and quack-run-scheme-prompt-defaults-to-last-p
                     ;;          last)
                     ;;     quack-default-program
                     ;;     scheme-program-name
                     ;;     last
                     ;;     "mzscheme")
                     )
            (program-name (let ((minibuffer-allow-text-properties nil))
                            (completing-read-vanilla
                             (concat "Scheme implementation "
                                     (if default
                                       (format "(default %S) "
                                               default-name)
                                       ""))
                             (mapcar (lambda (entry)
                                       (symbol->string
                                        (car entry)))
                                     +scheme-implementations+)
                             nil                              ;; predicate
                             nil                              ;; require-match
                             nil                              ;; initial-input
                             'quack-run-scheme-prompt-history ;; history
                             default-name)))
            (program (or (assoc-value 'command
                                      (assoc (string->symbol program-name)
                                             +scheme-implementations+))
                         default)))
       (quack-remember-program-maybe program)
       program))

   ;; this just removes annoying quack warnings on scheme-mode startup
   (redefun quack-shared-mode-hookfunc-stuff ()
     ;; Install the Quack keymap and menu items.
     (local-set-key quack-scheme-mode-keymap-prefix quack-scheme-mode-keymap)
     (quack-when-xemacs
      (when (featurep 'menubar)
        ;;(set-buffer-menubar current-menubar)
        ;; TODO: For XEmacs, we could have two versions of this menu -- the popup
        ;;       one would have the Global submenu, but the menubar one would have
        ;;       the Global submenu only if quack-global-menu-p were nil.
        (add-submenu nil quack-scheme-mode-menuspec)
        (set-menubar-dirty-flag)
        (setq mode-popup-menu quack-scheme-mode-menuspec)))

     ;; Bind the paren-matching keys.
     (local-set-key ")" 'quack-insert-closing-paren)
     (local-set-key "]" 'quack-insert-closing-bracket)

     (local-set-key "(" 'quack-insert-opening-paren)
     (local-set-key "[" 'quack-insert-opening-bracket)

     ;; Steal any find-file bindings.
     (when quack-remap-find-file-bindings-p
       (quack-locally-steal-key-bindings 'find-file     'quack-find-file)
       (quack-locally-steal-key-bindings 'ido-find-file 'quack-find-file))

     ;; Fight against tabs.
     (when quack-tabs-are-evil-p
       (setq indent-tabs-mode nil))

     ;; Remove character compositions, to get rid of any pretty-lambda.  (Note:
     ;; This is bad, if it turns out compositions are used for other purposes in
     ;; buffers that are edited with Scheme Mode.)
     (when quack-pretty-lambda-supported-p
       (eval '(decompose-region (point-min) (point-max))))

     ;; Install fontification
     (when quack-fontify-style
       (quack-install-fontification))

     ;; Die! Die! Die!
     (quack-when-xemacs
      (quack-install-global-menu)))))

(defun scheme-describe-current-symbol ()
  "Give some help on symbol under point if possible."
  (interactive)
  ;; do not put this message in the global log
  (let ((message-log-max nil))
    (let ((info (scheme-get-current-symbol-info)))
      (if (< 0 (length info))
        (message info)
        ;; plain message will do as well... what a hard choice!
        (error "no information")))))


(defun scheme-setup ()
  (lisp-setup)
  (scheme-highlight)
  (common-lisp-set-style "scheme")

  ;; (set (make-local-variable 'eldoc-documentation-function)
  ;;      'scheme-get-current-symbol-info)
  ;; (eldoc-mode 1)
  ;; (set (make-local-variable 'eldoc-idle-delay) 0.01)

  (set (make-local-variable 'lisp-indent-function)
       #'common-lisp-indent-function)
  ;; Fix this to recognize scheme keywords as well
  (set (make-local-variable 'lisp-indent-lambda-list-keywords-regexp)
       (rx (or "&"
               "#:"
               ":"
               "#!")
           (or "optional"
               "rest"
               "key"
               "allow-other-keys"
               "aux"
               "whole"
               "body"
               "environment"
               "more")
           symbol-end))

  (def-keys-for-map vim:normal-mode-local-keymap
    (", e"     scheme-send-last-sexp)
    (", E"     scheme-send-last-sexp)
    (", d"     scheme-describe-current-symbol)
    ("SPC SPC" switch-to-scheme-repl)
    ("j"       scheme-send-last-sexp)
    ("J"       scheme-send-last-sexp))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<f1>"    scheme-load-current-file)
    ("M-/"     scheme-smart-complete))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("j"       scheme-send-region)
    ("J"       scheme-send-region))

  (scheme-abbrev+-setup))


(define-switch-to-interpreter
    switch-to-scheme-repl
  ("*scheme*")
  (run-scheme scheme-program-name)
  :doc "Pop to scheme repl."
  :save-buffer t
  :error-msg "Can't switch to Scheme repl")

;; define scheme-load-current-file
(cond
  ((eq? +platform+ 'netbook-linux)
   (defvar *scheme-tmp-file* (format "/tmp/scheme%s.scm" (emacs-pid))
     "Temporary file to save Scheme buffers for futher loading.")

   (defun scheme-load-current-file (&optional noswitch)
     (interactive "P")
     (write-region (point-min) (point-max) *scheme-tmp-file*)
     ;; suppres write-region's message
     (message "")
     (let ((file-name *scheme-tmp-file*)
           (proc (scheme-proc)))
       (setf scheme-prev-l/c-dir/file
             (cons (file-name-directory    file-name)
                   (file-name-nondirectory file-name)))
       (comint-send-string proc (concat "(load \""
                                        file-name
                                        "\"\)\n"))
       (unless noswitch
         (switch-to-scheme-repl)))))
  ((eq? +platform+ 'asus-netbook)
   (defun scheme-load-current-file (&optional switch)
     (interactive "P")
     (when (buffer-modified-p)
       (save-buffer))

     (let ((file-name (buffer-file-name))
           (proc (scheme-proc)))
       (setf scheme-prev-l/c-dir/file
             (cons (file-name-directory    file-name)
                   (file-name-nondirectory file-name)))
       (comint-send-string proc (concat "(load \""
                                        file-name
                                        "\"\)\n"))
       (when switch
         (switch-to-scheme-repl)))))
  ((eq? +platform+ 'home-linux)
   (defun scheme-load-current-file (&optional noswitch)
     (interactive "P")
     (when (buffer-modified-p)
       (save-buffer))

     (let ((file-name (buffer-file-name))
           (proc (scheme-proc)))
       (setf scheme-prev-l/c-dir/file
             (cons (file-name-directory    file-name)
                   (file-name-nondirectory file-name)))
       (comint-send-string proc (concat "(load \""
                                        file-name
                                        "\"\)\n"))
       (unless noswitch
         (switch-to-scheme-repl)))))
  (else
   (error "invalid +platform+: %s" +platform+)))


;;;;

(defconst +scheme-prompt-regexp+
  ">+ *")

(define-circular-jumps
    scheme-repl-next-prompt
  scheme-repl-prev-prompt
  (concat "^" +scheme-prompt-regexp+)
  (unless (string= (buffer-name) "*scheme*")
    (error "Not in the scheme buffer")))

(defun scheme-repl-setup ()
  (lisp-repl-setup)
  (scheme-highlight)

  ;; this sexp deals with semibuggy case when repl returns only "> " output,
  ;; with no newlines which causes emacs to stack prompts like this "> > > "
  ;;
  ;; so we check that there are some lines around and output is a
  ;; prompt without newlines so we add newlines to the output
  (add-hook 'comint-preoutput-filter-functions
            (let ((prev-output nil))
              (lambda (output)
                (let ((new-output
                        (if (and
                             (not (= 0
                                     (count-lines (point-min)
                                                  (point-max))))
                             (string-match-pure?
                              (concat "\\`"
                                      +scheme-prompt-regexp+
                                      "\\'")
                              output)
                             (not (null? prev-output))
                             ;; last character is not newline
                             (not (char= (string->char "\n")
                                         (aref prev-output
                                               (- (length prev-output) 1)))))
                          (concat "\n" output)
                          output)))
                  (setf prev-output new-output)
                  new-output)))
            nil ;; put at the start
            t   ;; local
            )

  ;; this is a dirty hack to avoid unpleasant fontification of
  ;; interaction history
  ;; (set (make-local-variable 'face-remapping-alist)
  ;;      '((font-lock-comment-face)))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("C-<up>"   scheme-repl-prev-prompt)
    ("C-<down>" scheme-repl-next-prompt)
    ("S-<up>"   scheme-repl-prev-prompt)
    ("S-<down>" scheme-repl-next-prompt)))


(provide 'scheme-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; scheme-setup.el ends here
