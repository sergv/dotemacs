
;; eshell customization

(eval-after-load
 "eshell"
 '(progn
   (require 'em-prompt)
   (require 'em-term)
   (require 'em-cmpl)
   (require 'esh-ext)
   (require 'common)
   (require 'more-scheme)
   (require 'browse-kill-ring-setup)
   (require 'search-prop)
   ;; (setenv "PAGER" "cat")

   ;; try eshell-complex-commands if some command doesn't work

   (setf eshell-cmpl-cycle-completions t
         eshell-cmpl-dir-ignore
         "\\`\\(?:\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\|\\.hg\\|_darcs\\|\\.bzr\\)/\\'")

   ;; commands to run in separate buffer
   (push "ssh" eshell-visual-commands)
   (push "iotop" eshell-visual-commands)
   ;; (add-to-list 'eshell-visual-commands "tail")

   (labels ((define-programs (programs regexp)
              (loop
                for p in programs
                appending (list (cons p regexp)
                                (cons (concat (char->string
                                               eshell-explicit-command-char)
                                              p)
                                      regexp)))))
     (setf eshell-command-completions-alist
           (append (list
                    '("ar" . "\\.[ao]\\'"))

                   (define-programs '("acroread" " pdf")
                     "\\.pdf\\'")
                   (define-programs '("okular")
                     "\\.\\(?:pdf\\|djvu\\|ps\\)\\'")

                   (define-programs '("gcc" "g++" "cc" "CC" "acc" "bcc")
                       (rx "."
                           (regexp "[CcHh]")
                           (? (or (regexp "[CcHh]")
                                  (= 2 (regexp "[Pp]"))
                                  (= 2 (regexp "[Xx]"))
                                  (= 2 "+")))
                           eot))

                   (define-programs '("readelf" "objdump" "nm")
                     "\\(?:\\`[^.]*\\|\\.\\(?:[ao]\\|so\\)\\)\\'")

                   (define-programs '("gdb" "dbx" "sdb" "adb")
                     "\\`\\(?:[^.]*\\|a\\.out\\)\\'")


                   (define-programs '("tar" "untar")
                     "\\(?:\\.tar\\|\\.tgz\\|\\.tar\\.\\(?:gz\\|bz2\\)\\)\\'")

                   (define-programs '("ghc" "ghci")
                     "\\(?:\\.hs\\|\\.lhs\\|\\.hsc\\)\\'")

                   (define-programs '("sbcl" "clisp" "ecl" "ccl")
                       (eval `(rx (or ,@+common-lisp-file-extensions+)
                                  eot)))

                   (define-programs '("stalin" "guile" "csi" "csc" "scheme48" "bigloo")
                       (eval `(rx (or ,@+scheme-file-extensions+) eot)))

                   (define-programs '("python" "pypy" "python2.7" "python3" "ipython")
                     "py\\'")

                   (define-programs '("makeinfo" "texi2dvi" "texi2pdf")
                     "texi\\'")

                   '(("gunzip" . "gz\\'")))))

   ;; redefine some eshell functions

   ;; this is quite similar to original eshell function but
   ;; here the prompt is made intangible and a field so that
   ;; movement let alone editing would not affect it
   (redefun eshell-emit-prompt ()
     "Emit a prompt if eshell is being used interactively."
     (run-hooks 'eshell-before-prompt-hook)
     (if (not eshell-prompt-function)
       (set-marker eshell-last-output-end (point))
       (let ((prompt (funcall eshell-prompt-function)))
         (and eshell-highlight-prompt
              (add-text-properties 0 (length prompt)
                                   '(read-only t
                                     face eshell-prompt
                                     ;; these three properties transform this
                                     ;; piece of text into real prompt
                                     ;; which is plainly convenient to use
                                     intangible t
                                     rear-nonsticky t
                                     field 'prompt)
                                   prompt))
         (eshell-interactive-print prompt)))
     (run-hooks 'eshell-after-prompt-hook))

   ;; some eshell aliases

   (defun eshell/cleanup ()
     "Clean up current directory from various build files etc."
     (let ((files (directory-files "."
                                   ;; use absolute names to be "absolutely" sure
                                   t
                                   (rx "."
                                       (or "fasl"
                                           "o"
                                           "hi"
                                           "prof"
                                           "aux"
                                           "hp"
                                           ;; mostly texinfo gargabe
                                           "cp"
                                           "cps"
                                           "fn"
                                           "ky"
                                           "log"
                                           "pg"
                                           "toc"
                                           "tp"
                                           "vr")
                                       eol)
                                   t)))
       (if (< 0 (length files))
         (eshell/rm files)
         (eshell/echo "No files to cleanup found"))))

   (defun eshell/clear ()
     (interactive)
     (save-excursion
      (let ((inhibit-read-only t))
        (forward-line -1)
        (delete-region (point-min) (line-end-position))
        (delete-char 1))))

   ;; convenient commands
   (defun eshell-clear-prompt ()
     "Clear eshell prompt from input"
     (interactive)
     (eshell-bol)
     (delete-region (point) (line-end-position)))

   ;; (vimmize-motion eshell-bol)

   ;; property-based jumps, work better than regexp ones
   (define-circular-prompt-property-jumps
    eshell-jump-to-next-prompt
    eshell-jump-to-prev-prompt
    'face
    'eshell-prompt
    :init
    (unless (string-match-p "*eshell*\\(?:<[0-9]+>\\)?" (buffer-name))
      (error "Not in the eshell buffer"))
    :move-to-property-end t)

   ;; plain regex jumps
   ;; (define-circular-jumps
   ;;  eshell-jump-to-next-prompt
   ;;  eshell-jump-to-prev-prompt
   ;;  eshell-prompt-regexp
   ;;  (unless (string-match-p "*eshell*\\(?:<[0-9]+>\\)?" (buffer-name))
   ;;    (error "Not in the eshell buffer")))

   (defun eshell-setup ()
     (init-repl :show-directory t)

     (setq autopair-dont-activate t)
     (linum-mode t)

     (setf vim:normal-mode-local-keymap           (make-keymap)
           vim:insert-mode-local-keymap           (make-keymap)
           vim:operator-pending-mode-local-keymap (make-keymap)
           vim:motion-mode-local-keymap           (make-keymap))

     (def-keys-for-map2 vim:normal-mode-local-keymap
       ("C-<up>"   eshell-jump-to-prev-prompt)
       ("C-<down>" eshell-jump-to-next-prompt)
       ("S-<up>"   eshell-jump-to-prev-prompt)
       ("S-<down>" eshell-jump-to-next-prompt)

       ("M-p"      browse-kill-ring)
       ("M-P"      browse-eshell-input-history)
       ("M-/"      pcomplete)
       ;; clear all previous output
       ("SPC SPC"  eshell-clear-prompt)
       ("C-SPC"    eshell/clear)
       ;; ("S-SPC"   eshell/clear)
       )

     ;; (def-keys-for-map2 (vim:normal-mode-local-keymap
     ;;                     vim:operator-pending-mode-local-keymap
     ;;                     vim:motion-mode-local-keymap)
     ;;   ("^" vim:eshell-bol))

     (def-keys-for-map2 vim:insert-mode-local-keymap
       ("C-<up>"   eshell-jump-to-prev-prompt)
       ("C-<down>" eshell-jump-to-next-prompt)
       ("S-<up>"   eshell-jump-to-prev-prompt)
       ("S-<down>" eshell-jump-to-next-prompt)

       ("C-SPC"   eshell/clear))

     (def-keys-for-map2 eshell-mode-map
       ("C-<up>"   eshell-jump-to-prev-prompt)
       ("C-<down>" eshell-jump-to-next-prompt)
       ("S-<up>"   eshell-jump-to-prev-prompt)
       ("S-<down>" eshell-jump-to-next-prompt)

       ("M-p"      browse-kill-ring)
       ("M-P"      browse-eshell-input-history)
       ("C-d"      eshell-send-eof-to-process)
       ("M-/"      pcomplete)

       ("C-SPC"    eshell/clear)
       ;; ("S-SPC"    eshell/clear)
       ))

   (add-hook 'eshell-mode-hook #'eshell-setup)))


