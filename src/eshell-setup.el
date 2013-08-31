;; eshell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

;; eshell customization

(eval-when-compile (require 'cl-lib))

(setf eshell-directory-name (concat +prog-data-path+ "/eshell/")
      eshell-aliases-file (concat eshell-directory-name "/alias")
      eshell-buffer-maximum-lines 65536
      eshell-buffer-shorthand t
      eshell-cmpl-autolist nil ;; this option is really harmful when set to t
      eshell-cmpl-cycle-completions t
      eshell-cmpl-expand-before-complete nil
      eshell-cmpl-ignore-case t
      eshell-cmpl-recexact nil
      eshell-cmpl-cycle-cutoff-length 3
      eshell-show-lisp-completions nil
      eshell-command-interpreter-max-length 4096
      eshell-error-if-no-glob t
      eshell-glob-include-dot-dot nil
      eshell-hist-ignoredups t
      eshell-history-size 20000
      eshell-password-prompt-regexp "[Pp]ass\\(?:word\\|phrase\\).*:\\s *\\'"
      eshell-prefer-lisp-functions t
      eshell-scroll-show-maximum-output nil
      eshell-scroll-to-bottom-on-input nil
      eshell-scroll-to-bottom-on-output nil
      eshell-send-direct-to-subprocesses nil
      eshell-tar-regexp "\\.t\\(?:ar\\(?:\\.\\(?:gz\\|bz2\\|Z\\|7z\\)\\)?\\|gz\\|a[zZ]\\|z2\\|7z\\)\\'")

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
     (require 'completion-setup)
     (require 'search-prop)
     ;; (setenv "PAGER" "cat")

     ;; try eshell-complex-commands if some command doesn't work

     (setf eshell-cmpl-file-ignore "~\\'\\|\\`\\.#"
           eshell-cmpl-dir-ignore
           "\\`\\(?:\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\|\\.hg\\|_darcs\\|\\.bzr\\)/\\'")

     ;; commands to run in separate buffer
     (push "ssh" eshell-visual-commands)
     (push "iotop" eshell-visual-commands)
     ;; (add-to-list 'eshell-visual-commands "tail")

     (let ((define-programs
             (lambda (programs regexp)
               (loop
                 for p in programs
                 appending (list (cons p regexp)
                                 (cons (concat (char->string
                                                eshell-explicit-command-char)
                                               p)
                                       regexp))))))
       (setf eshell-command-completions-alist
             (append (funcall define-programs '("acroread" "pdf")
                              "\\.pdf\\'")
                     (funcall define-programs '("okular")
                              "\\.\\(?:pdf\\|djvu\\|ps\\)\\'")

                     (funcall define-programs '("gcc" "g++" "cc" "CC" "acc" "bcc" "tcc")
                              (rx "."
                                  (or (seq
                                       (regexp "[CcHh]")
                                       (? (or (regexp "[CcHh]")
                                              (= 2 (regexp "[Pp]"))
                                              (= 2 (regexp "[Xx]"))
                                              (= 2 "+"))))
                                      "o"
                                      "a"
                                      "so")
                                  eot))

                     (funcall define-programs '("readelf" "objdump" "nm")
                              "\\(?:\\`[^.]*\\|\\.\\(?:[ao]\\|so\\)\\)\\'")

                     (funcall define-programs '("gdb" "dbx" "sdb" "adb")
                              "\\`\\(?:[^.]*\\|a\\.out\\)\\'")

                     (funcall define-programs '("tar" "untar")
                              (rx (or ".tgz"
                                      ".t7z"
                                      (seq ".tbz"
                                           (? "2"))
                                      (seq ".tar"
                                           (? "."
                                              (or "gz"
                                                  "bz2"
                                                  "7z"))))
                                  eos))

                     (funcall define-programs '("ghc" "ghci")
                              "\\(?:\\.hs\\|\\.lhs\\|\\.hsc\\)\\'")

                     (funcall define-programs '("stalin" "guile" "csi" "csc" "scheme48" "bigloo")
                              (eval `(rx "." (or ,@+scheme-file-extensions+) eot)))

                     (funcall define-programs '("python" "pypy" "python2.7" "python3" "ipython")
                              "\\.py\\'")

                     (funcall define-programs '("makeinfo" "texi2dvi" "texi2pdf")
                              "\\.texi\\'")

                     `(("gunzip" . "\\.gz\\'")
                       ("ar" . "\\.[ao]\\'")))))

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
         (with-inhibited-read-only
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

     (defun eshell-setup ()
       (init-repl :show-directory t :bind-return nil :create-keymaps t)

       (setq autopair-dont-activate t)
       (linum-mode t)

       (def-keys-for-map vim:normal-mode-local-keymap
         ("C-<up>"   eshell-jump-to-prev-prompt)
         ("C-<down>" eshell-jump-to-next-prompt)
         ("S-<up>"   eshell-jump-to-prev-prompt)
         ("S-<down>" eshell-jump-to-next-prompt)

         ("M-p"      browse-kill-ring)
         ("C-M-p"    browse-eshell-input-history)
         ("M-/"      pcomplete)
         ;; clear all previous output
         ("SPC SPC"  eshell-clear-prompt)
         ("C-SPC"    eshell/clear)
         ;; ("S-SPC"   eshell/clear)
         )

       ;; (def-keys-for-map (vim:normal-mode-local-keymap
       ;;                     vim:operator-pending-mode-local-keymap
       ;;                     vim:motion-mode-local-keymap)
       ;;   ("^" vim:eshell-bol))

       (def-keys-for-map vim:insert-mode-local-keymap
         ("C-<up>"   eshell-jump-to-prev-prompt)
         ("C-<down>" eshell-jump-to-next-prompt)
         ("S-<up>"   eshell-jump-to-prev-prompt)
         ("S-<down>" eshell-jump-to-next-prompt)

         ("C-SPC"   eshell/clear))

       (def-keys-for-map eshell-mode-map
         ("C-<up>"   eshell-jump-to-prev-prompt)
         ("C-<down>" eshell-jump-to-next-prompt)
         ("S-<up>"   eshell-jump-to-prev-prompt)
         ("S-<down>" eshell-jump-to-next-prompt)

         ("M-p"      browse-kill-ring)
         ("C-M-p"    browse-eshell-input-history)
         ("C-d"      eshell-send-eof-to-process)
         ("M-/"      pcomplete)

         ("C-SPC"    eshell/clear)
         ;; ("S-SPC"    eshell/clear)
         )

       (def-keys-for-map (vim:normal-mode-local-keymap
                          vim:insert-mode-local-keymap)
         ("<return>"   eshell-send-input)
         ("C-<return>" sp-newline)))

     (add-hook 'eshell-mode-hook #'eshell-setup)))

(provide 'eshell-setup)

;; Local Variables:
;; End:

;; eshell-setup.el ends here
