;; eshell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago, restored Monday, 14 September 2020
;; Description:

;; eshell customization

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'set-up-platform)
  (require 'macro-util))

(require 'em-prompt)
(require 'em-term)
(require 'em-cmpl)
(require 'esh-ext)
(require 'search-prop)

(require 'browse-kill-ring-setup)
(require 'common)
(require 'completion-setup)
(require 'el-patch)
(require 'folding-setup)

(require 'eshell-autoload)

(declare-function eshell/echo "em-basic")
(declare-function eshell/rm "em-unix")

;;;###autoload
(el-patch-feature eshell)

(when-emacs-version (< it 30)
  (el-patch-defun eshell-emit-prompt ()
    "Emit a prompt if eshell is being used interactively."
    (when (boundp 'ansi-color-context-region)
      (setq ansi-color-context-region nil))
    (run-hooks 'eshell-before-prompt-hook)
    (if (not eshell-prompt-function)
        (set-marker eshell-last-output-end (point))
      (let ((prompt (funcall eshell-prompt-function)))
        (and eshell-highlight-prompt
             (add-text-properties 0 (length prompt)
                                  '(read-only
                                    t
                                    font-lock-face eshell-prompt
                                    front-sticky (el-patch-swap (font-lock-face read-only)
                                                                t)
                                    rear-nonsticky (el-patch-swap (font-lock-face read-only)
                                                                  t)
                                    (el-patch-add field prompt))
                                  prompt))
        (eshell-interactive-print prompt)))
    (run-hooks 'eshell-after-prompt-hook)))

(when-emacs-version (<= 30 it)
  (el-patch-defun eshell-emit-prompt ()
    "Emit a prompt if eshell is being used interactively."
    (when (boundp 'ansi-color-context-region)
      (setq ansi-color-context-region nil))
    (run-hooks 'eshell-before-prompt-hook)
    (if (not eshell-prompt-function)
        (set-marker eshell-last-output-end (point))
      (let ((prompt (funcall eshell-prompt-function)))
        (add-text-properties
         0 (length prompt)
         (if eshell-highlight-prompt
             '( read-only t
                field prompt
                font-lock-face eshell-prompt
                front-sticky (el-patch-swap (read-only field font-lock-face)
                                            t)
                rear-nonsticky (el-patch-swap (read-only field font-lock-face)
                                              t))
           '( field prompt
              front-sticky (field)
              rear-nonsticky (field)))
         prompt)
        (eshell-interactive-filter nil prompt)))
    (run-hooks 'eshell-after-prompt-hook)))

;;;###autoload
(setenv "PAGER" "cat")

;; Try eshell-complex-commands if some command doesn’t work.
(defalias 'eshell-read-aliases-list #'ignore)

(setf eshell-cmpl-file-ignore "~\\'\\|\\`\\.#"
      eshell-cmpl-dir-ignore
      (eval-when-compile
        (concat "\\`" (regexp-opt +ignored-directories+) "\\'"))
      eshell-command-aliases-list
      (append
       (fold-platform-os-type
        nil
        '(("find" "busybox find $*")))
       '(("l" "ls -C $*")
         ("la" "ls -A $*")
         ("ll" "ls --human-readable -Al $*")

         ("diff" "*diff --unified --recursive --ignore-tab-expansion --ignore-blank-lines $*")
         ("ediff" "ediff $1 $2")
         ("ediff3" "ediff3 $1 $2 $3")
         ("open" "find-file $1")

         (".." "cd ..")
         ("..." "cd ../..")
         ("...." "cd ../../.."))))

;; commands to run in separate buffer
(push "ssh" eshell-visual-commands)
(push "iotop" eshell-visual-commands)
;; (add-to-list 'eshell-visual-commands "tail")

(cl-macrolet ((define-programs (programs regexp)
                `(list ,@(loop
                           for p in programs
                           appending (list `(cons ,p ,regexp)
                                           `(cons ,(concat (char->string
                                                            eshell-explicit-command-char)
                                                           p)
                                                  ,regexp))))))
  (setf eshell-command-completions-alist
        (append (define-programs
                  ("acroread" "pdf")
                  "\\.pdf\\'")
                (define-programs
                  ("okular")
                  "\\.\\(?:pdf\\|djvu\\|ps\\)\\'")

                (define-programs
                  ("gcc" "g++" "cc" "CC" "acc" "bcc" "tcc" "clang")
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

                (define-programs
                  ("readelf" "objdump" "nm")
                  "\\(?:\\`[^.]*\\|\\.\\(?:[ao]\\|so\\)\\)\\'")

                (define-programs
                  ("gdb" "dbx" "sdb" "adb")
                  "\\`\\(?:[^.]*\\|a\\.out\\)\\'")

                (define-programs
                  ("tar" "untar")
                  +tar-regexp+)

                (define-programs
                  ("ghc" "ghci" "runghc" "runhaskell" "hugs" "jhc" "c2hs")
                  "\\.\\(?:hs\\|lhs\\|hsc\\|c2hs\\)\\'")

                (define-programs
                  ("hp2ps")
                  "\\.hp\\'")

                (define-programs
                  ("python" "pypy"
                   "python2.7" "ipython"
                   "python3" "python3.3" "ipython3")
                  "\\.py\\'")

                (define-programs
                  ("makeinfo" "texi2dvi" "texi2pdf")
                  "\\.texi\\'")

                (define-programs
                  ("gdb" "dbx" "sdb")
                  "\\`\\([^.]*\\|a\\.out\\)\\'")

                `(("gunzip" . "\\.gz\\'")
                  ("gunzip" . "\\.gz\\'")
                  ("ar" . "\\.[ao]\\'")))))

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
     (delete-region (point-min)
                    ;; add 1 to capture trailing \n
                    (+ 1 (line-end-position))))))

;; convenient commands
(defun eshell-clear-prompt ()
  "Clear eshell prompt from input"
  (interactive)
  (eshell-bol)
  (delete-region (point) (line-end-position)))

;; property-based jumps, work better than regexp ones

(defun eshell-jump-to-next-prompt ()
  (interactive)
  (text-property-jump-forward 'field 'prompt nil t))

(defun eshell-jump-to-prev-prompt ()
  (interactive)
  (text-property-jump-backward 'field 'prompt nil t))

;;;###autoload
(defun eshell-setup ()
  (init-repl :show-directory t
             :bind-return nil
             :create-keymaps t)

  (hl-line-mode +1)
  (hs-minor-mode-initialize
   :start (rx (or "[" "(" "{"))
   :comment-start-re (rx (or (+ "#")
                             (>= 2 "/"))))
  (setup-folding t nil)

  (def-keys-for-map vim-normal-mode-local-keymap
    ;; clear all previous output
    ("SPC SPC"          eshell-clear-prompt))

  (def-keys-for-map (vim-normal-mode-local-keymap vim-insert-mode-local-keymap)
    ("<return>"         eshell-send-input)
    ("C-<return>"       newline-and-indent)
    ("<up>"             eshell-previous-matching-input-from-input)
    ("<down>"           eshell-next-matching-input-from-input))

  (def-keys-for-map (vim-normal-mode-local-keymap vim-insert-mode-local-keymap eshell-mode-map)
    (("C-t" "C-<up>")   eshell-jump-to-prev-prompt)
    (("C-h" "C-<down>") eshell-jump-to-next-prompt)

    ("C-w"              backward-delete-word)
    ("C-S-w"            backward-delete-word*)

    ("M-p"              browse-eshell-input-history)
    ("C-d"              eshell-send-eof-to-process)
    ("<tab>"            completion-at-point)
    ("C-SPC"            eshell/clear)))

(provide 'eshell-setup)

;; Local Variables:
;; End:

;; eshell-setup.el ends here
