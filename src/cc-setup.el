;; cc-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  8 October 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'ctypes)
(require 'indentation)
(require 'find-file)
(require 'eproj)
(require 'company-eproj)

(require 'dtrt-indent) ;; indent offset guesser

(setf dtrt-indent-verbosity 2
      dtrt-indent-max-relevant-lines 10000)

;;;###autoload (autoload 'c-align-on-equals "cc-setup")
(make-align-function c-align-on-equals
                     "\\([+*|&/!%]\\|-\\|\\^\\)?=[^=]"
                     :require-one-or-more-spaces t)

(defun c-forward-preprocessor-conditional (count)
  "Clone of `c-scan-conditionals' with interface of `forward-sexp' for matching
#if with #else and #else with #endif."
  (save-match-data
    (let* ((forward (> count 0))
           (increment (if forward -1 1))
           (search-function (if forward 're-search-forward 're-search-backward))
           (target-depth 0)
           new
           (case-fold-search nil))
      (save-excursion
        (while (/= count 0)
          (let ((depth 0)
                (found nil))
            (save-excursion
              ;; Find the "next" significant line in the proper direction.
              (while (and (not found)
                          ;; Rather than searching for a # sign that
                          ;; comes at the beginning of a line aside from
                          ;; whitespace, search first for a string
                          ;; starting with # sign.  Then verify what
                          ;; precedes it.  This is faster on account of
                          ;; the fastmap feature of the regexp matcher.
                          (funcall search-function
                                   "#[ \t]*\\(if\\|elif\\|endif\\|else\\)"
                                   nil t))
                (beginning-of-line)
                ;; Now verify it is really a preproc line.
                (if (looking-at "^[ \t]*#[ \t]*\\(if\\|elif\\|endif\\|else\\)")
                    (let ((dchange nil)
                          (directive (match-string 1)))
                      (cond ((or (string= directive "if")
                                 (string= directive "ifdef")
                                 (string= directive "ifndef"))
                             (setq dchange (- increment)))
                            ((string= directive "endif")
                             (setq dchange increment))
                            ((= depth 0)
                             (setq dchange +1))
                            ((= depth +1)
                             (setq dchange -1)))
                      (when dchange
                        (setq depth (+ depth dchange))
                        ;; If we are trying to move across, and we find an
                        ;; end before we find a beginning, get an error.
                        (if (and (< depth target-depth) (< dchange 0))
                            (error (concat (if forward
                                               "No following conditional at this level"
                                             "No previous conditional at this level")
                                           ", depth = %s, target-depth = %s")
                                   depth
                                   target-depth)))
                      ;; When searching forward, start from next line so
                      ;; that we don't find the same line again.
                      (if forward (forward-line 1))
                      ;; We found something if we've arrived at the
                      ;; target depth.
                      (if (and dchange (= depth target-depth))
                          (setq found (point))))
                  ;; else
                  (if forward (forward-line 1)))))
            (or found
                (error "No containing preprocessor conditional"))
            (goto-char (setq new found)))
          (setq count (+ count increment))))
      (c-keep-region-active)
      (goto-char new))))

(defconst +c-preprocessor-directives-re+
  (rx (seq bol
           "#"
           (* (syntax whitespace))
           symbol-start
           (or "ifdef"
               "ifndef"
               "if"
               "elif"
               "endif"
               "else")
           symbol-end)))

(defun c-hideshow-forward-sexp (&optional arg)
  "Special version of `forward-sexp' for hideshow in c-mode."
  (if (char=? (char-after) ?\{)
      (forward-sexp arg)
    (let ((start (point)))
      (save-match-data
        (c-forward-preprocessor-conditional (or arg 1))
        (re-search-backward +c-preprocessor-directives-re+ start t)))))

(let ((hs-spec (list
                ;; This is like `+c-preprocessor-directives-re+' but does not
                ;; include ending directives like endif.
                (rx (or (seq bol
                             "#"
                             (* (syntax whitespace))
                             symbol-start
                             (or "ifdef"
                                 "ifndef"
                                 "if"
                                 "elif"
                                 "else")
                             symbol-end)
                        "{"))
                0
                "/[*/]"
                #'c-hideshow-forward-sexp
                nil)))
  (setf hs-special-modes-alist
        (cons `(c-mode ,hs-spec)
              (cons `(c++-mode ,hs-spec)
                    (assq-delete-all 'c-mode
                                     (assq-delete-all 'c++-mode
                                                      hs-special-modes-alist))))))

(defun cc-setup/set-up-c-basic-offset ()
  "Try to guess offset (`c-basic-offset') for current buffer or use value
dictated by code standard at work if use-work-code-style is non-nil.
Also propagate new offset to `vim:shift-width'."
  (let ((dtrt-indent-verbosity 0))
    (unless (dtrt-indent-try-set-offset)
      (awhen (eproj-query/any-mode/indent-offset (eproj-get-project-for-buf-lax (current-buffer))
                                                 major-mode
                                                 nil)
        (setq-local c-basic-offset it)))
    (setup-indent-size
     (if (integer? c-basic-offset)
         c-basic-offset
       4))))


(defhydra-ext hydra-c-align (:exit t :foreign-keys nil :hint nil)
  "
_a_:   generic
_=_: on equals"
  ("a" align)
  ("=" c-align-on-equals))

(defhydra-derive hydra-c-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_<tab>_: reindent function  _t_: jump to function start
                            _h_: jump to function end"
  ("TAB" c-indent-defun)

  ("t"   c-beginning-of-defun)
  ("h"   c-end-of-defun))

(defhydra-derive hydra-c-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign  _t_: jump to function start
       _h_: jump to function end"
  ("a" hydra-c-align/body)

  ("t" c-beginning-of-defun)
  ("h" c-end-of-defun))

(defhydra-derive hydra-c-vim-visual-z-ext hydra-vim-visual-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide c sexps in region
_o_: show c sexps in region"
  ("c" hs-hide-c-sexps-in-region)
  ("o" hs-show-c-sexps-in-region))

(defun* cc-setup (&key (define-special-keys t))
  (init-common :use-render-formula t
               :sp-slurp-sexp-insert-space nil
               :use-yasnippet t
               :use-whitespace 'tabs-only)
  (fontify-conflict-markers!)
  (which-function-mode -1)
  (company-mode +1)
  (setq-local company-backends '(company-eproj))

  (modify-syntax-entry ?_ "_")
  (modify-syntax-entry ?# ".")

  (setq-local whitespace-line-column 80)
  (setq-local whitespace-style '(face tabs lines-tail))
  (setq-local yas-expand-fallback #'tab-to-tab-stop)

  (setf c-tab-always-indent t)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g" hydra-c-vim-normal-g-ext/body))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("z" hydra-c-vim-visual-z-ext/body)
    ("g" hydra-c-vim-visual-g-ext/body))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("C-SPC"       company-complete)
    ("<backspace>" backward-delete-char))

  (bind-tab-keys #'tab-to-tab-stop
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t)

  (when define-special-keys
    (def-keys-for-map vim:normal-mode-local-keymap
      ("SPC SPC" ff-find-related-file))))


;; Indentation of c-style languages via AStyle command-line utility.

(defparameter c-indentation-indent-styles-alist
  '(("c-standard4"
     "--style=linux"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux")
    ("c-standard2"
     "--style=linux"
     "--indent=spaces=2"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux")
    ("c-gnu2"
     "--style=gnu"
     "--indent=spaces=2"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--convert-tabs"
     "--align-pointer=name"
     "--mode=c"
     "--suffix=none"
     "--lineend=linux")

    ("java-standard"
     "--style=java"
     "--align-pointer=middle"
     "--formatted"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--add-brackets"
     "--convert-tabs"
     "--mode=java"
     "--suffix=none"
     "--lineend=linux"
     "--indent-namespaces" ;; standard java indents namespaces
     )
    ("java-clojure"
     "--style=java"
     ;; "--style=break"
     "--align-pointer=middle"
     "--formatted"
     "--indent=spaces=4"
     "--pad-oper"
     "--pad-header"
     "--unpad-paren"
     "--keep-one-line-statements"
     "--keep-one-line-blocks"
     "--add-brackets"
     "--convert-tabs"
     "--mode=java"
     "--suffix=none"
     "--lineend=linux")))

(defparameter c-indentation-indent-styles
  (alist->hash-table
   (remq nil
         c-indentation-indent-styles-alist)))

(defparameter c-indentation-indent-style (caar c-indentation-indent-styles-alist)
  "Default indent style to use.")

(defparameter c-indentation-style-history nil)

(defun c-format-buffer (&optional change-indent-style)
  (interactive "p")
  (when change-indent-style
    (setf c-indentation-indent-style
          (completing-read "Choose style: "
                           (hash-table-keys c-indentation-indent-styles)
                           nil
                           t ;; require match
                           nil
                           c-indentation-style-history ;; history
                           )))
  (let ((file +buffer-indent-temporary-filename+)
        (p (point))
        (indent-options (gethash c-indentation-indent-style
                                 c-indentation-indent-styles)))
    (unless indent-options
      (error "No options for indent style %s" c-indentation-indent-style))
    (write-region (point-min) (point-max) file)
    (erase-buffer)
    (shell-command
     (join-lines (append (list (platform-dependent-executable
                                (concat +execs-path+ "/astyle.custom")))
                         indent-options
                         (list (format "<%s" file)))
                 " ")
     (current-buffer))
    (goto-char p)))

(defun c-format-file (filename &optional style)
  "Indent FILENAME according to STYLE by running astyle on it."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((c-indentation-indent-style (or style
                                          c-indentation-indent-style)))
      (c-format-buffer)
      (write-region (point-min) (point-max) filename))))

(provide 'cc-setup)

;; Local Variables:
;; End:

;; cc-setup.el ends here


