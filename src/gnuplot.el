;; gnuplot.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 16 August 2011
;; Keywords:
;; Requirements:
;; Status:

;; Define simple yet powerful mode for editing
;; gnuplot files. Some code is taken from old
;; gnuplot file by 1998 Phil Type and Bruce Ravel, 1999-2002 Bruce Ravel
;; version 0.6.0

(require 'compile)

(defconst gnuplot-program "gnuplot")

(defconst gnuplot-error-regexp "^\"\\(.*\\)\", line \\([0-9]+\\):")

(defun gnuplot-run ()
  (interactive)
  (save-buffer-if-modified)
  ;; if on moment of gnuplot-mode invokation buffer was fileless
  ;; but now it has file then set command...
  (unless compile-command
    (setq compile-command
          (if-buffer-has-file
            (concat gnuplot-program
                    " "
                    (shell-quote-argument
                     buffer-file-name)))))
  ;; but if buffer is still fileless then signal error
  (if compile-command
    (compilation-start compile-command
                       #'gnuplot-run-mode)
    (error "Buffer has no saved file to run")))

(define-compilation-mode gnuplot-run-mode "Gnuplot run"
  "Mode to run gnuplot scripts."
  ;; (setq-local compilation-process-setup-function
  ;; ',(intern (concat nm "-process-setup")))

  (setq-local compilation-error-regexp-alist
              (list
               (list gnuplot-error-regexp 1 2 nil 2)))

  (setq-local *compilation-jump-error-regexp*
              gnuplot-error-regexp)

  (setq-local compilation-disable-input t)
  (setq-local compilation-scroll-output nil)

  ;; (setq-local compilation-finish-functions
  ;; (list ',(intern (concat nm "-finish-hook"))))
  )


(defparameter gnuplot-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?* "."  table)
    (modify-syntax-entry ?+ "."  table)
    (modify-syntax-entry ?- "."  table)
    (modify-syntax-entry ?/ "."  table)
    (modify-syntax-entry ?% "."  table)
    ;;(modify-syntax-entry ?& "."  table) ; rarely used
    ;;(modify-syntax-entry ?^ "."  table) ; operators
    ;;(modify-syntax-entry ?| "."  table) ; in gnuplot,
    ;;(modify-syntax-entry ?& "."  table) ; (by me,
    ;;(modify-syntax-entry ?? "."  table) ;  anyway...)
    ;;(modify-syntax-entry ?~ "."  table) ;
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "w"  table)
    (modify-syntax-entry ?_ "w"  table)
    table)
  "Syntax table in use in `gnuplot-mode' buffers.
This is the same as the standard syntax table except that ' is a
string quote character, ` and _ are word characters, and math
operators are punctuation characters.")


(defparameter gnuplot-font-lock-keywords
  (list
   ;; comments
   '("#.*$" . font-lock-comment-face)
   ;; quoted things
   ;;'("['\"]\\(?:[^'\"\n]*\\)['\"]"
   ;;  1 font-lock-string-face)
   '("'[^'\n]*'" . font-lock-string-face)
   '("\".*[^\\]\"" . font-lock-string-face)

   '("\"[^\"]+$" . font-lock-warning-face)
   '(",[ \t]*\\(?:[^\\]$\\|$\\)" . font-lock-warning-face)
   ;; stuff in brackets, sugg. by <LB>
   '("\\[\\([^]]+\\)\\]"
     1 font-lock-reference-face)
   ;; variable/function definitions
   '("\\(\\<[a-z]+[a-z_0-9()]*\\)[ \t]*="
     1 font-lock-variable-name-face)
   ;; built-in function names
   (cons (concat
          "\\<\\("
          "a\\(?:bs\\|cosh\?\\|rg\\|sinh\?\\|"
          "tan\\(?:\\|[2h]\\)\\)\\|"
          "bes\\(?:j[01]\\|y[01]\\)\\|"
          "c\\(?:eil\\|o\\(?:lumn\\|sh\?\\)\\)\\|"
          "e\\(?:rfc\?\\|xp\\)\\|floor\\|gamma\\|"
          "i\\(?:beta\\|gamma\\|mag\\|"
          "n\\(?:t\\|v\\(?:erf\\|norm\\)\\)\\)\\|"
          "l\\(?:gamma\\|og\\(?:\\|10\\)\\)\\|"
          "norm\\|r\\(?:and\\|eal\\)\\|"
          "s\\(?:gn\\|inh\?\\|qrt\\)\\|"
          "t\\(?:anh\?\\|m_\\(?:hour\\|m\\(?:day\\|in\\|on\\)\\|"
          "sec\\|wday\\|y\\(?:day\\|ear\\)\\)\\)\\|"
          "valid"
          "\\)\\>")
         font-lock-function-name-face)
   ;; reserved words associated with
   ;; plotting <AL>
   '("\\<\\(axes\\|every\\|index\\|l\\(?:[stw]\\|ine\\(?:style\\|type\\|width\\)\\)\\|notitle\\|p\\(?:[st]\\|oint\\(?:size\\|type\\)\\)\\|smooth\\|t\\(?:hru\\|itle\\)\\|using\\|with\\)\\>" . font-lock-type-face)
   '("\\<\\(box\\(?:e\\(?:rrorbars\\|s\\)\\|xyerrorbars\\)\\|candlesticks\\|dots\\|errorbars\\|f\\(?:inancebars\\|steps\\)\\|histeps\\|impulses\\|lines\\(?:\\|points\\)\\|points\\|steps\\|vector\\|x\\(?:errorbars\\|yerrorbars\\)\\|yerrorbars\\)\\>" . font-lock-function-name-face)
   ;; (s)plot -- also thing (s)plotted
   '("\\<s?plot\\>" . font-lock-keyword-face)
   '("\\<s?plot\\s-+\\([^'\" ]+\\)[) \n,\\]"
     1 font-lock-variable-name-face)
   ;; other common commands
   ;; miscellaneous commands
   (cons (concat "\\<\\("
                 "bind\\|"
                 "c\\(?:d\\|lear\\)\\|exit\\|fit\\|h\\(?:elp\\|istory\\)\\|load\\|"
                 "p\\(?:ause\\|rint\\|wd\\)\\|quit\\|replot\\|"
                 "s\\(?:ave\\|et\\|how\\)\\|unset"
                 "\\)\\>\\|!.*$")
         font-lock-constant-face)))


(defun gnuplot-indent-line ()
  "Set indentation in gnuplot buffer.
For most lines, set indentation to previous level of indentation.
Attempt to add additional indentation for continued plot and splot
lines."
  (interactive)
  (let ((indent 0))
    (save-excursion
      (save-excursion
        (end-of-line 0)
        (if (bobp) ()
            (re-search-backward "^[ \t]*." (point-min) "to_limit")
            (back-to-indentation)
            (setq indent (current-column))
            (if (looking-at "s?pl\\(o?\\|\\(ot\\)?\\)[ \t]+.?")
              (let ((plus (1- (length (match-string 0)))))
                (end-of-line)
                (backward-char 1)
                (if (looking-at (regexp-quote "\\"))
                  (setq indent  (+ plus indent)))))))
      (if (= (current-indentation) indent)
        ()
        (beginning-of-line)
        (delete-horizontal-space)
        (insert (make-string indent ? ))))
    (if (looking-at "[ \t]+$")
      (end-of-line))))


;;;###autoload
(define-derived-mode gnuplot-mode nil "Gnuplot"
  "Major mode for editing gnuplot files"
  :syntax-table gnuplot-mode-syntax-table
  (define-key gnuplot-mode-map (kbd "<f9>") #'gnuplot-run)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "#[ \t]*")
  (set (make-local-variable 'indent-line-function) 'gnuplot-indent-line)

  (turn-on-font-lock)

  (set (make-local-variable 'font-lock-defaults)
       '(gnuplot-font-lock-keywords t t))

  (set (make-local-variable 'compile-command)
       (if-buffer-has-file
         (concat gnuplot-program
                 " "
                 (shell-quote-argument
                  buffer-file-name)))))


(provide 'gnuplot)

;; Local Variables:
;; End:

;; gnuplot.el ends here
