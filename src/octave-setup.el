;; octave-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 19 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'common)
(require 'octave-abbrev+)
(require 'browse-kill-ring-setup)

(add-to-list 'auto-mode-alist '("\\.\\(?:m\\|octaverc\\)$" . octave-mode))

;; (vimmize-motion octave-beginning-of-line)
;; (vimmize-motion octave-end-of-line)


;; set up hideshow
(add-to-list 'hs-special-modes-alist
             (list 'octave-mode
                   ;; start regex
                   (rx (or "do"
                           "for"
                           "parfor"
                           "function"
                           "if"
                           "switch"
                           "try"
                           "unwind_protect"
                           "while"))
                   ;; end regex
                   (rx (or "end"
                           "endfor"
                           "endfunction"
                           "endif"
                           "endswitch"
                           "end_try_catch"
                           "end_unwind_protect"
                           "endwhile"
                           "until"))
                   ;; comment-start regex
                   "\\(?:%+\\|#+\\)"
                   ;; forward-sexp function
                   nil
                   ;; adjust beg function
                   nil
                   ;; (lambda (arg)
                   ;;   (py-goto-beyond-block)
                   ;;   (skip-chars-backward " \t\n"))
                   ))

(eval-after-load
    "octave-mod"
  '(progn
     ;; use parse-partial-sexp, not ppss
     (redefun octave-not-in-string-or-comment-p ()
       "Return t if point is not inside an Octave string or comment."
       (save-excursion
         (let* ((end (point))
                (begin (line-beginning-position))
                (state (parse-partial-sexp begin
                                           end))
                (inside-string? (nth 3 state))
                (inside-comment? (nth 4 state)))
           (not (or inside-string? inside-comment?)))))

     ;; use looking-at-p
     (redefun octave-beginning-of-defun (&optional arg)
       "Move backward to the beginning of an Octave function.
With positive ARG, do it that many times.  Negative argument -N means
move forward to Nth following beginning of a function.
Returns t unless search stops at the beginning or end of the buffer."
       (let* ((arg (or arg 1))
              (inc (if (> arg 0) 1 -1))
              (found nil)
              (case-fold-search nil)
              (prev-pos (point))
              (start-pos (point)))
         (and (not (eobp))
              (not (and (> arg 0) (looking-at-p "\\_<function\\_>")))
              (skip-syntax-forward "w"))
         (while (and (/= arg 0)
                     (setq found
                           (re-search-backward "\\_<function\\_>" inc t)))
           (when (octave-not-in-string-or-comment-p)
             (decf arg inc)
             (setf prev-pos (match-beginning 0))))
         (if found
           (goto-char prev-pos)
           (goto-char start-pos))
         t))



     (defvar inferior-octave-on-output-hook nil
       "Hook to run after any output arrived from process.")

     ;; add invokation of inferior-octave-on-output-hook
     (redefun octave-send-region (beg end)
       "Send current region to the inferior Octave process."
       (interactive "r")
       (inferior-octave t)
       (let ((proc inferior-octave-process)
             (string (buffer-substring-no-properties beg end))
             line)
         (with-current-buffer inferior-octave-buffer
           (setq inferior-octave-output-list nil)
           (while (not (string-equal string ""))
             (if (string-match "\n" string)
               (setq line (substring string 0 (match-beginning 0))
                     string (substring string (match-end 0)))
               (setq line string string ""))
             (setq inferior-octave-receive-in-progress t)
             (inferior-octave-send-list-and-digest (list (concat line "\n")))
             (while inferior-octave-receive-in-progress
               (accept-process-output proc))
             (insert-before-markers
              (join-lines (append
                           (if octave-send-echo-input (list line) (list ""))
                           (map 'inferior-octave-strip-ctrl-g
                                inferior-octave-output-list)
                           (list inferior-octave-output-string)))))
           (run-hooks 'inferior-octave-on-output-hook)))
       (when octave-send-show-buffer
         (display-buffer inferior-octave-buffer)))

     (add-hook 'inferior-octave-on-output-hook
               #'octave-highlight-errors)


     ;; my own functions

     (make-align-function octave-align-on-commas ","
                          :repeat t
                          :require-one-or-more-spaces t
                          :put-align-spaces-after-str t)
     (make-align-function octave-align-on-equals "=")

     (define-switch-to-interpreter
       switch-to-octave
       (inferior-octave-buffer)
       (run-octave)
       :doc "Switch to `inferior-octave-buffer' buffer."
       :save-buffer t
       :error-msg "Can't switch to octave repl")

     (defun octave-load-current-file (&optional switch)
       "Send current buffer to octave process."
       (interactive "P")
       (when (buffer-modified-p)
         (save-buffer))
       (let ((fname (file-name-nondirectory
                     (file-name-sans-extension
                      (buffer-file-name)))))
         (with-temp-buffer
           (insert fname)
           (save-window-excursion
             (save-excursion
               (octave-send-region (point-min) (point-max)))))
         (when switch
           (pop-to-buffer inferior-octave-buffer t))))

     (define-circular-jumps
         octave-jump-to-next-prompt
         octave-jump-to-prev-prompt
       inferior-octave-prompt)

;;;

     (defconst +octave-repl-error-regexp+
       (rx bol
           (or (seq "error:"
                    (+ whitespace)
                    ;; 1
                    (group
                     (+ "/"
                        (+ (regexp "[^/\n]")))
                     ".m")
                    (+ whitespace)
                    "at"
                    (+ whitespace)
                    "line"
                    (+ whitespace)
                    ;; 2
                    (group
                     (+ digit))
                    ","
                    (+ whitespace)
                    "column"
                    (+ whitespace)
                    ;; 3
                    (group
                     (+ digit)))
               (seq "parse"
                    (+ whitespace)
                    "error"
                    (+ whitespace)
                    "near"
                    (+ whitespace)
                    "line"
                    (+ whitespace)
                    ;; 4
                    (group
                     (+ digit))
                    (+ whitespace)
                    "of"
                    (+ whitespace)
                    "file"
                    (+ whitespace)
                    ;; 5
                    (group
                     (+ "/"
                        (+ (regexp "[^/\n]")))
                     ".m"))
               (seq "error:"
                    (+ whitespace)
                    ;; 6
                    (group
                     (+? (regex "[^\n\r]")))
                    (? "near"
                       (+ whitespace))
                    "line"
                    (+ whitespace)
                    ;; 7
                    (group
                     (+ digit))
                    (+ whitespace)
                    "column"
                    (+ whitespace)
                    ;; 8
                    (group
                     (+ digit))))
           eol))

     ;; error-specific jumps
     (define-circular-jumps
         octave-jump-to-next-error
         octave-jump-to-prev-error
       +octave-repl-error-regexp+)


     (defun extract-group-bounds (group-nums)
       "Extract (beginning . end) pair of first non-nil group match by re by group
in GROUP-NUMS."
       (cond
         ((null group-nums)
          nil)
         ((not (null (match-beginning (car group-nums))))
          (cons (match-beginning (car group-nums))
                (match-end (car group-nums))))
         (t
          (extract-group-bounds (cdr group-nums)))))

     (defun bounds->string (bounds)
       (when bounds
         (buffer-substring-no-properties (car bounds) (cdr bounds))))

     (defun octave-error-line-number-bounds ()
       "Exctract error message's line number from match-data."
       (extract-group-bounds '(2 4 7)))

     (defun octave-error-column-number-bounds ()
       "Exctract error message's column number from match-data."
       (extract-group-bounds '(3 8)))

     (defun octave-error-filename-bounds ()
       "Exctract error message's filename from match-data."
       (extract-group-bounds '(1 5)))

     (defun octave-error-message-bounds ()
       "Exctract error message's actual message from match-data."
       (extract-group-bounds '(6)))


     (defun octave-jump-to-error ()
       "Jump to error matching `+octave-repl-error-regexp+' at point."
       (interactive)
       (let ((filename nil)
             (line nil)
             (column nil)
             (jump nil))
         (save-excursion
           (beginning-of-line)
           (when (looking-at +octave-repl-error-regexp+)
             (setf jump t
                   filename (bounds->string (octave-error-filename-bounds))
                   line (bounds->string (octave-error-line-number-bounds))
                   column (bounds->string (octave-error-column-number-bounds)))))
         (when (and jump filename)
           (find-file-other-window filename)
           (vim:save-position)
           (when line
             (goto-line1 (read line))
             (message "COLUMN: %s"
                      (pp-to-string column))
             (when column
               (line-beginning-position)
               (skip-to-indentation)
               (loop
                 with end = (line-end-position)
                 for i below (1+ (read column))
                 while (< (point) end)
                 do (forward-char)))))))

;;;

     (defvar octave-error-keymap
       (let ((k (make-keymap)))
         (define-key k (kbd "<return>") #'octave-jump-to-error)
         k))

     (defun octave-highlight-errors ()
       "Enable detection of octave runtime errors."
       (save-match-data
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward +octave-repl-error-regexp+ nil t)
             (goto-char (match-beginning 0))
             (put-text-property (match-beginning 0)
                                (match-end 0)
                                'keymap
                                octave-error-keymap)
             (let ((filename-bounds (octave-error-filename-bounds))
                   (message-bounds (octave-error-message-bounds))
                   (line-bounds (octave-error-line-number-bounds))
                   (column-bounds (octave-error-column-number-bounds)))
               (if filename-bounds
                 (put-text-property (car filename-bounds)
                                    (cdr filename-bounds)
                                    'font-lock-face
                                    'compilation-error)
                 ;; no filename was specified, highlight actual message
                 (when message-bounds
                   (put-text-property (car message-bounds)
                                      (cdr message-bounds)
                                      'font-lock-face
                                      'compilation-error)))
               (when line-bounds
                 (put-text-property (car line-bounds)
                                    (cdr line-bounds)
                                    'font-lock-face
                                    'compilation-line-number))
               (when column-bounds
                 (put-text-property (car column-bounds)
                                    (cdr column-bounds)
                                    'font-lock-face
                                    'compilation-column-number)))
             (goto-char (match-end 0))))))))


;;;

(defun octave-setup ()
  (init-common :use-yasnippet t
               :use-render-formula t
               :sp-slurp-sexp-insert-space nil
               :use-fci t)
  (hs-minor-mode t)

  (setq-local yas/indent-line 'fixed)
  (setq-local hs-set-up-overlay
              (lambda (ov)
                (when (eq 'code (overlay-get ov 'hs))
                  (overlay-put ov
                               'display
                               " ... "))))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f6>"    octave-load-current-file)
    ("SPC SPC" switch-to-octave)
    ("g <tab>" octave-indent-defun)
    ("g s s"   vim:replace-symbol-at-point)

    ("j"       octave-send-line)
    ("J"       octave-send-defun)
    ("g a ="   octave-align-on-equals)
    ("g a ,"   octave-align-on-commas)

    ("g t"     (lambda () (interactive) (octave-beginning-of-defun)))
    ("g h"     end-of-defun)

    ("z o"     hs-show-block)
    ("z c"     hs-hide-block)
    ("z C"     hs-hide-all)
    ("z O"     hs-show-all))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("g a ="   octave-align-on-equals)
    ("g a ,"   octave-align-on-commas)
    ("j"       octave-send-region))

  ;; (def-keys-for-map (vim:normal-mode-local-keymap
  ;;                     vim:visual-mode-local-keymap
  ;;                     vim:operator-pending-mode-local-keymap
  ;;                     vim:motion-mode-local-keymap)
  ;;   ("0" vim:octave-beginning-of-line)
  ;;   ("$" vim:octave-end-of-line))
  (octave-abbrev+-setup))

(add-hook 'octave-mode-hook #'octave-setup)


;;;

(defun inferior-octave-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :sp-slurp-sexp-insert-space nil
               :use-fci nil)
  (init-repl :bind-return (list vim:normal-mode-local-keymap
                                vim:insert-mode-local-keymap
                                inferior-octave-mode-map))
  ;; (enable-octave-interpreter-error-detection)

  (setf inferior-octave-prompt
        "^\\(octave\\(\\|.bin\\|.exe\\)\\(-[.0-9]+\\)?\\(:[0-9]+\\)?\\|^debug\\)?>+ ")

  (def-keys-for-map inferior-octave-mode-map
    ("M-/"      comint-dynamic-complete)
    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("S-<up>"   octave-jump-to-prev-prompt)
    ("S-<down>" octave-jump-to-next-prompt)
    ("C-<up>"   octave-jump-to-prev-error)
    ("C-<down>" octave-jump-to-next-error)

    ("C-S-p"    browse-kill-ring)
    ("M-p"      browse-comint-input-history)
    ("SPC SPC"  comint-clear-prompt)

    ("C-SPC"    comint-clear-buffer-above-prompt)))

(add-hook 'inferior-octave-mode-hook #'inferior-octave-setup)


(defun inferior-octave-custom-output-filter (proc string)
  "Wrap around `inferior-octave-output-filter' by adding a call to
run `inferior-octave-on-output-hook'."
  (unwind-protect
      (inferior-octave-output-filter proc string)
    (run-hooks 'inferior-octave-on-output-hook)))

(add-hook 'inferior-octave-startup-hook
          (lambda ()
            (set-process-filter inferior-octave-process
                                'inferior-octave-custom-output-filter)))

(provide 'octave-setup)

;; Local Variables:
;; End:

;; octave-setup.el ends here
