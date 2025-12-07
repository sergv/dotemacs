;; octave-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 19 January 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'macro-util)

  (defvar inferior-octave-buffer)
  (defvar inferior-octave-output-list)
  (defvar inferior-octave-output-list)
  (defvar inferior-octave-output-string)
  (defvar inferior-octave-process)
  (defvar inferior-octave-prompt)
  (defvar inferior-octave-receive-in-progress)
  (defvar inferior-octave-receive-in-progress)
  (defvar octave-send-echo-input)
  (defvar octave-send-show-buffer)
  (defvar inferior-octave-mode-map))

(declare-function inferior-octave-send-list-and-digest "octave")
(declare-function octave-beginning-of-defun "octave")
(declare-function octave-send-line "octave")
(declare-function octave-send-defun "octave")

(require 'align-util)
(require 'browse-kill-ring-setup)
(require 'common)
(require 'el-patch)
(require 'folding-setup)
(require 'hydra-setup)
(require 'octave-abbrev+)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:m\\|octaverc\\)\\'" . octave-mode))

;;;###autoload
(el-patch-feature octave)

(defvar inferior-octave-on-output-hook nil
  "Hook to run after any output arrived from process.")

;; add invokation of inferior-octave-on-output-hook
(el-patch-defun octave-send-region (beg end)
  "Send current region to the inferior Octave process."
  (interactive "r")
  (inferior-octave t)
  (let ((proc inferior-octave-process)
        (string (buffer-substring-no-properties beg end))
        line)
    (with-current-buffer inferior-octave-buffer
      ;; http://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00095.html
      (compilation-forget-errors)
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
         (mapconcat 'identity
                    (append
                     (if octave-send-echo-input (list line) (list ""))
                     inferior-octave-output-list
                     (list inferior-octave-output-string))
                    "\n")))
      (el-patch-add (run-hooks 'inferior-octave-on-output-hook))))
  (if octave-send-show-buffer
      (display-buffer inferior-octave-buffer)))

;;;###autoload
(add-hook 'inferior-octave-on-output-hook
          #'octave-highlight-errors)

;; my own functions

(defalign octave-align-on-commas ","
  :repeat t
  :require-one-or-more-spaces t
  :put-align-spaces-after-str t)

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
  (save-buffer-if-modified)
  (let ((fname (file-name-nondirectory
                (file-name-sans-extension buffer-file-name))))
    (with-temp-buffer
      (insert fname)
      (save-window-excursion
        (save-excursion
          (octave-send-region (point-min) (point-max)))))
    (when switch
      (pop-to-buffer inferior-octave-buffer t))))


(defun octave-jump-to-next-prompt ()
  (interactive)
  (circular-jump-forward inferior-octave-prompt nil))

(defun octave-jump-to-prev-prompt ()
  (interactive)
  (circular-jump-backward inferior-octave-prompt nil))

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
(defun octave-jump-to-next-error ()
  (interactive)
  (circular-jump-forward +octave-repl-error-regexp+ nil))
(defun octave-jump-to-prev-error ()
  (interactive)
  (circular-jump-backward +octave-repl-error-regexp+ nil))

(defun octave--extract-group-bounds (group-nums)
  "Extract (beginning . end) pair of first non-nil group match by re by group
in GROUP-NUMS."
  (cond
    ((null group-nums)
     nil)
    ((not (null (match-beginning (car group-nums))))
     (cons (match-beginning (car group-nums))
           (match-end (car group-nums))))
    (t
     (octave--extract-group-bounds (cdr group-nums)))))

(defun bounds->string (bounds)
  (when bounds
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun octave-error-line-number-bounds ()
  "Exctract error message's line number from match-data."
  (octave--extract-group-bounds '(2 4 7)))

(defun octave-error-column-number-bounds ()
  "Exctract error message's column number from match-data."
  (octave--extract-group-bounds '(3 8)))

(defun octave-error-filename-bounds ()
  "Exctract error message's filename from match-data."
  (octave--extract-group-bounds '(1 5)))

(defun octave-error-message-bounds ()
  "Exctract error message's actual message from match-data."
  (octave--extract-group-bounds '(6)))


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
      (vim-save-position)
      (when line
        (goto-line-dumb (read line))
        (when column
          (line-beginning-position)
          (skip-to-indentation)
          (cl-loop
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
        (goto-char (match-end 0))))))

(defun octave-beginning-of-defun-interactive ()
  (interactive)
  (vim-save-position)
  (octave-beginning-of-defun))

;;;

(defhydra-ext hydra-octave-align (:exit t :foreign-keys nil :hint nil)
  "
_=_:  on equals
_,_:  on commas"
  ("=" generic-align-on-equals)
  ("," octave-align-on-commas))

(defhydra-derive hydra-octave-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_<tab>_: indent defun

_t_: beginning of defun
_h_: end of defun"
  ("<tab>" prog-indent-sexp)

  ("t"     octave-beginning-of-defun-interactive)
  ("h"     end-of-defun))

(defhydra-derive hydra-octave-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign  _t_: beginning of defun
       _h_: end of defun"
  ("a" hydra-octave-align/body)
  ("t" octave-beginning-of-defun-interactive)
  ("h" end-of-defun))

(defhydra-derive hydra-octave-vim-normal-j-ext hydra-vim-normal-j-ext (:exit t :foreign-keys nil :hint nil)
  "
_j_: send line
_d_: send defun"
  ("j" octave-send-line)
  ("d" octave-send-defun))

(defhydra-derive hydra-octave-vim-visual-j-ext hydra-vim-visual-j-ext (:exit t :foreign-keys nil :hint nil)
  "
_j_: send region"
  ("j" octave-send-region))

;;;

;;;###autoload
(defun octave-setup ()
  (init-common :use-yasnippet t
               :use-render-formula t
               :use-fci t
               :use-whitespace 'tabs-only)
  (hs-minor-mode-initialize
   :start (rx (or "do"
                  "for"
                  "parfor"
                  "function"
                  "if"
                  "switch"
                  "try"
                  "unwind_protect"
                  "while"))
   :end (rx (or "end"
                "endfor"
                "endfunction"
                "endif"
                "endswitch"
                "end_try_catch"
                "end_unwind_protect"
                "endwhile"
                "until"))
   :comment-start-re "\\(?:%+\\|#+\\)"

   ;; :adjust-arg
   ;; (lambda (arg)
   ;;   (py-goto-beyond-block)
   ;;   (skip-chars-backward " \t\n"))
   )

  (setup-folding t nil)

  (setq-local yas-indent-line 'fixed
              hs-set-up-overlay
              (lambda (ov)
                (when (eq 'code (overlay-get ov 'hs))
                  (overlay-put ov
                               'display
                               " ... "))))

  (bind-tab-keys #'tab-to-tab-stop
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("<f6>"    octave-load-current-file)
    ("SPC SPC" switch-to-octave)
    ("g"       hydra-octave-vim-normal-g-ext/body)
    ("j"       hydra-octave-vim-normal-j-ext/body))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("g" hydra-octave-vim-visual-g-ext/body)
    ("j" hydra-octave-vim-visual-j-ext/body))

  (octave-abbrev+-setup))

;;;###autoload
(add-hook 'octave-mode-hook #'octave-setup)

;;;

;;;###autoload
(defun inferior-octave-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci nil
               :smerge nil)
  (init-repl :bind-return (list vim-normal-mode-local-keymap
                                vim-insert-mode-local-keymap
                                inferior-octave-mode-map))
  ;; (enable-octave-interpreter-error-detection)

  (setf inferior-octave-prompt
        (rx bol
            (? (or (seq
                    "octave"
                    (or "" ".bin" ".exe")
                    (? "-" (+ (any ?. (?0 . ?9))))
                    (? ":" (+ (any (?0 . ?9)))))
                   "debug"))
            (+ ">")
            " "))

  (vim-local-emap "clear" #'vim:comint-clear-buffer-above-prompt)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap
                     inferior-octave-mode-map)
    ("C-SPC"    vim:comint-clear-buffer-above-prompt:interactive)
    ("M-/"      comint-dynamic-complete)
    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-t"      octave-jump-to-prev-prompt)
    ("C-h"      octave-jump-to-next-prompt)
    ("M-t"      octave-jump-to-prev-error)
    ("M-h"      octave-jump-to-next-error)

    ("C-S-p"    browse-kill-ring)
    ("M-p"      browse-comint-input-history)))

;;;###autoload
(add-hook 'inferior-octave-mode-hook #'inferior-octave-setup)


;;;###autoload
(defun inferior-octave-custom-output-filter (proc string)
  "Wrap around `inferior-octave-output-filter' by adding a call to
run `inferior-octave-on-output-hook'."
  (unwind-protect
      (comint-output-filter proc string)
    (run-hooks 'inferior-octave-on-output-hook)))

;;;###autoload
(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (set-process-filter inferior-octave-process
                                'inferior-octave-custom-output-filter)))

(provide 'octave-setup)

;; Local Variables:
;; End:

;; octave-setup.el ends here
