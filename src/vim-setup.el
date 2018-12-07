;; vim-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'common)
(require 'completion-setup)
(require 'keys-def)
(require 'search)
(require 'smartparens-setup)
(require 'vim-ex)
(require 'vim-replace)

;;; configuration variables

(defcustom vim-scroll-move-point t
  "Controlls whether scrolling functions like `vim:scroll-line-up' should
move point to next/previous line."
  :type 'boolean)

;;; overriding keymap

(defvar-local vim:complex-command-override-local-keymap nil
  "Keymap for buffer-local overriding of commands that take motions,
like \"d w\".")

;;; keybindings

;; redefine motions

(def-keys-for-map (vim:normal-mode-keymap
                   vim:visual-mode-keymap
                   vim:operator-pending-mode-keymap
                   vim:motion-mode-keymap)
  ("0"         vim:motion-beginning-of-line-or-digit-argument)
  (("1" "2" "3" "4" "5" "6" "7" "8" "9")
   vim:digit-argument)
  (("-" "g -") vim:universal-argument-minus)

  ("g g" vim:motion-go-to-first-non-blank-beg)
  ("G"   vim:motion-go-to-first-non-blank-end)
  ("j"   nil)

  ("%"   nil)
  ;; short for matching
  ("m"   vim:motion-jump-item)

  ("q"   sp-up-sexp))

(def-keys-for-map vim:operator-pending-mode-keymap
  (("is" "s") vim:motion-inner-symbol)
  ("as"       vim:motion-outer-symbol))

(def-keys-for-map (vim:operator-pending-mode-keymap
                   vim:motion-mode-keymap)
  ("'" sp-backward-up-sexp)
  ;; ("u" vim:motion-search-next)
  ;; ("U" vim:motion-search-next-reverse)
  ("]" vim:motion-bwd-paragraph)
  ("[" vim:motion-fwd-paragraph))

(defconst +vim-navigation-keys+
  `(("d"         vim:motion-left)
    ("h"         vim:motion-down)
    ("t"         vim:motion-up)
    ("n"         vim:motion-right)

    (";"         vim:motion-repeat-last-find)
    (":"         vim:motion-repeat-last-find-opposite)

    ("C-:"       pp-eval-expression)
    ("g x"       ivy-smex)
    ("<down>"    vim:motion-fwd-paragraph)
    ("<up>"      vim:motion-bwd-paragraph)

    ,@+vim-interbuffer-navigation-keys+
    ,@+vim-character-navigation-keys+))

(defconst +vim-normal-mode-navigation-keys+
  '(("'"   sp-backward-up-sexp)
    ("]"   vim:motion-bwd-paragraph)
    ("["   vim:motion-fwd-paragraph)
    ("s"   vim:ex-read-command)
    ("g f" counsel-find-file)
    ("g r" egrep)))

(def-keys-for-map (vim:normal-mode-keymap
                   vim:visual-mode-keymap)
  +vim-navigation-keys+
  +vim-search-keys+
  +vim-search-extended-keys+

  (","       vim:cmd-delete)
  ("l"       vim:cmd-change-char)

  ("="       vim:cmd-paste-pop-prev)
  ("+"       vim:cmd-paste-pop-next)
  ("X"       vim:cmd-delete-char-backward)
  ("M"       vim:jump-to-prev-saved-position)

  ("g u"     undo-tree-visualize)

  ("S-<backspace>" delete-whitespace-backward)
  ("S-<delete>"    delete-whitespace-forward)
  ("C-w"           backward-delete-word)
  ("C-S-w"         backward-delete-word*)

  ("Z"       nil)

  ("g h"     nil)

  ;; ("<f5>"    vim:motion-mark)
  ("g m"     vim:cmd-set-mark)
  ("Q"       vim:cmd-toggle-macro-recording)
  ("S"       sp-split-sexp)
  ("g J"     sp-join-sexp)
  ("g j"     nil)
  ("g q"     nil))

;;; normal mode keybindigs

(def-keys-for-map vim:normal-mode-keymap
  +vim-normal-mode-navigation-keys+
  ("C-y"       nil)
  ;; names of these two functions are swapped for unknown reason
  ;; anyway, so don't change order
  ("{"         scroll-up)
  ("}"         scroll-down)
  ("!"         shell-command+)

  ("g ("       vim:sp-splice-sexp-killing-backward)
  ("g )"       vim:sp-splice-sexp-killing-forward)
  ("C-("       vim:sp-splice-sexp-killing-backward)
  ("C-)"       vim:sp-splice-sexp-killing-forward)
  ("( l"       vim:sp-absorb-sexp)
  ("( r"       sp-emit-sexp)

  ("( ("       vim:sp-backward-slurp-sexp)
  ("( )"       vim:sp-backward-barf-sexp)
  (") ("       vim:sp-forward-barf-sexp)
  (") )"       vim:sp-forward-slurp-sexp)

  ("M-?"       sp-convolute-sexp)
  ("M-<left>"  sp-absorb-sexp)
  ("M-<right>" sp-emit-sexp)

  ("x"         vim:cmd-delete-char)
  ("X"         vim:cmd-delete-char-backward)

  ("<insert>"  vim:scroll-line-up)
  ("<delete>"  vim:scroll-line-down)

  ("D"         vim:delete-current-line)
  ("k"         undo-tree-undo)
  ("K"         undo-tree-redo)
  ("J"         vim:cmd-join-lines)

  ("C-p"       yank)
  ("M-p"       browse-kill-ring)

  ("g s w"     vim-replace-word)
  ("g s W"     vim-replace-WORD)
  ("g s s"     vim-replace-symbol-at-point)

  ;; ("g g f"     find-filename-in-tree-recursive)
  ("g c c"     comment-util-comment-lines)
  ("g c u"     comment-util-uncomment-region)
  ("g c d"     comment-util-delete-commented-part)

  ("g TAB"     nil)
  ("g n"       nil)
  ("g t"       nil)
  ("g #"       server-edit)

  ("<f5>"      vim:revert-buffer)
  ("H"         vim:revert-buffer))

;;; visual keybindings

(def-keys-for-map vim:visual-mode-keymap
  ("'"       self-insert-command)

  ("!"       shell-command-on-region)
  ("TAB"     indent-region)
  ("<tab>"   indent-region)
  ("s"       vim:visual-ex-read-command)
  ("k"       vim:cmd-make-downcase)
  ("K"       vim:cmd-make-upcase)

  ("g c c"   comment-util-comment-region)
  ("g c u"   comment-util-uncomment-region-simple)

  ("SPC SPC" vim:visual-exchange-point-and-mark)

  ("g r"     egrep-region)
  ("g s"     vim-replace-selected)
  (("(" ")") vim:wrap-parens)
  (("[" "]") vim:wrap-braces)
  (("{" "}") vim:wrap-brackets)

  ("J"       vim:cmd-join)

  ("C-'"     vim:wrap-typographical-single-quotes)
  ("C-\""    vim:wrap-typographical-double-quotes))

(vim:defcmd vim:wrap-parens (nonrepeatable)
  "Wrap region in (...)."
  (sp-wrap-or-insert "("))

(vim:defcmd vim:wrap-braces (nonrepeatable)
  "Wrap region in [...]."
  (sp-wrap-or-insert "["))

(vim:defcmd vim:wrap-brackets (nonrepeatable)
  "Wrap region in {...}."
  (sp-wrap-or-insert "{"))

(vim:defcmd vim:wrap-typographical-single-quotes (nonrepeatable)
  "Wrap region in ‘...’."
  (sp-wrap-or-insert "‘"))

(vim:defcmd vim:wrap-typographical-double-quotes (nonrepeatable)
  "Wrap region in “...”."
  (sp-wrap-or-insert "“"))

(vim:defcmd vim:wrap-backticks (nonrepeatable)
  "Wrap region in `...`."
  (sp-wrap-or-insert "`"))

;;; insert mode keybindings

(def-keys-for-map vim:insert-mode-keymap
  ("S-<backspace>" delete-whitespace-backward)
  ("S-<delete>"    delete-whitespace-forward)
  ("C-w"           backward-delete-word)
  ("C-S-w"         backward-delete-word*)
  ("C-r"           nil)
  ("C-p"           yank)
  ("M-p"           browse-kill-ring)
  ("SPC"           abbrev+-insert-space-or-expand-abbrev)
  ("<insert>"      vim:scroll-line-up)
  ("C-b"           switch-to-buffer-or-file-in-current-project)
  ("M-b"           switch-to-buffer-or-file-in-current-or-related-projects)
  ("C-:"           pp-eval-expression)

  ("("             vim:wrap-parens)
  ("["             vim:wrap-braces)
  ("{"             vim:wrap-brackets)
  ("C-'"           typopunct-insert-single-quotation-mark)
  ("C-\""          typopunct-insert-quotation-mark)
  ("C--"           typopunct-insert-typographical-dashes))

;;; ex bindings and commands

(def-keys-for-map vim:ex-keymap
  ("C-v"     set-mark-command)
  ("C-y"     copy-region-as-kill)
  ("<prior>" nil)
  ("<next>"  nil))

(vim:defcmd vim:jump-to-prev-saved-position (nonrepeatable keep-visual)
  "Jump to position pointed to by ' mark.
Basically swap current point with previous one."
  (vim:motion-mark :argument ?\'))

(vim:defcmd vim:cmd-only (nonrepeatable keep-visual)
  "Close all window except current one, just like C-x 1."
  (delete-other-windows))

(vim:emap "only" #'vim:cmd-only)
(vim:emap "on" #'vim:cmd-only)

(vim:defcmd vim:cmd-close (nonrepeatable keep-visual)
  "Close current window, just like C-x 0."
  (delete-window))

(vim:emap "close" #'vim:cmd-close)
(vim:emap "cl" #'vim:cmd-close)

(vimmize-function
 split-window-horizontally
 ;; this is not a bug, name is correct!
 :name vim:cmd-split-vertically
 :doc   "Split current window vertically, just like C-x 3."
 :repeatable nil
 :keep-visual t)

(vimmize-function
 split-window-vertically
 ;; this is not a bug, this name is correct too!
 :name vim:cmd-split-horizontally
 :doc "Split current window horizontally, just like C-x 2."
 :repeatable nil
 :keep-visual t)

(vimmize-function
 transpose-windows
 :name vim:transpose-windows
 :has-count nil
 :repeatable nil
 :keep-visual t)

(vim:emap "hsplit" #'vim:cmd-split-horizontally)
(vim:emap "hs" #'vim:cmd-split-horizontally)
(vim:emap "vsplit" #'vim:cmd-split-vertically)
(vim:emap "vs" #'vim:cmd-split-vertically)
(vim:emap "transpose" #'vim:transpose-windows)
(vim:emap "tr" #'vim:transpose-windows)

(vim:emap "write" #'vim:cmd-write-current-buffer)
(vim:emap "w" #'vim:cmd-write-current-buffer)


(autoload 'awk-exit "awk+")
(autoload 'awk-on-region "awk+")

(vim:defcmd vim:start-awk (motion nonrepeatable)
  (when (get-buffer awk-buffer-name)
    (with-current-buffer (get-buffer awk-buffer-name)
      (awk-exit)))
  ;; turn visual mode off
  (when (region-active-p)
    (deactivate-mark)
    (vim:visual-mode-exit))
  (awk-on-region (if motion
                     (vim:motion-begin-pos motion)
                   (line-beginning-position))
                 (if motion
                     (vim:motion-end-pos motion)
                   (line-end-position))))

(vim:emap "awk" #'vim:start-awk)

;; this is absolutely necessary to make vim recognize local keymaps
;; on it's first entrance into some mode
(add-hook 'after-change-major-mode-hook 'vim:normal-mode-update-keymaps t)



;; apply given ex command to all ibuffer-selected buffers

(vim:defcmd vim:apply-to-selected-buffers
  ((argument:text command) nonrepeatable)
  (cl-assert command)
  (let* ((cmd (trim-whitespace-left command))
         (exec-command
          (lambda (buf)
            (let ((window (or (get-buffer-window buf)
                              (selected-window))))
              (with-current-buffer buf
                (with-selected-window window
                  ;; adapted from vim:ex-read-command
                  (let ((vim:ex-current-buffer buf)
                        (vim:ex-current-window window))
                    (vim:ex-execute-command cmd))))))))
    (cond ((eq? major-mode 'ibuffer-mode)
           (mapc exec-command
                 (ibuffer-get-marked-buffers)))
          (t
           (error "command works in ibuffer-mode only")))))

(vim:emap "in-bufs" #'vim:apply-to-selected-buffers)


(autoload 'render-buffer "render-formula" "" t)

(vim:defcmd vim:render-latex (nonrepeatable)
  (if (memq major-mode '(latex-mode tex-mode LaTeX-mode))
      (latex-toggle-preview)
    (render-formula-toggle-formulae)))

(vim:emap "latex" #'vim:render-latex)



(vim:defcmd vim:remove-tabs (motion nonrepeatable)
  (remove-tabs (if motion
                   (vim:motion-begin-pos motion)
                 (line-beginning-position))
               (if motion
                   (vim:motion-end-pos motion)
                 (line-end-position))))

(vim:emap "no-tabs" #'vim:remove-tabs)

(vim:defcmd vim:narrow-to-region-indirect (motion nonrepeatable)
  (narrow-to-region-indirect
   (if motion
       (vim:motion-begin-pos motion)
     (point-min))
   (if motion
       (vim:motion-end-pos motion)
     (point-max))))

(vim:emap "narrow-indirect" #'vim:narrow-to-region-indirect)
(vim:emap "ni" #'vim:narrow-to-region-indirect)


(vim:defcmd vim:indent (nonrepeatable)
  (aif (gethash major-mode *mode-indent-functions-table*)
      (funcall it)
    (error "No indentation function defined for %s" major-mode)))

(vim:emap "indent" #'vim:indent)

(vim:defcmd vim:magit (nonrepeatable)
  "Show git status for current file's repository."
  (aif buffer-file-name
      (if *have-git?*
          (progn
            (git-update-file-repository)
            (if git-repository
                (magit-status git-repository)
              (progn
                (message "File %s is not under git VCS" it)
                (magit-status))))
        (magit-status (file-name-nondirectory it)))
    (progn
      (message "Warning: current buffer has no associated file")
      (magit-status))))

(vim:emap "magit" #'vim:magit)
(vim:emap "g" #'vim:magit)


(vim:defcmd vim:blame (nonrepeatable)
  "Run `magit-blame-mode'."
  (call-interactively #'magit-blame)
  (vim:activate-blame-mode))

(vim:emap "blame" #'vim:blame)

(vim:defcmd vim:blame-quit (nonrepeatable)
  "Stop `magit-blame-mode'."
  (unwind-protect
      (call-interactively #'magit-blame-quit)
    (unless magit-blame-mode
      (vim:activate-normal-mode))))

(vim:emap "blame-quit" #'vim:blame-quit)

(vim:defcmd vim:git-add (nonrepeatable)
  "Run git add on current file."
  (if buffer-file-name
      (progn
        (save-some-buffers)
        (git-add)
        (git-update-file-repository))
    (error "current buffer has no associated file")))

(vim:emap "add" #'vim:git-add)

(vim:defcmd vim:git-amend (nonrepeatable)
  "Amend topmost git commit with all staged changes."
  (save-window-excursion
    (call-process
     "git"
     nil
     nil
     nil
     "--no-pager"
     "commit"
     "--amend"
     "--reuse-message=HEAD")
    (magit-refresh-all)))

(vim:emap "amend" #'vim:git-amend)

(vim:defcmd vim:ibuffer (nonrepeatable)
  "Open ibuffer buffer."
  (ibuffer))

(vim:emap "ibuffer" #'vim:ibuffer)
(vim:emap "ib" #'vim:ibuffer)

;; Frame commands

(vim:defcmd vim:cmd-new-frame (nonrepeatable keep-visual)
  "Pops up a new frame."
  (new-frame))

(vim:emap "newf" #'vim:cmd-new-frame)
(vim:emap "nf" #'vim:cmd-new-frame)

;; Buffer commands

(vim:defcmd vim:remove-buffer ((argument:buffer buf) nonrepeatable)
  "Remove current buffer."
  (remove-buffer buf))

(vim:emap "rb" #'vim:remove-buffer)
(vim:emap "bd" #'vim:remove-buffer)

(vim:defcmd vim:remove-buffer-and-window (nonrepeatable)
  "Remove current buffer and close its window."
  (remove-buffer-and-window))

(vim:emap "rbw" #'vim:remove-buffer-and-window)
(vim:emap "bdw" #'vim:remove-buffer-and-window)


(vim:defcmd vim:do-commands
  ((argument:text command) nonrepeatable)
  (mapc #'vim:ex-execute-command
        (split-string command
                      "[ ,]\\|&&"
                      t)))

(vim:emap "do" #'vim:do-commands)

(vim:defcmd vim:revert-buffer (nonrepeatable)
  (revert-buffer))

(vim:defcmd vim:comint-clear-buffer-above-prompt (nonrepeatable)
  (comint-clear-buffer-above-prompt))

(vim:defcmd vim:shell-command-on-region (motion nonrepeatable (argument:text command))
  (let ((buf (current-buffer)))
    (shell-command-on-region
     (if motion
         (vim:motion-begin-pos motion)
       (line-beginning-position))
     (if motion
         (vim:motion-end-pos motion)
       (line-end-position))
     command
     nil ;; output buffer
     t   ;; replace
     )
    (if-buffer-has-file
      (when (and (not (file-exists? buffer-file-name))
                 (y-or-n? (format "Kill buffer %s?" (buffer-name buf))))
        (kill-buffer buf)))))

(vim:emap "!" #'vim:shell-command-on-region)

(provide 'vim-setup)

;; Local Variables:
;; End:

;; vim-setup.el ends here
