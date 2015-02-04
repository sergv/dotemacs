;; vim-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(require 'util-vim-replace)
(require 'completion-setup)
(require 'smartparens-setup)
(require 'search)
(require 'common)

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

(dolist (keymap (list vim:normal-mode-keymap
                      vim:visual-mode-keymap
                      vim:operator-pending-mode-keymap
                      vim:motion-mode-keymap))
  (def-keys-for-map keymap
    ("g g" vim:motion-go-to-first-non-blank-beg)
    ("G"   vim:motion-go-to-first-non-blank-end)
    ("j"   nil)

    ("%"   nil)
    ;; short for matching
    ("m"   vim:motion-jump-item)

    ("q"   sp-up-sexp)
    ("Q"   sp-backward-up-sexp)

    ;; ("<mouse-1>" vim:mouse-symbol/string/sexp)
    ))

(def-keys-for-map vim:operator-pending-mode-keymap
  ("is" vim:motion-inner-symbol)
  ("as" vim:motion-outer-symbol)
  ("s"  vim:motion-inner-symbol))

(def-keys-for-map (vim:operator-pending-mode-keymap
                   vim:motion-mode-keymap)
  ("'" sp-backward-up-sexp)
  ("k" vim:motion-search-next)
  ("K" vim:motion-search-next-reverse)
  ("]" vim:motion-bwd-paragraph)
  ("[" vim:motion-fwd-paragraph))


(def-keys-for-map (vim:normal-mode-keymap
                   vim:visual-mode-keymap)
  (";"       vim:cmd-delete)

  ("d"       vim:motion-left)
  ("h"       vim:motion-down)
  ("t"       vim:motion-up)
  ("n"       vim:motion-right)
  ("l"       vim:cmd-change-char)

  (":"       vim:motion-repeat-last-find)

  ("/"       search-start-forward)
  ("C-/"     search-start-forward-new-color)
  ("?"       search-start-backward)
  ("C-?"     search-start-backward-new-color)
  ("k"       search-next)
  ("K"       search-prev)
  ("*"       search-for-symbol-at-point-forward)
  ("C-*"     search-for-symbol-at-point-forward-new-color)
  ("#"       search-for-symbol-at-point-backward)
  ("C-#"     search-for-symbol-at-point-backward-new-color)
  ;; ("g *"     search-for-word-at-point-forward)
  ;; ("g M-*"   search-for-word-at-point-forward-new-color)
  ;; ("g #"     search-for-word-at-point-backward)
  ;; ("g M-#"   search-for-word-at-point-backward-new-color)

  ("-"       vim:cmd-negate-or-paste-pop)
  ("="       vim:cmd-paste-pop-next)
  ("X"       vim:cmd-delete-char-backward)
  ("M"       vim:jump-to-prev-saved-position)
  ("J"       vim:cmd-join-lines)

  ("SPC u"   undo-tree-visualize)

  ("S-<backspace>" delete-whitespace-backward)
  ("S-<delete>"    delete-whitespace-forward)
  ("C-w"           backward-delete-word)
  ("C-S-w"         backward-delete-word*)

  ("Z"       nil)
  ("`"       nil)

  ("g u"     Control-X-prefix)
  ("g h"     nil)

  ;; ("<f5>"    vim:motion-mark)
  ("g m"     vim:cmd-set-mark)
  ("g M"     vim:cmd-toggle-macro-recording)
  ("S"       sp-split-sexp)
  ("g J"     sp-join-sexp)
  ("g j"     nil)
  ("g q"     nil)
  ("g x"     smex)

  ("<home>"  vim:motion-bwd-paragraph)
  ("<end>"   vim:motion-fwd-paragraph)
  (","       nil)

  ("C-b"     icicle-buffer)
  ("C-h"     search-toggle-highlighting)
  ;; rebind "C-h" for terminals that refuse to send "C-h" and
  ;; send "C-<backspace>" instead
  ("C-<backspace>" search-toggle-highlighting)
  ("<C-backspace>" search-toggle-highlighting)

  ("<down>"  vim:motion-fwd-paragraph)
  ("<up>"    vim:motion-bwd-paragraph)
  ("<left>"  prev-w)
  ("<right>" next-w))

;;; normal mode keybindigs

(def-keys-for-map vim:normal-mode-keymap
  ("C-y"       nil)
  ("s"         vim:ex-read-command)
  ("'"         sp-backward-up-sexp)
  ("]"         vim:motion-bwd-paragraph)
  ("["         vim:motion-fwd-paragraph)
  ;; names of these two functions are swapped for unknown reason
  ;; anyway, so don't change order
  ("{"         scroll-up)
  ("}"         scroll-down)
  ("!"         shell-command+)

  ("g ("       vim:sp-splice-sexp-killing-backward)
  ("g )"       vim:sp-splice-sexp-killing-forward)
  ("g <up>"    vim:sp-splice-sexp-killing-around)
  ("( l"       vim:sp-absorb-sexp)
  ("( r"       sp-emit-sexp)

  ("( ("       vim:sp-backward-slurp-sexp)
  ("( )"       vim:sp-backward-barf-sexp)
  (") ("       vim:sp-forward-barf-sexp)
  (") )"       vim:sp-forward-slurp-sexp)

  ("M-?"       sp-convolute-sexp)
  ("C-<left>"  vim:sp-backward-slurp-sexp)
  ("C-<right>" vim:sp-forward-slurp-sexp)
  ("M-<left>"  sp-absorb-sexp)
  ("M-<right>" sp-emit-sexp)

  ("x"         vim:cmd-delete-char)
  ("X"         vim:cmd-delete-char-backward)

  ("<insert>"  vim:scroll-line-up)
  ("<delete>"  vim:scroll-line-down)

  ("u"         undo-tree-undo)
  ("U"         undo-tree-redo)

  ("C-p"       yank)
  ("M-p"       browse-kill-ring)
  ("C-M-p"     browse-kill-ring)

  (", s"       nil)
  (", s w"     vim:replace-word)
  (", s W"     vim:replace-WORD)
  (", s s"     vim:replace-symbol-at-point)

  ("g f"       icicle-file)
  ;; ("g g f"     find-filename-in-tree-recursive)
  ("g c c"     comment-util-comment-lines)
  ("g c u"     comment-util-uncomment-region)

  ("g r"       rgrep-wrapper)
  ("g TAB"     nil)
  ("g n"       nil)
  ("g t"       nil)
  ("g <up>"    nil)
  ("g <down>"  nil)

  ("g c s"     remember-win-config-store-configuration)
  ("g c l"     remember-win-config-restore-configuration)
  ("g c r"     remember-win-config-restore-configuration))

;;; visual keybindings

(def-keys-for-map vim:visual-mode-keymap
  ("TAB"      indent-region)
  ("<tab>"    indent-region)
  ("s"        vim:visual-ex-read-command)

  ("g c c"    comment-util-comment-region)
  ("g c u"    comment-util-uncomment-region-simple)

  ("SPC SPC"  vim:visual-exchange-point-and-mark)

  ("g r"      rgrep-region)
  (", s"      vim:replace-selected)
  ("'"        sp--self-insert-command)
  ("\""       sp--self-insert-command)
  ("["        sp--self-insert-command)
  ("]"        sp--self-insert-command)
  ("{"        sp--self-insert-command)
  ("}"        sp--self-insert-command))

;;; insert mode keybindings

(def-keys-for-map vim:insert-mode-keymap
  ("S-<backspace>" delete-whitespace-backward)
  ("S-<delete>"    delete-whitespace-forward)
  ("C-w"           backward-delete-word)
  ("C-S-w"         backward-delete-word*)
  ("C-r"           nil)
  ("C-p"           yank)
  ("C--"           yank-previous)
  ("C-+"           yank-next)
  ("SPC"           abbrev+-insert-space-or-expand-abbrev)
  ("<insert>"      vim:scroll-line-up)
  ("C-b"           icicle-buffer))

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

(vim:emap "only" 'vim:cmd-only)
(vim:emap "on" 'vim:cmd-only)

(vim:defcmd vim:cmd-close (nonrepeatable keep-visual)
  "Close current window, just like C-x 0."
  (icicle-delete-window nil))

(vim:emap "close" 'vim:cmd-close)
(vim:emap "cl" 'vim:cmd-close)

(vim:defcmd vim:cmd-split-vertically (nonrepeatable keep-visual)
  "Split current window vertically, just like C-x 3."
  ;; this is not a bug, function is correct!
  (split-window-horizontally))

(vim:defcmd vim:cmd-split-horizontally (nonrepeatable keep-visual)
  "Split current window horizontally, just like C-x 2."
  ;; this is not a bug, function is correct too!
  (split-window-vertically))

(vim:emap "hsplit" 'vim:cmd-split-horizontally)
(vim:emap "hs" 'vim:cmd-split-horizontally)
(vim:emap "vsplit" 'vim:cmd-split-vertically)
(vim:emap "vs" 'vim:cmd-split-vertically)

(vim:emap "write" 'vim:cmd-write-current-buffer)
(vim:emap "w" 'vim:cmd-write-current-buffer)


(vim:defcmd vim:start-awk (motion nonrepeatable)
  (require 'awk+)
  (when (get-buffer awk-buffer-name)
    (with-current-buffer (get-buffer awk-buffer-name)
      (awk-exit)))
  ;; turn visual mode off
  (when (region-active?)
    (deactivate-mark)
    (run-if-fbound vim:visual-mode-exit))
  (awk-on-region (if motion
                   (vim:motion-begin motion)
                   (line-beginning-position))
                 (if motion
                   (vim:motion-end motion)
                   (line-end-position))))

(vim:emap "awk" 'vim:start-awk)

;; this is absolutely necessary to make vim recognize local keymaps
;; on it's first entrance into some mode
(add-hook 'after-change-major-mode-hook 'vim:normal-mode-update-keymaps t)



;; apply given ex command to all ibuffer-selected buffers

(vim:defcmd vim:apply-to-selected-buffers
  ((argument:text command) nonrepeatable)
  (let ((exec-command
         (lambda (buf)
           (let ((window (or (get-buffer-window buf)
                             (selected-window))))
             (with-current-buffer buf
               (with-selected-window window
                 ;; adapted from vim:ex-read-command
                 (let ((vim:ex-current-buffer buf)
                       (vim:ex-current-window window)
                       (cmd (trim-whitespace-left command)))
                   (if (and cmd
                            (not (zero? (length cmd))))
                     (vim:ex-execute-command cmd)
                     (error "invalid command \"%s\"" cmd)))))))))
    (cond ((eq? major-mode 'ibuffer-mode)
           (mapc exec-command
                 (ibuffer-get-marked-buffers)))
          (t
           (error "command works in ibuffer-mode only")))))

(vim:emap "in-bufs" 'vim:apply-to-selected-buffers)


(autoload 'render-buffer "render-formula" "" t)

(vim:defcmd vim:render-latex (nonrepeatable)
  (if (memq major-mode '(latex-mode tex-mode LaTeX-mode))
    (latex-toggle-preview)
    (render-formula-toggle-formulae)))

(vim:emap "latex" 'vim:render-latex)



(vim:defcmd vim:remove-tabs (motion nonrepeatable)
  (remove-tabs (if motion
                 (vim:motion-begin motion)
                 (line-beginning-position))
               (if motion
                 (vim:motion-end motion)
                 (line-end-position))))

(vim:emap "no-tabs" 'vim:remove-tabs)



(vim:defcmd vim:indent (nonrepeatable)
  (aif (assq major-mode *mode-buffer-indent-function-alist*)
    (funcall (cdr it))
    (error "No indentation function defined for %s" major-mode)))

(vim:emap "indent" 'vim:indent)

(vim:defcmd vim:magit (nonrepeatable)
  "Show git status for current file's repository."
  (aif (buffer-file-name)
    (if *have-git?*
      (begin
        (git-update-file-repository)
        (if git-repository
          (magit-status git-repository)
          (progn
            (message "File %s is not under git VCS" it)
            (magit-status default-directory))))
      (magit-status (file-name-nondirectory it)))
    (begin
      (message "Warning: current buffer has no associated file")
      (magit-status default-directory))))

(vim:emap "magit" 'vim:magit)
(vim:emap "g" 'vim:magit)


(vim:defcmd vim:git-blame (nonrepeatable)
  "Run `magit-blame-mode'."
  (magit-blame-mode 'toggle))

(vim:emap "gblame" 'vim:git-blame)

(vim:defcmd vim:git-add (nonrepeatable)
  "Run git add on current file."
  (aif (buffer-file-name)
    (progn
      (git-update-file-repository)
      (if git-repository
        (git-add)
        (error "File %s is not under git VCS" it)))
    (error "current buffer has no associated file")))

(vim:emap "gadd" 'vim:git-add)


(vim:defcmd vim:ibuffer (nonrepeatable)
  "Open ibuffer buffer."
  (ibuffer))

(vim:emap "ibuffer" 'vim:ibuffer)
(vim:emap "ib" 'vim:ibuffer)

;; Frame commands

(vim:defcmd vim:cmd-new-frame (nonrepeatable keep-visual)
  "Pops up a new frame."
  (new-frame))

(vim:emap "newf" 'vim:cmd-new-frame)

(vim:defcmd vim:next-f (count nonrepeatable)
  "Select next frame."
  (next-f (or count 1)))

(vim:emap "nextf" 'vim:next-f)
(vim:emap "nf" 'vim:next-f)

(vim:defcmd vim:prev-f (count nonrepeatable)
  "Select previous frame."
  (prev-f (or count 1)))

(vim:emap "prevf" 'vim:prev-f)
(vim:emap "pf" 'vim:prev-f)

;; Buffer commands

(vim:defcmd vim:remove-buffer ((argument:buffer buf) nonrepeatable)
  "Remove current buffer."
  (remove-buffer buf))

(vim:emap "rembuf" 'vim:remove-buffer)
(vim:emap "rb" 'vim:remove-buffer)

(vim:defcmd vim:remove-buffer-and-window (nonrepeatable)
  "Remove current buffer and close its window."
  (remove-buffer-and-window))

(vim:emap "rembufwin" 'vim:remove-buffer-and-window)
(vim:emap "rbw" 'vim:remove-buffer-and-window)

(provide 'vim-setup)

;; Local Variables:
;; End:

;; vim-setup.el ends here
