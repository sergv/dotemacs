;; vim-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'macro-util)

  (defvar awk-buffer-name)
  (defvar magit-blame-mode))

(declare-function magit-blame-quit "magit-blame")
(declare-function magit-refresh-all "magit-mode")
(declare-function server-edit "server")
(declare-function vim-activate-blame-mode "git-setup")

(require 'common)
(require 'completion-setup)
(require 'current-column-fixed)
(require 'hydra-setup)
(require 'keys-def)
(require 'search)
(require 'vim-ex)
(require 'vim-replace)
(require 'vim)

;;; configuration variables

(defcustom vim-scroll-move-point t
  "Controlls whether scrolling functions like `vim:scroll-line-up' should
move point to next/previous line."
  :type 'boolean
  :group 'vim-mode-general)

;;; keybindings

(defhydra-ext hydra-vim-normal-j-ext (:exit t :foreign-keys nil :hint nil)
  "
_t_oggle   _cc_: comment         _sw_: replace word    u_n_narrow
_w_indows  _cu_: uncomment       _sW_: relpace WORD
ta_b_s     _cd_: delete comment  _ss_: replace symbol

_C-(_: a (b | …) -> (a b | …)            _M-(_:    (a | b)         -> |b
_C-)_: (… | a) b -> (… |) a b            _M-)_:    (a | b)         -> a|
                                         _M-<up>_: (a | (b c) d)   -> |(b c)

_r_aise:          (a | (b c) d)   -> |(b c)
_?_ (convolute):  (a b (c d | e)) -> (c d (a b | e))
_S_plit sexp:     (a | b)         -> (a) |(b)
_J_oin sexp:      (a) | (b)       -> (a | b)

Full operation table:
_((_: a (b | …) -> (a b | …)             _(d_: (a | b)         -> |b
_()_: (a b | …) -> a (b | …)             _)d_: (a | b)         -> a|
_))_: (… | a) b -> (… | a b)
_)(_: (… | a b) -> (… | a) b
"
  ("w"          hydra-window-management/body)
  ("t"          toggle)
  ("b"          hydra-tab-management/body)

  ("cc"         vim:comment-util-comment-lines:interactive)
  ("cu"         vim:comment-util-uncomment-region:interactive)
  ("cd"         vim:comment-util-delete-commented-part:interactive)

  ("sw"         vim:replace-word:interactive)
  ("sW"         vim:replace-WORD:interactive)
  ("ss"         vim:replace-symbol-at-point:interactive)

  ("n"          unnarrow)

  ("M-\("       vim:splice-sexp-killing-backward:interactive)
  ("M-\)"       vim:splice-sexp-killing-forward:interactive)
  ("M-<up>"     vim:raise-sexp:interactive)

  ("C-\("       vim:backward-slurp-sexp:interactive)
  ("C-\)"       vim:forward-slurp-sexp:interactive)

  ("r"          vim:raise-sexp:interactive)
  ("?"          vim:convolute-sexp:interactive)
  ("S"          vim:split-sexp:interactive)
  ("J"          vim:join-sexps:interactive)

  ("\(d"        vim:splice-sexp-killing-backward:interactive)
  ("\)d"        vim:splice-sexp-killing-forward:interactive)

  ("\(\("       vim:backward-slurp-sexp:interactive)
  ("\(\)"       vim:backward-barf-sexp:interactive)
  ("\)\("       vim:forward-barf-sexp:interactive)
  ("\)\)"       vim:forward-slurp-sexp:interactive)

  ("\( <left>"  vim:backward-slurp-sexp:interactive)
  ("\( <right>" vim:backward-barf-sexp:interactive)
  ("\) <left>"  vim:forward-barf-sexp:interactive)
  ("\) <right>" vim:forward-slurp-sexp:interactive))

(defhydra-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
new _f_rame               _#_: finish server edit              previous word _e_nd
_g_o to start of file     _k_ill buffer                        previous WORD _E_nd
g_r_ep                    _K_ill buffer and delete window      _<_: keep upper merge
_u_ndo tree                                                  _>_: keep lower merge
reactivate _v_isual mode
M-_x_
"
  ("f" make-frame)
  ("g" vim:motion-go-to-first-non-blank-beg:interactive)
  ("r" egrep)
  ("u" undo-tree-visualize)
  ("v" vim:visual-mode-reactivate:interactive)
  ("x" counsel-M-x)

  ("#" server-edit)
  ("k" remove-buffer)
  ("K" remove-buffer-and-window)

  ("e" pseudovim-motion-bwd-word-end)
  ("E" pseudovim-motion-bwd-WORD-end)
  ("<" vim:smerge-keep-upper:interactive)
  (">" vim:smerge-keep-lower:interactive))

(defhydra-ext hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
scroll to _b_ottom
scroll to _t_op
_z_: scroll to center"
  ("b" vim:scroll-line-to-bottom:interactive)
  ("t" vim:scroll-line-to-top:interactive)
  ("z" vim:scroll-line-to-center:interactive))

(defhydra-ext hydra-vim-visual-z-ext (:exit t :foreign-keys nil :hint nil)
  "
scroll to _b_ottom
scroll to _t_op
_z_: scroll to center"
  ("b" vim:scroll-line-to-bottom:interactive)
  ("t" vim:scroll-line-to-top:interactive)
  ("z" vim:scroll-line-to-center:interactive))

(defhydra-ext hydra-vim-visual-j-ext (:exit t :foreign-keys nil :hint nil)
  "
_cc_: comment    replace _s_elected
_cu_: uncomment  _n_arrow to region
"
  ("cc" comment-util-comment-region)
  ("cu" comment-util-uncomment-region-simple)

  ("s"  vim:replace-selected:interactive)
  ("n"  narrow-to-region-indirect))

(defhydra-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_g_o to start of file
g_r_ep
M-_x_"
  ("g" vim:motion-go-to-first-non-blank-beg:interactive)
  ("r" egrep-region)
  ("x" counsel-M-x))

(defhydra-ext hydra-window-management (:exit t :foreign-keys warn :hint nil)
  "
_c_lose      _h_orizontal split  _b_alance
_o_nly       _v_ertical split    _B_alance other windows
_t_ranspose

_<left>_:    shrink horizontally
_<right>_:   enlarge horizontally
_<down>_:    shrink vertically
_<up>_:      enlarge vertically

_S-<left>_:  fast shrink horizontally
_S-<right>_: fast enlarge horizontally
_S-<down>_:  fast shrink vertically
_S-<up>_:    fast enlarge vertically
"
  ("c" delete-window)
  ("o" delete-other-windows)
  ("t" transpose-windows)
  ("b" balance-windows)
  ("B" balance-other-windows)

  ("h" split-window-vertically)
  ("v" split-window-horizontally)

  ("<left>"  shrink-window-horizontally :exit nil)
  ("<right>" enlarge-window-horizontally :exit nil)
  ("<up>"    enlarge-window :exit nil)
  ("<down>"  shrink-window :exit nil)

  ("S-<left>"  shrink-window-horizontally-fast :exit nil)
  ("S-<right>" enlarge-window-horizontally-fast :exit nil)
  ("S-<up>"    enlarge-window-fast :exit nil)
  ("S-<down>"  shrink-window-fast :exit nil))

(defun tabbar-move-tab-to-the-right ()
  (interactive)
  (tab-bar-move-tab 1))

(defun tabbar-move-tab-to-the-left ()
  (interactive)
  (tab-bar-move-tab -1))

(defhydra-ext hydra-tab-management (:exit t :foreign-keys warn :hint nil)
  "
_n_ew                 _s_elect by name
_c_lose               _r_ename
_o_nly (close other)
_u_ndo last close
cop_y_ tab

_<left>_:  move tab to the left
_<right>_: move tab to the right"
  ("n" tab-new)
  ("c" tab-close)
  ("o" tab-close-other)
  ("u" tab-undo)
  ("y" tab-bar-duplicate-tab)

  ("r" tab-rename)
  ("s" tab-bar-select-tab-by-name)

  ("<left>"  tabbar-move-tab-to-the-left :exit nil)
  ("<right>" tabbar-move-tab-to-the-right :exit nil))

;; redefine motions

(def-keys-for-map vim-motion-mode-keymap
  ("'" paredit-backward-up)
  ("q" paredit-forward-up))

(def-keys-for-map (vim-normal-mode-keymap
                   vim-visual-mode-keymap
                   vim-operator-pending-mode-keymap
                   vim-motion-mode-keymap)
  ("0"   vim-motion-beginning-of-line-or-digit-argument)
  (("1" "2" "3" "4" "5" "6" "7" "8" "9")
         vim-digit-argument)
  ("C--" vim-universal-argument-minus)

  ("G"   vim:motion-go-to-first-non-blank-end:interactive)
  ("j"   nil)

  ("%"   nil)
  ;; short for matching
  ("m"   vim:motion-jump-item:interactive))

(def-keys-for-map (vim-operator-pending-mode-keymap
                   vim-motion-mode-keymap)
  ("g g" vim:motion-go-to-first-non-blank-beg:interactive)
  ("-"   vim-universal-argument-minus))

(def-keys-for-map vim-operator-pending-mode-keymap
  (("is" "s") vim:motion-inner-symbol:interactive)
  ("as"       vim:motion-outer-symbol:interactive))

(def-keys-for-map (vim-operator-pending-mode-keymap
                   vim-motion-mode-keymap)
  ;; ("u" vim:motion-search-next)
  ;; ("U" vim:motion-search-next-reverse)
  (("<up>"   "]") vim:motion-bwd-paragraph:interactive)
  (("<down>" "[") vim:motion-fwd-paragraph:interactive))

(defconst +vim-navigation-keys+
  `(("d"         vim:motion-left:interactive)
    ("h"         vim:motion-down:interactive)
    ("t"         vim:motion-up:interactive)
    ("n"         vim:motion-right:interactive)

    (";"         vim:motion-repeat-last-find:interactive)
    (":"         vim:motion-repeat-last-find-opposite:interactive)

    ("C-:"       pp-eval-expression)
    ("<down>"    vim:motion-fwd-paragraph:interactive)
    ("<up>"      vim:motion-bwd-paragraph:interactive)

    ,@+vim-interbuffer-navigation-keys+
    ,@+vim-character-navigation-keys+))

(defconst +vim-normal-mode-navigation-keys+
  '(("'" paredit-backward-up)
    ("]" vim:motion-bwd-paragraph:interactive)
    ("[" vim:motion-fwd-paragraph:interactive)
    ("s" vim-ex-read-command)))

(def-keys-for-map (vim-normal-mode-keymap
                   vim-visual-mode-keymap)
  +vim-navigation-keys+
  +vim-search-keys+

  (","       vim:cmd-delete:interactive)

  ("X"       vim:cmd-delete-char-backward:interactive)
  ("M"       vim:jump-to-prev-saved-position:interactive)

  ("S-<backspace>" delete-whitespace-backward)
  ("S-<delete>"    delete-whitespace-forward)
  ("C-w"           backward-delete-word)
  ("C-S-w"         backward-delete-word*)

  ("Z"       nil)

  ("Q"       vim-cmd-toggle-macro-recording))

(def-keys-for-map (vim-normal-mode-keymap
                   vim-insert-mode-keymap)
  +vim-parens-keys+
  ("C-p"  vim-cmd-paste-after-no-adjust)
  ("<f4>" vim:render-latex:interactive))

;;; normal mode keybindigs

(def-keys-for-map vim-normal-mode-keymap
  +vim-normal-mode-navigation-keys+
  +vim-search-extended-keys+

  ("l"         vim:cmd-change-char:interactive)

  ("p"         vim:cmd-paste-after:interactive)
  ("P"         vim:cmd-paste-before:interactive)

  ("C-y"       nil)
  ;; names of these two functions are swapped for unknown reason
  ;; anyway, so don't change order
  ("{"          scroll-up)
  ("}"          scroll-down)
  ("!"          shell-command+)
  ("~"          vim:cmd-toggle-case-one-char:interactive)

  ("x"          vim:cmd-delete-char:interactive)
  ("X"          vim:cmd-delete-char-backward:interactive)

  ("y"          vim:cmd-yank:interactive)
  ("D"          vim:delete-current-line:interactive)
  ("Y"          vim:yank-current-line:interactive)

  ("k"          undo-tree-undo)
  ("K"          undo-tree-redo)
  ("J"          vim:cmd-join-lines:interactive)

  ("C-S-p"      browse-kill-ring)

  ("g"          hydra-vim-normal-g-ext/body)
  ("j"          hydra-vim-normal-j-ext/body)
  ("z"          hydra-vim-normal-z-ext/body)

  (("H" "<f5>") vim:revert-buffer:interactive)

  ("="          vim:cmd-decrement-at-point:interactive)
  ("+"          vim:cmd-increment-at-point:interactive))

;;; visual keybindings

(def-keys-for-map vim-visual-mode-keymap
  ("l"       vim:cmd-change:interactive)

  ("*"       vim:search-for-selected-region-forward:interactive)
  ("C-*"     vim:search-for-selected-region-forward-new-color:interactive)
  ("#"       vim:search-for-selected-region-backward:interactive)
  ("C-#"     vim:search-for-selected-region-backward-new-color:interactive)

  ("."       vim:visual-repeat:interactive)
  ("\""      vim-wrap-dquotes)

  ("p"       vim:visual-paste-after:interactive)
  ("P"       vim:visual-paste-before:interactive)

  ("I"       vim:visual-insert:interactive)
  ("A"       vim:visual-append:interactive)

  ("!"       shell-command-on-region)
  ("|"       shell-command-on-region-and-replace)
  ("s"       vim:visual-ex-read-command:interactive)
  ("k"       vim:cmd-make-downcase:interactive)
  ("K"       vim:cmd-make-upcase:interactive)

  ("SPC SPC" vim:visual-exchange-point-and-mark:interactive)

  ("j"       hydra-vim-visual-j-ext/body)
  ("g"       hydra-vim-visual-g-ext/body)
  ("z"       hydra-vim-visual-z-ext/body)

  (("(" ")")     vim-wrap-parens)
  (("[" "]")     vim-wrap-braces)
  (("{" "}")     vim-wrap-brackets)
  (("C-<" "C->") vim-wrap-angles)

  ("J"       vim:cmd-join:interactive)

  ("C-'"     vim-wrap-typographical-single-quotes)
  ("C-\""    vim-wrap-typographical-double-quotes)

  (("TAB" "<tab>") indent-region)

  ("="       vim:cmd-decrement:interactive)
  ("+"       vim:cmd-increment:interactive))

;;; insert mode keybindings

(def-keys-for-map vim-insert-mode-keymap
  ("S-<backspace>" delete-whitespace-backward)
  ("S-<delete>"    delete-whitespace-forward)
  ("C-w"           backward-delete-word)
  ("C-S-w"         backward-delete-word*)
  ("C-r"           nil)
  ("C-S-p"         browse-kill-ring)
  ("SPC"           abbrev+-insert-space-or-expand-abbrev)
  ("<insert>"      vim:scroll-line-up:interactive)
  ("C-:"           pp-eval-expression)

  ("C-\""          typopunct-insert-quotation-mark)
  ("C--"           typopunct-insert-typographical-dashes)

  ("\{"            pseudoparedit-insert-brace))

(def-keys-for-map (vim-insert-mode-keymap
                   vim-ex-keymap)
  ("C-'"           typopunct-insert-single-quotation-mark))

;;; ex bindings and commands

(def-keys-for-map vim-ex-keymap
  ("C-v"     set-mark-command)
  ("C-y"     copy-region-as-kill)
  ("<prior>" nil)
  ("<next>"  nil))

(vim-defcmd vim:jump-to-prev-saved-position (nonrepeatable keep-visual)
  "Jump to position pointed to by ' mark.
Basically swap current point with previous one."
  (vim:motion-mark:wrapper :argument ?\'))

(vim-defcmd vim:start-awk (motion nonrepeatable)
  (when (get-buffer awk-buffer-name)
    (with-current-buffer (get-buffer awk-buffer-name)
      (awk-exit)))
  ;; turn visual mode off
  (when (region-active-p)
    (deactivate-mark)
    (vim:visual-mode-exit:wrapper))
  (awk-on-region (if motion
                     (vim-motion-begin-pos motion)
                   (line-beginning-position))
                 (if motion
                     (vim-motion-end-pos motion)
                   (line-end-position))))

(vim-emap "awk" #'vim:start-awk)

;; this is absolutely necessary to make vim recognize local keymaps
;; on it's first entrance into some mode
(add-hook 'after-change-major-mode-hook 'vim-normal-mode-update-keymaps t)


(vim-defcmd vim:render-latex (nonrepeatable)
  (if (memq major-mode '(latex-mode tex-mode LaTeX-mode))
      (latex-toggle-preview)
    (render-formula-toggle-formulae)))

(vim-emap "latex" #'vim:render-latex)



(vim-defcmd vim:remove-tabs (motion nonrepeatable)
  (remove-tabs (if motion
                   (vim-motion-begin-pos motion)
                 (line-beginning-position))
               (if motion
                   (vim-motion-end-pos motion)
                 (line-end-position))))

(vim-emap "no-tabs" #'vim:remove-tabs)

(vim-defcmd vim:indent (nonrepeatable)
  (aif (gethash major-mode *mode-indent-functions-table*)
      (save-current-line-column
        (funcall it))
    (error "No indentation function defined for %s" major-mode)))

(vim-emap "indent" #'vim:indent)


(vim-defcmd vim:magit (nonrepeatable)
  "Show git status for current file's repository."
  (aif buffer-file-name
      (magit-status)
    (progn
      (message "Warning: current buffer has no associated file")
      (magit-status))))

(vim-emap "magit" #'vim:magit)
(vim-emap "g" #'vim:magit)


(vim-defcmd vim:blame (nonrepeatable)
  "Run `magit-blame-mode'."
  (call-interactively #'magit-blame-addition)
  (vim-activate-blame-mode))

(vim-emap "blame" #'vim:blame)


(vim-defcmd vim:blame-quit (nonrepeatable)
  "Stop `magit-blame-mode'."
  (unwind-protect
      (call-interactively #'magit-blame-quit)
    (unless magit-blame-mode
      (vim-activate-normal-mode))))

(vim-emap "blame-quit" #'vim:blame-quit)


(vim-defcmd vim:smerge (nonrepeatable)
  "Run `smerge-mode'."
  (unless smerge-mode
    (error "SMerge for this mode is not intended (run ‘M-x smerge-start-session’ to override)"))
  (unless (looking-at-p smerge-begin-re)
    (smerge-next))
  (vim-activate-smerge-mode))

(vim-emap "sm" #'vim:smerge)


(vim-defcmd vim:git-add (nonrepeatable)
  "Run 'git add' on current file."
  (save-some-buffers)
  (git-add))

(vim-emap "add" #'vim:git-add)

(vim-defcmd vim:git-amend (nonrepeatable)
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

(vim-emap "amend" #'vim:git-amend)

(vim-defcmd vim:git-rm (nonrepeatable)
  "Run 'git rm' on current file."
  (save-some-buffers)
  (git-rm))

(vim-emap "rm" #'vim:git-rm)


(vim-defcmd vim:ebuf (nonrepeatable)
  "Open ebuf buffer."
  (ebuf-start))

(vim-emap "b" #'vim:ebuf)

(vim-defcmd vim:do-commands
  ((argument:text command) nonrepeatable)
  (mapc #'vim-ex-execute-command
        (split-string command
                      "[ ,]\\|&&"
                      t)))

(vim-emap "do" #'vim:do-commands)

(vim-defcmd vim:revert-buffer (nonrepeatable)
  (revert-buffer))

(vim-defcmd vim:comint-clear-buffer-above-prompt (nonrepeatable)
  (comint-clear-buffer-above-prompt))

(vim-defcmd vim:shell-command-on-region (motion nonrepeatable (argument:text command))
  (let ((buf (current-buffer)))
    (shell-command-on-region
     (if motion
         (vim-motion-begin-pos motion)
       (line-beginning-position))
     (if motion
         (vim-motion-end-pos motion)
       (line-end-position))
     command
     nil ;; output buffer
     t   ;; replace
     )
    (when-buffer-has-file
      (when (and (not (file-exists? buffer-file-name))
                 (y-or-n-p (format "Kill buffer %s?" (buffer-name buf))))
        (kill-buffer buf)))))

(vim-emap "!" #'vim:shell-command-on-region)

(provide 'vim-setup)

;; Local Variables:
;; End:

;; vim-setup.el ends here
