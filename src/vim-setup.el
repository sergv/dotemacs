;; vim-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (defvar awk-buffer-name)
  (defvar git-repository)
  (defvar magit-blame-mode))

(declare-function ibuffer-get-marked-buffers "ibuffer")
(declare-function magit-blame-quit "magit-blame")
(declare-function magit-refresh-all "magit-mode")
(declare-function server-edit "server")
(declare-function vim:activate-blame-mode "git-setup")

(require 'common)
(require 'completion-setup)
(require 'hydra-setup)
(require 'keys-def)
(require 'search)
(require 'smartparens-setup)
(require 'vim-ex)
(require 'vim-replace)

;;; configuration variables

(defcustom vim-scroll-move-point t
  "Controlls whether scrolling functions like `vim:scroll-line-up' should
move point to next/previous line."
  :type 'boolean
  :group 'vim-mode-general)

;;; overriding keymap

(defvar-local vim:complex-command-override-local-keymap nil
  "Keymap for buffer-local overriding of commands that take motions,
like \"d w\".")

;;; keybindings

(defhydra-ext hydra-vim-normal-j-ext (:exit t :foreign-keys nil :hint nil)
  "
_t_oggle   _cc_: comment         _sw_: replace word
_w_indows  _cu_: uncomment       _sW_: relpace WORD
ta_b_s     _cd_: delete comment  _ss_: replace symbol

_(d_: (a | b)         -> |b               _((_: a (b | …) -> (a b | …)
_)d_: (a | b)         -> a|               _()_: a (b | …) -> a b (| …)
_(a_: (a | (b c) d)   -> |(b c)           _))_: (… | a) b -> (… |) a b
_)a_: (a | (b c) d)   -> |(b c)           _)(_: (… | a) b -> (… | a b)

_a_bsorb:         a (b | c)       -> (b a | c)
_e_mit:           (a b c | d)     -> b c (a | d)
_?_ (convolute):  (a b (c d | e)) -> (c d (a b | e))
_S_plit sexp:     (a | b)         -> (a) |(b)
_J_oin sexp:      (a) | (b)       -> (a |  b)"
  ("w"   hydra-window-management/body)
  ("t"   toggle)
  ("b"   hydra-tab-management/body)

  ("cc"  comment-util-comment-lines)
  ("cu"  comment-util-uncomment-region)
  ("cd"  comment-util-delete-commented-part)

  ("sw"  vim-replace-word)
  ("sW"  vim-replace-WORD)
  ("ss"  vim-replace-symbol-at-point)

  ("(d"  vim:sp-splice-sexp-killing-backward)
  (")d"  vim:sp-splice-sexp-killing-forward)
  ("(a"  vim:sp-splice-sexp-killing-around)
  (")a"  vim:sp-splice-sexp-killing-around)
  ("a"   sp-absorb-sexp)
  ("e"   sp-emit-sexp)
  ("?"   sp-convolute-sexp)
  ("S"   vim:sp-split-sexp)
  ("J"   vim:sp-join-sexp)

  ("(("  vim:sp-backward-slurp-sexp)
  ("()"  vim:sp-backward-barf-sexp)
  (")("  vim:sp-forward-barf-sexp)
  ("))"  vim:sp-forward-slurp-sexp)

  ("( <left>"  vim:sp-backward-slurp-sexp)
  ("( <right>" vim:sp-backward-barf-sexp)
  (") <left>"  vim:sp-forward-barf-sexp)
  (") <right>" vim:sp-forward-slurp-sexp))

(defhydra-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
new _f_rame           _#_: finish server edit          previous word _e_nd
_g_o to start of file _k_ill buffer                    previous WORD _E_nd
g_r_ep                _K_ill buffer and delete window
_u_ndo tree           set _m_ark
M-_x_                 reactivate _v_isual mode"
  ("f" make-frame)
  ("g" vim-mock:motion-go-to-first-non-blank-beg)
  ("r" egrep)
  ("u" undo-tree-visualize)
  ("x" ivy-smex)

  ("#" server-edit)
  ("k" remove-buffer)
  ("K" remove-buffer-and-window)
  ("m" vim:cmd-set-mark)
  ("v" vim:visual-mode-reactivate)

  ("e" vim-mock:motion-bwd-word-end)
  ("E" vim-mock:motion-bwd-WORD-end))

(defhydra-ext hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
scroll to _b_ottom
scroll to _t_op
_z_: scroll to center"
  ("b" vim:scroll-line-to-bottom)
  ("t" vim:scroll-line-to-top)
  ("z" vim:scroll-line-to-center))

(defhydra-ext hydra-vim-visual-z-ext (:exit t :foreign-keys nil :hint nil)
  "
scroll to _b_ottom
scroll to _t_op
_z_: scroll to center"
  ("b" vim:scroll-line-to-bottom)
  ("t" vim:scroll-line-to-top)
  ("z" vim:scroll-line-to-center))

(defhydra-ext hydra-vim-visual-j-ext (:exit t :foreign-keys nil :hint nil)
  "
_cc_: comment
_cu_: uncomment

replace _s_elected"
  ("cc" comment-util-comment-region)
  ("cu" comment-util-uncomment-region-simple)
  ("s" vim-replace-selected))

(defhydra-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_g_o to start of file
g_r_ep
M-_x_"
  ("g" vim-mock:motion-go-to-first-non-blank-beg)
  ("r" egrep-region)
  ("x" ivy-smex))

(defhydra-ext hydra-window-management (:exit t :foreign-keys warn :hint nil)
  "
_c_lose      _h_orizontal split
_o_nly       _v_ertical split
_t_ranspose  _b_alance

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
_n_ew
_c_lose
_o_nly (close other)
_u_ndo
_s_elect by name
_r_ename

_<left>_:  move tab to the left
_<right>_: move tab to the right"
  ("n" tab-new)
  ("c" tab-close)
  ("o" tab-close-other)
  ("u" tab-undo)
  ("s" tab-bar-select-tab-by-name)
  ("r" tab-rename)

  ("<left>"  tabbar-move-tab-to-the-left :exit nil)
  ("<right>" tabbar-move-tab-to-the-right :exit nil))

;; redefine motions

(def-keys-for-map (vim:normal-mode-keymap
                   vim:visual-mode-keymap
                   vim:operator-pending-mode-keymap
                   vim:motion-mode-keymap)
  ("0"         vim:motion-beginning-of-line-or-digit-argument)
  (("1" "2" "3" "4" "5" "6" "7" "8" "9")
   vim:digit-argument)
  ("-"   vim:universal-argument-minus)

  ("G"   vim:motion-go-to-first-non-blank-end)
  ("j"   nil)

  ("%"   nil)
  ;; short for matching
  ("m"   vim:motion-jump-item)

  ("q"   sp-up-sexp))

(def-keys-for-map (vim:operator-pending-mode-keymap
                   vim:motion-mode-keymap)
  ("g g" vim:motion-go-to-first-non-blank-beg))

(def-keys-for-map vim:operator-pending-mode-keymap
  (("is" "s") vim:motion-inner-symbol)
  ("as"       vim:motion-outer-symbol))

(def-keys-for-map (vim:operator-pending-mode-keymap
                   vim:motion-mode-keymap)
  ("'" sp-backward-up-sexp)
  ;; ("u" vim:motion-search-next)
  ;; ("U" vim:motion-search-next-reverse)
  (("<up>"   "]") vim:motion-bwd-paragraph)
  (("<down>" "[") vim:motion-fwd-paragraph))

(defconst +vim-navigation-keys+
  `(("d"         vim:motion-left)
    ("h"         vim:motion-down)
    ("t"         vim:motion-up)
    ("n"         vim:motion-right)

    (";"         vim:motion-repeat-last-find)
    (":"         vim:motion-repeat-last-find-opposite)

    ("C-:"       pp-eval-expression)
    ("<down>"    vim:motion-fwd-paragraph)
    ("<up>"      vim:motion-bwd-paragraph)

    ,@+vim-interbuffer-navigation-keys+
    ,@+vim-character-navigation-keys+))

(defconst +vim-normal-mode-navigation-keys+
  '(("'"   sp-backward-up-sexp)
    ("]"   vim:motion-bwd-paragraph)
    ("["   vim:motion-fwd-paragraph)
    ("s"   vim:ex-read-command)))

(def-keys-for-map (vim:normal-mode-keymap
                   vim:visual-mode-keymap)
  +vim-navigation-keys+
  +vim-search-keys+
  +vim-search-extended-keys+

  (","       vim:cmd-delete)
  ("l"       vim:cmd-change-char)

  ("X"       vim:cmd-delete-char-backward)
  ("M"       vim:jump-to-prev-saved-position)

  ("S-<backspace>" delete-whitespace-backward)
  ("S-<delete>"    delete-whitespace-forward)
  ("C-w"           backward-delete-word)
  ("C-S-w"         backward-delete-word*)

  ("Z"       nil)

  ("Q"       vim:cmd-toggle-macro-recording))

(def-keys-for-map (vim:normal-mode-keymap
                   vim:insert-mode-keymap)
  ("C-p"  vim:cmd-paste-behind-no-adjust)
  ("<f4>" vim:render-latex))

;;; normal mode keybindigs

(def-keys-for-map vim:normal-mode-keymap
  +vim-normal-mode-navigation-keys+
  ("C-y"       nil)
  ;; names of these two functions are swapped for unknown reason
  ;; anyway, so don't change order
  ("{"         scroll-up)
  ("}"         scroll-down)
  ("!"         shell-command+)
  ("~"         vim:cmd-toggle-case-one-char)

  ("M-?"       sp-convolute-sexp)
  ("M-<left>"  sp-absorb-sexp)
  ("M-<right>" sp-emit-sexp)

  ("x"         vim:cmd-delete-char)
  ("X"         vim:cmd-delete-char-backward)

  ("D"         vim:delete-current-line)
  ("k"         undo-tree-undo)
  ("K"         undo-tree-redo)
  ("J"         vim:cmd-join-lines)

  ("C-S-p"     browse-kill-ring)

  ("g"         hydra-vim-normal-g-ext/body)
  ("j"         hydra-vim-normal-j-ext/body)
  ("z"         hydra-vim-normal-z-ext/body)

  ("<f5>"      vim:revert-buffer)
  ("H"         vim:revert-buffer))

;;; visual keybindings

(def-keys-for-map vim:visual-mode-keymap
  ("'"       self-insert-command)

  ("!"       shell-command-on-region)
  ("|"       shell-command-on-region-and-replace)
  ("TAB"     indent-region)
  ("<tab>"   indent-region)
  ("s"       vim:visual-ex-read-command)
  ("k"       vim:cmd-make-downcase)
  ("K"       vim:cmd-make-upcase)

  ("SPC SPC" vim:visual-exchange-point-and-mark)

  ("j"       hydra-vim-visual-j-ext/body)
  ("g"       hydra-vim-visual-g-ext/body)
  ("z"       hydra-vim-visual-z-ext/body)

  (("(" ")")     vim:wrap-parens)
  (("[" "C")     vim:wrap-braces)
  (("{" "}")     vim:wrap-brackets)
  (("C-<" "C->") vim:wrap-angles)

  ("J"       vim:cmd-join)

  ("C-'"     vim:wrap-typographical-single-quotes)
  ("C-\""    vim:wrap-typographical-double-quotes))


(defun vim:wrap-parens ()
  "Wrap region in (...)."
  (interactive)
  (sp-wrap-or-insert "("))

(defun vim:wrap-braces ()
  "Wrap region in [...]."
  (interactive)
  (sp-wrap-or-insert "["))

(defun vim:wrap-brackets ()
  "Wrap region in {...}."
  (interactive)
  (sp-wrap-or-insert "{"))

(defun vim:wrap-angles ()
  "Wrap region in <...>."
  (interactive)
  (sp-wrap-or-insert "<"))

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
  ("C-S-p"         browse-kill-ring)
  ("SPC"           abbrev+-insert-space-or-expand-abbrev)
  ("<insert>"      vim:scroll-line-up)
  ("C-:"           pp-eval-expression)

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


(autoload 'render-formula-toggle-formulae "render-formula" "" t)

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
      (save-current-line-column
        (funcall it))
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
  (call-interactively #'magit-blame-addition)
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
  "Run 'git add' on current file."
  (save-some-buffers)
  (git-add)
  (git-update-file-repository))

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

(vim:defcmd vim:git-rm (nonrepeatable)
  "Run 'git rm' on current file."
  (save-some-buffers)
  (git-rm)
  (git-update-file-repository))

(vim:emap "rm" #'vim:git-rm)


(vim:defcmd vim:ibuffer (nonrepeatable)
  "Open ibuffer buffer."
  (ibuffer))

(vim:emap "ibuffer" #'vim:ibuffer)
(vim:emap "ib" #'vim:ibuffer)

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
    (when-buffer-has-file
      (when (and (not (file-exists? buffer-file-name))
                 (y-or-n? (format "Kill buffer %s?" (buffer-name buf))))
        (kill-buffer buf)))))

(vim:emap "!" #'vim:shell-command-on-region)

(provide 'vim-setup)

;; Local Variables:
;; End:

;; vim-setup.el ends here
