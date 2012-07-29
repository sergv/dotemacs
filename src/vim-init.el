
(require 'util-vim-replace)
(require 'completion-setup)
(require 'search)

(defun vimrc-redefine-motions (keymap)
  (def-keys-for-map2 keymap
    ("g g" nil)
    ("g <" vim:motion-go-to-first-non-blank-beg)
    ("g >" vim:motion-go-to-first-non-blank-end)
    ("G"   nil)
    ("j"   nil)
    ("J"   nil)

    ("%"   nil)
    ;; short for matching
    ("m"   vim:motion-jump-item)

    ("]"   vim:motion-bwd-paragraph)
    ("["   vim:motion-fwd-paragraph)

    ("<mouse-1>" vim:mouse-symbol/string/sexp)))

(vimrc-redefine-motions vim:normal-mode-keymap)
(vimrc-redefine-motions vim:visual-mode-keymap)
(vimrc-redefine-motions vim:operator-pending-mode-keymap)
(vimrc-redefine-motions vim:motion-mode-keymap)


(def-keys-for-map2 vim:operator-pending-mode-keymap
  ("k"   vim:motion-search-next)
  ("K"   vim:motion-search-next-reverse)

  ("is" vim:motion-inner-symbol)
  ("as" vim:motion-outer-symbol)
  ("s"  vim:motion-inner-symbol))

(def-keys-for-map2 vim:motion-mode-keymap
  ("k"   vim:motion-search-next)
  ("K"   vim:motion-search-next-reverse))


(setf +vimrc:normal&visual-keys+
  '(("t"       vim:motion-down)
    ("n"       vim:motion-up)
    ("s"       vim:motion-right)
    ("l"       vim:cmd-change-char)

    ;; names of these two functions are swapped for unknown reason
    ;; anyway, so don't change order
    ("{"       scroll-up)
    ("}"       scroll-down)

    (":"       vim:motion-repeat-last-find)

    ("/"       search-start-forward)
    ("?"       search-start-backward)
    ("k"       search-next)
    ("K"       search-prev)
    ("*"       search-for-symbol-at-point-forward)
    ("#"       search-for-symbol-at-point-backward)
    ("g *"     search-for-word-at-point-forward)
    ("g #"     search-for-word-at-point-backward)

    ("C-h"     search-toggle-highlighting)
    ("-"       vim:cmd-paste-pop)
    ("+"       vim:cmd-paste-pop-next)
    ("X"       vim:cmd-delete-char-backward)
    ("M"       vim:jump-to-prev-saved-position)

    ("C-u"     undo-tree-visualize)
    ("C-b"     switch-to-buffer)

    ("S-<backspace>" delete-whitespaces-backward)
    ("S-<delete>"    delete-whitespaces-forward)
    ("C-w"           backward-delete-word)
    ("C-S-w"         backward-delete-word*)

    ("q"       nil)
    ("Q"       nil)

    ("Z"       nil)
    ("`"       nil)

    ("g u"     Control-X-prefix)
    ("g h"     help-command)

    ("g g"     nil)
    ("G"       vim:motion-mark)
    ("<f5>"    vim:motion-mark)
    ("g m"     vim:cmd-set-mark)
    ("g M"     vim:cmd-toggle-macro-recording)
    ("g J"     vim:cmd-join-lines)
    ("g j"     nil)
    ("g q"     nil)
    ("M-x"     smex-nohist)
    ("g x"     smex)
    ("g X"     smex-major-mode-commands)

    (","       nil)
    ("'"       nil)))



(def-keys-for-map1 (vim:normal-mode-keymap
                    vim:visual-mode-keymap)
  +vimrc:normal&visual-keys+)

(def-keys-for-map2 vim:normal-mode-keymap
  ("C-y"      nil)
  (";"        vim:ex-read-command)

  ("<insert>" vim:scroll-line-up)
  ("<delete>" vim:scroll-line-down)

  ("u"        undo-tree-undo)
  ("U"        undo-tree-redo)

  ("C-p"      yank)
  ("C-k"      remove-buffer)
  ("C-S-k"    remove-buffer-and-window)

  ;; (", w"      save-buffer)
  (", b"      ibuffer)
  (", s"      nil)
  (", s w"    vim:replace-word)
  (", s W"    vim:replace-WORD)
  (", s s"    vim:replace-symbol-at-point)

  ("g f"      find-file)
  ("g c c"    comment-util-comment-lines)
  ("g c u"    comment-util-uncomment-region)

  ("g TAB"    nil)
  ("g n"      nil)
  ("g t"      nil)

  ("g C"      remember-win-config-store-configuration)
  ("<f6>"     remember-win-config-restore-configuration))

(def-keys-for-map2 vim:visual-mode-keymap
  ("TAB"      indent-region)
  ("<tab>"    indent-region)
  (";"        vim:visual-ex-read-command)

  ("g c c"    comment-util-comment-region)
  ("g c u"    comment-util-uncomment-region-simple)

  ("SPC SPC"  exchange-point-and-mark)

  (", s"      vim:replace-selected))


(def-keys-for-map2 vim:insert-mode-keymap
  ("S-<backspace>" delete-whitespaces-backward)
  ("S-<delete>"    delete-whitespaces-forward)
  ("C-w"           backward-delete-word)
  ("C-S-w"         backward-delete-word*)
  ("C-r"           nil)
  ("C-p"           yank)
  ("C--"           yank-previous)
  ("C-+"           yank-next)
  ("SPC"           abbrev+-insert-space-or-expand-abbrev)

  ("<insert>"      vim:scroll-line-up))


(def-keys-for-map2 vim:ex-keymap
  ("C-v" set-mark-command)
  ("C-y" copy-region-as-kill))


(vim:defcmd vim:cmd-new-frame (nonrepeatable keep-visual)
  "Pops up a new frame."
  (new-frame))

(vim:emap "new frame" 'vim:cmd-new-frame)
(vim:emap "nf" "new frame")


(vim:defcmd vim:jump-to-prev-saved-position (nonrepeatable keep-visual)
  "Jump to position pointed to by ' mark.
Basically swap current point with previous one."
  (vim:motion-mark :argument ?\'))

(vim:defcmd vim:cmd-only (nonrepeatable keep-visual)
  "Close all window except current one, just like C-x 1."
  (delete-other-windows))

(vim:emap "only" 'vim:cmd-only)

(vim:defcmd vim:cmd-close (nonrepeatable keep-visual)
  "Close current window, just like C-x 0."
  (icicle-delete-window nil))

(vim:emap "close" 'vim:cmd-close)

(vim:defcmd vim:cmd-split-vertically (nonrepeatable keep-visual)
  "Split current window vertically, just like C-x 3."
  ;; this is not a bug, function is correct!
  (split-window-horizontally))

(vim:defcmd vim:cmd-split-horizontally (nonrepeatable keep-visual)
  "Split current window horizontally, just like C-x 2."
  ;; this is not a bug, function is correct too!
  (split-window-vertically))

(vim:emap "horizontal split" 'vim:cmd-split-horizontally)
(vim:emap "hsplit" "horizontal split")
(vim:emap "hs" "horizontal split")
(vim:emap "vertical split" 'vim:cmd-split-vertically)
(vim:emap "vsplit" "vertical split" )
(vim:emap "vs" "vertical split" )

(vim:emap "write" 'vim:cmd-write-current-buffer)
(vim:emap "w" "write")


(vim:defcmd vim:start-awk (motion nonrepeatable)
  (require 'awk+)
  (when (get-buffer awk-buffer-name)
    (with-current-buffer (get-buffer awk-buffer-name)
      (awk-exit)))
  ;; turn visual mode off
  (when (or (region-active-p)
            (run-if-fbound vim:visual-mode-p))
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




;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'vim-init)

