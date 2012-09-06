(require 'custom)

(require 'keys-def)

(def-keys-for-map global-map
  ("<f1>"     abort-recursive-edit)
  ("<f2>"     universal-argument)
  ("<f3>"     #'(lambda () (interactive)
                  (tabbar-scroll (tabbar-current-tabset) -1)))
  ("<f4>"     #'(lambda () (interactive)
                  (tabbar-scroll (tabbar-current-tabset) 1)))

  ("<f6>"     remember-win-config-restore-configuration)
  ("<f7>"     prev-w)
  ("<f8>"     next-w)

  ("<f9>"     compile)
  ("<f10>"    eshell)
  ("<f11>"    tabbar-backward-tab)
  ("<f12>"    tabbar-forward-tab)


  ("S-<f3>"   tabbar-forward-group)
  ("S-<f4>"   tabbar-backward-group)

  ("S-<f7>"   swap-buffers-backward)
  ("S-<f8>"   swap-buffers-forward)
  ("S-<f11>"  tabbar-move-selected-tab-left)
  ("S-<f12>"  tabbar-move-selected-tab-right)


  ("C-<f9>"   enlarge-window)
  ("C-<f10>"  shrink-window)
  ("C-<f11>"  enlarge-window-horizontally)
  ("C-<f12>"  shrink-window-horizontally)


  ;; use abort, don't exit here
  ("<escape>" abort-recursive-edit)
  ("M-x"      execute-extended-command)
  ("C-!"      shell-command)
  ("C-|"      shell-command-on-region)
  ("M-!"      nil)
  ("M-|"      nil)
  ("C-v"      nil)
  ("C-r"      nil)
  ("C-v"      set-mark-command)
  ("C-y"      copy-region-as-kill)
  ("C-f"      read-and-insert-filename)
  ("M-p"      browse-kill-ring)
  ("C-t"      nil)
  ("C-<down>" nil)
  ("C-<up>"   nil)

  ;; for icicle
  ("<M-tab>"  nil)
  ("M-<tab>"  nil)
  ("M-TAB"    nil)
  ("C-SPC"    nil)
  ("M-SPC"    nil)
  ("M-."      nil)
  ("M-/"      nil)
  ("M-f"      nil)
  ("M-m"      nil)
  ("M-M"      nil)

  ("C-x <f1>" nil)
  ("C-z"      nil)
  ("C-x b"    ibuffer)
  ("C-b"      switch-to-buffer)
  ("C-x f"    find-file)
  ("C-x k"    remove-buffer)
  ("C-k"      remove-buffer)
  ("C-x K"    remove-buffer-and-window)
  ("C-S-k"    remove-buffer-and-window)
  ("C-x u"    undo-tree-visualize)
  ("C-u"      undo-tree-visualize))



(eval-after-load "compile"
                 '(progn
                   (def-keys-for-map compilation-mode-map
                     +control-x-prefix+
                     +vi-keys+
                     +vim-special-keys+
                     +vim-word-motion-keys+
                     ("<up>"     compilation-jump-to-prev-error)
                     ("<down>"   compilation-jump-to-next-error)
                     ("M-p"      nil)
                     ("<escape>" remove-buffer)

                     ("C-v"      set-mark-command)
                     ("C-y"      copy-region-as-kill)
                     ("v"        set-mark-command)
                     ("y"        copy-region-as-kill)

                     ("<f9>"     recompile))))


(eval-after-load "help-mode"
                 '(progn
                   (def-keys-for-map help-mode-map
                     +vi-keys+
                     +control-x-prefix+
                     ("<escape>"   remove-buffer)
                     ("<up>"       help-go-back)
                     ("<down>"     help-go-forward))))

;; view-mode
(eval-after-load "view"
                 '(progn
                   (def-keys-for-map view-mode-map
                     +vi-keys+
                     +control-x-prefix+
                     ("<escape>"   remove-buffer))))

(eval-after-load "undo-tree"
                 '(progn
                   ;;(def-keys-for-map undo-tree-visualizer-map +vi-keys+)
                   (def-keys-for-map undo-tree-visualizer-map
                     ("t"        undo-tree-visualize-redo)
                     ("n"        undo-tree-visualize-undo)
                     ("h"        undo-tree-visualize-switch-branch-left)
                     ("s"        undo-tree-visualize-switch-branch-right)
                     ("SPC"      undo-tree-visualizer-toggle-timestamps)
                     ("a"        undo-tree-visualizer-toggle-timestamps)
                     ("<left>"   undo-tree-visualizer-scroll-left)
                     ("<right>"  undo-tree-visualizer-scroll-right)
                     ("<up>"     scroll-up)
                     ("<down>"   scroll-down)

                     ("q"        undo-tree-visualizer-quit)
                     ("<escape>" undo-tree-visualizer-quit)
                     ("<return>" undo-tree-visualizer-quit)
                     ("C-u"      undo-tree-visualizer-quit))))

(eval-after-load "cus-edit"
                 '(progn
                   (def-keys-for-map custom-mode-map
                     +control-x-prefix+
                     +vi-keys+
                     +vim-special-keys+
                     +vim-word-motion-keys+
                     (", w"        customize-save-customized)
                     ("w"          vim:motion-fwd-word)
                     ("b"          vim:motion-bwd-word)
                     ("e"          vim:motion-fwd-word-end)

                     ("<escape>"   remove-buffer)
                     ("<down>"     widget-forward)
                     ("<up>"       widget-backward))

                   (def-keys-for-map Custom-mode-map
                     +control-x-prefix+
                     +vi-keys+
                     +vim-special-keys+
                     +vim-word-motion-keys+
                     (", w"        customize-save-customized)
                     ("w"          vim:motion-fwd-word)
                     ("b"          vim:motion-bwd-word)
                     ("e"          vim:motion-fwd-word-end)

                     ("<escape>"   remove-buffer)
                     ("<down>"     widget-forward)
                     ("<up>"       widget-backward))))

(eval-after-load "apropos"
                 '(progn
                   (def-keys-for-map apropos-mode-map
                     ("<f1>"       remove-buffer)
                     ("<escape>"   remove-buffer))))



(def-keys-for-map occur-mode-map
  +control-x-prefix+
  ("<up>"   custom-occur-prev)
  ("<down>" custom-occur-next)
  ("n"      custom-occur-prev)
  ("t"      custom-occur-next)

  ("k"      remove-buffer)
  ("K"      remove-buffer-and-window))

(eval-after-load "doc-view"
                 '(progn
                   (def-keys-for-map doc-view-mode-map
                     ("S-<up>"   doc-view-previous-page)
                     ("S-<down>" doc-view-next-page))))




(defun define-cyrillic-keys ()
  ;; just all choices from set, 2**n, n - sets' power
  (labels ((choices (list)
             (cond
               ((null list)
                '(nil))
               (t
                (let ((p (choices (cdr list))))
                  (append p
                          (mapcar (lambda (x) (cons (car list) x))
                                  p)))))))
    (let ((modifiers (remove nil
                             (choices '(control meta super hyper)))))
      (mapcar*
       (lambda (r e) ;; R and E are matching Russian and English keysyms
         ;; iterate over modifiers
         (mapc (lambda (mods)
                 (define-key input-decode-map
                     (vector (append mods (list r))) (vector (append mods (list e)))))
               modifiers)
         ;; finally, if Russian key maps nowhere,
         ;; remap it to the English key without any modifiers
         (define-key ;; local-
             function-key-map (vector r) (vector e)))
       "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ"
       "',.pyfgcrl/=aoeuidhtns-;qjkxbmwv\"<>PYFGCRL?+AOEUIDHTNS_:QJKXBMWV"

       ;; "йцукенгшщзхъфывапролджэячсмитьбю"
       ;; "qwertyuiop[]asdfghjkl;'zxcvbnm,."
       ))))

(define-cyrillic-keys)



(provide 'keys)

;; Local Variables:
;; lexical-binding: t
;; End:

