;; undo-tree-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  6 April 2013
;; Description:

(require 'common)
(require 'undo-tree)

;; (autoload 'undo-tree-visualize "undo-tree" "" t)
;; (autoload 'undo-tree-undo "undo-tree" "" t)
;; (autoload 'undo-tree-redo "undo-tree" "" t)

(global-undo-tree-mode t)
(setf undo-tree-visualizer-diff t
      undo-tree-enable-undo-in-region nil
      ;; display absolute timestamps
      undo-tree-visualizer-relative-timestamps nil
      undo-tree-visualizer-timestamps t)

(eval-after-load "undo-tree"
  '(progn
     (def-keys-for-map undo-tree-map
       ("C-/" nil)
       ("C-_" nil)
       ("C-?" nil)
       ("M-_" nil))
     ;;(def-keys-for-map undo-tree-visualizer-mode-map +vi-keys+)
     (def-keys-for-map undo-tree-visualizer-mode-map
       ("h"        undo-tree-visualize-redo)
       ("t"        undo-tree-visualize-undo)
       ("d"        undo-tree-visualize-switch-branch-left)
       ("n"        undo-tree-visualize-switch-branch-right)
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


(provide 'undo-tree-setup)

;; Local Variables:
;; End:

;; undo-tree-setup.el ends here
