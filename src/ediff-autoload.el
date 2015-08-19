;; ediff-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 19 August 2015
;; Description:

(autoload 'ediff-quit "ediff-util" "" t)

;; don't spawn separate ediff frame
(setf ediff-window-setup-function 'ediff-setup-windows-plain)

;; don't be fooled by function names here:
;; #'split-window-horizontally causes windows to be split in vertical,
;; and #'split-window-vertically causes windows to be split in horizontal!
(setf ediff-split-window-function
      (if (platform-use? 'netbook)
        ;; when on netbook, the screen isn't wide enough so split horizontally
        #'split-window-vertical
        ;; otherwise split vertically
        #'split-window-horizontally))
(setf ediff-merge-split-window-function ediff-split-window-function)

(setf ediff-custom-diff-options "-u --ignore-tab-expansion --ignore-space-change --ignore-blank-lines"
      ediff-diff-options "--ignore-tab-expansion --ignore-space-change --ignore-blank-lines"
      ediff-patch-options "")

(autoload 'ediff-keymap-setup "ediff-setup")

(eval-after-load
    "ediff"
  '(progn
     (add-hook 'ediff-keymap-setup-hook #'ediff-keymap-setup)))

(autoload 'ediff-diff-texts-recursive-edit "ediff-setup")
(autoload 'ediff-diff-files-recursive-edit "ediff-setup")
(autoload 'ediff/line-in-buffer? "ediff-setup")
(autoload 'ediff/read-line-in-buffer "ediff-setup")
(autoload 'ediff-two-regions "ediff-setup")

(provide 'ediff-autoload)

;; Local Variables:
;; End:

;; ediff-autoload.el ends here
