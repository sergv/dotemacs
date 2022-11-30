;; ediff-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 November 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util)
  (require 'keys-def))

(require 'el-patch)
(require 'hydra-setup)

;; Don’t need mouse help in ediff control buffer.
(define-key ediff-help-region-map [mouse-2] nil)
(fmakunbound #'ediff-help-for-quick-help)
(fmakunbound #'ediff-submit-report)

(fmakunbound #'ediff-set-help-overlays)
(defun ediff-set-help-overlays ())

(defhydra-ext hydra-ediff-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_z_: scroll to center
"
  ("z" ediff-recenter))

(defhydra-derive hydra-ediff-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_A_: go to difference closest to the point in buffer A
_B_: go to difference closest to the point in buffer B
_C_: go to difference closest to the point in buffer C
"
  ("A" ediff-jump-to-difference-at-point)
  ("B" ediff-jump-to-difference-at-point)
  ("C" ediff-jump-to-difference-at-point))

;;;###autoload
(defun ediff-keymap-setup ()
  (def-keys-for-map ediff-mode-map
    +vim-interbuffer-navigation-keys+
    (("p" "n" " " "C-l" "\C-?" [delete] "G" "E") nil)

    ("z"            hydra-ediff-z-ext/body)
    ("g"            hydra-ediff-g-ext/body)
    ("j"            hydra-vim-normal-j-ext/body)
    ("J"            ediff-jump-to-difference)
    ("H"            ediff-update-diffs)
    (("h" "<down>") ediff-next-difference)
    (("t" "<up>")   ediff-previous-difference)
    ("<escape>"     ediff-quit)))

(setf ediff-long-help-message-head
      "Move around               Toggle features                Manipulate"

      ediff-long-help-message-tail
      "\n
R         show registry   =        compare regions       M         show session group
D         diff output     ?        help off              q         quit
i         status info"

      ediff-long-help-message-compare2
      "
t,<up>    previous diff   |        vert/horiz split      {A,B}     copy A/B’s region to B/A
h,<down>  next diff       h        highlighting          r{A,B}    restore buf X’s old diff
J         jump to diff    @        auto-refinement       *         refine current region
g{A,B,C}  goto X’s point  ##       ignore whitespace     !         update diff regions
zz        recenter        #c       ignore case
v/V       scroll up/dn    #f/#h    focus/hide regions    w{A,B,C}  save buf X
</>       scroll lt/rt    {A,B,C}  read-only in buf X    wd        save diff output
~         swap variants   m        wide display
"

      ediff-long-help-message-compare3
      "
t,<up>    previous diff   |        vert/horiz split      {A,B}{A,B}  copy buf X’s region to Y
h,<down>  next diff       h        highlighting          rx          restore buf X’s old diff
J         jump to diff    @        auto-refinement       *           refine current region
g{A,B,C}  goto X’s point  ##       ignore whitespace     !           update diff regions
zz        recenter        #c       ignore case
v/V       scroll up/dn    #f/#h    focus/hide regions    w{A,B,C}    save buf X
</>       scroll lt/rt    {A,B,C}  read-only in buf X    wd          save diff output
~         rotate buffers  m        wide display
"

      ediff-long-help-message-narrow2
      "
t,<up>    previous diff   |        vert/horiz split      {A,B}     copy A/B’s region to B/A
h,<down>  next diff       h        highlighting          r{A,B}    restore buf X’s old diff
J         jump to diff    @        auto-refinement       *         refine current region
g{A,B,C}  goto X’s point  ##       ignore whitespace     !         update diff regions
zz        recenter        #c       ignore case           %         narrow/widen buffs
v/V       scroll up/dn    #f/#h    focus/hide regions    w{A,B,C}  save buf X
</>       scroll lt/rt    {A,B,C}  read-only in buf X    wd        save diff output
~         swap variants   m        wide display
"

      ediff-long-help-message-word-mode
      "
p,DEL    -previous diff   |        vert/horiz split      {A,B}     copy buf X’s region to Y
n,SPC    -next diff       h        highlighting          r{A,B}    restore buf X’s old diff
J        -jump to diff
g{A,B,C} -goto X’s point  %        narrow/widen buffs    !         recompute diffs
zz       -recenter        #c       ignore case
v/V      -scroll up/dn    #f/#h    focus/hide regions    w{A,B,C}  save buf X
</>      -scroll lt/rt    {A,B,C}  read-only in buf X    wd        save diff output
~        -swap variants   m        wide display
"

      ediff-long-help-message-merge
      "
t,<up>    previous diff   |        vert/horiz split      x         copy buf X’s region to C
h,<down>  next diff       h        highlighting          r         restore buf C’s old diff
J         jump to diff    @        auto-refinement       *         refine current region
g{A,B,C}  goto X’s point  ##       ignore whitespace     !         update diff regions
zz        recenter        #f/#h    focus/hide regions    +         combine diff regions
v/V       scroll up/dn    {A,B,C}  read-only in buf X    w{A,B,C}  save buf X
</>       scroll lt/rt    m        wide display          wD        save diff output
~         swap variants   s        shrink window C       /         show/hide ancestor buff
                          $$       show clashes only     &         merge w/new default
                          $*       skip changed regions
")

(el-patch-feature ediff-util)

(el-patch-defun ediff-scroll-horizontally (&optional arg)
  "Horizontally scroll buffers A, B (and C if appropriate).
With prefix argument ARG, scroll that many columns, else nearly
the width of the A/B/C windows."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)

  ;; make sure windows aren't dead
  (if (not (and (window-live-p ediff-window-A) (window-live-p ediff-window-B)))
      (ediff-recenter 'no-rehighlight))
  (if (not (and (ediff-buffer-live-p ediff-buffer-A)
                (ediff-buffer-live-p ediff-buffer-B)
                (or (not ediff-3way-job)
                    (ediff-buffer-live-p ediff-buffer-C))
                (or (not ediff-merge-with-ancestor-job)
                    (not ediff-show-ancestor)
                    (ediff-buffer-live-p ediff-ancestor-buffer))
                ))
      (error ediff-KILLED-VITAL-BUFFER))

  (ediff-operate-on-windows
   ;; Arrange for scroll-left and scroll-right being called
   ;; interactively so that they set the window's min_hscroll.
   ;; Otherwise, automatic hscrolling will undo the effect of
   ;; hscrolling.
   (if (= last-command-event (el-patch-swap ?< ?>))
       (lambda (arg)
         (let ((current-prefix-arg arg))
           (call-interactively #'scroll-left)))
     (lambda (arg)
       (let ((current-prefix-arg arg))
         (call-interactively #'scroll-right))))
   ;; calculate argument to scroll-left/right
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount
     ;; (half the window width)
     (if (null ediff-control-window)
         ;; no control window, use nil
         nil
       (let ((default-amount
               (- (/ (min (window-width ediff-window-A)
                          (window-width ediff-window-B)
                          (if ediff-3way-comparison-job
                              (window-width ediff-window-C)
                            500) ; some large number
                          (if (and ediff-merge-with-ancestor-job
                                   ediff-show-ancestor)
                              (window-height ediff-window-Ancestor)
                            500)) ; some large number
                     2)
                  3)))
         ;; window found
         (if arg
             ;; C-u as argument means half of default amount
             (/ default-amount 2)
           ;; no argument means default amount
           default-amount))))))

;;;###autoload
(cl-defun ediff-diff-texts-recursive-edit (text-a
                                           text-b
                                           &key
                                           (read-only t)
                                           (a-buf-name "text A")
                                           (b-buf-name "text B"))
  "Quickly show difference between two texts TEXT-A and TEXT-B using ediff.
Register quick exit function and show difference in recursive edit."
  (let ((buf-a    (get-buffer-create a-buf-name))
        (buf-b    (get-buffer-create b-buf-name))
        (win-conf (current-window-configuration)))
    (with-current-buffer buf-a
      (erase-buffer)
      (insert text-a))
    (with-current-buffer buf-b
      (erase-buffer)
      (insert text-b))
    (let (;; (ediff-quit-hook
          ;;   (append ediff-quit-hook
          ;;           (list (lambda ()
          ;;                   (exit-recursive-edit)))))
          (ediff-make-buffers-readonly-at-startup read-only)
          (orig-ediff-quit (symbol-function #'ediff-quit))
          (new-ediff-quit
           (lambda (reverse-default-keep-variants)
             (interactive "P")
             (ediff-barf-if-not-control-buffer)
             (let ((minibuffer-auto-raise t))
               (ediff-really-quit reverse-default-keep-variants)
               (exit-recursive-edit)))))
      (fset 'ediff-quit new-ediff-quit)
      (ediff-buffers buf-a buf-b)
      ;; protect in case of abort-recursive-edit
      (unwind-protect
          (recursive-edit)
        (set-window-configuration win-conf)
        (kill-buffer buf-a)
        (kill-buffer buf-b)
        (fset 'ediff-quit orig-ediff-quit)))))

;;;###autoload
(cl-defun ediff-diff-files-recursive-edit (file-a
                                           file-b
                                           &key
                                           (read-only t))
  "Run `ediff' on a pair of files. Also register quick exit function and restore
window configuration on end of ediff session."
  (let ((win-conf (current-window-configuration)))
    (let (;; (ediff-quit-hook
          ;;   (append ediff-quit-hook
          ;;           (list (lambda ()
          ;;                   (exit-recursive-edit)))))
          (ediff-make-buffers-readonly-at-startup read-only))
      (ediff file-a file-b)
      (let ((orig-ediff-quit (symbol-function #'ediff-quit))
            (new-ediff-quit
             (lambda (reverse-default-keep-variants)
               (interactive "P")
               (ediff-barf-if-not-control-buffer)
               (let ((minibuffer-auto-raise t))
                 (ediff-really-quit reverse-default-keep-variants)
                 (exit-recursive-edit)))))
        (fset 'ediff-quit new-ediff-quit)
        ;; protect in case of abort-recursive-edit
        (unwind-protect
            (recursive-edit)
          (set-window-configuration win-conf)
          (fset 'ediff-quit orig-ediff-quit))))))

(provide 'ediff-setup)

;; Local Variables:
;; End:

;; ediff-setup.el ends here
