;; ediff-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 November 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util)
  (require 'keys-def)
  (require 'set-up-platform))

(require 'ediff)
(require 'el-patch)
(require 'hydra-setup)
(require 'vim-setup)

(setf ediff-verbose-p nil)

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
    (("p" "n" " " "C-l" "\C-?" [delete] "G" "E" "m" "##") nil)

    ("z"            hydra-ediff-z-ext/body)
    ("g"            hydra-ediff-g-ext/body)
    ("j"            hydra-vim-normal-j-ext/body)
    ("J"            ediff-jump-to-difference)
    ("H"            ediff-update-diffs)
    (("h" "<down>") ediff-next-difference)
    (("t" "<up>")   ediff-previous-difference)
    ("<escape>"     ediff-quit)
    ("#w"           ediff-toggle-ignore-whitespace)))

(setf ediff-long-help-message-head
      "Move around               Toggle features                     Manipulate"

      ediff-long-help-message-tail
      "\n
R         show registry   =        compare regions            M         show session group
D         diff output     ?        help off                   q         quit
i         status info"

      ediff-long-help-message-compare2
      "
t,<up>    previous diff   |        vert/horiz split           {A,B}     copy A/B’s region to B/A
h,<down>  next diff       h        highlighting               r{A,B}    restore buf X’s old diff
J         jump to diff    @        auto-refinement            *         refine current region
g{A,B,C}  goto X’s point  #w       ignore whitespace          !         update diff regions
zz        recenter        #c       ignore case
v/V       scroll up/dn    #f/#h    focus/hide regions by re   w{A,B,C}  save buf X
</>       scroll lt/rt    {A,B,C}  read-only in buf X         wd        save diff output
~         swap variants
"

      ediff-long-help-message-compare3
      "
t,<up>    previous diff   |        vert/horiz split           {A,B}{A,B}  copy buf X’s region to Y
h,<down>  next diff       h        highlighting               rx          restore buf X’s old diff
J         jump to diff    @        auto-refinement            *           refine current region
g{A,B,C}  goto X’s point  #w       ignore whitespace          !           update diff regions
zz        recenter        #c       ignore case
v/V       scroll up/dn    #f/#h    focus/hide regions by re   w{A,B,C}    save buf X
</>       scroll lt/rt    {A,B,C}  read-only in buf X         wd          save diff output
~         rotate buffers
"

      ediff-long-help-message-narrow2
      "
t,<up>    previous diff   |        vert/horiz split           {A,B}     copy A/B’s region to B/A
h,<down>  next diff       h        highlighting               r{A,B}    restore buf X’s old diff
J         jump to diff    @        auto-refinement            *         refine current region
g{A,B,C}  goto X’s point  #w       ignore whitespace          !         update diff regions
zz        recenter        #c       ignore case                %         narrow/widen buffs
v/V       scroll up/dn    #f/#h    focus/hide regions by re   w{A,B,C}  save buf X
</>       scroll lt/rt    {A,B,C}  read-only in buf X         wd        save diff output
~         swap variants
"

      ediff-long-help-message-word-mode
      "
p,DEL    -previous diff   |        vert/horiz split           {A,B}     copy buf X’s region to Y
n,SPC    -next diff       h        highlighting               r{A,B}    restore buf X’s old diff
J        -jump to diff
g{A,B,C} -goto X’s point  %        narrow/widen buffs         !         recompute diffs
zz       -recenter        #c       ignore case
v/V      -scroll up/dn    #f/#h    focus/hide regions by re   w{A,B,C}  save buf X
</>      -scroll lt/rt    {A,B,C}  read-only in buf X         wd        save diff output
~        -swap variants
"

      ediff-long-help-message-merge
      "
t,<up>    previous diff   |        vert/horiz split           x         copy buf X’s region to C
h,<down>  next diff       h        highlighting               r         restore buf C’s old diff
J         jump to diff    @        auto-refinement            *         refine current region
g{A,B,C}  goto X’s point  #w       ignore whitespace          !         update diff regions
zz        recenter        #f/#h    focus/hide regions by re   +         combine diff regions
v/V       scroll up/dn    {A,B,C}  read-only in buf X         w{A,B,C}  save buf X
</>       scroll lt/rt    s        shrink window C            wD        save diff output
~         swap variants   $$       show clashes only          /         show/hide ancestor buff
                          $*       skip changed regions       &         merge w/new default
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


(ediff-defvar-local ediff-ignore-whitespace nil
  "If non-nile, don’t show whitespace differences.")

(defcustom ediff-ignore-whitespace-option "-w"
  "Option that causes the diff program to ignore whitespace differences."
  :type 'string)

(el-patch-feature ediff-diff)

(el-patch-defun ediff-set-actual-diff-options ()
  (el-patch-swap
    (if ediff-ignore-case
        (setq ediff-actual-diff-options
              (concat ediff-diff-options " " ediff-ignore-case-option)
              ediff-actual-diff3-options
              (concat ediff-diff3-options " " ediff-ignore-case-option3))
      (setq ediff-actual-diff-options ediff-diff-options
            ediff-actual-diff3-options ediff-diff3-options))
    (if (or ediff-ignore-case ediff-ignore-whitespace)
        (setq ediff-actual-diff-options
              (concat ediff-diff-options
                      (when ediff-ignore-case
                        (concat " " ediff-ignore-case-option))
                      (when ediff-ignore-whitespace
                        (concat " " ediff-ignore-whitespace-option)))
              ediff-actual-diff3-options
              (concat ediff-diff3-options
                      (when ediff-ignore-case
                        (concat " " ediff-ignore-case-option3))
                      (when ediff-ignore-whitespace
                        (concat " " ediff-ignore-whitespace-option))))
      (setq ediff-actual-diff-options ediff-diff-options
            ediff-actual-diff3-options ediff-diff3-options)))
  (setq-default ediff-actual-diff-options ediff-actual-diff-options
                ediff-actual-diff3-options ediff-actual-diff3-options))

(defun ediff-toggle-ignore-whitespace ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (setq ediff-ignore-whitespace (not ediff-ignore-whitespace))
  (ediff-set-actual-diff-options)
  (message "ediff-actual-diff-options = %s"
           (pp-to-string ediff-actual-diff-options))
  (message (if ediff-ignore-whitespace
               "Ignoring whitespace differences turned ON"
             "Ignoring whitespace differences turned OFF"))
  (ediff-update-diffs))

(el-patch-defun ediff-make-diff2-buffer (diff-buffer file1 file2)
  (let ((file1-size (ediff-file-size file1))
	(file2-size (ediff-file-size file2)))
    (cond ((not (numberp file1-size))
           (el-patch-remove
	     (message "Can't find file: %s"
		      (ediff-abbreviate-file-name file1)))
           (el-patch-remove
	     (sit-for 2))
           (el-patch-add
             (error "Can't find file: %s"
		    (ediff-abbreviate-file-name file1)))
	   ;; 1 is an error exit code
	   1)
	  ((not (numberp file2-size))
           (el-patch-remove
	     (message "Can't find file: %s"
		      (ediff-abbreviate-file-name file2)))
           (el-patch-remove
	     (sit-for 2))
           (el-patch-add
	     (error "Can't find file: %s"
		    (ediff-abbreviate-file-name file2)))
	   ;; 1 is an error exit code
	   1)
	  (t ;; this erases the diff buffer automatically
	   (ediff-exec-process ediff-diff-program
			       diff-buffer
			       'synchronize
			       ediff-actual-diff-options file1 file2)
	   (el-patch-remove (message ""))
	   (ediff-with-current-buffer diff-buffer
	     (buffer-size))))))

(el-patch-defun ediff-convert-diffs-to-overlays (diff-list)
  (ediff-set-diff-overlays-in-one-buffer 'A diff-list)
  (ediff-set-diff-overlays-in-one-buffer 'B diff-list)
  (if ediff-3way-job
      (ediff-set-diff-overlays-in-one-buffer 'C diff-list))
  (if ediff-merge-with-ancestor-job
      (ediff-set-diff-overlays-in-one-buffer 'Ancestor diff-list))
  ;; set up vector showing the status of merge regions
  (if ediff-merge-job
      (setq ediff-state-of-merge
	    (vconcat
	     (mapcar (lambda (elt)
		       (let ((state-of-merge (aref elt 9))
			     (state-of-ancestor (aref elt 10)))
			 (vector
			  ;; state of merge: prefers/default-A/B or combined
			  (if state-of-merge (format "%S" state-of-merge))
			  ;; whether the ancestor region is empty
			  state-of-ancestor)))
		     ;; the first elt designates type of list
		     (cdr diff-list))
	     )))
  (el-patch-remove
    (message "Processing difference regions ... done")))

(el-patch-defun ediff-set-diff-overlays-in-one-buffer (buf-type diff-list)
  (let* ((current-diff -1)
	 (buff (ediff-get-buffer buf-type))
	 (ctl-buf ediff-control-buffer)
	 ;; ediff-extract-diffs puts the type of diff-list as the first elt
	 ;; of this list. The type is either 'points or 'words
	 (diff-list-type (car diff-list))
	 (shift (ediff-overlay-start
		 (ediff-get-value-according-to-buffer-type
		  buf-type ediff-narrow-bounds)))
	 (limit (ediff-overlay-end
		 (ediff-get-value-according-to-buffer-type
		  buf-type ediff-narrow-bounds)))
	 diff-overlay-list list-element total-diffs
	 begin end pt-saved overlay state-of-diff)

    (setq diff-list (cdr diff-list)) ; discard diff list type
    (setq total-diffs (length diff-list))

    ;; shift, if necessary
    (ediff-with-current-buffer buff (setq pt-saved shift))

    (while diff-list
      (setq current-diff (1+ current-diff)
	    list-element (car diff-list)
	    begin 	 (aref list-element (cond ((eq buf-type 'A) 0)
						  ((eq buf-type 'B) 2)
						  ((eq buf-type 'C) 4)
						  (t 6)))  ; Ancestor
	    end 	 (aref list-element (cond ((eq buf-type 'A) 1)
						  ((eq buf-type 'B) 3)
						  ((eq buf-type 'C) 5)
						  (t 7)))  ; Ancestor
	    state-of-diff (aref list-element 8)
	    )

      (cond ((and (not (eq buf-type state-of-diff))
		  (not (eq buf-type 'Ancestor))
		  (memq state-of-diff '(A B C)))
	     (setq state-of-diff
		   (car (delq buf-type (delq state-of-diff (list 'A 'B 'C)))))
	     (setq state-of-diff (format "=diff(%S)" state-of-diff))
	     )
	    (t (setq state-of-diff nil)))

      ;; Put overlays at appropriate places in buffer
      ;; convert word numbers to points, if necessary
      (if (eq diff-list-type 'words)
	  (progn
	    (ediff-with-current-buffer buff (goto-char pt-saved))
	    (ediff-with-current-buffer ctl-buf
	      (setq begin (ediff-goto-word (1+ begin) buff)
		    end (ediff-goto-word end buff 'end)))
	    (if (> end limit) (setq end limit))
	    (if (> begin end) (setq begin end))
	    (setq pt-saved (ediff-with-current-buffer buff (point)))))
      (setq overlay (ediff-make-bullet-proof-overlay begin end buff))

      (ediff-overlay-put overlay 'ediff-diff-num current-diff)
      (if (and (ediff-has-face-support-p)
	       ediff-use-faces ediff-highlight-all-diffs)
	  (ediff-set-overlay-face
	   overlay (ediff-background-face buf-type current-diff)))

      (el-patch-remove
        (if (= 0 (mod current-diff 10))
	    (message "Buffer %S: Processing difference region %d of %d"
		     buf-type current-diff total-diffs)))
      ;; Record all overlays for this difference.
      ;; The 2-d elt, nil, is a place holder for the fine diff vector.
      ;; The 3-d elt, nil, is a place holder for no-fine-diffs flag.
      ;; The 4-th elt says which diff region is different from the other two
      ;; (3-way jobs only).
      (setq diff-overlay-list
	    (nconc
	     diff-overlay-list
	     (list (vector overlay nil nil state-of-diff)))
	    diff-list
	    (cdr diff-list))
      ) ; while

    (set (ediff-get-symbol-from-alist buf-type ediff-difference-vector-alist)
	 (vconcat diff-overlay-list))
    ))

(when-emacs-version (= 28 it)
  (el-patch-defun ediff-setup-diff-regions3 (file-A file-B file-C)
    ;; looking for '-i' or a 'i' among clustered non-long options
    (if (string-match "^-i\\| -i\\|\\(^\\| \\)-[^- ]+i" ediff-diff-options)
        (error "Option `-i' is not allowed in `ediff-diff3-options'"))

    (or (ediff-buffer-live-p ediff-diff-buffer)
        (setq ediff-diff-buffer
	      (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*"))))

    (el-patch-remove
      (message "Computing differences ..."))
    (ediff-exec-process ediff-diff3-program ediff-diff-buffer 'synchronize
		        ediff-actual-diff3-options file-A file-B file-C)

    (ediff-prepare-error-list ediff-diff3-ok-lines-regexp ediff-diff-buffer)
    ;;(message "Computing differences ... done")
    (ediff-convert-diffs-to-overlays
     (ediff-extract-diffs3
      ediff-diff-buffer
      ediff-word-mode ediff-3way-comparison-job ediff-narrow-bounds))))

(when-emacs-version (<= 29 it)
  (el-patch-defun ediff-setup-diff-regions3 (file-A file-B file-C)
    ;; looking for '-i' or a 'i' among clustered non-long options
    (if (string-match "^-i\\| -i\\|\\(^\\| \\)-[^- ]+i" ediff-diff-options)
        (error "Option `-i' is not allowed in `ediff-diff3-options'"))

    (or (ediff-buffer-live-p ediff-diff-buffer)
        (setq ediff-diff-buffer
	      (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*"))))

    (el-patch-remove
      (message "Computing differences ..."))
    (apply #'ediff-exec-process ediff-diff3-program ediff-diff-buffer 'synchronize
	   ediff-actual-diff3-options
           (cons file-A (if ediff-merge-with-ancestor-job
                            ;; Ancestor must be the middle file
                            (list file-C file-B)
                          (list file-B file-C))))

    (ediff-prepare-error-list ediff-diff3-ok-lines-regexp ediff-diff-buffer)
    ;;(message "Computing differences ... done")
    (ediff-convert-diffs-to-overlays
     (ediff-extract-diffs3
      ediff-diff-buffer
      ediff-word-mode ediff-3way-comparison-job ediff-narrow-bounds))))

(el-patch-defun ediff-update-diffs ()
  "Recompute difference regions in buffers A, B, and C.
Buffers are not synchronized with their respective files, so changes done
to these buffers are not saved at this point---the user can do this later,
if necessary."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((point-A (ediff-with-current-buffer ediff-buffer-A (point)))
	;;(point-B (ediff-with-current-buffer ediff-buffer-B (point)))
	(tmp-buffer (get-buffer-create ediff-tmp-buffer))
	(buf-A-file-name (buffer-file-name ediff-buffer-A))
	(buf-B-file-name (buffer-file-name ediff-buffer-B))
	;; (null ediff-buffer-C) is no problem, as we later check if
	;; ediff-buffer-C is alive
	(buf-C-file-name (buffer-file-name ediff-buffer-C))
	(buf-ancestor-file-name (buffer-file-name ediff-ancestor-buffer))
	(overl-A (ediff-get-value-according-to-buffer-type
		  'A ediff-narrow-bounds))
	(overl-B (ediff-get-value-according-to-buffer-type
		  'B ediff-narrow-bounds))
	(overl-C (ediff-get-value-according-to-buffer-type
		  'C ediff-narrow-bounds))
        (overl-Ancestor (ediff-get-value-according-to-buffer-type
                         'Ancestor ediff-narrow-bounds))
	beg-A end-A beg-B end-B beg-C end-C beg-Ancestor end-Ancestor
	file-A file-B file-C file-Ancestor)

    (if (stringp buf-A-file-name)
	(setq buf-A-file-name (file-name-nondirectory buf-A-file-name)))
    (if (stringp buf-B-file-name)
	(setq buf-B-file-name (file-name-nondirectory buf-B-file-name)))
    (if (stringp buf-C-file-name)
	(setq buf-C-file-name (file-name-nondirectory buf-C-file-name)))
    (if (stringp buf-ancestor-file-name)
        (setq buf-ancestor-file-name (file-name-nondirectory buf-ancestor-file-name)))

    (ediff-unselect-and-select-difference -1)

    (setq beg-A (ediff-overlay-start overl-A)
	  beg-B (ediff-overlay-start overl-B)
	  beg-C (ediff-overlay-start overl-C)
	  beg-Ancestor (ediff-overlay-start overl-Ancestor)
	  end-A (ediff-overlay-end overl-A)
	  end-B (ediff-overlay-end overl-B)
	  end-C (ediff-overlay-end overl-C)
          end-Ancestor (ediff-overlay-end overl-Ancestor))

    (if ediff-word-mode
	(progn
	  (ediff-wordify beg-A end-A ediff-buffer-A tmp-buffer)
	  (setq file-A (ediff-make-temp-file tmp-buffer "regA"))
	  (ediff-wordify beg-B end-B ediff-buffer-B tmp-buffer)
	  (setq file-B (ediff-make-temp-file tmp-buffer "regB"))
	  (when ediff-3way-job
            (ediff-wordify beg-C end-C ediff-buffer-C tmp-buffer)
            (setq file-C (ediff-make-temp-file tmp-buffer "regC")))
          (when ediff-merge-with-ancestor-job
            (ediff-wordify beg-Ancestor end-Ancestor ediff-ancestor-buffer tmp-buffer)
            (setq file-Ancestor (ediff-make-temp-file tmp-buffer "regAncestor")))
	  )
      ;; not word-mode
      (setq file-A (ediff-make-temp-file ediff-buffer-A buf-A-file-name))
      (setq file-B (ediff-make-temp-file ediff-buffer-B buf-B-file-name))
      (if ediff-3way-job
	  (setq file-C (ediff-make-temp-file ediff-buffer-C buf-C-file-name)))
      (when ediff-merge-with-ancestor-job
        (setq file-Ancestor
              (ediff-make-temp-file
               ediff-ancestor-buffer
               buf-ancestor-file-name)))
      )
    (ediff-clear-diff-vector 'ediff-difference-vector-A 'fine-diffs-also)
    (ediff-clear-diff-vector 'ediff-difference-vector-B 'fine-diffs-also)
    (ediff-clear-diff-vector 'ediff-difference-vector-C 'fine-diffs-also)
    (ediff-clear-diff-vector
     'ediff-difference-vector-Ancestor 'fine-diffs-also)
    (setq ediff-killed-diffs-alist nil) ; invalidate saved killed diff regions
    (funcall ediff-setup-diff-regions-function file-A file-B
             (if ediff-merge-with-ancestor-job file-Ancestor file-C))
    (setq ediff-number-of-differences (length ediff-difference-vector-A))
    (delete-file file-A)
    (delete-file file-B)
    (and file-C (delete-file file-C))
    (and file-Ancestor (delete-file file-Ancestor))

    (if ediff-3way-job
	(ediff-set-state-of-all-diffs-in-all-buffers ediff-control-buffer))

    (ediff-jump-to-difference (ediff-diff-at-point 'A point-A))
    (el-patch-remove
      (message ""))))

(provide 'ediff-setup)

;; Local Variables:
;; End:

;; ediff-setup.el ends here
