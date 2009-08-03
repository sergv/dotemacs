;;; vim-window.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; Description:

;; This file contains implementations for the window operations.
;; Window operations are usually just simple commands and should not
;; be repeatable.

(provide 'vim-window)

(defmacro vim:save-window-resize (&rest body)
  "Makes all resizing functions on `body' save in the sense that they are only
executed if the do not delete any other window."
  `(let ((noldwin (length (window-list)))
         (wincfg nil)
         (ret nil))
     (save-window-excursion
       (setq ret (progn ,@body))
       (when (= noldwin (length (window-list)))
         (setq wincfg (current-window-configuration))))
     (if wincfg
         (set-window-configuration wincfg)
       (error "Operation would delete a window."))
     ret))
     

(defun vim:get-buffer-tree (wintree)
  "Extracts the buffer tree from a given window-tree."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'vim:get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))


(defun vim:restore-window-tree (win tree)
  "Restores the given buffer-tree layout as subwindows of win."
  (cond
   ((and (consp tree) (cddr tree))
    (let ((newwin (split-window win nil (not (car tree)))))
      (vim:restore-window-tree win (cadr tree))
      (vim:restore-window-tree newwin (cons (car tree) (cddr tree)))))
   ((consp tree)
    (set-window-buffer win (cadr tree)))
   (t (set-window-buffer win tree))))


(vim:defcmd vim:window-split (count nonrepeatable)
  "Splits the current window horizontally, `count' lines height."            
  (split-window (selected-window) count))


(vim:defcmd vim:window-vsplit (count nonrepeatable)
  "Splits the current window vertically, `count' columns width."            
  (split-window (selected-window) count t))


(vim:defcmd vim:window-close (nonrepeatable)
  "Closes the current window."
  (delete-window))


(vim:defcmd vim:window-only (nonrepeatable)
  "Closes all but the current window."
  (delete-other-windows))


(vim:defcmd vim:window-left (count nonrepeatable)
  "Move the cursor to new `count'-th window left of the current one."
  (dotimes (i (or count 1))
    (windmove-left)))


(vim:defcmd vim:window-right (count nonrepeatable)
  "Move the cursor to new `count'-th window right of the current one."
  (dotimes (i (or count 1))
    (windmove-right)))


(vim:defcmd vim:window-up (count nonrepeatable)
  "Move the cursor to new `count'-th window above the current one."
  (dotimes (i (or count 1))
    (windmove-up)))


(vim:defcmd vim:window-down (count nonrepeatable)
  "Move the cursor to new `count'-th window below the current one."
  (dotimes (i (or count 1))
    (windmove-down)))


(vim:defcmd vim:window-bottom-right (nonrepeatable)
  "Move the cursor to bottom-right window."
  (do ((success t))
      ((not success))
    (setq success nil)
    (condition-case nil
        (progn
          (windmove-right)
          (setq success t))
      (error nil))
    (condition-case nil
        (progn
          (windmove-down)
          (setq success t))
      (error nil))))
     

(vim:defcmd vim:window-top-left (nonrepeatable)
  "Move the cursor to top-left window."
  (do ((success t))
      ((not success))
    (setq success nil)
    (condition-case nil
        (progn
          (windmove-left)
          (setq success t))
      (error nil))
    (condition-case nil
        (progn
          (windmove-up)
          (setq success t))
      (error nil))))



(vim:defcmd vim:window-previous (nonrepeatable)
            :type 'simple
            :repeatable nil
            :count nil
  "Move the cursor to the previous (last accessed) window."
  (select-window (get-lru-window)))
            

(vim:defcmd vim:window-new (count nonrepeatable)
            :type 'simple
            :repeatable nil
  "Splits the current window horizontally and opens a new buffer names `new'."
  (split-window (selected-window) count)
  (set-window-buffer (selected-window) (generate-new-buffer "*new*")))


(vim:defcmd vim:window-balance (nonrepeatable)
            :type 'simple
            :repeatable nil
            :count nil
  "Balances all window sizes."
  (balance-windows))


(vim:defcmd vim:window-increase-height (count nonrepeatable)
            :type 'simple
            :repeatable nil
  "Increase current window height by `count'."
  (condition-case nil
      (dotimes (i (or count 1))
        (vim:save-window-resize
         (enlarge-window 1)))
    (error nil)))
            

(vim:defcmd vim:window-decrease-height (count nonrepeatable)
            :type 'simple
            :repeatable nil
  "Decrease current window height by `count'."
  (shrink-window (min (or count 1)
                      (- (window-height) window-min-height))))
            

(vim:defcmd vim:window-increase-width (count nonrepeatable)
  "Increase current window width by `count'."
  (condition-case nil
      (dotimes (i (or count 1))
        (vim:save-window-resize
         (enlarge-window-horizontally 1)))
    (error nil)))
            

(vim:defcmd vim:window-decrease-width (count nonrepeatable)
  "Decrease current window width by `count'."
  (shrink-window-horizontally (min (or count 1)
                                   (- (window-width) window-min-width))))
            

(vim:defcmd vim:window-set-height (count nonrepeatable)
   "Sets the height of the current window to `count'."
   (condition-case nil
       (cond
        ((null count)
         ;; maximize
         (while (/= (window-body-height)
                    (vim:save-window-resize
                     (enlarge-window 1)
                     (window-body-height)))))
        
        ((< count (window-body-height))
         ;; shrink window
         (while (/= (window-body-height) (max count window-min-height))
           (vim:save-window-resize
            (shrink-window 1))))
        (t
         ;; enlarge window
         (while (/= (window-body-height) count)
           (vim:save-window-resize
            (enlarge-window 1)))))))
         
   
(vim:defcmd vim:window-set-width (count nonrepeatable)
   "Sets the width of the current window to `count'."
   (condition-case nil
       (cond
        ((null count)
         ;; maximize
         (while (/= (window-width)
                    (vim:save-window-resize
                     (enlarge-window-horizontally 1)
                     (window-width)))))
        
        ((< count (window-width))
         ;; shrink window
         (while (/= (window-width) (max count window-min-width))
           (vim:save-window-resize
            (shrink-window-horizontally 1))))
        (t
         ;; enlarge window
         (while (/= (window-width) count)
           (vim:save-window-resize
            (enlarge-window-horizontally 1)))))))


(vim:defcmd vim:window-rotate-upwards (nonrepeatable)
   "Rotates the windows according to the currenty cyclic ordering."
   (let ((wlist (window-list))
         (blist (mapcar #'(lambda (w) (window-buffer w))
                        (window-list))))
     (setq blist (append (cdr blist) (list (car blist))))
     (while (and wlist blist)
       (set-window-buffer (car wlist) (car blist))
       (setq wlist (cdr wlist)
             blist (cdr blist)))
     (select-window (car (last (window-list))))))
     
     
(vim:defcmd vim:window-rotate-downwards (nonrepeatable)
   "Rotates the windows according to the currenty cyclic ordering."
   (let ((wlist (window-list))
         (blist (mapcar #'(lambda (w) (window-buffer w))
                        (window-list))))
     (setq blist (append (last blist) blist))
     (while (and wlist blist)
       (set-window-buffer (car wlist) (car blist))
       (setq wlist (cdr wlist)
             blist (cdr blist)))
     (select-window (cadr (window-list)))))


(vim:defcmd vim:window-move-very-top (nonrepeatable)
   "Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
   (unless (one-window-p)
     (let ((b (current-buffer)))
       (delete-window)
       (let ((btree (vim:get-buffer-tree (car (window-tree)))))
         (vim:window-only)
         (let ((newwin (selected-window))
               (subwin (split-window)))
           (vim:restore-window-tree subwin btree)
           (set-window-buffer newwin b)
           (select-window newwin))))
     (balance-windows)))


(vim:defcmd vim:window-move-far-left (nonrepeatable)
   "Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
   (unless (one-window-p)
     (let ((b (current-buffer)))
       (delete-window)
       (let ((btree (vim:get-buffer-tree (car (window-tree)))))
         (vim:window-only)
         (let ((newwin (selected-window))
               (subwin (split-window-horizontally)))
           (vim:restore-window-tree subwin btree)
           (set-window-buffer newwin b)
           (select-window newwin))))
     (balance-windows)))
     

(vim:defcmd vim:window-move-far-right (nonrepeatable)
   "Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
   (unless (one-window-p)
     (let ((b (current-buffer)))
       (delete-window)
       (let ((btree (vim:get-buffer-tree (car (window-tree)))))
         (vim:window-only)
         (let ((subwin (selected-window))
               (newwin (split-window-horizontally)))
           (vim:restore-window-tree subwin btree)
           (set-window-buffer newwin b)
           (select-window newwin))))
     (balance-windows)))
     

     

(vim:defcmd vim:window-move-very-bottom (nonrepeatable)
   "Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
   (unless (one-window-p)
     (let ((b (current-buffer)))
       (delete-window)
       (let ((btree (vim:get-buffer-tree (car (window-tree)))))
         (vim:window-only)
         (let ((subwin (selected-window))
               (newwin (split-window)))
           (vim:restore-window-tree subwin btree)
           (set-window-buffer newwin b)
           (select-window newwin))))
     (balance-windows)))
        

            
