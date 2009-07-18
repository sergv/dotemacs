;;; vim-scroll.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; Description:

;; This file contains implementations for the scrolling and window
;; operations.  Scroll operations are usually just simple commands and should
;; not be repeatable.


(provide 'vim-scroll)

(require 'windmove nil t)

(defun vim:num-visible-lines ()
  "Returns the number of currently visible lines."
  (- (window-height) 1))

(defun vim:max-scroll-up ()
  "Returns the maximal number of lines that can be scrolled up."
  (1- (line-number-at-pos (window-start))))

(defun vim:max-scroll-down ()
  "Returns the maximal number of lines that can be scrolled down."
  (if (pos-visible-in-window-p (window-end))
      0
      (1+ (- (line-number-at-pos (point-max))
             (line-number-at-pos (window-end))))))

(vim:define vim:scroll-line-up (count)
            :type 'simple
            :repeatable nil
  "Scrolls the window `count' lines upwards."
  (vim:use-last-column)
  (scroll-down (or count 1)))


(vim:define vim:scroll-line-down (count)
            :type 'simple
            :repeatable nil
  "Scrolls the window `count' lines downwards."
  (vim:use-last-column)
  (scroll-up (or count 1)))


(vim:define vim:scroll-up (count)
            :type 'simple
            :repeatable nil
  "Scrolls the window and the cursor `count' lines upwards, default half of the screen."
  (vim:use-last-column)
  (let ((p (point))
        (c (or count (/ (vim:num-visible-lines) 2))))
    (save-excursion
      (scroll-down (min (vim:max-scroll-up) c)))
    (forward-line (- c))
    (when (= (line-number-at-pos p)
             (line-number-at-pos (point)))
      (ding))))


(vim:define vim:scroll-down (count)
            :type 'simple
            :repeatable nil
  "Scrolls the window and the cursor `count' lines downwards, default half of the screen."
  (vim:use-last-column)
  (let ((p (point))
        (c (or count (/ (vim:num-visible-lines) 2))))
    (save-excursion
      (scroll-up (min (vim:max-scroll-down) c)))
    (forward-line c)
    (when (= (line-number-at-pos p)
             (line-number-at-pos (point)))
      (ding))))


(vim:define vim:scroll-page-up (count)
            :type 'simple
            :repeatable nil
  "Scrolls the window `count' pages upwards."
  (vim:use-last-column)
  (condition-case nil
      (dotimes (i (or count 1))
        (scroll-down nil))
    (error (goto-char (point-min)))))


(vim:define vim:scroll-page-down (count)
            :type 'simple
            :repeatable nil
  "Scrolls the window `count' pages upwards."
  (vim:use-last-column)
  (condition-case nil
      (dotimes (i (or count 1))
        (scroll-up nil))
    (error (goto-char (point-max)))))


(vim:define vim:scroll-line-to-top (count)
            :type 'simple
            :repeatable nil
  "Scrolls line number `count' (or the cursor line) to the top of the window."            
  (vim:use-last-column)
  (goto-line (or count (line-number-at-pos (point))))
  (recenter 0))


(vim:define vim:scroll-line-to-center (count)
            :type 'simple
            :repeatable nil
  "Scrolls line number `count' (or the cursor line) to the center of the window."            
  (vim:use-last-column)
  (goto-line (or count (line-number-at-pos (point))))
  (recenter nil))


(vim:define vim:scroll-line-to-bottom (count)
            :type 'simple
            :repeatable nil
  "Scrolls line number `count' (or the cursor line) to the bottom of the window."            
  (vim:use-last-column)
  (goto-line (or count (line-number-at-pos (point))))
  (recenter -1))


(vim:define vim:scroll-bottom-line-to-top (count)
            :type 'simple
            :repeatable nil
  "Scrolls the line right below the window or line `count' to the top of the window."
  (if count
      (goto-line count)
    (goto-char (window-end)))
  (recenter 0)
  (goto-char (vim:motion-first-non-blank)))


(vim:define vim:scroll-top-line-to-bottom (count)
            :type 'simple
            :repeatable nil
  "Scrolls the line right below the window or line `count' to the top of the window."
  (if count
      (goto-line count)
    (goto-char (max (1- (window-start)) (point-min))))
  (recenter -1)
  (goto-char (window-start))
  (recenter -1)
  (goto-char (vim:motion-first-non-blank)))




(vim:define vim:window-split (count)
            :type 'simple
            :repeatable nil
  "Splits the current window horizontally, `count' lines height."            
  (split-window (selected-window) count))


(vim:define vim:window-vsplit (count)
            :type 'simple
            :repeatable nil
  "Splits the current window vertically, `count' columns width."            
  (split-window (selected-window) count t))


(vim:define vim:window-close ()
            :type 'simple
            :repeatable nil
            :count nil
  "Closes the current window."
  (delete-window))


(vim:define vim:window-only ()
            :type 'simple
            :repeatable nil
            :count nil
  "Closes all but the current window."
  (delete-other-windows))


;(vim:define vim:window-left ()
;            :type 'simple
;            :repeatable nil
;            :count nil
;  "Select the window at the left."
;  (windmove-left))


;(vim:define vim:window-up ()
;            :type 'simple
;            :repeatable nil
;            :count nil
;  "Select the window above."
;  (windmove-up))


;(vim:define vim:window-down ()
;            :type 'simple
;            :repeatable nil
;            :count nil
;  "Select the window below."
;  (windmove-down))


;(vim:define vim:window-right ()
;            :type 'simple
;            :repeatable nil
;            :count nil
;  "Select the window below at the right."
;  (windmove-right))

;(vim:define vim:window-set-height (count)
;            :type 'simple
;            :repeatable nil
;   "Sets the height of the current window to `count'."
   
            
