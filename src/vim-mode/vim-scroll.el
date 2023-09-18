;; vim-scroll.el - Implementation of scrolling commands --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains implementations for the scrolling.  Scroll
;; operations are usually just simple commands and should not be
;; repeatable but should keep visual mode.

;;; Code:

(eval-when-compile
  (require 'cl)

  (defvar vim-scroll-move-point))


(require 'vim-macs)
(require 'vim-motions)

(defun vim--num-visible-lines ()
  "Returns the number of currently visible lines."
  (1- (window-height)))

(defun vim--max-scroll-up ()
  "Returns the maximal number of lines that can be scrolled up."
  (1- (line-number-at-pos (window-start))))

(defun vim--max-scroll-down ()
  "Returns the maximal number of lines that can be scrolled down."
  (if (pos-visible-in-window-p (window-end))
      0
    (1+ (- (line-number-at-pos (point-max))
           (line-number-at-pos (window-end))))))

(vim-defcmd vim:scroll-line-up (count nonrepeatable keep-visual)
  "Scrolls the window `count' lines upwards."
  (vim--use-last-column!)
  (scroll-down (or count 1))
  (when vim-scroll-move-point
    (vim:motion-up:wrapper :count (or count 1))))

(vim-defcmd vim:scroll-line-down (count nonrepeatable keep-visual)
  "Scrolls the window `count' lines downwards."
  (vim--use-last-column!)
  (scroll-up (or count 1))
  (when vim-scroll-move-point
    (vim:motion-down:wrapper :count (or count 1))))

(vim-defcmd vim:scroll-up (count nonrepeatable keep-visual)
  "Scrolls the window and the cursor `count' lines upwards, default half of the screen."
  (vim--use-last-column!)
  (let ((p (point))
        (c (or count (/ (vim--num-visible-lines) 2))))
    (save-excursion
      (scroll-down (min (vim--max-scroll-up) c)))
    (forward-line (- c))
    (when (= (line-number-at-pos p)
             (line-number-at-pos (point)))
      (ding))))

(vim-defcmd vim:scroll-down (count nonrepeatable keep-visual)
  "Scrolls the window and the cursor `count' lines downwards, default half of the screen."
  (vim--use-last-column!)
  (let ((p (point))
        (c (or count (/ (vim--num-visible-lines) 2))))
    (save-excursion
      (scroll-up (min (vim--max-scroll-down) c)))
    (forward-line c)
    (when (= (line-number-at-pos p)
             (line-number-at-pos (point)))
      (ding))))

(vim-defcmd vim:scroll-line-to-top (count nonrepeatable keep-visual)
  "Scrolls line number `count' (or the cursor line) to the top of the window."
  (vim--use-last-column!)
  (goto-line-dumb (or count (line-number-at-pos (point))))
  (recenter 0))

(vim-defcmd vim:scroll-line-to-center (count nonrepeatable keep-visual)
  "Scrolls line number `count' (or the cursor line) to the center of the window."
  (vim--use-last-column!)
  (goto-line-dumb (or count (line-number-at-pos (point))))
  (recenter nil))

(vim-defcmd vim:scroll-line-to-bottom (count nonrepeatable keep-visual)
  "Scrolls line number `count' (or the cursor line) to the bottom of the window."
  (vim--use-last-column!)
  (goto-line-dumb (or count (line-number-at-pos (point))))
  (recenter -1))

(vim-defcmd vim:scroll-bottom-line-to-top (count nonrepeatable keep-visual)
  "Scrolls the line right below the window or line `count' to the top of the window."
  (if count
      (goto-line-dumb count)
    (goto-char (window-end))
    (unless (bobp) (backward-char)))
  (recenter 0)
  (vim:motion-first-non-blank:wrapper))

(vim-defcmd vim:scroll-top-line-to-bottom (count nonrepeatable keep-visual)
  "Scrolls the line right below the window or line `count' to the top of the window."
  (if count
      (goto-line-dumb count)
    (goto-char (window-start)))
  (recenter -1)
  (vim:motion-first-non-blank:wrapper))

(provide 'vim-scroll)

;; Local Variables:
;; End:

;; vim-scroll.el ends here
