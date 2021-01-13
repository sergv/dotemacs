;;; profile-startup.el --- Profile your Emacs init file

;; Copyright (C) 2010  David Engster

;; Author: David Engster <dengste@eml.cc>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is to easily profile your Emacs init file (or any other
;; script-like Emacs Lisp file, for that matter).

;; It will go over all sexp's (balanced expressions) in the file and
;; run them through `benchmark-run'.  It will then show the file with
;; overlays applied in a way that let you easily find out which sexp's
;; take the most time.  Since time is relative, it's not the absolute
;; value that counts but the percentage of the total running time.
;;
;; * All other sexp's with a percentage greater than
;;   `profile-startup-low-percentage' will be preceded by a
;;   highlighted line, showing the results from `benchmark-run'.
;;   Also, the more 'reddish' the background of the sexp, the more
;;   time it needs.

;; * All other sexp's will be grayed out to indicate that their
;;   running time is miniscule.  You can still see the benchmark
;;   results in the minibuffer by hovering over the sexp with the
;;   mouse.

;; You can only benchmark full sexp's, so if you wrapped large parts
;; of your init file in some conditional clause, you'll have to remove
;; that for getting finer granularity.

;;; Usage:

;; Start emacs as follows:
;;
;;    emacs -Q -l <PATH>/profile-startup.el -f profile-startup
;;
;; with <PATH> being the path to where this file resides.

;;; Download:

;;  You can always get the latest version from
;;       http://randomsample.de/profile-startup.el


;;; Code:

(require 'thingatpt)
(require 'benchmark)

;; User variables

(defvar profile-startup-file "~/emacs/src/start.el" ;;"~/emacs/.emacs"
  "File to be profiled.")

(defun profile-startup-minimal-setup ()
  "Perform minimal setup of emacs."
  (require 'cl)
  (setq max-lisp-eval-depth 1000
        ;; to handle all unwind-protects and similar stuff
        ;; in deep recursion
        max-specpdl-size    5000)

  (setq gc-cons-threshold (* 5 1024 1024)
        gc-cons-percentage 0.01)

  (defsubst load-silent (x)
    (load x nil t))

  (dolist (path (cl-remove-duplicates
                 (list "/home/sergey/emacs/src"
                       (expand-file-name "~/emacs/src")
                       "C:\\emacs/src"
                       "D:\\emacs/src")
                 :test #'string=))
    (when (and (file-exists-p path)
               (file-directory-p path))
      (add-to-list 'load-path path)))

  ;; extended setup

  (load-library "set-up-platform")
  (load-library "set-up-environment-variables")
  (load-library "set-up-paths")
  (load-library "set-up-font")

  (load-library "base-emacs-fixes"))

(defvar profile-startup-low-percentage 2
  "Percentage which should be considered low.
All sexp's with a running time below this percentage will be
grayed out.")

(defface profile-startup-time-face
    '((((background dark)) (:underline "OrangeRed1"))
      (t (:underline "red")))
  "Background color to indicate percentage of total time.")

(defface profile-startup-low-percentage-face
    '((t (:foreground "#93a1a1")))
  "Face for sexps below `profile-startup-low-percentage'.")

(defface profile-startup-highlight-face
    '((((background dark)) (:background "#cfefff"))
      (t (:background "#ffffbf")))
  "Highlight face for benchmark results.")

;; Main function

(defun profile-startup ()
  "Load `profile-startup-file' and benchmark its sexps."
  (interactive)
  (with-current-buffer (find-file-noselect profile-startup-file t)
    (profile-startup-minimal-setup)
    (setq buffer-read-only t) ;; just to be sure
    (goto-char (point-min))
    (let (start end results)
      (while
       (< (point)
          (setq end (progn
                      (forward-sexp 1)
                      (point))))
       (forward-sexp -1)
       (setq start (point))
       (add-to-list
        'results
        `(,start ,end
                 ,(benchmark-run
                   (eval (sexp-at-point)))))
       (goto-char end))
      (profile-startup-show-results results)
      (switch-to-buffer (current-buffer)))))

;; Helper functions

(defun profile-startup-show-results (results)
  "Show timings from RESULTS in current buffer."
  (let ((totaltime (profile-startup-totaltime results))
        current percentage ov)
    (while results
           (let* ((current (pop results))
                  (ov (make-overlay (car current) (cadr current)))
                  (current (car (last current)))
                  (percentage (/ (+ (car current) (nth 2 current))
                                 totaltime))
                  col benchstr lowface)
             (setq col
                   (profile-startup-percentage-color
                    percentage
                    (face-background 'default)
                    (face-attribute 'profile-startup-time-face :underline)))
             (setq percentage (round (* 100 percentage)))
             (setq benchstr (profile-startup-make-benchstr current))
             (overlay-put ov 'help-echo benchstr)
             (if (and (numberp profile-startup-low-percentage)
                      (< percentage profile-startup-low-percentage))
               (overlay-put ov 'face 'profile-startup-low-percentage-face)
               (overlay-put ov 'before-string
                            (propertize benchstr
                                        'face 'profile-startup-highlight-face))
               (overlay-put ov 'face
                            `(:underline ,col)))))
    (setq ov (make-overlay (1- (point-max)) (point-max)))
    (overlay-put ov 'after-string
                 (propertize
                  (format "\n-----------------\nTotal time: %.2fs\n"
                          totaltime)
                  'face 'profile-startup-highlight-face))))

(defun profile-startup-totaltime (results)
  "Calculate total time of RESULTS."
  (let ((totaltime 0))
    (mapc (lambda (x)
            (let ((cur (car (last x))))
              (setq totaltime (+ totaltime (car cur) (nth 2 cur)))))
          results)
    totaltime))

(defun profile-startup-percentage-color (percent col-begin col-end)
  "Calculate color according to PERCENT between COL-BEGIN and COL-END."
  (let* ((col1 (color-values col-begin))
         (col2 (color-values col-end))
         (col
           (mapcar (lambda (c)
                     (round
                      (+ (* (- 1 percent) (nth c col1))
                         (* percent (nth c col2)))))
                   '(0 1 2))))
    (format "RGB:%04x/%04x/%04x"
            (car col)
            (nth 1 col)
            (nth 2 col))))

(defun profile-startup-make-benchstr (timings)
  "Create descriptive benchmark string from TIMINGS."
  (format
   (concat
    "<Percentage: %d ; "
    "Time: %.2f ; "
    "Number of GC: %d ; "
    "Time for GC: %.2f>\n")
   percentage
   (car timings) (nth 1 timings) (nth 2 timings)))


;; profile-startup.el ends here
