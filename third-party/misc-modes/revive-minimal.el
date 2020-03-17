;; revive-minimal.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  7 February 2015
;; Description:

;; Filename: revive+.el
;; Description: Selected window memoization for revive
;; Author: Martial Boniou
;; Maintainer: Martial Boniou
;; Created: Thu Mar 10 12:12:09 2011 (+0100)
;; Version: 0.9
;; Last-Updated: Sat Nov 26 18:16:23 2011 (+0100)
;;           By: Martial Boniou
;;     Update #: 173
;; URL: https://github.com/martialboniou/revive-plus.git
;; Keywords: window configuration serialization
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;              Let revive preserve the window focus to restore
;;              Support frames restoring in the printable window configuration
;;              Support escreen restoring in the printable window configuration
;;              Support special case like `ecb'
;;
;;              The window configuration has the following form:
;;              * for windows: ( WIDTH HEIGHT ... ) and so on (the same as in `revive'
;;              * for frames: ( ( WIDTH1 HEIGHT1 ... ) ( WIDTH2 HEIGHT2 ...) ... )
;;              * for escreen: ( POS ( WIDTH1 HEIGHT1 ... ) ( WIDTH2 HEIGHT2 ... ) )
;;              ie. the current escreen in this frame followed by the order printable
;;              window configuration for all the escreens: eg. ( 1 (WCONF0) (WCONF1)
;;              (WCONF2) )
;;              * for escreened frames: ( ( POS1 ( WIDTH1 ... ) ( WIDTH1' ... ) ... )
;;                                        ( POS2 ( WIDTH2 ... ) ( WIDTH2' ... ) ... )
;;                                        ... )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Installation:
;;               (require 'revive+)
;;               (revive-plus:demo)
;;
;;  You may customize revive-plus:all-frames to save all frames:
;;
;;               (require 'revive+)
;;               (setq revive-plus:all-frames t)
;;               (revive-plus:demo)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Usage:
;;
;;        After calling (revive-plus:demo) or (revive-plus:minimal-setup),
;;        your window configuration is automatically saved on Emacs killing
;;        and restore at Emacs startup. The file ~/.emacs.d/last-wconf is
;;        used for this. If you want to prevent crashes by periodically
;;        autosave the latest window configuration you just need to ensure
;;        DESKTOP is autosaved:
;;
;;        (add-hook 'auto-save-hook #'(lambda () (call-interactively #'desktop-save)))
;;
;;        Remember: there are two functions to save window configurations:
;;
;;        #'CURRENT-WINDOW-CONFIGURATION-PRINTABLE : save the windows only (used by
;;                                                   <f6> keys in DEMO mode)
;;        #'WINDOW-CONFIGURATION-PRINTABLE : save the windows for all escreens and,
;;        if REVIVE-PLUS:ALL-FRAMES is set to true, in all frames too.
;;
;;        If REVIVE-PLUS:ALL-FRAMES is NIL and `escreen' is not enabled in your
;;        init file (~/.emacs), those two functions behave the same.
;;
;;        Additional functions are available:
;;        <f6><f6>: save the current window configuration (no frames / no escreen,
;;                  only the displayed windows in the current frame)
;;        <f6><f5>: restore the previously saved configuration
;;        <f6>2 .. <f6>0: restore the second to tenth window configuration
;;        All the last ten window configurations saved via <f6> keys are stored
;;        in the file ~/.emacs.d/wconf-archive and will be loaded at Emacs startup.
;;        This functionality is useful to transfer a window configuration to another
;;        frame, for example.
;;
;;        <f5><f5>: switch from a window configuration with multiple windows
;;                  to a single view and back. Useful to focus on a buffer.
;;        This functionality uses the standard window configuration system b/c
;;        there's no need to save it between sessions. The marker position is
;;        not saved; it's only the window configuration.
;;
;;        Beware: If you use Emacs in NO-WINDOW-SYSTEM (ie. in a terminal), you
;;                lose the window configuration in the other frames than the
;;                last focused one. If `escreen' is enabled, all the content of
;;                those other frames is restored as new ESCREEN. Eg.:
;;
;;                WINDOW-SYSTEM (saving):
;;
;;                -------------     -------------     -----------
;;                |  frame 2  |     |  frame 1  |     | frame 3 |
;;                |           |  +  |           |  +  |         |
;;                | ( 0 1 2 ) |     | ( 0 1 2 ) |     | ( 0 1 ) |
;;                -------------     -------------     -----------
;;
;;                where frame 1 is the focused one and ( X Y )s are the ESCREENs.
;;
;;                NO-WINDOW-SYSTEM (restoring):
;;
;;                -----------------------        /   ( 0 1 2 ) <= ( 0 1 2 ) in frame 1
;;                |     * no-frame *    |        |
;;                |                     | where  |   ( 4 5 6 ) <= ( 0 1 2 ) in frame 2
;;                | ( 0 1 2 4 5 6 7 8 ) |        |
;;                -----------------------        \   ( 7 8 )   <= ( 0 1 ) in frame 3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'dash)

;;;; old revive

(defconst revive:version
  "$Id: revive.el,v 2.21 2012/08/12 11:56:06 yuuji Exp yuuji $"
  "Version of revive.el")

(cond
  ((fboundp 'screen-height)
   (fset 'revive:screen-height 'screen-height)
   (fset 'revive:screen-width 'screen-width))
  ((fboundp 'frame-height)
   (fset 'revive:screen-height 'frame-height)
   (fset 'revive:screen-width 'frame-width))
  (t (error "I don't know how to run revive.el on this Emacs...")))

(defsubst revive:minx () 0)
(defsubst revive:miny ()
  (car (cdr (window-edges (frame-first-window nil)))))

(defun revive:split-window-safe (window size &optional hor-flag)
  "Same as split-window but avoids error."
  (split-window
   window
   (min (max (if hor-flag window-min-width window-min-height) size)
        (if hor-flag (- (revive:screen-width) window-min-width 1)
          (- (revive:screen-height) window-min-height 1)))
   hor-flag))

(defun revive:restore-winconf (x1 y1 x2 y2 edges)
  "Restore partial window configuration.
Assume (X1, Y1), (X2, Y2) as diagonal corners of partial window.
EDGES is a list of sub-windows' edges."
  (let*((topwin (car edges)) (width (- x2 x1)) (height (- y2 y1))
        right lower)
    (cond
      ((= (length edges) 1) nil)                ;nothing to do.

      ;;if the top window has the same width as whole frame.
      ;; +---------+
      ;; |top      |
      ;; +-----+---+
      ;; |2    |3  |
      ;; +-----+---+
      ((= width (- (nth 2 topwin) (car topwin)))
       (setq lower (cdr edges))
       (revive:select-window-by-edge x1 y1)
       (revive:split-window-safe nil (- (nth 3 topwin) (nth 1 topwin)))
       (revive:restore-winconf
        (car (car lower)) (nth 1 (car lower)) x2 y2 lower))

      ;;if the top window has the same height as whole frame.
      ;; +-----+---+
      ;; |top  |2  |
      ;; |     +---+
      ;; |     |3  |
      ;; +-----+---+
      ((= height (- (nth 3 topwin) (nth 1 topwin)))
       (setq right (cdr edges))
       (revive:select-window-by-edge x1 y1)
       (revive:split-window-safe nil (- (nth 2 topwin) (car topwin)) t)
       (revive:restore-winconf
        (car (car right)) (nth 1 (car right)) x2 y2 right))

      ;;These two cases above are specialized solution of below for speed.

      ;;general cases.
      ;; +------+--+  Detect whether window is mainly divided vertically or
      ;; |top   |2 |  horizontally.  And call this function recursively on
      ;; +---+--+--+  former (that is, upper half in vertical division or
      ;; |3  |4..  |  left half in horizontal) and latter configuration.
      ;; +---+-----+
      (t
       (let ((flist (list topwin))
             (elist (cdr edges)) divwin div-x div-y former latter)
         (while elist
           (if (or (and (= x1 (car (car elist)))
                        (not (eq (car divwin) x1)))
                   (and (= y1 (nth 1 (car elist)))
                        (not (eq (nth 1 divwin) y1))))
             (setq divwin (car elist)
                   former flist
                   latter elist))
           (setq flist (append flist (list (car elist))))
           (setq elist (cdr elist)))
         (setq div-x (car divwin) div-y (nth 1 divwin))
         (cond
           ((= x1 (car divwin))        ;Mainly divided vertically
            (revive:select-window-by-edge x1 y1)
            (revive:split-window-safe nil (- div-y y1))
            (revive:restore-winconf x1 y1 x2 div-y former)
            (revive:restore-winconf x1 div-y x2 y2 latter)
            (message "=="))
           ((= y1 (nth 1 divwin))
            (revive:select-window-by-edge x1 y1)
            (revive:split-window-safe nil (- div-x x1) t)
            (revive:restore-winconf x1 y1 div-x y2 former)
            (revive:restore-winconf div-x y1 x2 y2 latter)
            (message "||"))
           (t (message "dame!"))))))))

(defun revive:normalize-edges (width height edgelist)
  "Normalize all coordinates for current screen size.
'(WIDTH, HEIGHT) is old screen size and EDGELIST is a list of
window-edges."
  (let (normalized
        (curw (revive:screen-width))
        (curh (revive:screen-height))
        n)
    (if (and (equal curw width) (equal curh height))
      edgelist
      (dolist (e edgelist)
        (setq n (list (/ (+ (* curw (nth 0 e)) (/ width 2)) width)
                      (/ (+ (* curh (nth 1 e)) (/ height 2)) height)
                      (/ (+ (* curw (nth 2 e)) (/ width 2)) width)
                      (/ (+ (* curh (nth 3 e)) (/ height 2)) height))
              normalized (append normalized (list n))))
      normalized)))

(defun revive:construct-window-configuration (edgelist)
  "Restore window configuration by EDGELIST.  EDGELIST should be sorted."
  (delete-other-windows)
  (revive:restore-winconf (revive:minx) (revive:miny)
                          (revive:screen-width)
                          (1- (revive:screen-height)) edgelist))

(defsubst revive:make-window-config (file buffer point window-start)
  (list file buffer point window-start))
(defsubst revive:get-file (x)
  (car x))
(defsubst revive:get-buffer (x)
  (cadr x))
(defsubst revive:get-point (x)
  (caddr x))
(defsubst revive:get-window-start (x)
  (cadddr x))

(defun revive:find-file (file)
  "Make the best effort to find-file FILE."
  (cond
    ((or (null file) (not (stringp file))) nil)
    ((file-exists-p file) (find-file file) (current-buffer))
    ((string-match-p ":" file)                ;maybe ange-ftp's external file
     (if (progn (load "ange-ftp" t) (featurep 'ange-ftp))
       (progn (condition-case err
                  (find-file file)
                (ftp-error
                 (message "Can't remote file `%s'" file)
                 (condition-case err2        ;give a user one more chance.
                     (find-file file)
                   (ftp-error (error "Maybe you made mistake twice.")))))
              (current-buffer))))
    (t nil)))

;;;; old revive+

(defconst revive-plus:version
  "revive+.el,v 0.9 <hondana@gmx.com>"
  "Version of revive+.el")

(defgroup revive-plus nil
  " Revive window configurations anywhere."
  :group 'convenience)

(defcustom revive-plus:all-frames nil
  "Revive all frames and their window-configurations.
If NIL, saving and restoring will be enabled for the currently
focused frame."
  :group 'revive-plus
  :type 'boolean)

(defvar revive-plus:previous-window-configuration nil
  "State of window configuration to restore. Used by
`revive-plus:toggle-single-window'.")

(defun revive:window-list (&optional frame)
  "Return the all window list in sorted order."
  (let*((curwin (if (null frame)
                  (selected-window)
                  (frame-selected-window frame)))
        (win curwin) wlist)
    (if (null
         (catch 'found
           (while t
             (if (and (= (revive:minx) (car (window-edges win)))
                      (= (revive:miny) (car (cdr (window-edges win)))))
               (throw 'found t))
             (if (eq (setq win (next-window win)) curwin)
               (throw 'found nil)))))
      (error "Unexpected window configuration."))
    (setq curwin win wlist (list win))
    (while (not (eq curwin (setq win (next-window win))))
      (setq wlist (append wlist (list win)))) ;use append to preserve order
    wlist))

(defun revive:window-buffer-list (&optional frame)
  "Return the all shown buffer list.
Each element consists of '(buffer-file-name window-start point)"
  (let ((curw (if (null frame)
                (selected-window)
                (frame-selected-window frame)))
        (wlist (revive:window-list)) wblist)
    (save-excursion
     (while wlist
       (select-window (car wlist))
       (set-buffer (window-buffer (car wlist))) ;for Emacs 19
       (setq wblist
             (append wblist
                     (list (list
                            (if (and (fboundp 'abbreviate-file-name)
                                     (buffer-file-name))
                              (abbreviate-file-name (buffer-file-name))
                              (buffer-file-name))
                            (window-start)
                            (point))))
             wlist (cdr wlist)))
     (select-window curw)
     wblist)))

(defun revive:select-window-by-edge (x y &optional frame)
  "Select window whose north west corner is (X, Y).
If the matching window is not found, select the nearest window."
  (let*((curwin (if (null frame)
                  (selected-window)
                  (frame-selected-window frame)))
        (win (next-window curwin)) edges
        s2 (min 99999) minwin)
    (or
     (catch 'found
       (while t
         (setq edges (window-edges win)
               s2 (+ (* (- (car edges) x) (- (car edges) x))
                     (* (- (nth 1 edges) y) (- (nth 1 edges) y))))
         (cond
           ((= s2 0)
            (select-window win)
            (throw 'found t))
           ((< s2 min)
            (setq min s2 minwin win)))
         (if (eq win curwin) (throw 'found nil)) ;select the nearest window
         (setq win (next-window win))))
     (select-window minwin))))

(defun revive:all-window-edges (&optional frame)
  "Return the all windows edges by list."
  (let ((wlist (revive:window-list frame)) edges)
    (while wlist
      (setq edges (append edges (list (window-edges (car wlist))))
            wlist (cdr wlist)))
    edges))

(defun current-window-configuration-printable (&optional single-frame)
  "Print window configuration for a frame. Override the same
function defined in `revive'."
  (let ((curwin (if (or (null single-frame) (not (framep single-frame)))
                  (selected-window)
                  (frame-selected-window single-frame)))
        (wlist (revive:window-list single-frame))
        (edges (revive:all-window-edges single-frame)) buflist)
    (save-window-excursion
     (save-excursion
       (dolist (win wlist)
         (select-window win)
         ;should set buffer on Emacs 19
         (set-buffer (window-buffer win))
         (let ((buf (list
                     (if (and
                          (buffer-file-name)
                          (fboundp 'abbreviate-file-name))
                       (abbreviate-file-name
                        (buffer-file-name))
                       (buffer-file-name))
                     (buffer-name)
                     (point)
                     (window-start))))
           (when (eq curwin (selected-window))
             (setq buf (append buf (list nil 'focus))))
           (setq buflist
                 (append buflist (list buf)))))
      (select-window curwin)
      (list (revive:screen-width) (revive:screen-height) edges buflist)))))

;;;###autoload
(defun revive-plus:window-configuration-printable ()
  "Print window configuration for the current frame.
If REVIVE-PLUS:ALL-FRAMES is true, print window configuration for
all frames as a list of current-window-configuration-printable."
  (if (null revive-plus:all-frames)
      (current-window-configuration-printable)
    (let ((focus (selected-frame)))
      (mapcar #'(lambda (frame)
                  (current-window-configuration-printable frame))
              (--filter (null (frame-parent it))
                        (cons focus (remq focus (frame-list))))))))

;;;###autoload
(defun revive-plus:restore-window-configuration (config)
  (if (listp (car config))              ; multiple frame case
    (progn
      (if (window-system)
        (progn
          (let ((frame-config-diff (- (length config) (length (frame-list)))))
            (when (< 0 frame-config-diff)
              (dotimes (i frame-config-diff)
                (make-frame))))
          (loop
            for frame being the frames
            for conf in config
            do
               (with-selected-frame frame
                 (revive-plus:restore-window-configuration conf))))
        (revive-plus:restore-window-configuration (car config)))) ; lose other frames
    ; restoration of single frame
    (pcase config
      (`(,width ,height ,edges ,buflist)
        (set-buffer (get-buffer-create "*scratch*"))
        (setq edges (revive:normalize-edges width height edges))
        (revive:construct-window-configuration edges)
        (revive:select-window-by-edge (revive:minx) (revive:miny))
        (let (focus)
          (dolist (buf buflist)
            (cond
              ((and (revive:get-buffer buf)
                    (get-buffer (revive:get-buffer buf)))
               (switch-to-buffer (revive:get-buffer buf))
               (when (eq 'focus (car (last buf)))
                 (setq focus (selected-window)))
               (goto-char (revive:get-window-start buf)) ; to prevent high-bit missing
               (set-window-start nil (point))
               (goto-char (revive:get-point buf)))
              ((and (stringp (revive:get-file buf))
                    (not (file-directory-p (revive:get-file buf)))
                    (revive:find-file (revive:get-file buf)))
               (set-window-start nil (revive:get-window-start buf))
               (goto-char (revive:get-point buf))))
            (other-window 1))
          (when focus
            (select-window focus)))))))

;;;###autoload
(defun revive-plus:toggle-single-window ()
  "Toggle to single window and back. It uses standard
window configuration vector instead of REVIVE one as we
don't want to register the mark."
  (interactive)
  (if (cdr (window-list nil 0))
    (progn
      (setq revive-plus:previous-window-configuration
            (current-window-configuration))
      (delete-other-windows))
    (unless (null revive-plus:previous-window-configuration)
      (set-window-configuration revive-plus:previous-window-configuration))))

(provide 'revive-minimal)

;; Local Variables:
;; End:

;; revive-minimal.el ends here
