;;; browse-kill-ring.el --- interactively insert items from kill-ring -*- coding: utf-8 -*-

;; Copyright (C) 2001, 2002 Colin Walters <walters@verbum.org>

;; Author: Colin Walters <walters@verbum.org>
;; Maintainer: browse-kill-ring <browse-kill-ring@tonotdo.com>
;; Created: 7 Apr 2001
;; Version: 1.5a
;; URL: https://github.com/browse-kill-ring/browse-kill-ring
;; Keywords: convenience

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Ever feel that 'C-y M-y M-y M-y ...' is not a great way of trying
;; to find that piece of text you know you killed a while back?  Then
;; browse-kill-ring.el is for you.

;; This package is simple to install; add (require 'browse-kill-ring)
;; to your ~/.emacs file, after placing this file somewhere in your
;; `load-path'.  If you want to use 'M-y' to invoke
;; `browse-kill-ring', also add (browse-kill-ring-default-keybindings)
;; to your ~/.emacs file.  Alternatively, you can bind it to another
;; key such as "C-c k", with:
;; (global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Note that the command keeps track of the last window displayed to
;; handle insertion of chosen text; this might have unexpected
;; consequences if you do 'M-x browse-kill-ring', then switch your
;; window configuration, and try to use the same *Kill Ring* buffer
;; again.

;;; Change Log:

;; Changes from 1.5 to 1.5a:

;; * 2013-Oct-29: skyer9 <skyer9 at gmail dot com>
;;   Works with `delete-selection-mode'.
;;
;;   Modify `save-and-restore' option when exit.
;;   If Emacs version is 24+, browse-kill-ring works with `query-replace' properly.
;;
;;   Swap RET (for `browse-kill-ring-insert-move-and-quit') and u key bindings.
;;   When kill is selected from browse-kill-ring (by typing RET),
;;   selected kill is changed to last kill of kill-ring,
;;   and can yank selected kill directy.
;;
;;   Some bug fix.

;; Changes from 1.4 to 1.5:

;; * 2013-Aug-10: Ethan Glasser-Camp
;;   Fix browse-kill-ring-update. Commit ca0b5f4 broke it. Fixing it
;;   also exposed some problems with how the overlay is handled.

;; * 2013-Jul-29: Ethan Glasser-Camp
;;   Make `browse-kill-ring-quit-action' default to
;;   `save-and-restore'. This seems to DWIM in most cases: running
;;   browse-kill-ring with only one window and then pressing q will
;;   close that window, but if you have two windows open when you run
;;   browse-kill-ring, it will restore the windows you had open. For
;;   more information and history, see
;;   https://github.com/browse-kill-ring/browse-kill-ring/issues/11.

;; * 2013-Jan-19: Ethan Glasser-Camp
;;   browse-kill-ring-mode now uses an overlay to show what your
;;   buffer would look like if you inserted the current item.

;; * 2013-Jan-02: Ethan Glasser-Camp
;;   Fix a bug with default faces used to highlight items.
;;   The symbols browse-kill-ring-current-entry-face and
;;   browse-kill-ring-inserted-item-face are now treated like
;;   variables specifying a face instead of faces themselves. If you
;;   defined faces with these names, you will need to set the symbols
;;   (as variables) to the names of the faces you defined.
;;
;;   (setq browse-kill-ring-current-entry-face 'browse-kill-ring-current-entry-face)
;;
;;   Fix another bug with highlighting inserted items. (Previously, it
;;   highlighted arbitrary and incorrect parts of the buffer.)
;;
;;   Add custom variable browse-kill-ring-replace-yank. When t, this
;;   makes browse-kill-ring after a yank replace the yanked text, like
;;   yank-pop.
;;
;;   Refactor browse-kill-ring-do-prepend-insert,
;;   browse-kill-ring-do-append-insert, and browse-kill-ring-do-insert
;;   to all call the same function to do the actual inserting.

;; Changes from 1.3c to 1.4:

;; * 16-Aug-2012: Toon Claes
;;   No actual changes to the code, but released again by the
;;   browse-kill-ring user at Github.com.

;; * 28-Feb-2011: Andrew Burgess <aburgess@broadcom.com>
;;   Fix a bug where having other overlays active in the kill ring buffer,
;;   for example with show-paren-mode would block insertion.

;; Changes from 1.3a to 1.3b:

;; * 24-Feb-2011: Andrew Burgess <aburgess@broadcom.com>
;;   Correctly handle inserting when multiple windows exist for the
;;   same buffer.

;; Changes from 1.3 to 1.3a:

;; * Sneak update by Benjamin Andresen <bandresen@gmail.com>
;; * Added the read-only bugfix (http://bugs.debian.org/225082) from
;;   the emacs-goodies-el package

;; Changes from 1.2 to 1.3:

;; * New maintainer, Nick Hurley <hurley@cis.ohio-state.edu>
;; * New functions `browse-kill-ring-prepend-insert', and
;;   `browse-kill-ring-append-insert', bound to 'b' and 'a' by
;;   default. There are also the unbound functions
;;   `browse-kill-ring-prepend-insert-and-quit',
;;   `browse-kill-ring-prepend-insert-and-move',
;;   `browse-kill-ring-prepend-insert-move-and-quit',
;;   `browse-kill-ring-append-insert-and-quit',
;;   `browse-kill-ring-append-insert-and-move',
;;   `browse-kill-ring-append-insert-move-and-quit'.

;; Changes from 1.1 to 1.2:

;; * New variable `browse-kill-ring-resize-window', which controls
;;   whether or not the browse-kill-ring window will try to resize
;;   itself to fit the buffer.  Implementation from Juanma Barranquero
;;   <lektu@terra.es>.
;; * New variable `browse-kill-ring-highlight-inserted-item'.
;;   Implementation from Yasutaka SHINDOH <ring-pub@fan.gr.jp>.
;; * `browse-kill-ring-mouse-insert' (normally bound to mouse-2) now
;;   calls `browse-kill-ring-quit'.
;; * Some non-user-visible code cleanup.
;; * New variable `browse-kill-ring-recenter', implementation from
;;   René Kyllingstad <kyllingstad@users.sourceforge.net>.
;; * Patch from Michal Maršuka <mmc@maruska.dyndns.org> which handles
;;   read-only text better.
;; * New ability to move unkilled entries back to the beginning of the
;;   ring; patch from Yasutaka SHINDOH <ring-pub@fan.gr.jp>.
;; * Do nothing if the user invokes `browse-kill-ring' when we're
;;   already in a *Kill Ring* buffer (initial patch from Juanma
;;   Barranquero <lektu@terra.es>).

;; Changes from 1.0 to 1.1:

;; * Important keybinding change!  The default bindings of RET and 'i'
;;   have switched; this means typing RET now by default inserts the
;;   text and calls `browse-kill-ring-quit'; 'i' just inserts.
;; * The variable `browse-kill-ring-use-fontification' is gone;
;;   browse-kill-ring.el has been rewritten to use font-lock.  XEmacs
;;   users who want fontification will have to do:
;;   (add-hook 'browse-kill-ring-hook 'font-lock-mode)
;; * Integrated code from Michael Slass <mikesl@wrq.com> into
;;   `browse-kill-ring-default-keybindings'.
;; * New Japanese homepage for browse-kill-ring.el, thanks to
;;   Yasutaka SHINDOH <ring-pub@fan.gr.jp>.
;; * Correctly restore window configuration after editing an entry.
;; * New command `browse-kill-ring-insert-and-delete'.
;; * Bug reports and patches from Michael Slass <mikesl@wrq.com> and
;;   Juanma Barranquero <lektu@terra.es>.

;; Changes from 0.9b to 1.0:

;; * Add autoload cookie to `browse-kill-ring'; suggestion from
;;   D. Goel <deego@glue.umd.edu> and Dave Pearson <davep@davep.org>.
;; * Add keybinding tip from Michael Slass <mikesl@wrq.com>.

;; Changes from 0.9a to 0.9b:

;; * Remove extra parenthesis.  Duh.

;; Changes from 0.9 to 0.9a:

;; * Fix bug making `browse-kill-ring-quit-action' uncustomizable.
;;   Patch from Henrik Enberg <henrik@enberg.org>.
;; * Add `url-link' and `group' attributes to main Customization
;;   group.

;; Changes from 0.8 to 0.9:

;; * Add new function `browse-kill-ring-insert-and-quit', bound to 'i'
;;   by default (idea from Yasutaka Shindoh).
;; * Make default `browse-kill-ring-quit-action' be
;;   `bury-and-delete-window', which handles the case of a single window
;;   more nicely.
;; * Note change of home page and author address.

;; Changes from 0.7 to 0.8:

;; * Fix silly bug in `browse-kill-ring-edit' which made it impossible
;;   to edit entries.
;; * New variable `browse-kill-ring-quit-action'.
;; * `browse-kill-ring-restore' renamed to `browse-kill-ring-quit'.
;; * Describe the keymaps in mode documentation.  Patch from
;;   Marko Slyz <mslyz@eecs.umich.edu>.
;; * Fix advice documentation for `browse-kill-ring-no-duplicates'.

;; Changes from 0.6 to 0.7:

;; * New functions `browse-kill-ring-search-forward' and
;;   `browse-kill-ring-search-backward', bound to "s" and "r" by
;;   default, respectively.
;; * New function `browse-kill-ring-edit' bound to "e" by default, and
;;   a associated new major mode.
;; * New function `browse-kill-ring-occur', bound to "l" by default.

;; Changes from 0.5 to 0.6:

;; * Fix bug in `browse-kill-ring-forward' which sometimes would cause
;;   a message "Wrong type argument: overlayp, nil" to appear.
;; * New function `browse-kill-ring-update'.
;; * New variable `browse-kill-ring-highlight-current-entry'.
;; * New variable `browse-kill-ring-display-duplicates'.
;; * New optional advice `browse-kill-ring-no-kill-new-duplicates',
;;   and associated variable `browse-kill-ring-no-duplicates'.  Code
;;   from Klaus Berndl <Klaus.Berndl@sdm.de>.
;; * Bind "?" to `describe-mode'.  Patch from Dave Pearson
;;   <dave@davep.org>.
;; * Fix typo in `browse-kill-ring-display-style' defcustom form.
;;   Thanks "Kahlil (Kal) HODGSON" <kahlil@discus.anu.edu.au>.

;; Changes from 0.4 to 0.5:

;; * New function `browse-kill-ring-delete', bound to "d" by default.
;; * New function `browse-kill-ring-undo', bound to "U" by default.
;; * New variable `browse-kill-ring-maximum-display-length'.
;; * New variable `browse-kill-ring-use-fontification'.
;; * New variable `browse-kill-ring-hook', called after the
;;   "*Kill Ring*" buffer is created.

;; Changes from 0.3 to 0.4:

;; * New functions `browse-kill-ring-forward' and
;;   `browse-kill-ring-previous', bound to "n" and "p" by default,
;;   respectively.
;; * Change the default `browse-kill-ring-display-style' to
;;   `separated'.
;; * Removed `browse-kill-ring-original-window-config'; Now
;;   `browse-kill-ring-restore' just buries the "*Kill Ring*" buffer
;;   and deletes its window, which is simpler and more intuitive.
;; * New variable `browse-kill-ring-separator-face'.

;;; Bugs:

;; * Sometimes, in Emacs 21, the cursor will jump to the end of an
;;   entry when moving backwards using `browse-kill-ring-previous'.
;;   This doesn't seem to occur in Emacs 20 or XEmacs.

;; Code:

(require 'common)
(require 'dash)
(require 'ring)

(eval-when-compile
  (require 'cl)
  (require 'cl-lib)
  (require 'derived))

(defun browse-kill-ring-start-for-variable (variable buffer-name)
  "Display items within the VARIABLE symbol in browse buffer named BUFFER-NAME."
  (interactive)
  (cl-assert (symbolp variable))
  (when (eq major-mode 'browse-kill-ring-mode)
    (error "Already viewing the kill ring"))
  (let ((browse-buf (get-buffer-create buffer-name)))
    (setf browse-kill-ring--ring-var variable)
    (browse-kill-ring-setup browse-buf (current-buffer) (selected-window))
    (pop-to-buffer browse-buf)
    (browse-kill-ring-resize-window)
    nil))

(defmacro browse-kill-ring-with-original-buffer (&rest body)
  `(with-current-buffer
       (window-buffer browse-kill-ring-original-window)
     ,@body))

(defvar browse-kill-ring--ring-var nil
  "Holder for symbol pointing to list or ring with paste history.")

(defcustom browse-kill-ring-depropertize nil
  "If non-nil, remove text properties from `browse-kill-ring--ring-var' items.
This only changes the items for display and insertion from
`browse-kill-ring'; if you call `yank' directly, the items will be
inserted with properties."
  :type 'boolean
  :group 'browse-kill-ring)

(defun browse-kill-ring--get-ring-value ()
  (buffer-local-value browse-kill-ring--ring-var
                      (window-buffer browse-kill-ring-original-window)))

(defun browse-kill-ring--set-ring-value (value)
  (browse-kill-ring-with-original-buffer
   (set browse-kill-ring--ring-var value)))

(defun browse-kill-ring--insert-ring-value (item)
  "Add ITEM to ring pointed to by `*browse-kill-ring-ring-var*'."
  (let ((val (browse-kill-ring--get-ring-value)))
    (cond
      ((ring-p val)
       (ring-insert val time))
      ((listp val)
       (browse-kill-ring--set-ring-value
        (cons item val)))
      (t
       (error "Unrecognized kill ring type: %s" val)))))

(defun browse-kill-ring--delete-ring-value (item)
  "Delete ITEM from ring pointed to by `*browse-kill-ring-ring-var*'."
  (let ((val (browse-kill-ring--get-ring-value)))
    (cond
      ((ring-p val)
       (ring-remove val
                    (ring-member val
                                 item)))
      ((listp val)
       (browse-kill-ring--set-ring-value
        (delete item val)))
      (t
       (error "Unrecognized kill ring type %s" val)))))

(defun browse-kill-ring--to-list (val)
  "Convert contents of `*browse-kill-ring-ring-var*' to list."
  ;; this cond should be replaced by generic functions,
  (cond
    ((ring-p val)
     (ring-elements val))
    ((listp val)
     val)
    (t
     (error "Don't know how to conver to list: %s" val))))

(defun browse-kill-ring--kill-new (string)
  "Generic version of `kill-new', works with value of
`browse-kill-ring--ring-var'."
  (browse-kill-ring-with-original-buffer
   (if (eq browse-kill-ring--ring-var 'kill-ring)
     (kill-new string)
     (browse-kill-ring--insert-ring-value string))))

(defvar *browse-kill-ring-after-insertion-point* nil
  "Value of point after latest insertion.")


(defun browse-kill-ring-undo-tree-undo-other-window ()
  "Undo with `undo-tree-undo' the most recent change in the
other window's buffer. You most likely want to use this command
for undoing an insertion of yanked text from the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-with-original-buffer (undo-tree-undo)))

(defun browse-kill-ring-undo-tree-redo-other-window ()
  "Redo with `undo-tree-redo' the most recent change in the
other window's buffer. You most likely want to use this command
for undoing an insertion of yanked text from the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-with-original-buffer (undo-tree-redo)))


(defgroup browse-kill-ring nil
  "A package for browsing and inserting the items in `browse-kill-ring--ring-var'."
  :link '(url-link "http://freedom.cis.ohio-state.edu/~hurley/")
  :group 'convenience)

(defvar browse-kill-ring-display-styles
  '((separated . browse-kill-ring-insert-as-separated)
    (one-line . browse-kill-ring-insert-as-one-line)))

(defcustom browse-kill-ring-display-style 'separated
  "How to display the kill ring items.

If `one-line', then replace newlines with \"\\n\" for display.

If `separated', then display `browse-kill-ring-separator' between
entries."
  :type '(choice (const :tag "One line" one-line)
                 (const :tag "Separated" separated))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-quit-action 'save-and-restore
  "What action to take when `browse-kill-ring-quit' is called.

If `bury-buffer', then simply bury the *Kill Ring* buffer, but keep
the window.

If `bury-and-delete-window', then bury the buffer, and (if there is
more than one window) delete the window.

If `save-and-restore', then save the window configuration when
`browse-kill-ring' is called, and restore it at quit.  This is
the default.

If `kill-and-delete-window', then kill the *Kill Ring* buffer, and
delete the window on close.

Otherwise, it should be a function to call."
  :type '(choice (const :tag "Bury buffer" :value bury-buffer)
                 (const :tag "Delete window" :value delete-window)
                 (const :tag "Save and restore" :value save-and-restore)
                 (const :tag "Bury buffer and delete window" :value bury-and-delete-window)
                 (const :tag "Kill buffer and delete window" :value kill-and-delete-window)
                 function)
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-resize-window nil
  "Whether to resize the `browse-kill-ring' window to fit its contents.
Value is either t, meaning yes, or a cons pair of integers,
 (MAXIMUM . MINIMUM) for the size of the window.  MAXIMUM defaults to
the window size chosen by `pop-to-buffer'; MINIMUM defaults to
`window-min-height'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (cons (integer :tag "Maximum") (integer :tag "Minimum")))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-separator "-------"
  "The string separating entries in the `separated' style.
See `browse-kill-ring-display-style'."
  :type 'string
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-recenter nil
  "If non-nil, then always keep the current entry at the top of the window."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-highlight-current-entry nil
  "If non-nil, highlight the currently selected `browse-kill-ring--ring-var' entry."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-highlight-inserted-item browse-kill-ring-highlight-current-entry
  "If non-nil, temporarily highlight the inserted `browse-kill-ring--ring-var' entry."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-separator-face 'bold
  "The face in which to highlight the `browse-kill-ring-separator'."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-current-entry-face 'highlight
  "The face in which to highlight the browse kill current entry."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-inserted-item-face 'highlight
  "The face in which to highlight the inserted item."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-maximum-display-length nil
  "Whether or not to limit the length of displayed items.

If this variable is an integer, the display of `browse-kill-ring--ring-var' will be
limited to that many characters.
Setting this variable to nil means no limit."
  :type '(choice (const :tag "None" nil)
                 integer)
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-display-duplicates t
  "If non-nil, then display duplicate items in `browse-kill-ring--ring-var'."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-replace-yank t
  "If non-nil, browse-kill-ring will replace just-yanked items
when it inserts its own. That is, if you call `yank', and then
`browse-kill-ring', and then insert something via
`browse-kill-ring', the yanked text that you originally inserted
will be deleted. This makes browse-kill-ring behave more like
`yank-pop'.

This doesn't change the behavior of `yank-pop' or
`browse-kill-ring-default-keybindings'. Instead, for this to take
effect, you will have to bind a key to `browse-kill-ring'
directly."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-show-preview t
  "If non-nil, browse-kill-ring will show a preview of what the
buffer would look like if the item under point were inserted.

If you find the preview distracting, or something about your
setup leaves the preview in place after you're done with it, you
can disable it by setting this to nil."
  :type 'boolean
  :group 'browse-kill-ring)

(defvar browse-kill-ring-original-window-config nil
  "The window configuration to restore for `browse-kill-ring-quit'.")
(make-variable-buffer-local 'browse-kill-ring-original-window-config)

(defvar browse-kill-ring-original-window nil
  "The window in which chosen kill ring data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `browse-kill-ring' again.")

(defvar browse-kill-ring-original-buffer nil
  "The buffer in which chosen kill ring data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `browse-kill-ring' again.")

(defvar browse-kill-ring-original-buffer-position nil
  "Position in `browse-kill-ring-original-buffer'.")

(defvar browse-kill-ring-preview-overlay nil
  "The overlay used to preview what would happen if the user
  inserted the given text.")

(defvar browse-kill-ring-this-buffer-replace-yanked-text nil
  "Whether or not to replace yanked text before an insert.")

(defun browse-kill-ring-resize-window ()
  (when browse-kill-ring-resize-window
    (fit-window-to-buffer
     (selected-window)
     (car-safe browse-kill-ring-resize-window)
     (or (cdr-safe browse-kill-ring-resize-window)
         window-min-height))))

(defun browse-kill-ring-insert (&optional quit)
  "Insert the kill ring item at point into the last selected buffer.
If optional argument QUIT is non-nil, close the *Kill Ring* buffer as
well."
  (interactive "P")
  (browse-kill-ring-do-insert (current-buffer)
                              (point))
  (when quit
    (browse-kill-ring-quit)))

(defun browse-kill-ring-insert-and-delete (&optional quit)
  "Insert the kill ring item at point, and remove it from the kill ring.
If optional argument QUIT is non-nil, close the *Kill Ring* buffer as
well."
  (interactive "P")
  (browse-kill-ring-do-insert (current-buffer)
                              (point))
  (browse-kill-ring-delete)
  (when quit
    (browse-kill-ring-quit)))

(defun browse-kill-ring-insert-and-quit ()
  "Like `browse-kill-ring-insert', but close the *Kill Ring* buffer afterwards."
  (interactive)
  (browse-kill-ring-insert t))

(defun browse-kill-ring-insert-and-move (&optional quit)
  "Like `browse-kill-ring-insert', but move the entry to the front."
  (interactive "P")
  (let ((buf (current-buffer))
        (pt (point)))
    (browse-kill-ring-do-insert buf pt)
    (let ((str (browse-kill-ring-current-string buf pt)))
      (browse-kill-ring-delete)
      (browse-kill-ring--kill-new str)))
  (if quit
      (browse-kill-ring-quit)
    (browse-kill-ring-update)))

(defun browse-kill-ring-insert-move-and-quit ()
  "Like `browse-kill-ring-insert-and-move', but close the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-insert-and-move t))

(defun browse-kill-ring-insert-and-highlight (str)
  "Helper function to insert text at point, highlighting it if appropriate."
  (let ((before-insert (point)))
    (let (deactivate-mark)
      (insert-for-yank str))
    (setf *browse-kill-ring-after-insertion-point* (point))
    (when browse-kill-ring-highlight-inserted-item
      (let ((o (make-overlay before-insert (point))))
        (overlay-put o 'face browse-kill-ring-inserted-item-face)
        (sit-for 0.15)
        (delete-overlay o)))))

(defun browse-kill-ring-do-prepend-insert (buf pt)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (with-current-buffer browse-kill-ring-original-buffer
      (save-excursion
        (goto-char (point-min))
        (browse-kill-ring-insert-and-highlight str)))))

(defun browse-kill-ring-append-insert (&optional quit)
  "Like `browse-kill-ring-insert', but places the entry at the end of the
buffer as opposed to point."
  (interactive "P")
  (browse-kill-ring-do-append-insert (current-buffer)
                                     (point))
  (when quit
    (browse-kill-ring-quit)))

(defun browse-kill-ring-append-insert-and-quit ()
  "Like `browse-kill-ring-append-insert', but close the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-append-insert t))

(defun browse-kill-ring-append-insert-and-move (&optional quit)
  "Like `browse-kill-ring-append-insert', but move the entry to the front
of the *Kill Ring*."
  (interactive "P")
  (let ((buf (current-buffer))
        (pt (point)))
    (browse-kill-ring-do-append-insert buf pt)
    (let ((str (browse-kill-ring-current-string buf pt)))
      (browse-kill-ring-delete)
      (browse-kill-ring--kill-new str)))
  (if quit
      (browse-kill-ring-quit)
    (browse-kill-ring-update)))

(defun browse-kill-ring-append-insert-move-and-quit ()
  "Like `browse-kill-ring-append-insert-and-move', but close the
*Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-append-insert-and-move t))

(defun browse-kill-ring-do-append-insert (buf pt)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (with-current-buffer browse-kill-ring-original-buffer
      (save-excursion
        (goto-char (point-max))
        (browse-kill-ring-insert-and-highlight str)))))

(defun browse-kill-ring-delete ()
  "Remove the item at point from the `kill-ring'."
  (interactive)
  (forward-line 0)
  (let* ((over (browse-kill-ring-target-overlay-at (point)))
         (target (overlay-get over 'browse-kill-ring-target))
         (inhibit-read-only t))
    (unless (overlayp over)
      (error "No kill ring item here"))
    (delete-region (overlay-start over) (1+ (overlay-end over)))
    (browse-kill-ring--delete-ring-value target)
    (when (get-text-property (point) 'browse-kill-ring-extra)
      (let ((prev (previous-single-property-change (point) 'browse-kill-ring-extra))
            (next (next-single-property-change (point) 'browse-kill-ring-extra)))
        (when prev (cl-incf prev))
        (when next (cl-incf next))
        (delete-region (or prev (point-min)) (or next (point-max))))))
  (browse-kill-ring-resize-window)
  (browse-kill-ring-forward 0))

;; code from browse-kill-ring+.el
(defun browse-kill-ring-target-overlay-at (position)
  "Return overlay at POSITION that has property `browse-kill-ring-target'.
If no such overlay, raise an error."
  (let ((ovs  (overlays-at (point))))
    (catch 'browse-kill-ring-target-overlay-at
      (dolist (ov  ovs)
        (when (overlay-get ov 'browse-kill-ring-target)
          (throw 'browse-kill-ring-target-overlay-at ov)))
      (error "No selection-ring item here"))))

;; Helper function for browse-kill-ring-current-string, takes a list of
;; overlays and returns the string from the first overlay that has the
;; property. There might be more than just our overlay at this point.
(defun browse-kill-ring-current-string-1 (overs)
  (if overs
      (let ((str (overlay-get (car overs) 'browse-kill-ring-target)))
        (if str str (browse-kill-ring-current-string-1 (cdr overs))))
    nil))

;; Find the string to insert at the point by looking for the overlay.
(defun browse-kill-ring-current-string (buf pt &optional no-error)
  (or (browse-kill-ring-current-string-1 (overlays-at pt))
      (unless no-error
        (error "No kill ring item here"))))

(defun browse-kill-ring-do-insert (buf pt)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (with-current-buffer browse-kill-ring-original-buffer
      (when browse-kill-ring-this-buffer-replace-yanked-text
        (delete-region (mark) (point)))
      (when (and delete-selection-mode
                 (not buffer-read-only)
                 transient-mark-mode mark-active)
        (delete-active-region))
      (save-excursion
        (goto-char browse-kill-ring-original-buffer-position)
        (browse-kill-ring-insert-and-highlight str)))))

(defun browse-kill-ring-forward (&optional arg)
  "Move forward by ARG `kill-ring' entries."
  (interactive "p")
  (beginning-of-line)
  (let ((all-overlays (overlays-at (point))))
    (while (not (zerop arg))
      (if (< arg 0)
          (progn
            (cl-incf arg)
            (if all-overlays
                (progn
                  (goto-char (overlay-start (car all-overlays)))
                  (goto-char (previous-overlay-change (point)))
                  (goto-char (previous-overlay-change (point))))
              (progn
                (goto-char (1- (previous-overlay-change (point))))
                (unless (bobp)
                  (goto-char (overlay-start (car (overlays-at (point)))))))))
        (progn
          (cl-decf arg)
          (if all-overlays
              (progn
                (goto-char (overlay-end (car all-overlays)))
                (goto-char (next-overlay-change (point))))
            (goto-char (next-overlay-change (point)))
            (unless (eobp)
              (goto-char (overlay-start (car (overlays-at (point)))))))))))

  (let ((new-overlays (overlays-at (point))))
    ;; This could probably be implemented in a more intelligent manner.
    ;; Perhaps keep track over the overlay we started from?  That would
    ;; break when the user moved manually, though.
    (when (and browse-kill-ring-highlight-current-entry
               new-overlays)
      (let ((overs (overlay-lists))
            (current-overlay (car new-overlays)))
        (--map (overlay-put it 'face nil)
               (nconc (car overs) (cdr overs)))
        (overlay-put current-overlay 'face browse-kill-ring-current-entry-face)))
    (when browse-kill-ring-recenter
      (recenter 1))))

(defun browse-kill-ring-previous (&optional arg)
  "Move backward by ARG `browse-kill-ring--ring-var' entries."
  (interactive "p")
  (browse-kill-ring-forward (- arg)))

(defun browse-kill-ring-read-regexp (msg)
  (let* ((default (car regexp-history))
         (input
          (read-from-minibuffer
           (if default
               (format "%s for regexp (default `%s'): "
                       msg
                       default)
             (format "%s (regexp): " msg))
           nil
           nil
           nil
           'regexp-history)))
    (if (equal input "")
        default
      input)))


(defvar browse-kill-ring-last-search-regexp nil
  "Regexp used by previous search.")

(defvar browse-kill-ring-last-search-direction nil
  "Direction used by previous search. May be nil, 'forward or 'backward.")

(defun browse-kill-ring-search-repeat ()
  "Repeat last search command preserving direction, if there was one.
Similar to vim's search."
  (interactive)
  (if (and (not (null browse-kill-ring-last-search-regexp))
           (not (null browse-kill-ring-last-search-direction)))
    (progn
      (message "searching for %s %ss"
               browse-kill-ring-last-search-regexp
               browse-kill-ring-last-search-direction)
      (browse-kill-ring-search-forward
       browse-kill-ring-last-search-regexp
       (cond ((eq browse-kill-ring-last-search-direction 'forward) nil)
             ((eq browse-kill-ring-last-search-direction 'backward) t)
             (t (error "invalid browse-kill-ring-last-search-direction: %s"
                       browse-kill-ring-last-search-direction)))))
    (error "No previous search command")))

(defun browse-kill-ring-search-repeat-opposite-direction ()
  "Repeat last search command in opposite direction, if there was one.
Similar to vim's search."
  (interactive)
  (if (and (not (null browse-kill-ring-last-search-regexp))
           (not (null browse-kill-ring-last-search-direction)))
    (let ((orig-direction browse-kill-ring-last-search-direction))
      (unwind-protect
          (progn
            (setf browse-kill-ring-last-search-direction
                  (cond ((eq browse-kill-ring-last-search-direction 'forward)
                         'backward)
                        ((eq browse-kill-ring-last-search-direction 'backward)
                         'forward)
                        (t
                         (error "invalid browse-kill-ring-last-search-direction: %s"
                                browse-kill-ring-last-search-direction))))
            (browse-kill-ring-search-repeat))
        (setf browse-kill-ring-last-search-direction orig-direction)))
    (error "No previous search command")))

(defun browse-kill-ring-search-forward (regexp &optional backwards)
  "Move to the next `browse-kill-ring--ring-var' entry matching REGEXP from point.
If optional arg BACKWARDS is non-nil, move to the previous matching
entry."
  (interactive
   (list (browse-kill-ring-read-regexp "Search forward")
         current-prefix-arg))
  (setf browse-kill-ring-last-search-direction (if backwards 'backward 'forward)
        browse-kill-ring-last-search-regexp regexp)
  (let ((orig (point))
        (direction (if backwards -1 1)))
    (browse-kill-ring-forward direction)
    (let ((overs (overlays-at (point))))
      (while (and overs
                  (not (if backwards (bobp) (eobp)))
                  (not (string-match-p regexp
                                       (overlay-get (car overs)
                                                    'browse-kill-ring-target))))
        (browse-kill-ring-forward direction)
        (setq overs (overlays-at (point))))
      (unless (and overs
                   (string-match-p regexp
                                 (overlay-get (car overs)
                                              'browse-kill-ring-target)))
        (progn
          (goto-char orig)
          (message "No more `%s' entries matching %s" browse-kill-ring--ring-var regexp))))))

(defun browse-kill-ring-search-backward (regexp)
  "Move to the previous `browse-kill-ring--ring-var' entry matching REGEXP from point."
  (interactive
   (list (browse-kill-ring-read-regexp "Search backward")))
  (browse-kill-ring-search-forward regexp t))

(defun browse-kill-ring-quit ()
  "Take the action specified by `browse-kill-ring-quit-action'."
  (interactive)
  (when browse-kill-ring-preview-overlay
    (delete-overlay browse-kill-ring-preview-overlay))
  ;; clean after ourselves
  (setf browse-kill-ring--ring-var nil)
  (cl-case browse-kill-ring-quit-action
    (save-and-restore
      (if (< emacs-major-version 24)
        (let (buf (current-buffer))
             (set-window-configuration browse-kill-ring-original-window-config)
           (kill-buffer buf))
       (quit-window))
      (when *browse-kill-ring-after-insertion-point*
        (with-selected-window browse-kill-ring-original-window
          (with-current-buffer
              (window-buffer browse-kill-ring-original-window)
            (goto-char *browse-kill-ring-after-insertion-point*)
            (setf *browse-kill-ring-after-insertion-point* nil)))))
    (kill-and-delete-window
     (kill-buffer (current-buffer))
     (unless (= (count-windows) 1)
       (delete-window)))
    (bury-and-delete-window
     (bury-buffer)
     (unless (= (count-windows) 1)
       (delete-window)))
    (t
     (funcall browse-kill-ring-quit-action))))

(put 'browse-kill-ring-mode 'mode-class 'special)
(define-derived-mode browse-kill-ring-mode fundamental-mode
  "Kill Ring"
  "A major mode for browsing the `browse-kill-ring--ring-var'.
You most likely do not want to call `browse-kill-ring-mode' directly; use
`browse-kill-ring' instead.

\\{browse-kill-ring-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(nil t nil nil nil
             (font-lock-fontify-region-function . browse-kill-ring-fontify-region)))
  (define-key browse-kill-ring-mode-map (kbd "q") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "d") 'browse-kill-ring-delete)
  (define-key browse-kill-ring-mode-map (kbd "s") 'browse-kill-ring-search-forward)
  (define-key browse-kill-ring-mode-map (kbd "r") 'browse-kill-ring-search-backward)
  (define-key browse-kill-ring-mode-map (kbd "g") 'browse-kill-ring-update)
  (define-key browse-kill-ring-mode-map (kbd "l") 'browse-kill-ring-occur)
  (define-key browse-kill-ring-mode-map (kbd "e") 'browse-kill-ring-edit)
  (define-key browse-kill-ring-mode-map (kbd "n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "p") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map (kbd "?") 'describe-mode)
  (define-key browse-kill-ring-mode-map (kbd "h") 'describe-mode)
  (define-key browse-kill-ring-mode-map (kbd "y") 'browse-kill-ring-insert)
  (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-move-and-quit)
  (define-key browse-kill-ring-mode-map (kbd "i") 'browse-kill-ring-insert)
  (define-key browse-kill-ring-mode-map (kbd "o") 'browse-kill-ring-insert-and-move)
  (define-key browse-kill-ring-mode-map (kbd "x") 'browse-kill-ring-insert-and-delete)
  (define-key browse-kill-ring-mode-map (kbd "u") 'browse-kill-ring-insert-and-quit)
  (define-key browse-kill-ring-mode-map (kbd "a") 'browse-kill-ring-append-insert))

(define-derived-mode browse-kill-ring-edit-mode fundamental-mode
  "Kill Ring Edit"
  "A major mode for editing a `browse-kill-ring--ring-var' entry.
You most likely do not want to call `browse-kill-ring-edit-mode'
directly; use `browse-kill-ring' instead.

\\{browse-kill-ring-edit-mode-map}"
  (define-key browse-kill-ring-edit-mode-map (kbd "C-c C-c")
    'browse-kill-ring-edit-finish))

(defvar browse-kill-ring-edit-target nil)
(make-variable-buffer-local 'browse-kill-ring-edit-target)

(defun browse-kill-ring-edit ()
  "Edit the `browse-kill-ring--ring-var' entry at point."
  (interactive)
  (let ((overs (overlays-at (point))))
    (unless overs
      (error "No kill ring entry here"))
    (let* ((target (overlay-get (car overs)
                                'browse-kill-ring-target))
           (target-cell
            (generic/member target
                            (browse-kill-ring--get-ring-value))))
      (unless target-cell
        (error "Item deleted from the %s" browse-kill-ring--ring-var))
      (switch-to-buffer (get-buffer-create "*Kill Ring Edit*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert target)
      (goto-char (point-min))
      (browse-kill-ring-resize-window)
      (browse-kill-ring-edit-mode)
      (message "%s"
               (substitute-command-keys
                "Use \\[browse-kill-ring-edit-finish] to finish editing."))
      (setq browse-kill-ring-edit-target target-cell))))

(defun browse-kill-ring-edit-finish ()
  "Commit the changes to the `browse-kill-ring--ring-var'."
  (interactive)
  (if browse-kill-ring-edit-target
    (setcar browse-kill-ring-edit-target (buffer-string))
    (when (y-or-n-p "The item has been deleted; add to front? ")
      (browse-kill-ring--insert-ring-value (buffer-string))))
  (bury-buffer)
  ;; The user might have rearranged the windows
  (when (eq major-mode 'browse-kill-ring-mode)
    (browse-kill-ring-setup (current-buffer)
                            browse-kill-ring-original-buffer
                            browse-kill-ring-original-window
                            nil
                            browse-kill-ring-original-window-config)
    (browse-kill-ring-resize-window)))

(defmacro browse-kill-ring-add-overlays-for (item &rest body)
  (declare (indent 1))
  (let ((beg '#:begin)
        (end '#:end))
    `(let ((,beg (point))
           (,end (progn ,@body (point))))
       (let ((o (make-overlay ,beg ,end)))
         (overlay-put o 'browse-kill-ring-target ,item)
         (overlay-put o 'mouse-face 'highlight)))))

(defun browse-kill-ring-elide (str)
  (if (and browse-kill-ring-maximum-display-length
           (> (length str)
              browse-kill-ring-maximum-display-length))
      (concat (substring str 0 (- browse-kill-ring-maximum-display-length 3))
              (propertize "..." 'browse-kill-ring-extra t))
    str))

(defun browse-kill-ring-insert-as-one-line (items)
  (dolist (item items)
    (browse-kill-ring-add-overlays-for item
      (let* ((item (browse-kill-ring-elide item))
             (len (length item))
             (start 0)
             (newl (propertize "\\n" 'browse-kill-ring-extra t)))
        (while (and (< start len)
                    (string-match "\n" item start))
          (insert (substring item start (match-beginning 0))
                  newl)
          (setq start (match-end 0)))
        (insert (substring item start len))))
    (insert "\n")))

(defun browse-kill-ring-insert-as-separated (items)
  (let ((separator (propertize browse-kill-ring-separator
                               'browse-kill-ring-extra t
                               'browse-kill-ring-separator t))
        (tmp (cdr items)))
    (while tmp
      (browse-kill-ring-insert-as-separated-1 (car items) separator)
      (setq items tmp
            tmp (cdr items)))
    (when items
      (browse-kill-ring-insert-as-separated-1 (car items) nil))))

(defun browse-kill-ring-insert-as-separated-1 (origitem separator)
  (let* ((item (browse-kill-ring-elide origitem))
         (len (length item)))
    (browse-kill-ring-add-overlays-for origitem
                                       (insert item))
    ;; When the kill-ring has items with read-only text property at
    ;; **the end of** string, browse-kill-ring-setup fails with error
    ;; `Text is read-only'.  So inhibit-read-only here.
    ;; See http://bugs.debian.org/225082
    ;; - INOUE Hiroyuki <dombly@kc4.so-net.ne.jp>
    (let ((inhibit-read-only t))
      (insert "\n")
      (when separator
        (insert separator)
        (insert "\n")))))

(defun browse-kill-ring-occur (regexp)
  "Display all `browse-kill-ring--ring-var' entries matching REGEXP."
  (interactive
   (list
    (browse-kill-ring-read-regexp "Display kill ring entries matching")))
  (cl-assert (eq major-mode 'browse-kill-ring-mode))
  (browse-kill-ring-setup (current-buffer)
                          browse-kill-ring-original-buffer
                          browse-kill-ring-original-window
                          regexp)
  (browse-kill-ring-resize-window))

(defun browse-kill-ring-fontify-on-property (prop face beg end)
  (save-excursion
    (goto-char beg)
    (let ((prop-end nil))
      (while
          (setq prop-end
                (let ((prop-beg (or (and (get-text-property (point) prop) (point))
                                    (next-single-property-change (point) prop nil end))))
                  (when (and prop-beg (not (= prop-beg end)))
                    (let ((prop-end (next-single-property-change prop-beg prop nil end)))
                      (when (and prop-end (not (= prop-end end)))
                        (put-text-property prop-beg prop-end 'face face)
                        prop-end)))))
        (goto-char prop-end)))))

(defun browse-kill-ring-fontify-region (beg end &optional verbose)
  (when verbose (message "Fontifying..."))
  (let ((inhibit-read-only t))
    (browse-kill-ring-fontify-on-property 'browse-kill-ring-extra 'bold beg end)
    (browse-kill-ring-fontify-on-property 'browse-kill-ring-separator
                                          browse-kill-ring-separator-face beg end))
  (when verbose (message "Fontifying...done")))

(defun browse-kill-ring-update ()
  "Update the buffer to reflect outside changes to `browse-kill-ring--ring-var'."
  (interactive)
  (cl-assert (eq major-mode 'browse-kill-ring-mode))
  (browse-kill-ring-setup (current-buffer)
                          browse-kill-ring-original-buffer
                          browse-kill-ring-original-window)
  (browse-kill-ring-resize-window))

(defun browse-kill-ring-preview-update (&optional pt)
  "Update `browse-kill-ring-preview-overlay' to show the
  current text as if it were inserted."
  (let* ((new-text (browse-kill-ring-current-string
                    (current-buffer) (or pt (point)) t))
         ;; If new-text is nil, replacement should be nil too.
         (replacement (when new-text
                        (propertize new-text 'face 'highlight))))
    (overlay-put browse-kill-ring-preview-overlay
                 'before-string replacement)))

(defun browse-kill-ring-setup (kill-buf orig-buf window
                                        &optional regexp window-config)
  (setq browse-kill-ring-this-buffer-replace-yanked-text
        (and
         browse-kill-ring-replace-yank
         (eq last-command 'yank)))
  (with-current-buffer orig-buf
    (let* ((will-replace
            (or browse-kill-ring-this-buffer-replace-yanked-text
                (region-active-p)))
           (start (if will-replace
                    (min (point) (mark))
                    (point)))
           (end (if will-replace
                  (max (point) (mark))
                  (point))))
      (when browse-kill-ring-show-preview
        (when browse-kill-ring-preview-overlay
          (delete-overlay browse-kill-ring-preview-overlay))
        (setq browse-kill-ring-preview-overlay
              (make-overlay start end orig-buf)))))
  (overlay-put browse-kill-ring-preview-overlay
               'invisible t)
  (with-current-buffer kill-buf
    (let ((inhibit-read-only t))
      (browse-kill-ring-mode)
      (when (eq browse-kill-ring-display-style 'one-line)
        (setq truncate-lines t))
      (erase-buffer)
      (setq browse-kill-ring-original-buffer orig-buf
            browse-kill-ring-original-buffer-position
            (with-current-buffer orig-buf
              (point))
            browse-kill-ring-original-window window
            browse-kill-ring-original-window-config
            (or window-config
                (current-window-configuration)))
      (let ((browse-kill-ring-maximum-display-length
             (if (and browse-kill-ring-maximum-display-length
                      (<= browse-kill-ring-maximum-display-length 3))
               4
               browse-kill-ring-maximum-display-length))
            (items (-map
                    (if browse-kill-ring-depropertize
                      #'substring-no-properties
                      #'copy-sequence)
                    (browse-kill-ring--to-list
                     (browse-kill-ring--get-ring-value)))))
        (when (stringp regexp)
          (setq items (--filter (string-match-p regexp it) items)))
        (when (not browse-kill-ring-display-duplicates)
          (setq items (remove-duplicates-hashing items #'equal)))
        (funcall (or (cdr (assq browse-kill-ring-display-style
                                browse-kill-ring-display-styles))
                     (error "Invalid `browse-kill-ring-display-style': %s"
                            browse-kill-ring-display-style))
                 items)
        (browse-kill-ring-preview-update (point-min))
        ;; Local post-command-hook, only happens in the *Kill
        ;; Ring* buffer
        (add-hook 'post-command-hook 'browse-kill-ring-preview-update nil t)
        ;; Code from Michael Slass <mikesl@wrq.com>
        (message
         (let ((len (generic/length (browse-kill-ring--get-ring-value)))
               (entry (if (= 1 (length kill-ring)) "entry" "entries")))
           (concat
            (if (and (not regexp)
                     browse-kill-ring-display-duplicates)
              (format "%s %s in the %s."
                      len entry browse-kill-ring--ring-var)
              (format "%s (of %s) %s in the kill ring shown."
                      (length items) len entry browse-kill-ring--ring-var))
            (substitute-command-keys
             (concat "    Type \\[browse-kill-ring-quit] to quit.  "
                     "\\[describe-mode] for help.")))))
        ;; End code from Michael Slass <mikesl@wrq.com>
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (browse-kill-ring-forward 0)
        (when regexp
          (setq mode-name (concat "Kill Ring [" regexp "]")))
        (run-hooks 'browse-kill-ring-hook)))))

(provide 'browse-kill-ring)

;;; browse-kill-ring.el ends here
