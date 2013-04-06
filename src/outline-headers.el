;; outline-headers.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  9 May 2012
;; Description:


(defvar-local +outline-headers-min-header-length+ nil
  "Minimum number of `+outline-headers-section-symbol+''s allowed in header.")

(defvar-local +outline-headers-max-header-length+ nil
  "Maximum number of `+outline-headers-section-symbol+''s allowed in header.")


(defvar-local +outline-headers-section-start+
  "^"
  "Beginning part of `+outline-headers-header-re+'.")

(defvar-local +outline-headers-section-symbol+
  nil
  "Main part of `+outline-headers-header-re+' that defines headers of different length.")

(defvar-local +outline-headers-section-end+
  "\\(?: \\|$\\)"
  "End part of `+outline-headers-header-re+'.")



(defvar-local +outline-headers-header-re+
  nil
  "Regular expression that defines headers")

;;;;; original outline-headers

(defun outline-headers-count-header-symbols ()
  "Return number of `+outline-headers-section-symbol+' symbols in header
at point."
  (assert (looking-at-p +outline-headers-header-re+))
  (save-excursion
    (save-match-data
      (if (looking-at +outline-headers-section-start+)
        (let ((match-length 0))
          (goto-char (match-end 0))
          (while (and (looking-at-p +outline-headers-section-symbol+)
                      (not (looking-at-p +outline-headers-section-end+)))
            (forward-char 1)
            (incf match-length))
          match-length)
        0))))

(defun outline-headers-outline-level ()
  "Calculate header nesting level."
  (max (- +outline-headers-max-header-length+
          (outline-headers-count-header-symbols))
       0))

(defun outline-headers-hide-all (&optional count)
  "Find out maximum length of buffer headings and hide all those toplevel
headings."
  (interactive "P")
  (save-excursion
    (save-match-data
      (let (header-re)
        (unless count
          (setf count +outline-headers-min-header-length+)
          (goto-char (point-min))
          (while (re-search-forward +outline-headers-header-re+ nil t)
            (goto-char (match-beginning 0))
            (setf count (max count
                             (outline-headers-count-header-symbols)))
            (goto-char (match-end 0))))
        (setf header-re
              (format "%s%s\\{%d\\}%s"
                      +outline-headers-section-start+
                      +outline-headers-section-symbol+
                      count
                      +outline-headers-section-end+))
        (goto-char (point-min))
        (while (re-search-forward header-re nil t)
          (when (and hs-minor-mode ;; do check only if hideshow enabled
                     (hs-already-hidden-p))
            ;; if we're in hideshow-hidden block then show it
            (save-excursion
              ;; since hs-show-block repositions point to the beginning of
              ;; the block we need to surround it with save-excursion in order
              ;; to retain our position of outline heading matched by header-re
              (hs-show-block)))
          (&&hdr-hide-subtree)
          (forward-line 1)
          (beginning-of-line))))))


;;;;; setup function


(defun* setup-outline-headers (&key
                               (header-start "^")
                               (header-symbol nil)
                               (header-end "\\(?: \\|$\\)")
                               (length-min 3)
                               (length-max 10))
  (unless header-symbol
    (setf header-symbol
          (assoc 'one-line
                 (assoc major-mode
                        +comment-util-comment-format-alist+)))
    (when (< 1 (length header-symbol))
      (error "setup-outline-headers: error: fetched header-symbol from comment-util but it's length is greater than 1: \"%s\" and no other header-symbol was provided" header-symbol)))
  (assert (and (string? header-symbol)
               (= 1 (length header-symbol)))
          nil
          "header-symbol must be string of length 1")
  (assert (string? header-start)
          nil
          "header-start must me a string")
  (assert (string? header-end)
          nil
          "header-end must me a string")
  (assert (and (integer? length-min)
               (>= length-min 1))
          nil
          "length-min must be integer >= 1")
  (assert (and (integer? length-max)
               (>= length-max length-min))
          nil
          "length-max must be integer >= length-min")

  (setf +outline-headers-section-start+     header-start
        +outline-headers-section-symbol+    (regexp-quote header-symbol)
        +outline-headers-section-end+       header-end
        +outline-headers-min-header-length+ length-min
        +outline-headers-max-header-length+ length-max)

  (setf +outline-headers-header-re+
        (format "%s%s\\{%d,%d\\}%s"
                +outline-headers-section-start+
                +outline-headers-section-symbol+
                +outline-headers-min-header-length+
                +outline-headers-max-header-length+
                +outline-headers-section-end+))

  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))

  (setq-local &&hdr-outline-regexp
              +outline-headers-header-re+)
  (setq-local &&hdr-outline-heading-end-regexp
              (concat "\\(?:"
                      +outline-headers-header-re+
                      ".*?"
                      "\\(?:\\\\\n.*\\)?"
                      "\n"
                      "\\)+"))

  (setq-local &&hdr-outline-level #'outline-headers-outline-level)
  (&&hdr-outline-minor-mode)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("z F" outline-headers-hide-all)
    ("z f" &&hdr-hide-subtree)
    ("z U" &&hdr-show-all)
    ("z u" &&hdr-show-subtree)))

;;;;; actual outline headers copied from emacs with names mangled
;;;;; using prefix &&hdr-
;;;;; do :%s/&&hdr-//g and original outline.el will show up
;;;;;
;;;;; I want to make this clear: this is horrible and shameless copypaste
;;;;; that should have never existed in the firts place, it has gazillion
;;;;; drawbacks but one - it does it's job for haskell mode and will not
;;;;; clash with original outline.el
;;;;;
;;;;; Emacs already has 41k symbols defined and this is just another nail
;;;;; to it's coffin...

;;; &&hdr-outline.el --- &&hdr-outline mode commands for Emacs

;; Copyright (C) 1986, 1993-1995, 1997, 2000-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: &&hdr-outlines

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a major mode for editing &&hdr-outline-format documents.
;; An &&hdr-outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Todo:

;; - subtree-terminators
;; - better handle comments before function bodies (i.e. heading)
;; - don't bother hiding whitespace

;;; Code:


(defgroup &&hdr-outlines nil
  "Support for hierarchical outlining."
  :prefix "&&hdr-outline-"
  :group 'wp)

(defcustom &&hdr-outline-regexp "[*\^L]+"
  "Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
Note that Outline mode only checks this regexp at the start of a line,
so the regexp need not (and usually does not) start with `^'.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also `&&hdr-outline-heading-end-regexp'."
  :type 'regexp
  :group '&&hdr-outlines)
;;;###autoload(put '&&hdr-outline-regexp 'safe-local-variable 'stringp)

(defcustom &&hdr-outline-heading-end-regexp "\n"
  "Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.
The recommended way to set this is with a `Local Variables:' list
in the file it applies to."
  :type 'regexp
  :group '&&hdr-outlines)
;;;###autoload(put '&&hdr-outline-heading-end-regexp 'safe-local-variable 'stringp)

(defvar &&hdr-outline-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "@" '&&hdr-outline-mark-subtree)
    (define-key map "\C-n" '&&hdr-outline-next-visible-heading)
    (define-key map "\C-p" '&&hdr-outline-previous-visible-heading)
    (define-key map "\C-i" '&&hdr-gshow-children)
    (define-key map "\C-s" '&&hdr-show-subtree)
    (define-key map "\C-d" '&&hdr-hide-subtree)
    (define-key map "\C-u" '&&hdr-outline-up-heading)
    (define-key map "\C-f" '&&hdr-outline-forward-same-level)
    (define-key map "\C-b" '&&hdr-outline-backward-same-level)
    (define-key map "\C-t" '&&hdr-hide-body)
    (define-key map "\C-a" '&&hdr-show-all)
    (define-key map "\C-c" '&&hdr-hide-entry)
    (define-key map "\C-e" '&&hdr-show-entry)
    (define-key map "\C-l" '&&hdr-hide-leaves)
    (define-key map "\C-k" '&&hdr-show-branches)
    (define-key map "\C-q" '&&hdr-hide-sublevels)
    (define-key map "\C-o" '&&hdr-hide-other)
    (define-key map "\C-^" '&&hdr-outline-move-subtree-up)
    (define-key map "\C-v" '&&hdr-outline-move-subtree-down)
    (define-key map [(control ?<)] '&&hdr-outline-promote)
    (define-key map [(control ?>)] '&&hdr-outline-demote)
    (define-key map "\C-m" '&&hdr-outline-insert-heading)
    ;; Where to bind &&hdr-outline-cycle ?
    map))

(defvar &&hdr-outline-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))

    (define-key map [hide] (cons "Hide" (make-sparse-keymap "Hide")))

    (define-key map [hide &&hdr-hide-other]
      '(menu-item "Hide Other" &&hdr-hide-other
                  :help "Hide everything except current body and parent and top-level headings"))
    (define-key map [hide &&hdr-hide-sublevels]
      '(menu-item "Hide Sublevels" &&hdr-hide-sublevels
                  :help "Hide everything but the top LEVELS levels of headers, in whole buffer"))
    (define-key map [hide &&hdr-hide-subtree]
      '(menu-item "Hide Subtree" &&hdr-hide-subtree
                  :help "Hide everything after this heading at deeper levels"))
    (define-key map [hide &&hdr-hide-entry]
      '(menu-item "Hide Entry" &&hdr-hide-entry
                  :help "Hide the body directly following this heading"))
    (define-key map [hide &&hdr-hide-body]
      '(menu-item "Hide Body" &&hdr-hide-body
                  :help "Hide all body lines in buffer, leaving all headings visible"))
    (define-key map [hide &&hdr-hide-leaves]
      '(menu-item "Hide Leaves" &&hdr-hide-leaves
                  :help "Hide the body after this heading and at deeper levels"))

    (define-key map [show] (cons "Show" (make-sparse-keymap "Show")))

    (define-key map [show &&hdr-show-subtree]
      '(menu-item "Show Subtree" &&hdr-show-subtree
                  :help "Show everything after this heading at deeper levels"))
    (define-key map [show &&hdr-gshow-children]
      '(menu-item "Show Children" &&hdr-gshow-children
                  :help "Show all direct subheadings of this heading"))
    (define-key map [show &&hdr-show-branches]
      '(menu-item "Show Branches" &&hdr-show-branches
                  :help "Show all subheadings of this heading, but not their bodies"))
    (define-key map [show &&hdr-show-entry]
      '(menu-item "Show Entry" &&hdr-show-entry
                  :help "Show the body directly following this heading"))
    (define-key map [show &&hdr-show-all]
      '(menu-item "Show All" &&hdr-show-all
                  :help "Show all of the text in the buffer"))

    (define-key map [headings]
      (cons "Headings" (make-sparse-keymap "Headings")))

    (define-key map [headings demote-subtree]
      '(menu-item "Demote Subtree" &&hdr-outline-demote
                  :help "Demote headings lower down the tree"))
    (define-key map [headings promote-subtree]
      '(menu-item "Promote Subtree" &&hdr-outline-promote
                  :help "Promote headings higher up the tree"))
    (define-key map [headings move-subtree-down]
      '(menu-item "Move Subtree Down" &&hdr-outline-move-subtree-down
                  :help "Move the current subtree down past arg headlines of the same level"))
    (define-key map [headings move-subtree-up]
      '(menu-item "Move Subtree Up" &&hdr-outline-move-subtree-up
                  :help "Move the current subtree up past arg headlines of the same level"))
    (define-key map [headings copy]
      '(menu-item "Copy to Kill Ring" &&hdr-outline-headers-as-kill
                  :enable mark-active
                  :help "Save the visible &&hdr-outline headers in region at the start of the kill ring"))
    (define-key map [headings &&hdr-outline-insert-heading]
      '(menu-item "New Heading" &&hdr-outline-insert-heading
                  :help "Insert a new heading at same depth at point"))
    (define-key map [headings &&hdr-outline-backward-same-level]

      '(menu-item "Previous Same Level" &&hdr-outline-backward-same-level
                  :help "Move backward to the arg'th subheading at same level as this one."))
    (define-key map [headings &&hdr-outline-forward-same-level]

      '(menu-item "Next Same Level" &&hdr-outline-forward-same-level
                  :help "Move forward to the arg'th subheading at same level as this one"))
    (define-key map [headings &&hdr-outline-previous-visible-heading]

      '(menu-item "Previous" &&hdr-outline-previous-visible-heading
                  :help "Move to the previous heading line"))
    (define-key map [headings &&hdr-outline-next-visible-heading]

      '(menu-item "Next" &&hdr-outline-next-visible-heading
                  :help "Move to the next visible heading line"))
    (define-key map [headings &&hdr-outline-up-heading]

      '(menu-item "Up" &&hdr-outline-up-heading
                  :help "Move to the visible heading line of which the present line is a subheading"))
    map))

(defvar &&hdr-outline-minor-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [&&hdr-outline]
      (cons "Outline"
            (nconc (make-sparse-keymap "Outline")
                   ;; Remove extra separator
                   (cdr
                    ;; Flatten the major mode's menus into a single menu.
                    (apply 'append
                           (map (lambda (x)
                                  (if (consp x)
                                    ;; Add a separator between each
                                    ;; part of the unified menu.
                                    (cons '(--- "---") (cdr x))))
                                &&hdr-outline-mode-menu-bar-map))))))
    map))


(defvar &&hdr-outline-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defvar &&hdr-outline-font-lock-keywords
  '( ;;
    ;; Highlight headings according to the level.
    (eval . (list (concat "^\\(?:" &&hdr-outline-regexp "\\).+")
                  0 '(&&hdr-outline-font-lock-face) nil t)))
  "Additional expressions to highlight in Outline mode.")

(defface &&hdr-outline-1
  '((t :inherit 'outline-1))
  "Level 1."
  :group '&&hdr-outlines)

(defface &&hdr-outline-2
  '((t :inherit 'outline-2))
  "Level 2."
  :group '&&hdr-outlines)

(defface &&hdr-outline-3
  '((t :inherit 'outline-3))
  "Level 3."
  :group '&&hdr-outlines)

(defface &&hdr-outline-4
  '((t :inherit 'outline-4))
  "Level 4."
  :group '&&hdr-outlines)

(defface &&hdr-outline-5
  '((t :inherit 'outline-5))
  "Level 5."
  :group '&&hdr-outlines)

(defface &&hdr-outline-6
  '((t :inherit 'outline-6))
  "Level 6."
  :group '&&hdr-outlines)

(defface &&hdr-outline-7
  '((t :inherit 'outline-7))
  "Level 7."
  :group '&&hdr-outlines)

(defface &&hdr-outline-8
  '((t :inherit 'outline-8))
  "Level 8."
  :group '&&hdr-outlines)

(defvar &&hdr-outline-font-lock-faces
  [&&hdr-outline-1 &&hdr-outline-2 &&hdr-outline-3 &&hdr-outline-4
                   &&hdr-outline-5 &&hdr-outline-6 &&hdr-outline-7 &&hdr-outline-8])

;; (defvar &&hdr-outline-font-lock-levels nil)
;; (make-variable-buffer-local '&&hdr-outline-font-lock-levels)

(defun &&hdr-outline-font-lock-face ()
  ;; (save-excursion
  ;;   (&&hdr-outline-back-to-heading t)
  ;;   (let* ((count 0)
  ;;       (start-level (funcall &&hdr-outline-level))
  ;;       (level start-level)
  ;;       face-level)
  ;;     (while (not (setq face-level
  ;;                    (if (or (bobp) (eq level 1)) 0
  ;;                      (cdr (assq level &&hdr-outline-font-lock-levels)))))
  ;;    (&&hdr-outline-up-heading 1 t)
  ;;    (setq count (1+ count))
  ;;    (setq level (funcall &&hdr-outline-level)))
  ;;     ;; Remember for later.
  ;;     (unless (zerop count)
  ;;    (setq face-level (+ face-level count))
  ;;    (push (cons start-level face-level) &&hdr-outline-font-lock-levels))
  ;;     (condition-case nil
  ;;      (aref &&hdr-outline-font-lock-faces face-level)
  ;;    (error font-lock-warning-face))))
  (save-excursion
    (goto-char (match-beginning 0))
    (looking-at &&hdr-outline-regexp)
    (aref &&hdr-outline-font-lock-faces (% (1- (funcall &&hdr-outline-level)) (length &&hdr-outline-font-lock-faces)))))

(defvar &&hdr-outline-view-change-hook nil
  "Normal hook to be run after &&hdr-outline visibility changes.")

(defvar &&hdr-outline-mode-hook nil
  "*This hook is run when &&hdr-outline mode starts.")

(defvar &&hdr-outline-blank-line nil
  "*Non-nil means to leave unhidden blank line before heading.")

;;;###autoload
(define-derived-mode &&hdr-outline-mode text-mode "HOutline"
  "Set major mode for editing &&hdr-outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<&&hdr-outline-mode-map>
\\[&&hdr-outline-next-visible-heading]   &&hdr-outline-next-visible-heading      move by visible headings
\\[&&hdr-outline-previous-visible-heading]   &&hdr-outline-previous-visible-heading
\\[&&hdr-outline-forward-same-level]   &&hdr-outline-forward-same-level        similar but skip subheadings
\\[&&hdr-outline-backward-same-level]   &&hdr-outline-backward-same-level
\\[&&hdr-outline-up-heading]   &&hdr-outline-up-heading                 move from subheading to heading

\\[&&hdr-hide-body]   make all text invisible (not headings).
\\[&&hdr-show-all]    make everything in buffer visible.
\\[&&hdr-hide-sublevels]  make only the first N levels of headers visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[&&hdr-hide-subtree]   &&hdr-hide-subtree make body and subheadings invisible.
\\[&&hdr-show-subtree]   &&hdr-show-subtree make body and subheadings visible.
\\[&&hdr-gshow-children]   &&hdr-gshow-children       make direct subheadings visible.
                 No effect on body, or subheadings 2 or more levels down.
                 With arg N, affects subheadings N levels down.
\\[&&hdr-hide-entry]     make immediately following body invisible.
\\[&&hdr-show-entry]     make it visible.
\\[&&hdr-hide-leaves]    make body under heading and under its subheadings invisible.
                     The subheadings remain visible.
\\[&&hdr-show-branches]  make all subheadings at all levels visible.

The variable `&&hdr-outline-regexp' can be changed to control what is a heading.
A line is a heading if `&&hdr-outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on &&hdr-outline mode calls the value of `text-mode-hook' and then of
`&&hdr-outline-mode-hook', if they are non-nil."
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(&&hdr-outline . t))
  (set (make-local-variable 'paragraph-start)
       (concat paragraph-start "\\|\\(?:" &&hdr-outline-regexp "\\)"))
  ;; Inhibit auto-filling of header lines.
  (set (make-local-variable 'auto-fill-inhibit-regexp) &&hdr-outline-regexp)
  (set (make-local-variable 'paragraph-separate)
       (concat paragraph-separate "\\|\\(?:" &&hdr-outline-regexp "\\)"))
  (set (make-local-variable 'font-lock-defaults)
       '(&&hdr-outline-font-lock-keywords t nil nil backward-paragraph))
  (setq imenu-generic-expression
        (list (list nil (concat "^\\(?:" &&hdr-outline-regexp "\\).*$") 0)))
  (add-hook 'change-major-mode-hook '&&hdr-show-all nil t))

(defcustom &&hdr-outline-minor-mode-prefix "\C-c@"
  "Prefix key to use for Outline commands in Outline minor mode.
The value of this variable is checked as part of loading Outline mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string
  :group '&&hdr-outlines)

;;;###autoload
(define-minor-mode &&hdr-outline-minor-mode
  "Toggle Outline minor mode.
With a prefix argument ARG, enable Outline minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

See the command `&&hdr-outline-mode' for more information on this mode."
  nil " Outl" (list (cons [menu-bar] &&hdr-outline-minor-mode-menu-bar-map)
                    (cons &&hdr-outline-minor-mode-prefix &&hdr-outline-mode-prefix-map))
  :group '&&hdr-outlines
  (if &&hdr-outline-minor-mode
    (progn
      ;; Turn off this mode if we change major modes.
      (add-hook 'change-major-mode-hook
                (lambda () (&&hdr-outline-minor-mode -1))
                nil t)
      (set (make-local-variable 'line-move-ignore-invisible) t)
      ;; Cause use of ellipses for invisible text.
      (add-to-invisibility-spec '(&&hdr-outline . t)))
    (setq line-move-ignore-invisible nil)
    ;; Cause use of ellipses for invisible text.
    (remove-from-invisibility-spec '(&&hdr-outline . t))
    ;; When turning off &&hdr-outline mode, get rid of any &&hdr-outline hiding.
    (&&hdr-show-all)))

(defvar &&hdr-outline-level '&&hdr-outline-level
  "*Function of no args to compute a header's nesting level in an &&hdr-outline.
It can assume point is at the beginning of a header line and that the match
data reflects the `&&hdr-outline-regexp'.")
;;;###autoload(put '&&hdr-outline-level 'risky-local-variable t)

(defvar &&hdr-outline-heading-alist ()
  "Alist associating a heading for every possible level.
Each entry is of the form (HEADING . LEVEL).
This alist is used two ways: to find the heading corresponding to
a given level and to find the level of a given heading.
If a mode or document needs several sets of &&hdr-outline headings (for example
numbered and unnumbered sections), list them set by set and sorted by level
within each set.  For example in texinfo mode:

     (setq &&hdr-outline-heading-alist
      '((\"@chapter\" . 2) (\"@section\" . 3) (\"@subsection\" . 4)
           (\"@subsubsection\" . 5)
        (\"@unnumbered\" . 2) (\"@unnumberedsec\" . 3)
           (\"@unnumberedsubsec\" . 4)  (\"@unnumberedsubsubsec\" . 5)
        (\"@appendix\" . 2) (\"@appendixsec\" . 3)...
           (\"@appendixsubsec\" . 4) (\"@appendixsubsubsec\" . 5) ..))

Instead of sorting the entries in each set, you can also separate the
sets with nil.")
(make-variable-buffer-local '&&hdr-outline-heading-alist)

;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the &&hdr-outline-level variable
;; as appropriate.
(defun &&hdr-outline-level ()
  "Return the depth to which a statement is nested in the &&hdr-outline.
Point must be at the beginning of a header line.
This is actually either the level specified in `&&hdr-outline-heading-alist'
or else the number of characters matched by `&&hdr-outline-regexp'."
  (or (cdr (assoc (match-string 0) &&hdr-outline-heading-alist))
      (- (match-end 0) (match-beginning 0))))

(defun &&hdr-outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (if (re-search-forward (concat "\n\\(?:" &&hdr-outline-regexp "\\)")
                         nil 'move)
    (goto-char (match-beginning 0)))
  (if (and (bolp) (or &&hdr-outline-blank-line (eobp)) (not (bobp)))
    (forward-char -1)))

(defun &&hdr-outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  ;; Make sure we don't match the heading we're at.
  (if (and (bolp) (not (eobp))) (forward-char 1))
  (if (re-search-forward (concat "^\\(?:" &&hdr-outline-regexp "\\)")
                         nil 'move)
    (goto-char (match-beginning 0))))

(defun &&hdr-outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (re-search-backward (concat "^\\(?:" &&hdr-outline-regexp "\\)")
                      nil 'move))

(defsubst &&hdr-outline-invisible-p (&optional pos)
  "Non-nil if the character after point is invisible."
  (get-char-property (or pos (point)) 'invisible))

(defun &&hdr-outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (&&hdr-outline-on-heading-p invisible-ok)
      (let (found)
        (save-excursion
          (while (not found)
            (or (re-search-backward (concat "^\\(?:" &&hdr-outline-regexp "\\)")
                                    nil t)
                (error "before first heading"))
            (setq found (and (or invisible-ok (not (&&hdr-outline-invisible-p)))
                             (point)))))
        (goto-char found)
        found)))

(defun &&hdr-outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (not (&&hdr-outline-invisible-p)))
         (looking-at &&hdr-outline-regexp))))

(defun &&hdr-outline-insert-heading ()
  "Insert a new heading at same depth at point."
  (interactive)
  (let ((head (save-excursion
                (condition-case nil
                    (&&hdr-outline-back-to-heading)
                  (error (&&hdr-outline-next-heading)))
                (if (eobp)
                  (or (caar &&hdr-outline-heading-alist) "")
                  (match-string 0)))))
    (unless (or (string-match "[ \t]\\'" head)
                (not (string-match (concat "\\`\\(?:" &&hdr-outline-regexp "\\)")
                                   (concat head " "))))
      (setq head (concat head " ")))
    (unless (bolp) (end-of-line) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (run-hooks '&&hdr-outline-insert-heading-hook)))

(defun &&hdr-outline-invent-heading (head up)
  (save-match-data
    ;; Let's try to invent one by repeating or deleting the last char.
    (let ((new-head (if up (substring head 0 -1)
                        (concat head (substring head -1)))))
      (if (string-match (concat "\\`\\(?:" &&hdr-outline-regexp "\\)")
                        new-head)
        ;; Why bother checking that it is indeed higher/lower level ?
        new-head
        ;; Didn't work, so ask what to do.
        (read-string (format "%s heading for `%s': "
                             (if up "Parent" "Demoted") head)
                     head nil nil t)))))

(defun &&hdr-outline-promote (&optional which)
  "Promote headings higher up the tree.
If transient-mark-mode is on, and mark is active, promote headings in
the region (from a Lisp program, pass `region' for WHICH).  Otherwise:
without prefix argument, promote current heading and all headings in the
subtree (from a Lisp program, pass `subtree' for WHICH); with prefix
argument, promote just the current heading (from a Lisp program, pass
nil for WHICH, or do not pass any argument)."
  (interactive
   (list (if (and transient-mark-mode mark-active) 'region
             (&&hdr-outline-back-to-heading)
             (if current-prefix-arg nil 'subtree))))
  (cond
    ((eq which 'region)
     (&&hdr-outline-map-region '&&hdr-outline-promote (region-beginning) (region-end)))
    (which
     (&&hdr-outline-map-region '&&hdr-outline-promote
                               (point)
                               (save-excursion (&&hdr-outline-get-next-sibling) (point))))
    (t
     (&&hdr-outline-back-to-heading t)
     (let* ((head (match-string-no-properties 0))
            (level (save-match-data (funcall &&hdr-outline-level)))
            (up-head (or (&&hdr-outline-head-from-level (1- level) head)
                         ;; Use the parent heading, if it is really
                         ;; one level less.
                         (save-excursion
                           (save-match-data
                             (&&hdr-outline-up-heading 1 t)
                             (and (= (1- level) (funcall &&hdr-outline-level))
                                  (match-string-no-properties 0))))
                         ;; Bummer!! There is no lower level heading.
                         (&&hdr-outline-invent-heading head 'up))))

       (unless (rassoc level &&hdr-outline-heading-alist)
         (push (cons head level) &&hdr-outline-heading-alist))

       (replace-match up-head nil t)))))

(defun &&hdr-outline-demote (&optional which)
  "Demote headings lower down the tree.
If transient-mark-mode is on, and mark is active, demote headings in
the region (from a Lisp program, pass `region' for WHICH).  Otherwise:
without prefix argument, demote current heading and all headings in the
subtree (from a Lisp program, pass `subtree' for WHICH); with prefix
argument, demote just the current heading (from a Lisp program, pass
nil for WHICH, or do not pass any argument)."
  (interactive
   (list (if (and transient-mark-mode mark-active) 'region
             (&&hdr-outline-back-to-heading)
             (if current-prefix-arg nil 'subtree))))
  (cond
    ((eq which 'region)
     (&&hdr-outline-map-region '&&hdr-outline-demote (region-beginning) (region-end)))
    (which
     (&&hdr-outline-map-region '&&hdr-outline-demote
                               (point)
                               (save-excursion (&&hdr-outline-get-next-sibling) (point))))
    (t
     (let* ((head (match-string-no-properties 0))
            (level (save-match-data (funcall &&hdr-outline-level)))
            (down-head
             (or (&&hdr-outline-head-from-level (1+ level) head)
                 (save-excursion
                   (save-match-data
                     (while (and (progn (&&hdr-outline-next-heading) (not (eobp)))
                                 (<= (funcall &&hdr-outline-level) level)))
                     (when (eobp)
                       ;; Try again from the beginning of the buffer.
                       (goto-char (point-min))
                       (while (and (progn (&&hdr-outline-next-heading) (not (eobp)))
                                   (<= (funcall &&hdr-outline-level) level))))
                     (unless (eobp)
                       (looking-at &&hdr-outline-regexp)
                       (match-string-no-properties 0))))
                 ;; Bummer!! There is no higher-level heading in the buffer.
                 (&&hdr-outline-invent-heading head nil))))

       (unless (rassoc level &&hdr-outline-heading-alist)
         (push (cons head level) &&hdr-outline-heading-alist))
       (replace-match down-head nil t)))))

(defun &&hdr-outline-head-from-level (level head &optional alist)
  "Get new heading with level LEVEL from ALIST.
If there are no such entries, return nil.
ALIST defaults to `&&hdr-outline-heading-alist'.
Similar to (car (rassoc LEVEL ALIST)).
If there are several different entries with same new level, choose
the one with the smallest distance to the association of HEAD in the alist.
This makes it possible for promotion to work in modes with several
independent sets of headings (numbered, unnumbered, appendix...)"
  (unless alist (setq alist &&hdr-outline-heading-alist))
  (let ((l (rassoc level alist))
        ll h hl l2 l2l)
    (cond
      ((null l) nil)
      ;; If there's no HEAD after L, any other entry for LEVEL after L
      ;; can't be much better than L.
      ((null (setq h (assoc head (setq ll (memq l alist))))) (car l))
      ;; If there's no other entry for LEVEL, just keep L.
      ((null (setq l2 (rassoc level (cdr ll)))) (car l))
      ;; Now we have L, L2, and H: see if L2 seems better than L.
      ;; If H is after L2, L2 is better.
      ((memq h (setq l2l (memq l2 (cdr ll))))
       (&&hdr-outline-head-from-level level head l2l))
      ;; Now we have H between L and L2.
      ;; If there's a separator between L and H, prefer L2.
      ((memq h (memq nil ll))
       (&&hdr-outline-head-from-level level head l2l))
      ;; If there's a separator between L2 and H, prefer L.
      ((memq l2 (memq nil (setq hl (memq h ll)))) (car l))
      ;; No separator between L and L2, check the distance.
      ((< (* 2 (length hl)) (+ (length ll) (length l2l)))
       (&&hdr-outline-head-from-level level head l2l))
      ;; If all else fails, just keep L.
      (t (car l)))))

(defun &&hdr-outline-map-region (fun beg end)
  "Call FUN for every heading between BEG and END.
When FUN is called, point is at the beginning of the heading and
the match data is set appropriately."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (when (re-search-forward (concat "^\\(?:" &&hdr-outline-regexp "\\)") end t)
      (goto-char (match-beginning 0))
      (funcall fun)
      (while (and (progn
                    (&&hdr-outline-next-heading)
                    (< (point) end))
                  (not (eobp)))
        (funcall fun)))))

;; Vertical tree motion

(defun &&hdr-outline-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (&&hdr-outline-move-subtree-down (- arg)))

(defun &&hdr-outline-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (let ((movfunc (if (> arg 0) '&&hdr-outline-get-next-sibling
                     '&&hdr-outline-get-last-sibling))
        (ins-point (make-marker))
        (cnt (abs arg))
        beg end folded)
    ;; Select the tree
    (&&hdr-outline-back-to-heading)
    (setq beg (point))
    (save-match-data
      (save-excursion (&&hdr-outline-end-of-heading)
                      (setq folded (&&hdr-outline-invisible-p)))
      (&&hdr-outline-end-of-subtree))
    (if (= (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
          (progn (goto-char beg)
                 (error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
      ;; Moving forward - still need to move over subtree
      (progn (&&hdr-outline-end-of-subtree)
             (if (= (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (insert (delete-and-extract-region beg end))
    (goto-char ins-point)
    (if folded (&&hdr-hide-subtree))
    (move-marker ins-point nil)))

(defun &&hdr-outline-end-of-heading ()
  (if (re-search-forward &&hdr-outline-heading-end-regexp nil 'move)
    (forward-char -1)))

(defun &&hdr-outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that starts with a `*' (or that
`&&hdr-outline-regexp' matches)."
  (interactive "p")
  (if (< arg 0)
    (beginning-of-line)
    (end-of-line))
  (let (found-heading-p)
    (while (and (not (bobp)) (< arg 0))
      (while (and (not (bobp))
                  (setq found-heading-p
                        (re-search-backward
                         (concat "^\\(?:" &&hdr-outline-regexp "\\)")
                         nil 'move))
                  (&&hdr-outline-invisible-p)))
      (setq arg (1+ arg)))
    (while (and (not (eobp)) (> arg 0))
      (while (and (not (eobp))
                  (setq found-heading-p
                        (re-search-forward
                         (concat "^\\(?:" &&hdr-outline-regexp "\\)")
                         nil 'move))
                  (&&hdr-outline-invisible-p (match-beginning 0))))
      (setq arg (1- arg)))
    (if found-heading-p (beginning-of-line))))

(defun &&hdr-outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`&&hdr-outline-regexp' matches)."
  (interactive "p")
  (&&hdr-outline-next-visible-heading (- arg)))

(defun &&hdr-outline-mark-subtree ()
  "Mark the current subtree in an &&hdr-outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (&&hdr-outline-on-heading-p)
      ;; we are already looking at a heading
      (beginning-of-line)
      ;; else go back to previous heading
      (&&hdr-outline-previous-visible-heading 1))
    (setq beg (point))
    (&&hdr-outline-end-of-subtree)
    (push-mark (point) nil t)
    (goto-char beg)))


(defvar &&hdr-outline-isearch-open-invisible-function nil
  "Function called if `isearch' finishes in an invisible overlay.
The function is called with the overlay as its only argument.
If nil, `&&hdr-show-entry' is called to reveal the invisible text.")

(put '&&hdr-outline 'reveal-toggle-invisible '&&hdr-outline-reveal-toggle-invisible)
(defun &&hdr-outline-flag-region (from to flag)
  "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (remove-overlays from to 'invisible '&&hdr-outline)
  (when flag
    ;; We use `front-advance' here because the invisible text begins at the
    ;; very end of the heading, before the newline, so text inserted at FROM
    ;; belongs to the heading rather than to the entry.
    (let ((o (make-overlay from to nil 'front-advance)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'invisible '&&hdr-outline)
      (overlay-put o 'isearch-open-invisible
                   (or &&hdr-outline-isearch-open-invisible-function
                       '&&hdr-outline-isearch-open-invisible))))
  ;; Seems only used by lazy-lock.  I.e. obsolete.
  (run-hooks '&&hdr-outline-view-change-hook))

(defun &&hdr-outline-reveal-toggle-invisible (o hidep)
  (save-excursion
    (goto-char (overlay-start o))
    (if hidep
      ;; When hiding the area again, we could just clean it up and let
      ;; reveal do the rest, by simply doing:
      ;; (remove-overlays (overlay-start o) (overlay-end o)
      ;;                  'invisible '&&hdr-outline)
      ;;
      ;; That works fine as long as everything is in sync, but if the
      ;; structure of the document is changed while revealing parts of it,
      ;; the resulting behavior can be ugly.  I.e. we need to make
      ;; sure that we hide exactly a subtree.
      (progn
        (let ((end (overlay-end o)))
          (delete-overlay o)
          (while (progn
                   (&&hdr-hide-subtree)
                   (&&hdr-outline-next-visible-heading 1)
                   (and (not (eobp)) (< (point) end))))))

      ;; When revealing, we just need to reveal sublevels.  If point is
      ;; inside one of the sublevels, reveal will call us again.
      ;; But we need to preserve the original overlay.
      (let ((o1 (copy-overlay o)))
        (overlay-put o 'invisible nil)  ;Show (most of) the text.
        (while (progn
                 (&&hdr-show-entry)
                 (&&hdr-gshow-children)
                 ;; Normally just the above is needed.
                 ;; But in odd cases, the above might fail to show anything.
                 ;; To avoid an infinite loop, we have to make sure that
                 ;; *something* gets shown.
                 (and (equal (overlay-start o) (overlay-start o1))
                      (< (point) (overlay-end o))
                      (= 0 (forward-line 1)))))
        ;; If still nothing was shown, just kill the damn thing.
        (when (equal (overlay-start o) (overlay-start o1))
          ;; I've seen it happen at the end of buffer.
          (delete-overlay o1))))))

;; Function to be set as an &&hdr-outline-isearch-open-invisible' property
;; to the overlay that makes the &&hdr-outline invisible (see
;; `&&hdr-outline-flag-region').
(defun &&hdr-outline-isearch-open-invisible (_overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (&&hdr-show-entry))

(defun &&hdr-hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (save-excursion
    (&&hdr-outline-back-to-heading)
    (&&hdr-outline-end-of-heading)
    (&&hdr-outline-flag-region (point) (progn (&&hdr-outline-next-preface) (point)) t)))

(defun &&hdr-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (&&hdr-outline-back-to-heading t)
    (&&hdr-outline-flag-region (1- (point))
                               (progn (&&hdr-outline-next-preface) (point)) nil)))

(defun &&hdr-hide-body ()
  "Hide all body lines in buffer, leaving all headings visible."
  (interactive)
  (&&hdr-hide-region-body (point-min) (point-max)))

(defun &&hdr-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  ;; Nullify the hook to avoid repeated calls to `&&hdr-outline-flag-region'
  ;; wasting lots of time running `lazy-lock-fontify-after-&&hdr-outline'
  ;; and run the hook finally.
  (let (&&hdr-outline-view-change-hook)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (if (&&hdr-outline-on-heading-p)
          (&&hdr-outline-end-of-heading)
          (&&hdr-outline-next-preface))
        (while (not (eobp))
          (&&hdr-outline-flag-region (point)
                                     (progn (&&hdr-outline-next-preface) (point)) t)
          (unless (eobp)
            (forward-char (if (looking-at "\n\n") 2 1))
            (&&hdr-outline-end-of-heading))))))
  (run-hooks '&&hdr-outline-view-change-hook))

(defun &&hdr-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (&&hdr-outline-flag-region (point-min) (point-max) nil))

(defun &&hdr-hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (&&hdr-outline-flag-subtree t))

(defun &&hdr-hide-leaves ()
  "Hide the body after this heading and at deeper levels."
  (interactive)
  (save-excursion
    (&&hdr-outline-back-to-heading)
    ;; Turned off to fix bug reported by Otto Maddox on 22 Nov 2005.
    ;;    (&&hdr-outline-end-of-heading)
    (&&hdr-hide-region-body (point) (progn (&&hdr-outline-end-of-subtree) (point)))))

(defun &&hdr-show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (&&hdr-outline-flag-subtree nil))

(defun &&hdr-outline-show-heading ()
  "Show the current heading and move to its end."
  (&&hdr-outline-flag-region (- (point)
                                (if (bobp) 0
                                    (if (and &&hdr-outline-blank-line
                                             (eq (char-before (1- (point))) ?\n))
                                      2 1)))
                             (progn (&&hdr-outline-end-of-heading) (point))
                             nil))

(defun &&hdr-hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer."
  (interactive (list
                (cond
                  (current-prefix-arg (prefix-numeric-value current-prefix-arg))
                  ((save-excursion (beginning-of-line)
                                   (looking-at &&hdr-outline-regexp))
                   (funcall &&hdr-outline-level))
                  (t 1))))
  (if (< levels 1)
    (error "Must keep at least one level of headers"))
  (save-excursion
    (let* (&&hdr-outline-view-change-hook
           (beg (progn
                  (goto-char (point-min))
                  ;; Skip the prelude, if any.
                  (unless (&&hdr-outline-on-heading-p t) (&&hdr-outline-next-heading))
                  (point)))
           (end (progn
                  (goto-char (point-max))
                  ;; Keep empty last line, if available.
                  (if (bolp) (1- (point)) (point)))))
      (if (< end beg)
        (setq beg (prog1 end (setq end beg))))
      ;; First hide everything.
      (&&hdr-outline-flag-region beg end t)
      ;; Then unhide the top level headers.
      (&&hdr-outline-map-region
       (lambda ()
         (if (<= (funcall &&hdr-outline-level) levels)
           (&&hdr-outline-show-heading)))
       beg end)
      ;; Finally unhide any trailing newline.
      (goto-char (point-max))
      (if (and (bolp) (not (bobp)) (&&hdr-outline-invisible-p (1- (point))))
        (&&hdr-outline-flag-region (1- (point)) (point) nil))))
  (run-hooks '&&hdr-outline-view-change-hook))

(defun &&hdr-hide-other ()
  "Hide everything except current body and parent and top-level headings."
  (interactive)
  (&&hdr-hide-sublevels 1)
  (let (&&hdr-outline-view-change-hook)
    (save-excursion
      (&&hdr-outline-back-to-heading t)
      (&&hdr-show-entry)
      (while (condition-case nil (progn (&&hdr-outline-up-heading 1 t) (not (bobp)))
               (error nil))
        (&&hdr-outline-flag-region (1- (point))
                                   (save-excursion (forward-line 1) (point))
                                   nil))))
  (run-hooks '&&hdr-outline-view-change-hook))

(defun &&hdr-outline-toggle-children ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (&&hdr-outline-back-to-heading)
    (if (not (&&hdr-outline-invisible-p (line-end-position)))
      (&&hdr-hide-subtree)
      (&&hdr-gshow-children)
      (&&hdr-show-entry))))

(defun &&hdr-outline-flag-subtree (flag)
  (save-excursion
    (&&hdr-outline-back-to-heading)
    (&&hdr-outline-end-of-heading)
    (&&hdr-outline-flag-region (point)
                               (progn (&&hdr-outline-end-of-subtree) (point))
                               flag)))

(defun &&hdr-outline-end-of-subtree ()
  (&&hdr-outline-back-to-heading)
  (let ((first t)
        (level (funcall &&hdr-outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall &&hdr-outline-level) level)))
      (setq first nil)
      (&&hdr-outline-next-heading))
    (if (and (bolp) (not (eolp)))
      ;; We stopped at a nonempty line (the next heading).
      (progn
        ;; Go to end of line before heading
        (forward-char -1)
        (if (and &&hdr-outline-blank-line (bolp))
          ;; leave blank line before heading
          (forward-char -1))))))

(defun &&hdr-show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (&&hdr-gshow-children 1000))

(defun &&hdr-gshow-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
        (if level (prefix-numeric-value level)
            (save-excursion
              (&&hdr-outline-back-to-heading)
              (let ((start-level (funcall &&hdr-outline-level)))
                (&&hdr-outline-next-heading)
                (if (eobp)
                  1
                  (max 1 (- (funcall &&hdr-outline-level) start-level)))))))
  (let (&&hdr-outline-view-change-hook)
    (save-excursion
      (&&hdr-outline-back-to-heading)
      (setq level (+ level (funcall &&hdr-outline-level)))
      (&&hdr-outline-map-region
       (lambda ()
         (if (<= (funcall &&hdr-outline-level) level)
           (&&hdr-outline-show-heading)))
       (point)
       (progn (&&hdr-outline-end-of-subtree)
              (if (eobp) (point-max) (1+ (point)))))))
  (run-hooks '&&hdr-outline-view-change-hook))



(defun &&hdr-outline-up-heading (arg &optional invisible-ok)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines."
  (interactive "p")
  (and (eq this-command '&&hdr-outline-up-heading)
       (or (eq last-command '&&hdr-outline-up-heading) (push-mark)))
  (&&hdr-outline-back-to-heading invisible-ok)
  (let ((start-level (funcall &&hdr-outline-level)))
    (when (<= start-level 1)
      (error "Already at top level of the &&hdr-outline"))
    (while (and (> start-level 1) (> arg 0) (not (bobp)))
      (let ((level start-level))
        (while (not (or (< level start-level) (bobp)))
          (if invisible-ok
            (&&hdr-outline-previous-heading)
            (&&hdr-outline-previous-visible-heading 1))
          (setq level (funcall &&hdr-outline-level)))
        (setq start-level level))
      (setq arg (- arg 1))))
  (looking-at &&hdr-outline-regexp))

(defun &&hdr-outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (&&hdr-outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
                              (&&hdr-outline-get-next-sibling))))
      (if point-to-move-to
        (progn
          (goto-char point-to-move-to)
          (setq arg (1- arg)))
        (progn
          (setq arg 0)
          (error "No following same-level heading"))))))

(defun &&hdr-outline-get-next-sibling ()
  "Move to next heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((level (funcall &&hdr-outline-level)))
    (&&hdr-outline-next-visible-heading 1)
    (while (and (not (eobp)) (> (funcall &&hdr-outline-level) level))
      (&&hdr-outline-next-visible-heading 1))
    (if (or (eobp) (< (funcall &&hdr-outline-level) level))
      nil
      (point))))

(defun &&hdr-outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (&&hdr-outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
                              (&&hdr-outline-get-last-sibling))))
      (if point-to-move-to
        (progn
          (goto-char point-to-move-to)
          (setq arg (1- arg)))
        (progn
          (setq arg 0)
          (error "No previous same-level heading"))))))

(defun &&hdr-outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((opoint (point))
        (level (funcall &&hdr-outline-level)))
    (&&hdr-outline-previous-visible-heading 1)
    (when (and (/= (point) opoint) (&&hdr-outline-on-heading-p))
      (while (and (> (funcall &&hdr-outline-level) level)
                  (not (bobp)))
        (&&hdr-outline-previous-visible-heading 1))
      (if (< (funcall &&hdr-outline-level) level)
        nil
        (point)))))

(defun &&hdr-outline-headers-as-kill (beg end)
  "Save the visible &&hdr-outline headers in region at the start of the kill ring.

Text shown between the headers isn't copied.  Two newlines are
inserted between saved headers.  Yanking the result may be a
convenient way to make a table of contents of the buffer."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((buffer (current-buffer))
            start end)
        (with-temp-buffer
          (with-current-buffer buffer
            ;; Boundary condition: starting on heading:
            (when (&&hdr-outline-on-heading-p)
              (&&hdr-outline-back-to-heading)
              (setq start (point)
                    end (progn (&&hdr-outline-end-of-heading)
                               (point)))
              (insert-buffer-substring buffer start end)
              (insert "\n\n")))
          (let ((temp-buffer (current-buffer)))
            (with-current-buffer buffer
              (while (&&hdr-outline-next-heading)
                (unless (&&hdr-outline-invisible-p)
                  (setq start (point)
                        end (progn (&&hdr-outline-end-of-heading) (point)))
                  (with-current-buffer temp-buffer
                    (insert-buffer-substring buffer start end)
                    (insert "\n\n"))))))
          (kill-new (buffer-string)))))))

;;; &&hdr-outline.el ends here

;;;;; end of *horrors*


(provide 'outline-headers)

;; Local Variables:
;; End:

;; outline-headers.el ends here
