;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!NCoast.ORG!allbery Thu Dec 21 23:01:34 EST 1989
;Article 863 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!NCoast.ORG!allbery
;>From: allbery@NCoast.ORG
;Newsgroups: gnu.emacs
;Subject: Bison mode for Gnu Emacs
;Message-ID: <8912212039.AA08451@NCoast.ORG>
;Date: 21 Dec 89 20:39:31 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 671
;
;Well, having given up on finding my original "yacc mode" code and needing to
;do some major hacking on yacc / bison sources, I bit the bullet and started
;from scratch.  But this time, I did it right.  All of the original warts in
;the old yacc-mode have been avoided, and the result is actually useful.  It is
;also more complete as a major mode, and takes pains to retain the user's
;environment while switching modes to deal with code blocks.
;
;I leave it to the FSF to decide whether this should become part of GNU Emacs.
;
;++Brandon
;--
;Brandon S. Allbery    allbery@NCoast.ORG, BALLBERY (MCI Mail), ALLBERY (Delphi)
;uunet!hal.cwru.edu!ncoast!allbery ncoast!allbery@hal.cwru.edu bsa@telotech.uucp

;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!NCoast.ORG!allbery Thu Dec 21 23:02:57 EST 1989
;Article 865 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!NCoast.ORG!allbery
;>From: allbery@NCoast.ORG
;Newsgroups: gnu.emacs
;Subject: Bison mode for Gnu Emacs (repost)
;Message-ID: <8912220033.AA11027@NCoast.ORG>
;Date: 22 Dec 89 00:32:57 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 664
;
;I screwed up on the copyright in the previous post.  Wipe the previous copy
;and use this instead.
;
;++Brandon

;; Bison (or Yacc) mode for Gnu Emacs
;; Brandon S. Allbery, allbery@NCoast.ORG; buggestions welcome

;; Copyright (C) 1989 Brandon S. Allbery

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Bison, Yacc, and similar parser compilers all are unusual in that
;; parts of the file are in C mode instead of the parser language mode.
;; This requires some interesting hackery on the part of Emacs; when
;; switching from one part of the file to another, the state of the buffer
;; is saved on an alist and the buffer's mode is changed.  This is
;; arranged such that each buffer has its own saved state for both Bison
;; and Bison-Block (aka C) modes; much work is done to avoid losing
;; buffer-local information in either major mode while not confusing the
;; two.

;; Bison mode knows quite a bit about C mode, and uses the known values
;; of c-indent-level and c-auto-newline, among others.  Unfortunately,
;; it does not properly handle all such combinations of C-mode settings.
;; It does support both my old coding style and my current style, which
;; is sufficient for my purposes.

;; Also included is an "auto-fill" mode which continues definition lines
;; (%token, %left, etc.) and wraps long token sequences to the proper
;; column automagically.  This is disabled by default; see the function
;; (bison-auto-continuation-mode) for more information.

;; The code has survived mostly intact my most recent go at updating USP
;; (see comp.sources.misc), and I therefore consider it (mostly) stable.
;; No doubt someone will find something that needs to be fixed; send mail
;; to allbery@ncoast.org in that case.  I intend to keep Bison mode up to
;; date, since it makes my life a lot easier.  (Now if I could use gcc and
;; Bison without running into collisions between the GPL and other license
;; agreements....)

(defvar bison-buffer-local-alist nil
  "Alist of bison mode buffers, and saved local variables thereto.")

(defconst bison-colon-column 16 "\
*The column in which to place a colon separating a token from its definition.")

(defconst bison-percent-column 41 "\
*The column in which to place a percent introducing a modifier (e.g. %prec).")

(defvar bison-mode-abbrev-table nil
  "Abbrev table used in bison-mode buffers.")
(define-abbrev-table 'bison-mode-abbrev-table ())

(defvar bison-mode-map ()
  "Keymap used in bison mode.")
(if bison-mode-map
    ()
  (setq bison-mode-map (make-sparse-keymap))
  (define-key bison-mode-map "{" 'bison-insert-edit-code-block)
  (define-key bison-mode-map ";" 'electric-bison-semi)
  (define-key bison-mode-map ":" 'electric-bison-colon)
  (define-key bison-mode-map "|" 'electric-bison-colon)
  (define-key bison-mode-map "%" 'electric-bison-per)
  (define-key bison-mode-map "\C-c%" 'bison-edit-c-division)
  (define-key bison-mode-map "\177" 'backward-delete-char-untabify)
  (define-key bison-mode-map "\t" 'bison-indent-command))

(defvar bison-mode-syntax-table nil
  "Syntax table in use in bison-mode buffers.")
(if bison-mode-syntax-table
    ()
  (setq bison-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 14" bison-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" bison-mode-syntax-table)
  (modify-syntax-entry ?{ "(}  " bison-mode-syntax-table)
  (modify-syntax-entry ?} "){  " bison-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\  " bison-mode-syntax-table)
  (modify-syntax-entry ?\' "\"   " bison-mode-syntax-table)
  (modify-syntax-entry ?\: "(;  " bison-mode-syntax-table)
  (modify-syntax-entry ?\; "):  " bison-mode-syntax-table))

(fset 'F:local-map (symbol-function 'use-local-map))
(fset 'F:syntax-table (symbol-function 'set-syntax-table))

(defun bison-mode ()
  "Major mode for editing Bison or Yacc code for a C target.
Blocks of C code are replaced with ellipses unless expanded, which causes the
buffer to be narrowed and switched to C mode; the C and Bison environments are
preserved when not active.  { inserts a new block if necessary.
\\{bison-mode-map}
Turning on Bison mode calls the value of the variable bison-mode-hook with
no args if it is non-nil.  The first time the buffer is narrowed to a block,
the value of c-mode-hook will be called with no args within the narrowed
environment if it is non-nil."
  (interactive)
  (kill-all-local-variables)
  ;; anyone got a better way to do this?
  (let ((elt (if bison-buffer-local-alist
		 (assoc (current-buffer) bison-buffer-local-alist))))
    (and elt
	 (setcdr elt nil)))
  (use-local-map bison-mode-map)
  (setq major-mode 'bison-mode)
  (setq mode-name "Bison")
  (setq local-abbrev-table bison-mode-abbrev-table)
  (set-syntax-table bison-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'bison-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'selective-display)
  (setq selective-display t)
  (make-local-variable 'selective-display-ellipses)
  (setq selective-display-ellipses t)
  (make-local-variable 'block-indent-level)
  (make-local-variable 'auto-fill-hook)
  (bison-hide-code-blocks)
  (run-hooks 'bison-mode-hook 'c-mode-hook))

(defun electric-bison-colon (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let ((state (parse-partial-sexp
		(save-excursion
		  (if (re-search-backward
		       "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
		       nil 'move)
		      (- (match-end 0) 1)
		    (point-min)))
		(point))))
    (if (or (nth 3 state) (nth 4 state) (nth 5 state))
	(self-insert-command (prefix-numeric-value arg))
      (if (and (not arg) (eolp))
	  (progn
	    (bison-indent-line)
	    (and c-auto-newline
		 (eq last-command-char ?\|)
		 (save-excursion
		   (beginning-of-line)
		   (not (looking-at "[ \t]*$")))
		 (newline))
	    (delete-horizontal-space)
	    (indent-to bison-colon-column)
	    (insert last-command-char)
	    (insert " "))
	(self-insert-command (prefix-numeric-value arg))))))

(defun electric-bison-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if c-auto-newline
      (electric-bison-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-bison-per (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let ((state (parse-partial-sexp
		(save-excursion
		  (if (re-search-backward
		       "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
		       nil 'move)
		      (- (match-end 0) 1)
		    (point-min)))
		(point))))
    (if (and (not arg)
	     (eolp)
	     (not (eq (preceding-char) ?%))
	     (not (or (nth 3 state) (nth 4 state) (nth 5 state))))
	(if (not (save-excursion
		   (skip-chars-backward " \t")
		   (bolp)))
	    (indent-to bison-percent-column)
	  (delete-region (save-excursion
			   (beginning-of-line)
			   (point))
			 (point))))
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-bison-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let ((state (parse-partial-sexp
		(save-excursion
		  (if (re-search-backward
		       "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
		       nil 'move)
		      (- (match-end 0) 1)
		    (point-min)))
		(point)))
	insertpos)
    (if (or (nth 3 state) (nth 4 state) (nth 5 state))
	(self-insert-command (prefix-numeric-value arg))
      (if (and (not arg) (eolp)
	       (not (save-excursion
		      (beginning-of-line)
		      (skip-chars-forward " \t")
		      (= (following-char) ?%))))
	  (progn
	    (and c-auto-newline
		 (progn
		   (if (save-excursion
			 (beginning-of-line)
			 (not (looking-at "[ \t]*$")))
		       (newline))
		   (bison-indent-line)
		   (backward-delete-char-untabify 2)))
	    (insert last-command-char)
	    (bison-indent-line)
	    (and c-auto-newline
		 (progn
		   (newline)
		   (setq insertpos (- (point) 2))
		   (bison-indent-line)))
	    (save-excursion
	      (if insertpos (goto-char (1+ insertpos)))
	      (delete-char -1))))
      (if insertpos
	  (save-excursion
	    (goto-char insertpos)
	    (self-insert-command (prefix-numeric-value arg)))
	(self-insert-command (prefix-numeric-value arg))))))

(defun bison-indent-command (&optional whole-exp)
  "Indent current line as Bison code, or in some cases insert a tab character.
If c-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
      (let ((shift-amount (bison-indent-line))
	    beg end)
	(save-excursion
	  (if c-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (re-search-forward ";\\|^%%" nil 'move)
	  (if (save-excursion
		(beginning-of-line)
		(looking-at "%%"))
	      (progn
		(forward-line -1)
		(end-of-line)))
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amount "%")))
    (if (and (not c-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (bison-indent-line))))

(defun bison-indent-line ()
  "Indent current line as Bison code.
Return the amount the indentation changed by."
  ;; Lines are indented if and only if a colon is found before a semicolon
  ;; while searching backward.  String-quoted characters are ignored.
  (let (indent)
    (save-excursion
      (cond
       ((save-excursion
	  (let ((limit (point))
		state)
	    (goto-char (point-min))
	    (not (and (re-search-forward "^%%" limit t)
		      (progn
			(parse-partial-sexp
			 (save-excursion
			   (if (re-search-backward
				"^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
				nil 'move)
			       (- (match-end 0) 1)
			     (point-min)))
			 (point))
			(not (or (nth 3 state)
				 (nth 4 state)
				 (nth 5 state))))))))
	(setq indent 0))
       ((save-excursion
	  (beginning-of-line)
	  (looking-at "[ \t]*%"))
	(setq indent 0))
       ((save-excursion
	  (skip-chars-backward " \t\n\f")
	  (eq (preceding-char) ?\;))
	(setq indent 0))
       (t
	(beginning-of-line)
	(while (not (or (bobp)
			(looking-at "[ \t]*\\(\sw\\|\s_\\)*[ \t]*[|:]")
			(eq (following-char) ?%)))
	  (forward-line -1))
	(skip-chars-forward "^:|")
	(skip-chars-forward ":| \t")
	(setq indent (current-column)))))
    (indent-to indent)
    indent))

(defun bison-insert-edit-code-block (arg)
  "Edit the code block associated with the current line of parser description.
If no such block is found, create one."
  (interactive "P")
  (cond
   ((let ((state (parse-partial-sexp
		  (save-excursion
		    (if (re-search-backward
			 "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
			 nil 'move)
			(- (match-end 0) 1)
		      (point-min)))
		  (point))))
      (or (nth 3 state) (nth 4 state) (nth 5 state)))
    (self-insert-command (prefix-numeric-value arg)))
   ((eq (preceding-char) ?%)
    (insert "{\n\n%}\n")
    (forward-line -2)
    (indent-to c-indent-level)
    (bison-edit-code-block))
   ((and (eolp)
	 (save-excursion
	   (beginning-of-line)
	   (looking-at "%[ \t]*union[ \t]*$")))
    (and c-auto-newline
	 (insert "\n"))
    (insert "{\n\n}\n")
    (forward-line -2)
    (indent-to c-indent-level)
    (skip-chars-backward "^{")
    (forward-char -1)
    (bison-edit-code-block))
   ((looking-at "[^\n]*\\(\n[ \t]*\\)?{")
    (bison-edit-code-block))
   (t
    (let (indent)
      (end-of-line)
      (if c-auto-newline
	  (progn
	    (newline)
	    (setq indent (bison-indent-line))
	    (delete-horizontal-space)
	    (indent-to (+ indent c-indent-level))))
      (insert "{\n\n")
      (indent-to (+ indent c-indent-level))
      (insert "}")
      (if (eolp)
	  (forward-line 1)
	(insert "\n"))
      (forward-line -2)
      (indent-to (+ indent c-indent-level c-indent-level))
      (skip-chars-backward "^{")
      (forward-char -1))
    (bison-edit-code-block))))

(defun bison-edit-code-block ()
  "Edit the code block associated with the current line of parser description."
  (interactive)
  (or (looking-at "[^\n]*\\(\n[ \t]*\\)?{")
      (error "No code block attached to this parser description."))
  (while (let (state)
	   (skip-chars-forward "^{")
	   (setq state (parse-partial-sexp
			(save-excursion
			  (if (re-search-backward
			       "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
			       nil 'move)
			      (- (match-end 0) 1)
			    (point-min)))
			(point)))
	   (or (nth 3 state) (nth 4 state) (nth 5 state)))
    (forward-char 1))
  (narrow-to-region (point)
		    (save-excursion
		      (bison-forward-list)
		      (point)))
  (bison-reveal-code-blocks)
  (forward-line 1)
  (skip-chars-forward " \t")
  (setq block-indent-level (current-column))
  (let ((modp (buffer-modified-p))
	indent block)
    (setq block block-indent-level)
    (bison-swap-to-mode 'bison-block-mode)
    (setq bison-block-is-division nil)
    (goto-char (point-min))
    (forward-char 1)
    (if (looking-at "[ \t]*\n")
	(forward-line 1))
    (while (not (eobp))
      (delete-region (point) (save-excursion
			       (skip-chars-forward " \t")
			       (setq indent (current-column))
			       (point)))
      (if (not (or (eq (following-char) ?\n)
		   (and (eq (following-char) ?}) (save-excursion
						   (forward-char 1)
						   (eobp)))))
	  (indent-to (- indent (- c-indent-level) block)))
      (forward-line 1))
    (set-buffer-modified-p modp))
  (goto-char (+ (point-min) 1))
  (and (eolp)
       (forward-char 1))
  (skip-chars-forward " \t")
  (message (if (eq (key-binding "\C-c\C-c") 'bison-widen)
	       "Enter C-c C-c to return to Bison mode."
	     (substitute-command-keys
	      "Enter \\[bison-widen] to return to Bison mode."))))

(defun bison-swap-to-mode (mode-def)
  "Restore the saved alternate mode, or create it by funcall-ing MODE-DEF."
  (let ((elt (assoc (current-buffer) bison-buffer-local-alist))
	state)
    (setq state (append
		 (buffer-local-variables)
		 (list (cons 'F:local-map (current-local-map)))
		 (list (cons 'F:syntax-table (syntax-table)))))
    (if elt
	(progn
	  (kill-all-local-variables)
	  (mapcar (function (lambda (arg)
			      (if (string-match "^F:" (symbol-name (car arg)))
				  (funcall (car arg) (cdr arg))
				(make-local-variable (car arg))
				(set (car arg) (cdr arg)))))
		  (cdr elt))
	  (setcdr elt state))
      (if mode-def
	  (funcall mode-def)
	(error "No saved buffer modes for this buffer."))
      (setq bison-buffer-local-alist (cons (cons (current-buffer) state)
					   bison-buffer-local-alist)))))

(defun bison-hide-code-blocks ()
  "Hide all blocks of C code (balanced {} expressions) within the buffer."
  (message "Prefrobnicating...")
  (let ((modp (buffer-modified-p))
	(selective-display nil)
	(divisions 1)
	state end c-division)
    (save-excursion
      (goto-char (point-min))
      (while (and (< divisions 3)
		  (re-search-forward "^%%\\|{" nil 'move))
	(if (progn
	      (setq state (parse-partial-sexp
			   (save-excursion
			     (if (re-search-backward
				  "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
				  nil 'move)
				 (- (match-end 0) 1)
			       (point-min)))
			   (point)))
	      (or (nth 3 state) (nth 4 state) (nth 5 state)))
	    (forward-line 1)
	  (if (save-excursion
		(beginning-of-line)
		(looking-at "^%%"))
	      (progn
		(if (= (setq divisions (1+ divisions)) 3)
		    (save-excursion
		      (forward-line 1)
		      (setq c-division (point))))
		(forward-char 1))
	    (or (bobp) (forward-char -1))
	    (if (save-excursion
		  (beginning-of-line)
		  (and (not (bobp)) (looking-at "[ \t]*{")))
		(progn
		  (beginning-of-line)
		  (forward-char -1)))
	    (setq end (save-excursion
			(bison-forward-list)
			(point)))
	    (while (search-forward "\n" end 'move)
	      (delete-char -1)
	      (insert "\r")))))
      (if (= divisions 3)
	  (progn
	    (goto-char c-division)
	    (narrow-to-region (point-min) (point)))))
    (set-buffer-modified-p modp))
  (message "Prefrobnicating... done."))

(defun bison-reveal-code-blocks ()
  "Un-hide code blocks in the current restriction of the buffer."
  (let ((modp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (replace-string "\r" "\n"))
    (set-buffer-modified-p modp)))

(defun bison-forward-list ()
  "Skip forward over a valid C-mode list."
  (let ((syntax (syntax-table)))
    (set-syntax-table c-mode-syntax-table)
    (forward-sexp)
    (set-syntax-table syntax)))

(defun bison-block-mode ()
  "Major mode (actually, submode) for editing blocks of C code within a Bison
file.  See the documentation for C mode for details.
\\{c-mode-map}"
  (interactive)
  (let (c-mode-hook)
    (c-mode))
  (setq mode-name "Bison Block")
  (setq major-mode 'bison-block-mode)
  (make-variable-buffer-local 'bison-block-is-division)
  (setq bison-block-is-division nil)
  (local-set-key "\C-c\C-c" 'bison-widen)
  (local-set-key "\C-xw" 'bison-widen)
  (run-hooks 'c-mode-hook 'bison-block-mode-hook))

(defun bison-edit-c-division ()
  "Switch to editing the third division of the Bison buffer, creating it if
it doesn't exist.  The buffer is placed in Bison Block mode."
  (interactive)
  (let ((divisions 1)
	(endpoint (point-max))
	state)
    (widen)
    (save-excursion
      (goto-char (point-min))
      (while (and (< divisions 3)
		  (re-search-forward "^%%" nil t)
		  (progn
		    (setq state (parse-partial-sexp
				 (save-excursion
				   (if (re-search-backward
					"^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
					nil 'move)
				       (- (match-end 0) 1)
				     (point-min)))
				 (point)))
		    (not (or (nth 3 state) (nth 4 state) (nth 5 state)))))
	(setq divisions (1+ divisions))))
    (goto-char endpoint)
    (if (not (eq (preceding-char) ?\n))
	(insert "\n"))
    (while (< divisions 3)
      (insert "%%\n\n")
      (and (= (setq divisions (1+ divisions)) 3)
	   (forward-char -1)))
    (narrow-to-region (point) (point-max))
    (bison-swap-to-mode 'bison-block-mode)
    (setq bison-block-is-division t)
    (message (if (eq (key-binding "\C-c\C-c") 'bison-widen)
		 "Enter C-c C-c to return to Bison mode."
	       (substitute-command-keys
		"Enter \\[bison-widen] to return to Bison mode.")))))

(defun bison-widen ()
  "Save the current block of embedded C code and return to Bison mode."
  (interactive)
  (if (and (boundp 'bison-block-is-division)
	   bison-block-is-division)
      ()
    (let ((indent (assoc 'block-indent-level
			 (assoc (current-buffer) bison-buffer-local-alist))))
      (setq indent (if indent (cdr indent) 0))
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
	(skip-chars-forward " \t")
	(if (looking-at "[ \t]*\n")
	    (delete-region (save-excursion
			     (beginning-of-line)
			     (point))
			   (point))
	  (indent-to (+ (current-column) (- c-indent-level) indent)))
	(forward-line 1))))
  (goto-char (point-min))
  (save-excursion
    (widen)
    (bison-hide-code-blocks)
    (bison-swap-to-mode nil))
  (if (save-excursion
	(beginning-of-line)
	(looking-at "[ \t]*{"))
      (beginning-of-line))
  (or (bobp)
      (forward-char -1)))

(defun bison-auto-continuation-mode (&optional arg)
  "Arrange for definition lines to continue themselves."
  (interactive)
  (setq auto-fill-hook
	(if (or (and (numberp arg)
		     (> arg 0))
		(not (eq auto-fill-hook 'bison-auto-continue)))
	    'bison-auto-continue
	  nil)))

(defun bison-auto-continue ()
  "Copy the current line's prefix to a new line, iff it starts with %.
Otherwise, indent past bison-colon-column."
  (let ((state (parse-partial-sexp
		(save-excursion
		  (if (re-search-backward
		       "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
		       nil 'move)
		      (- (match-end 0) 1)
		    (point-min)))
		(point))))
    (if (or (nth 3 state) (nth 4 state) (nth 5 state))
	()
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "%[ \t]*[^u%{}]"))
	  (let ((prefix (save-excursion
			  (beginning-of-line)
			  (buffer-substring (point)
					    (save-excursion
					      (skip-chars-forward "^ \t\n<")
					      (point))))))
	    (insert "\n" prefix)
	    (or (eq (preceding-char) ?\ )
		(eq (preceding-char) ?\t)
		(insert " ")))
	(insert "\n")
	(indent-to (+ bison-colon-column 2))))))
