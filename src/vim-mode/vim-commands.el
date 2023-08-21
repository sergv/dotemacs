;; vim-commands.el - Implementation of VIM commands. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vim-mode commands are defined using the macro vim-defcmd, which has the following form.
;;
;;   (vim-defcmd command-name ((count [count-name])
;;                             (motion [motion-name])
;;                             (argument[:{char,text,file,buffer,...}] [arg-name])
;;                             [nonrepeatable]
;;                             [keep-visual])
;;     body ...)
;;
;; The first three arguments are keyword arguments similar to
;; defun*. The fourth and fifth argument are not real arguments,
;; i.e., they do not bind a value. Instead they define special
;; attributes of the command.
;;
;; Each of the arguments is optional. An argument is either given as
;; two element list (parameter parameter-name) or without explicit
;; name just parameter. When no parameter name is specified the
;; name is the same as the parameter itself, i.e., the parameter
;; count defines the variable count whereas the parameter
;; (count cnt) defines the variable cnt.
;;
;; The parameters have the following meanings.
;;
;; count:
;;   A numeric argument usually defining how many times a certain
;;   command should be repeated. If no explicit count has been given,
;;   the parameter has the value nil.
;;
;; motion:
;;   If the command operates on a certain region, the region is
;;   usually defined by a motion and this argument should be
;;   specified. When the command is executed in normal-mode vim-mode
;;   will switch to operator-pending to wait for the motion to be
;;   specified. Afterwards the command is executed with this motion.
;;   Note that usually count is nil if the command takes a motion
;;   because the repeat count will be used by the motion. If this
;;   parameter is not present the command will not take a motion but
;;   will be executed without switching to operator-pending mode.
;;
;; argument:
;;   Some commands take another argument besides the motion and the
;;   count. There are several types of arguments, the type is
;;   specified by appending a colon and the type-name after argument,
;;   i.e., argument:char defines an argument which is a single
;;   character, argument:text a general string, argument:file a
;;   file-path and argument:buffer a buffer-name. If no explicit type
;;   is given the argument type will be char. Note that the only
;;   allowed argument type for commands bound in another mode than
;;   ex-mode (using vim-emap or vim-local-emap) is char, i.e., when
;;   calling the command an additional character is read an passed to
;;   the function. An example for a command like this is the r command
;;   of Vim. All other argument types make only sense for ex-mode
;;   commands.
;;
;; nonrepeatable:
;;   If specified the command cannot be repeated by the repeat command
;;   bound to '.' by default. This is usually the case for scrolling
;;   or window commands.
;;
;; keep-visual:
;;   If specified the command does not end visual-mode when executed
;;   in visual-mode. This is usually the case for scrolling or window
;;   commands. Note that most editing commands do disable visual-mode.
;;
;; As described above vim-defcmd can be used to define commands for
;; both normal-mode and ex-mode. Each command should place (point) at
;; the correct position after the operation.
;;
;; In order to call a command from lisp-code, one has to use keyword
;; arguments, e.g.,
;;
;;   (vim:cmd-delete-line :count 5)
;;
;; deletes five lines. Note that the keyword used to call a commands
;; are always :count, :motion, or :argument no matter which
;; parameter names are used to define the command.
;;
;; For more information about the vim:motion struct look at vim-core.el.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'current-column-fixed)
(require 'macro-util)

(require 'vim-defs)
(require 'vim-macs)
(require 'vim-core)
(require 'vim-compat)
(require 'vim-motions)
(require 'vim-macro)

(defgroup vim-commands nil
  "Commands"
  :group 'vim-mode)

(defcustom vim-shift-width 2
  "The number of columns for shifting commands like < or >."
  :type 'integer
  :group 'vim-commands)

(vim-defcmd vim:cmd-insert (count)
  "Switches to insert-mode before point."
  (vim-start-insert-mode count))

(vim-defcmd vim:cmd-append (count)
  "Switches to insert-mode after point."
  (unless (eolp) (forward-char))
  (vim-start-insert-mode count))

(vim-defcmd vim:cmd-Insert (count)
  "Moves the cursor to the beginning of the current line
and switches to insert-mode."
  (vim:motion-first-non-blank)
  (vim:cmd-insert :count count))

(vim-defcmd vim:cmd-Append (count)
  "Moves the cursor to the end of the current line
and switches to insert-mode."
  (end-of-line)
  (vim:cmd-append :count count))

(vim-defcmd vim:cmd-insert-line-above (count)
  "Inserts a new line above the current one and goes to insert mode."
  (vim-start-insert-mode count 'above))

(vim-defcmd vim:cmd-insert-line-below (count)
  "Inserts a new line below the current one and goes to insert mode."
  (vim-start-insert-mode count 'below))

(vim-defcmd vim:cmd-replace ()
  "Goes to replace-mode."
  (vim-activate-insert-mode)
  (vim:insert-mode-toggle-replace))

(defvar vim--insert-mode-exit-move-point t
  "Whether to move point backwards on `vim:insert-mode-exit'. Can be t, nil or
'dont-move-at-line-end.")

(vim-defcmd vim:cmd-delete-line (count)
  "Deletes the next count lines."
  (vim--cmd-delete-line-impl count t))

(defun vim--cmd-delete-line-impl (count yank?)
  (when yank?
    (vim:cmd-yank-line :count count))
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (let ((n (1- (or count 1))))
                 (when (/= 0 n)
                   (forward-line n)))
               (line-end-position))))
    (if (= beg (point-min))
        (if (= end (point-max))
            (erase-buffer)
          (delete-region beg (save-excursion
                               (goto-char end)
                               (forward-line)
                               (line-beginning-position))))
      (delete-region (save-excursion
                       (goto-char beg)
                       (forward-line -1)
                       (line-end-position))
                     end))
    (goto-char beg)
    (vim:motion-first-non-blank)))

(vim-defcmd vim:cmd-delete (motion)
  "Deletes the characters defined by motion."
  (vim--cmd-delete-impl motion t))

(defun vim--cmd-delete-impl (motion yank?)
  (pcase (vim-motion-type motion)
    (`linewise
     (goto-line-dumb (vim-motion-first-line motion))
     (vim--cmd-delete-line-impl (vim-motion-line-count motion) yank?))

    (`block
     (when yank?
       (vim:cmd-yank :motion motion))
     (delete-rectangle (vim-motion-begin-pos motion)
                       (vim-motion-end-pos motion)))

    (_
     (when yank?
       (vim:cmd-yank :motion motion))
     (delete-region (vim-motion-begin-pos motion) (vim-motion-end-pos motion))
     (goto-char (vim-motion-begin-pos motion)))))

(vim-defcmd vim:delete-current-line ()
  "Function that does what \"d$\" does in vanilla vim."
  (vim:cmd-delete :motion (vim:motion-end-of-line)))

(vim-defcmd vim:cmd-delete-char (count)
  "Deletes the next count characters."
  (vim:cmd-delete :motion (vim:motion-right :count (or count 1))))

(vim-defcmd vim:cmd-delete-char-backward (count)
  "Deletes the next count characters."
  (vim:cmd-delete :motion (vim:motion-left :count (or count 1))))

(vim-defcmd vim:cmd-change (motion)
  "Deletes the characters defined by motion and goes to insert mode."
  (pcase (vim-motion-type motion)
    (`linewise
     (goto-line-dumb (vim-motion-first-line motion))
     (vim:cmd-change-line :count (vim-motion-line-count motion)))

    (`block
      ;; Column number at vim-motion-begin-pos can be larger that column at vim-motion-end-pos.
      ;; It’s important for visual insertion so we adjust it here.
      (let ((begin (save-excursion
                     (goto-char (vim-motion-begin-pos motion))
                     (move-to-column (vim-motion-first-col motion))
                     (point))))
        (vim--init--vim-visual-insert-info-end! begin
                                                (vim-motion-end-pos motion)
                                                (vim-motion-first-col motion))
        (vim:cmd-delete :motion motion)
        (vim-visual--start-insert)))

    (_
     ;; deal with cw and cW
     (when (and (not (null vim--current-motion))
                (not (eobp))
                (not (member (char-after) '(?\s ?\r ?\n ?\t))))
       (let ((cnt (* (or vim--current-cmd-count 1)
                     (or vim--current-motion-count 1))))
         (pcase vim--current-motion
           (`vim:motion-fwd-word
            (setq motion (vim:motion-fwd-word-end :count cnt)))
           (`vim:motion-fwd-WORD
            (setq motion (vim:motion-fwd-WORD-end :count cnt))))))

     (vim:cmd-delete :motion motion)
     (if (eolp)
         (vim:cmd-append :count 1)
       (vim:cmd-insert :count 1)))))

(vim-defcmd vim:cmd-change-line (count)
  "Deletes count lines and goes to insert mode."
  (let ((pos (line-beginning-position)))
    (vim:cmd-delete-line :count count)
    (if (< (point) pos)
      (progn
        (end-of-line)
        (newline))
      (progn
        (beginning-of-line)
        (newline)
        (forward-line -1)))
    (indent-according-to-mode)
    (if (eolp)
        (vim:cmd-append :count 1)
      (vim:cmd-insert :count 1))))

(vim-defcmd vim:cmd-change-rest-of-line ()
  "Deletes the rest of the current line."
  (let* ((start (point))
         (end (max start (line-end-position))))
    (vim:cmd-delete :motion (vim-make-motion :begin start
                                             :end end
                                             :type 'exclusive))
    (vim:cmd-insert :count 1)))

(vim-defcmd vim:cmd-change-char (count)
  "Deletes the next count characters and goes to insert mode."
  (let ((pos (point)))
    (vim:cmd-delete-char :count count)
    (if (< (point) pos)
      (vim:cmd-append)
      (vim:cmd-insert))))

(vim-defcmd vim:cmd-replace-char (count (argument:char arg))
  "Replaces the next count characters with arg."
  (unless (characterp arg)
    (error "Expected a character"))
  (when (< (- (line-end-position) (point))
           (or count 1))
    (error "Too few characters to end of line"))
  (delete-region (point) (+ (point) (or count 1)))
  (insert-char arg (or count 1))
  (backward-char))

(vim-defcmd vim:cmd-replace-region (motion (argument:char arg))
  "Replace complete region with `arg'"
  (vim--apply-on-motion
   motion
   (lambda (beg end)
     (save-excursion
       (goto-char beg)
       (delete-region beg end)
       (insert-char arg (- end beg))))))

(vim-defcmd vim:cmd-yank (motion nonrepeatable)
  "Saves the characters in motion into the kill-ring."
  (pcase (vim-motion-type motion)
    (`block (vim:cmd-yank-rectangle :motion motion))
    (`linewise (goto-line-dumb (vim-motion-first-line motion))
               (vim:cmd-yank-line :count (vim-motion-line-count motion)))
    (_
     (let ((text (buffer-substring-no-properties
                  (vim-motion-begin-pos motion)
                  (vim-motion-end-pos motion))))
       (kill-new-ignoring-duplicates text)))))

(vim-defcmd vim:yank-current-line ()
  "Function that does what \"y$\" does in vanilla vim."
  (vim:cmd-yank :motion (save-excursion (vim:motion-end-of-line))))

(vim-defcmd vim:cmd-yank-line (count nonrepeatable)
  "Saves the next count lines into the kill-ring."
  (let ((beg (line-beginning-position)))
    (save-excursion
      (forward-line (1- (or count 1)))
      (let ((txt (concat (buffer-substring beg (line-end-position)) "\n")))
        (put-text-property 0
                           (length txt)
                           'yank-handler
                           (list #'vim--yank-line-handler txt)
                           txt)
        (kill-new-ignoring-duplicates txt)))))

(vim-defcmd vim:cmd-yank-rectangle (motion nonrepeatable)
  "Stores the rectangle defined by motion into the kill-ring."
  (unless (eq (vim-motion-type motion) 'block)
    (error "Motion must be of type block"))
  (let ((begrow (vim-motion-first-line motion))
        (begcol (vim-motion-first-col motion))
        (endrow (vim-motion-last-line motion))
        (endcol (vim-motion-last-col motion))
        (parts nil))
    (goto-line-dumb endrow)
    (dotimes (_ (1+ (- endrow begrow)))
      (let ((beg (save-excursion (move-to-column begcol) (point)))
            (end (save-excursion (move-to-column (1+ endcol)) (point))))
        (push (cons (save-excursion (goto-char beg)
                                    (- (current-column-fixed) begcol))
                    (buffer-substring beg end))
              parts)
        (forward-line -1)))
    (let ((txt (mapconcat #'cdr parts "\n")))
      ;; `txt' contains the block as single lines
      (put-text-property 0 (length txt)
                         'yank-handler
                         (list #'vim--yank-block-handler
                               (cons (- endcol begcol -1) parts))
                         txt)
      (kill-new-ignoring-duplicates txt))
    (goto-line-dumb begrow)
    (move-to-column begcol)))

(defun vim--yank-line-handler (text)
  "Inserts the current text linewise."
  (beginning-of-line)
  ;; not sure if this is the right thing to do
  ;; (set-mark (point))
  (insert text))

(defun vim--yank-block-handler (text)
  "Inserts the current text as block."
  ;; TODO: yank-pop with count will not work for blocks, because
  ;; it's difficult to place (point) (or (mark)) at the correct
  ;; position since they may no exist.

  ;; We may need to undo what we did here, so prepare yourself!
  (vim--prepare-buffer-undo-list!)

  (let ((ncols (car text))
        (parts (cdr text))
        (col (current-column-fixed))
        (current-line (line-number-at-pos (point)))
        insert-newlines?
        (last-pos (point))
        (undo-at-start buffer-undo-list))

    (set-mark (point))
    (dolist (part parts)

      (let* ((offset (car part))
             (txt (cdr part))
             (len (length txt)))

        ;; maybe we have to insert a new line at eob
        (when (or insert-newlines?
                  (< (line-number-at-pos (point))
                     current-line))
          ;; Cache it
          (setf insert-newlines? t)
          (goto-char (point-max))
          (newline))
        (cl-incf current-line)

        (unless (and (< (current-column-fixed) col) ;; nothing in this line
                     (<= offset 0)
                     (zerop len) ;; and nothing to insert
                     )
          (move-to-column (+ col (max 0 offset)) t)
          (insert txt)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert-char ?\s (- ncols len))))
        (setq last-pos (point))
        (forward-line 1)))
    (goto-char last-pos)
    (exchange-point-and-mark)
    (setq yank-undo-function
          (lambda (start end)
            (when (eq buffer-undo-list t)
              (user-error "No undo information in this buffer"))
            (let ((pending (vim--copy-list-until buffer-undo-list undo-at-start)))
              (let ((undo-in-progress t))
                (while pending
                  ;; Note: The following, while pulling elements off
                  ;; `pending-undo-list' will call primitive change functions which
                  ;; will push more elements onto `buffer-undo-list'.
                  (setq pending (primitive-undo 1 pending)))))))))

(cl-defstruct (vim-paste-info
               (:constructor vim--make-paste-info))
  point  ;; point where command took place
  begin  ;; beginning of inserted region
  end    ;; end of inserted region
  count  ;; repeat count of insertion
  at-eob ;; t iff last paste-after took place at eob
  )

(defvar vim--last-paste nil
  "Information of the latest paste as a `vim-paste-info' structure.")


(defun vim--cmd-paste-get-text (counter)
  (let ((text (current-kill counter t)))
    (if text
        text
      (error "Kill-ring empty"))))

(defun vim--cmd-paste-undo! ()
  (awhen vim--last-paste
    (funcall (or yank-undo-function #'delete-region)
             (vim-paste-info-begin it)
             (vim-paste-info-end it))
    (goto-char (vim-paste-info-point it))))

(defvar vim--cmd-paste-before-impl-counter nil)

(defun vim--cmd-paste-before-impl (count)
  "Implementation of the vim’s paste before command."
  (let* ((pos (point))
         beg
         end
         (text (vim--cmd-paste-get-text vim--cmd-paste-before-impl-counter))
         (yhandler (get-text-property 0 'yank-handler text))
         (is-line-handler? (eq (car yhandler) 'vim--yank-line-handler)))
    (save-excursion
      (dotimes (_ (or count 1))
        (let ((start (if is-line-handler?
                         (line-beginning-position)
                       (point))))
          (insert-for-yank text)
          (let ((finish (point)))
            (setq beg (min start finish (or beg finish))
                  end (max start finish (or end finish)))))))
    (when is-line-handler?
      ;; Place cursor at for non-blank of first inserted line.
      (goto-char pos)
      (vim:motion-first-non-blank))
    (if vim--last-paste
        ;; Reuse the structure to reduce allocations!
        (setf (vim-paste-info-point  vim--last-paste) pos
              (vim-paste-info-begin  vim--last-paste) beg
              (vim-paste-info-end    vim--last-paste) end
              (vim-paste-info-count  vim--last-paste) count
              (vim-paste-info-at-eob vim--last-paste) nil)
      (setq vim--last-paste
            (vim--make-paste-info :point pos
                                  :begin beg
                                  :end end
                                  :count count)))))

(vim-defcmd vim:cmd-paste-before (count)
  "Pastes the latest yanked text before the cursor position."
  (if (eq last-command 'vim:cmd-paste-before:interactive)
      (progn
        (setf vim--cmd-paste-before-impl-counter
              (if vim--cmd-paste-before-impl-counter
                  (1+ vim--cmd-paste-before-impl-counter)
                1))
        (vim--cmd-paste-undo!)
        (vim--cmd-paste-before-impl (vim-paste-info-count vim--last-paste)))
    (progn
      (setf vim--cmd-paste-before-impl-counter 0)
      (vim--cmd-paste-before-impl count))))

(defvar vim--cmd-paste-after-counter nil)

(defun vim--cmd-paste-after (count adjust?)
  "Implementation of the vim’s paste behind command."
  (let ((yhandler (get-text-property 0
                                     'yank-handler
                                     (vim--cmd-paste-get-text vim--cmd-paste-after-counter)))
        (pos (point)))
    (setf vim--cmd-paste-before-impl-counter vim--cmd-paste-after-counter)
    (pcase (car yhandler)
      (`vim--yank-line-handler
       (let ((at-eob? (= (line-end-position) (point-max))))
         ;; We have to take care of the special case where we cannot
         ;; go to the next line because we reached eob.
         (forward-line)
         (when at-eob? (newline))
         (vim--cmd-paste-before-impl count)
         (when at-eob?
           ;; we have to remove the final newline and update paste-info
           (goto-char (vim-paste-info-end vim--last-paste))
           (delete-char -1)
           (setf (vim-paste-info-begin vim--last-paste)  (max (point-min)
                                                              (1- (vim-paste-info-begin vim--last-paste)))
                 (vim-paste-info-end vim--last-paste)    (1- (vim-paste-info-end vim--last-paste))
                 (vim-paste-info-at-eob vim--last-paste) t))
         (vim:motion-first-non-blank)))

      (`vim--yank-block-handler
       (forward-char)
       (vim--cmd-paste-before-impl count))

      (_
       (when (and adjust?
                  (not (eobp)))
         (forward-char))
       (vim--cmd-paste-before-impl count)
       ;; goto end of paste
       (goto-char (if adjust?
                      (1- (vim-paste-info-end vim--last-paste))
                    (vim-paste-info-end vim--last-paste)))))
    (setf (vim-paste-info-point vim--last-paste) pos)))

(vim-defcmd vim:cmd-paste-after (count)
  "Pastes the latest yanked text behind point.

This is the version for normal mode that moves cursor around as usual vim command."
  (if (or (eq last-command 'vim:cmd-paste-after:interactive)
          (eq last-command 'vim-cmd-paste-after-no-adjust))
      (progn
        (setf vim--cmd-paste-after-counter
              (if vim--cmd-paste-after-counter
                  (1+ vim--cmd-paste-after-counter)
                1))
        (vim--cmd-paste-undo!)
        (vim--cmd-paste-after count t))
    ;; Paste behind works by moving the cursor and calling
    ;; vim:cmd-paste-before afterwards. Afterwards the information of
    ;; vim--last-paste is updated.
    (progn
      (setf vim--cmd-paste-after-counter 0)
      (vim--cmd-paste-after count t))))

(defun vim-cmd-paste-after-no-adjust (count)
  "Pastes the latest yanked text behind point.

This is the version for insert mode that won’t move the cursor
around at all so that paste cycling happens at the same place in buffer."
  (interactive "p")
  (if (or (eq last-command 'vim:cmd-paste-after:interactive)
          (eq last-command 'vim-cmd-paste-after-no-adjust))
      (progn
        (setf vim--cmd-paste-after-counter
              (if vim--cmd-paste-after-counter
                  (1+ vim--cmd-paste-after-counter)
                1))
        (vim--cmd-paste-undo!)
        (vim--cmd-paste-after count nil))
    ;; Paste behind works by moving the cursor and calling
    ;; vim:cmd-paste-before afterwards. Afterwards the information of
    ;; vim--last-paste is updated.
    (progn
      (setf vim--cmd-paste-after-counter 0)
      (vim--cmd-paste-after count nil))))

(vim-defcmd vim:cmd-paste-before-and-indent (count)
  "Pastes the latest yanked text before point.
If the inserted text consists of full lines those lines are
indented according to the current mode."
  (vim:cmd-paste-before :count count)
  (let* ((txt (current-kill 0))
         (yhandler (get-text-property 0 'yank-handler txt)))
    (when (eq (car yhandler) #'vim--yank-line-handler)
      ;; We have to reindent the lines and update the paste-data.
      (let ((endln (line-number-at-pos (vim-paste-info-end vim--last-paste))))
        (indent-region (vim-paste-info-begin vim--last-paste)
                       (vim-paste-info-end vim--last-paste))
        (setf (vim-paste-info-end vim--last-paste)
              (save-excursion
                (goto-line-dumb endln)
                (line-beginning-position)))
        (vim:motion-first-non-blank)))))

(vim-defcmd vim:cmd-paste-after-and-indent (count)
  "Pastes the latest yanked text behind point.
If the inserted text consists of full lines those lines are
indented according to the current mode."
  (vim:cmd-paste-after :count count)
  (let* ((txt (current-kill 0))
         (yhandler (get-text-property 0 'yank-handler txt)))
    (when (eq (car yhandler) #'vim--yank-line-handler)
      ;; We have to reindent the lines and update the paste-data.
      (let ((endln (line-number-at-pos (vim-paste-info-end vim--last-paste))))
        (if (vim-paste-info-at-eob vim--last-paste)
            (progn
              (indent-region (1+ (vim-paste-info-begin vim--last-paste))
                             (1+ (vim-paste-info-end vim--last-paste)))
              (setf (vim-paste-info-end vim--last-paste)
                    (save-excursion
                      (goto-line-dumb endln)
                      (line-end-position))))
          (progn
            (indent-region (vim-paste-info-begin vim--last-paste)
                           (vim-paste-info-end vim--last-paste))
            (setf (vim-paste-info-end vim--last-paste)
                  (save-excursion
                    (goto-line-dumb endln)
                    (line-beginning-position)))))
        (vim:motion-first-non-blank)))))

(vim-defcmd vim:cmd-join-lines (count)
  "Join `count' lines with a minimum of two lines."
  (setf count (or count 1))
  (if (< count 0)
      (dotimes (_ (abs count))
        (split-line))
    (save-match-data
      (dotimes (_ (max 1 (1- count)))
        (when (re-search-forward "\\(\\s-*\\)\\(\n\\s-*\\)\\()?\\)")
          (delete-region (match-beginning 2)
                         (match-end 2))
          (when (and (= (match-beginning 1) (match-end 1))
                     (= (match-beginning 3) (match-end 3)))
            (insert-char ?\s 1))
          (backward-char))))))

(vim-defcmd vim:cmd-join (motion)
  "Join the lines covered by `motion'."
  (goto-line-dumb (vim-motion-first-line motion))
  (vim:cmd-join-lines :count (vim-motion-line-count motion)))

(defun vim:cmd-shift--ident (start-line end-line offset)
  (save-current-line-column
    (goto-line-dumb start-line)
    (beginning-of-line)
    (let ((i 0))
      (while (<= (+ start-line i) end-line)
        (let ((curr-indent (current-indentation))
              (whitespace-chars 0)
              (is-empty-line? nil))
          (save-excursion
            (setf whitespace-chars (skip-chars-forward " \t"))
            (setq is-empty-line? (eolp)))
          (unless is-empty-line?
              (indent-to (max 0 (+ curr-indent offset)) 0))
            (delete-region (point) (+ whitespace-chars (point))))
        (beginning-of-line)
        (cl-incf i)
        (forward-line 1)))))

(vim-defcmd vim:cmd-shift-left (motion keep-visual)
  "Shift the lines covered by `motion' leftwards."
  (when (= 0 vim-shift-width)
    (error "vim-shift-width is zero"))
  (vim:cmd-shift--ident (vim-motion-first-line motion)
                        (vim-motion-last-line motion)
                        (- vim-shift-width))
  (setf deactivate-mark nil))

(vim-defcmd vim:cmd-shift-right (motion keep-visual)
  "Shift the lines covered by `motion' rightwards."
  (when (= 0 vim-shift-width)
    (error "vim-shift-width is zero"))
  (vim:cmd-shift--ident (vim-motion-first-line motion)
                        (vim-motion-last-line motion)
                        vim-shift-width)
  (setf deactivate-mark nil))

(vim-defcmd vim:cmd-toggle-case (motion)
  "Toggles the case of all characters defined by `motion'."
  (vim--apply-on-motion
   motion
   (lambda (beg end)
     (save-excursion
       (goto-char beg)
       (while (< beg end)
         (let ((c (following-char)))
           (delete-char 1 nil)
           (insert-char (if (eq c (upcase c)) (downcase c) (upcase c)) 1)
           (setq beg (1+ beg))))))))

(vim-defcmd vim:cmd-toggle-case-one-char (count)
  "Toggles the case of a single character at point and moves the point forward."
  (interactive)
  (vim:cmd-toggle-case :motion (vim:motion-right :count count)))

(vim-defcmd vim:cmd-make-upcase (motion)
  "Upcases all characters defined by `motion'."
  (vim--apply-on-motion motion #'upcase-region))

(vim-defcmd vim:cmd-make-downcase (motion)
  "Downcases all characters defined by `motion'."
  (vim--apply-on-motion motion #'downcase-region))

(defun vim--apply-on-motion (motion func)
  "Applys `func' to the region defined my a certain `motion'.
The function `func' should take two parameters, the begin and end
position of a region on which it should be applied. Note that
`func' can be called more than once of motion covers a
non-continuous region. This usually happens for linewise and
block motions."
  (pcase (vim-motion-type motion)
    (`block
        (save-excursion
          (let ((begrow (vim-motion-first-line motion))
                (begcol (vim-motion-first-col motion))
                (endrow (vim-motion-last-line motion))
                (endcol (vim-motion-last-col motion)))
            (goto-line-dumb begrow)
            (dotimes (_ (1+ (- endrow begrow)))
              (let ((beg (save-excursion
                           (move-to-column begcol)
                           (point)))
                    (end (save-excursion
                           (move-to-column (1+ endcol))
                           (point))))
                (funcall func beg end))
              (forward-line)))))
    (`linewise
     (save-excursion
       (goto-char (vim-motion-begin-pos motion))
       (dotimes (_ (vim-motion-line-count motion))
         (funcall func (line-beginning-position) (line-end-position))
         (forward-line))))
    (_
     (funcall func (vim-motion-begin-pos motion) (vim-motion-end-pos motion))
     (goto-char (vim-motion-end-pos motion)))))

(vim-defcmd vim:cmd-repeat (count nonrepeatable)
  "Repeats the last command."
  (let ((events (vim--reify-events vim--repeat-events)))
    (vim--cmd-repeat-impl count events t)))

(defun vim--cmd-repeat-impl (count events handle-undo?)
  "Repeats the last command."
  (unless events
    (error "Nothing to repeat"))
  (vim--reset-key-state!)
  (let ((vim--repeat-events nil)
        (last-undo buffer-undo-list))
    (vim--with-clear-command-keys
      (execute-kbd-macro events count))
    (when handle-undo?
      (vim--connect-undos! last-undo))))

(vim-defcmd vim:cmd-emacs (nonrepeatable)
  "Switches to Emacs for the next command."
  (vim-notify "Switch to Emacs for the next command.")
  (vim-escape-to-emacs nil))

(defconst vim:cmd-inc-dec-at-point--numbers "0-9+\\-")

(defun vim-cmd--plus-region (delta start end)
  (cl-assert (numberp delta))
  (cl-assert (< start end))
  (let* ((sign-char (char-after start))
         (sign (pcase sign-char
                 (?+ "+")
                 ;; Minus will be automatically added for negative numbers.
                 (_  "")))
         (n (string->number (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert sign (number->string (+ delta n)))))

(vim-defcmd vim:cmd-increment-at-point (count)
  (let ((mid (point))
        start
        end)
    (skip-chars-backward vim:cmd-inc-dec-at-point--numbers)
    (setf start (point))
    (goto-char mid)
    (skip-chars-forward vim:cmd-inc-dec-at-point--numbers)
    (setf end (point))
    (if (eq start end)
        (error "No number at point")
      (vim-cmd--plus-region (or count 1) start end))))

(vim-defcmd vim:cmd-increment (count motion)
  (let ((start (vim-motion-begin-pos motion))
        (end   (vim-motion-end-pos motion)))
    (if (eq start end)
        (error "No number at point")
      (progn
        (goto-char start)
        (skip-chars-forward vim:cmd-inc-dec-at-point--numbers end)
        (if (eq (point) end)
            (vim-cmd--plus-region (or count 1) start end)
          (error "Region contains something beside number: ‘%s’"
                 (buffer-substring-no-properties start end)))))))

(vim-defcmd vim:cmd-decrement-at-point (count)
  (let ((mid (point))
        start
        end)
    (skip-chars-backward vim:cmd-inc-dec-at-point--numbers)
    (setf start (point))
    (goto-char mid)
    (skip-chars-forward vim:cmd-inc-dec-at-point--numbers)
    (setf end (point))
    (if (eq start end)
        (error "No number at point")
      (vim-cmd--plus-region (if count (- count) -1) start end))))

(vim-defcmd vim:cmd-decrement (count motion)
  (let ((start (vim-motion-begin-pos motion))
        (end   (vim-motion-end-pos motion)))
    (if (eq start end)
        (error "No number at point")
      (progn
        (goto-char start)
        (skip-chars-forward vim:cmd-inc-dec-at-point--numbers end)
        (if (eq (point) end)
            (vim-cmd--plus-region (if count (- count) -1) start end)
          (error "Region contains something beside number: ‘%s’"
                 (buffer-substring-no-properties start end)))))))

(vim-defcmd vim:narrow-to-region (nonrepeatable)
  (with-region-bounds start end
    (narrow-to-region start end)))

(provide 'vim-commands)

;; Local Variables:
;; End:

;; vim-commands.el ends here
