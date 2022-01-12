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
  (require 'cl-lib)
  (require 'macro-util))

(require 'macro-util)

(require 'vim-defs)
(require 'vim-macs)
(require 'vim-core)
(require 'vim-compat)
(require 'vim-motions)

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

(vim-defcmd vim:insert-mode-exit (nonrepeatable)
  "Deactivates insert-mode, returning to normal-mode."
  (vim-activate-normal-mode)
  (goto-char (max (line-beginning-position)
                  (cond ((eq? vim--insert-mode-exit-move-point
                              'dont-move-at-line-end)
                         (if (= (point) (line-end-position))
                           (point)
                           (1- (point))))
                        (vim--insert-mode-exit-move-point
                         (1- (point)))
                        (t
                         (point))))))

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
        (let ((insert-info (vim-make-visual-insert-info
                            :first-line (vim-motion-first-line motion)
                            :last-line (vim-motion-last-line motion)
                            :column (vim-motion-first-col motion))))
          (vim:cmd-delete :motion motion)
          (vim-visual--start-insert insert-info)))

    (_
     ;; deal with cw and cW
     (when (and (not (null? vim--current-motion))
                (not (eob?))
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
  (vim:cmd-yank :motion (vim:motion-end-of-line)))

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
        (put-text-property 0
                           (length txt)
                           'vim--yank-handler
                           #'vim--yank-line-handler
                           txt)
        (kill-new-ignoring-duplicates txt)))))

(vim-defcmd vim:cmd-yank-rectangle (motion nonrepeatable)
  "Stores the rectangle defined by motion into the kill-ring."
  (unless (eq (vim-motion-type motion) 'block)
    (error "Motion must be of type block"))
  ;; TODO: yanking should not insert spaces or expand tabs.
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
                                    (- (current-column) begcol))
                    (buffer-substring beg end))
              parts)
        (forward-line -1)))
    (let ((txt (mapconcat #'cdr parts "\n")))
      ;; `txt' contains the block as single lines
      (put-text-property 0 (length txt)
                         'yank-handler
                         (list #'vim--yank-block-handler
                               (cons (- endcol begcol -1) parts)
                               nil
                               #'delete-rectangle)
                         txt)
      (put-text-property 0 (length txt)
                         'vim--yank-handler
                         #'vim--yank-block-handler
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
  (let ((ncols (car text))
        (parts (cdr text))
        (col (current-column))
        (current-line (line-number-at-pos (point)))
        (last-pos (point)))

    (set-mark (point))
    (dolist (part parts)

      (let* ((offset (car part))
             (txt (cdr part))
             (len (length txt)))

        ;; maybe we have to insert a new line at eob
        (when (< (line-number-at-pos (point))
                 current-line)
          (goto-char (point-max))
          (newline))
        (cl-incf current-line)

        (unless (and (< (current-column) col)   ; nothing in this line
                     (<= offset 0) (zerop len)) ; and nothing to insert
          (move-to-column (+ col (max 0 offset)) t)
          (insert txt)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert (make-string (- ncols len) ? ))))
        (setq last-pos (point))
        (forward-line 1)))
    (goto-char last-pos)
    (exchange-point-and-mark)))

(cl-defstruct (vim-paste-info
               (:constructor vim--make-paste-info))
  point   ;; point where command took place
  begin   ;; beginning of inserted region
  end     ;; end of inserted region
  count   ;; repeat count of insertion
  at-eob  ;; t iff last paste-after took place at eob
  )

(defvar vim--last-paste nil
  "Information of the latest paste as a `vim-paste-info' structure.")


(defun vim--cmd-paste-get-text (counter)
  (let ((text (current-kill counter t)))
    (if text
        text
      (error "Kill-ring empty"))))

(defun vim--cmd-paste-undo! ()
  (when vim--last-paste
    (funcall (or yank-undo-function #'delete-region)
             (vim-paste-info-begin vim--last-paste)
             (vim-paste-info-end vim--last-paste))
    (goto-char (vim-paste-info-point vim--last-paste))))

(defvar vim--cmd-paste-before-counter nil)

(defun vim--cmd-paste-before (count)
  "Implementation of the vim’s paste before command."
  (let ((pos (point))
        beg
        end
        (text (vim--cmd-paste-get-text vim--cmd-paste-before-counter)))
    (save-excursion
      (dotimes (_ (or count 1))
        (let ((start (point)))
          (insert-for-yank text)
          (let ((finish (point)))
            (setq beg (min start finish (or beg finish))
                  end (max start finish (or end finish)))))))
    (let ((yhandler (get-text-property 0 'vim--yank-handler text)))
      (when (eq yhandler 'vim--yank-line-handler)
        ;; Place cursor at for non-blank of first inserted line.
        (goto-char pos)
        (vim:motion-first-non-blank)))
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
        (setf vim--cmd-paste-before-counter
              (if vim--cmd-paste-before-counter
                  (1+ vim--cmd-paste-before-counter)
                1))
        (vim--cmd-paste-undo!)
        (vim--cmd-paste-before (vim-paste-info-count vim--last-paste)))
    (progn
      (setf vim--cmd-paste-before-counter 0)
      (vim--cmd-paste-before count))))

(defvar vim--cmd-paste-after-counter nil)

(defun vim--cmd-paste-after (count adjust?)
  "Implementation of the vim’s paste behind command."
  (let ((yhandler (get-text-property 0
                                     'vim--yank-handler
                                     (vim--cmd-paste-get-text vim--cmd-paste-after-counter)))
        (pos (point)))
    (setf vim--cmd-paste-before-counter vim--cmd-paste-after-counter)
    (pcase yhandler
      (`vim--yank-line-handler
       (let ((at-eob? (= (line-end-position) (point-max))))
         ;; We have to take care of the special case where we cannot
         ;; go to the next line because we reached eob.
         (forward-line)
         (when at-eob? (newline))
         (vim--cmd-paste-before count)
         (when at-eob?
           ;; we have to remove the final newline and update paste-info
           (goto-char (vim-paste-info-begin vim--last-paste))
           (delete-char -1)
           (setf (vim-paste-info-begin vim--last-paste)  (max (point-min)
                                                              (1- (vim-paste-info-begin vim--last-paste)))
                 (vim-paste-info-end vim--last-paste)    (1- (vim-paste-info-end vim--last-paste))
                 (vim-paste-info-at-eob vim--last-paste) t))
         (vim:motion-first-non-blank)))

      (`vim--yank-block-handler
       (forward-char)
       (vim--cmd-paste-before count))

      (_
       (when (and adjust?
                  (not (eob?)))
         (forward-char))
       (vim--cmd-paste-before count)
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
         (yhandler (get-text-property 0 'vim--yank-handler txt)))
    (when (eq (car-safe yhandler) 'vim--yank-line-handler)
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
         (yhandler (get-text-property 0 'vim--yank-handler txt)))
    (when (eq yhandler 'vim--yank-line-handler)
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

(vim-defcmd vim:cmd-shift-left (motion)
  "Shift the lines covered by `motion' leftwards."
  (save-current-line-column
   (goto-line-dumb (vim-motion-first-line motion))
   (indent-rigidly (line-beginning-position)
                   (line-end-position (vim-motion-line-count motion))
                   (- vim-shift-width))))

(vim-defcmd vim:cmd-shift-right (motion)
  "Shift the lines covered by `motion' rightwards."
  (save-current-line-column
   (goto-line-dumb (vim-motion-first-line motion))
   (indent-rigidly (line-beginning-position)
                   (line-end-position (vim-motion-line-count motion))
                   vim-shift-width)))

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
  (unless vim--repeat-events
    (error "Nothing to repeat"))
  (vim--reset-key-state!)
  (let ((repeat-events vim--repeat-events)
        vim--repeat-events
        vim--current-key-sequence)
    (execute-kbd-macro repeat-events count)))

(vim-defcmd vim:cmd-emacs (nonrepeatable)
  "Switches to Emacs for the next command."
  (vim-notify "Switch to Emacs for the next command.")
  (vim-escape-to-emacs nil))

(defvar vim--current-macro nil
  "The name of the currently recorded macro.")

(defvar vim--fresh-macro-id 0
  "Numeric id for a macro name that has not been used yet.")

(defvar vim--defined-macro-names nil
  "Linked list of cons cells (<bool> . <name>) macro names in reverse order of definition.

<bool> refers to whether a name was provided by the user (‘t’) or made up (‘nil’).
If <bool> is ‘t’ then <name> is a string, otherwise it’s an integer obtained from ‘vim:fresh-macro-id’.")

(defvar vim--macro-definitions (make-hash-table :test #'equal)
  "Hash table mapping string names to macro definitions.")

(defun vim--fresh-macro-name ()
  "Return macro name that has not been used yet."
  (prog1 vim--fresh-macro-id
    (cl-incf vim--fresh-macro-id)))

(defvar vim--macro-names-history nil)

(defun vim-cmd-toggle-macro-recording (ask-for-a-name?)
  (interactive "P")
  (if vim--current-macro
      (progn
        (vim--cmd-stop-macro (cdr vim--current-macro))
        (setf vim--defined-macro-names (cons vim--current-macro vim--defined-macro-names)
              vim--current-macro nil))
    (let ((name (if ask-for-a-name?
                    (read-string "Macro name: " nil 'vim--macro-names-history)
                  (vim--fresh-macro-name))))
      (when (gethash name vim--macro-definitions nil)
        (unless (y-or-n-p (format "Macro with name ‘%s’ is already defined. Overwrite?" name))
          (error "Refusing to overwrite")))
      (setf vim--current-macro (cons ask-for-a-name? name))
      (vim--cmd-start-macro name))))

(defun vim--cmd-start-macro (name)
  "Starts recording a macro with name ‘name’."
  (start-kbd-macro nil)
  (vim-notify "Start recording keyboard macro named '%s'" name))

(defun vim--cmd-stop-macro (name)
  "Stops recording of a macro."
  (end-kbd-macro)
  (vim-notify "Stop recording keyboard macro")
  (puthash name last-kbd-macro vim--macro-definitions))

(defun vim--render-macro-names (entries)
  (let* ((res (cons nil nil))
         (tmp res)
         (n 0)
         (name-mapping nil))
    (dolist (entry entries)
      (cl-assert (consp entry))
      (let ((name
             (if (car entry)
                 (progn
                   (cl-assert (stringp (cdr entry)))
                   (cdr entry))
               (progn
                 (cl-assert (numberp (cdr entry)))
                 (let* ((real-name (cdr entry))
                        (user-name (if (= n 0)
                                       "latest"
                                     (concat "latest~" (number->string n)))))
                   (while (gethash user-name vim--macro-definitions)
                     (setf user-name (concat user-name " (automatic name)")))
                   (setf name-mapping (cons (cons user-name real-name) name-mapping)
                         n (+ n 1))
                   user-name)))))
        (setf tmp (setcdr-sure tmp (cons name nil)))))
    (cons (cdr res) name-mapping)))

(defvar vim--executed-macro-names-history nil)

(defun vim-cmd-execute-macro ()
  "Executes either the last defined keyboard macro or asks user to pick one if universal
argument was *explicitly* provided."
  (interactive)
  (unless vim--defined-macro-names
    (error "Nothing to execute: no macros defined"))
  (let ((count (cond
                 ((numberp current-prefix-arg)
                  current-prefix-arg)
                 (t
                  1))))
    (vim--reset-key-state!)
    (let* ((name-for-user nil)
           (macro (if vim--current-universal-argument-provided?
                      (cl-destructuring-bind (rendered . name-mapping) (vim--render-macro-names vim--defined-macro-names)
                        (let* ((name (completing-read "Macro name: " rendered nil t nil 'vim--executed-macro-names-history))
                               (real-name (or (cdr (assoc name name-mapping))
                                              name)))
                          (cl-assert (not (null name)))
                          (cl-assert (not (null real-name)))
                          (setf name-for-user real-name)
                          (gethash real-name
                                   vim--macro-definitions
                                   nil)))
                    (let ((name (cdar vim--defined-macro-names)))
                      (cl-assert (not (null name)))
                      (setf name-for-user name)
                      (gethash name
                               vim--macro-definitions
                               nil)))))
      (unless macro
        (error "Macro ‘%s’ is not defined" name-for-user))
      (execute-kbd-macro macro count))))

(defvar vim--universal-argument-provided? nil)
(defvar vim--current-universal-argument-provided? nil)

(defun vim-universal-argument-minus (arg)
  "Wrapper around `universal-argument-minus' that does the necessary
bookkeeping to maintain `vim--current-key-sequence' in order. That is needed
to make `vim:cmd-repeat' and visual block mode work as expected."
  (interactive "P")
  ;; (call-interactively #'universal-argument-minus arg)
  (call-interactively #'negative-argument arg)
  (vim--remember-this-command-keys!))

(defun vim-digit-argument (arg)
  "Wrapper around `digit-argument' that does the necessary bookkeeping to
maintain `vim--current-key-sequence' in order. That is needed to make
`vim:cmd-repeat' and visual block mode work as expected."
  (interactive "P")
  (call-interactively #'digit-argument arg)
  (vim--remember-this-command-keys!))

(define-key universal-argument-map [remap digit-argument] 'vim-digit-argument)
(define-key universal-argument-map [?-] nil)
(define-key universal-argument-map (kbd "C--") 'vim-universal-argument-minus)
(define-key universal-argument-map [kp-subtract] 'vim-universal-argument-minus)

(provide 'vim-commands)

;; Local Variables:
;; End:

;; vim-commands.el ends here
