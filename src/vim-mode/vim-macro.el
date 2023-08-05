;; vim-macro.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 14 January 2022
;; Description:
;;
;; Working with macro definitions from vim: define new, execute and
;; edit existing.

(eval-when-compile
  (require 'macro-util))

(require 'edmacro)

(require 'append-list)
(require 'vim-core)

;;;; Define new macros and execute existing ones

(defvar vim--current-macro nil
  "The name of the currently recorded macro.")

(defvar vim--fresh-macro-id 0
  "Numeric id for a macro name that has not been used yet.")

(defvar vim--defined-macro-names nil
  "Linked list of cons cells (<bool> . <name>) macro names in reverse order of definition.

<bool> refers to whether a name was provided by the user (‘t’) or made up (‘nil’).
If <bool> is ‘t’ then <name> is a string, otherwise it’s an integer obtained from ‘vim:fresh-macro-id’.")

(cl-defstruct vim-macro-def
  keys     ;; Typicaly vector of keypresses that can be executed
  detailed ;; either nil or string obtained from ‘edmacro-format-keys’ and potentially modified by the user
  )

(defvar vim--macro-definitions (make-hash-table :test #'equal)
  "Hash table mapping string names to ‘vim-macro-def’ structures.")

(defun vim--fresh-macro-name ()
  "Return macro name that has not been used yet."
  (prog1 vim--fresh-macro-id
    (cl-incf vim--fresh-macro-id)))

(defvar vim--macro-names-history nil)

(defun vim-cmd-toggle-macro-recording (ask-for-a-name?)
  (interactive "P")
  (if vim--current-macro
      (unwind-protect
          (vim--cmd-stop-macro vim--current-macro)
        (setf vim--current-macro nil))
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

(defun vim--cmd-stop-macro (full-name)
  "Stops recording of a macro."
  (end-kbd-macro)
  (vim-notify "Stop recording keyboard macro")
  (vim-macro--add-new-definition! full-name last-kbd-macro))

(defun vim-macro--add-new-definition! (full-name keys)
  (cl-assert (consp full-name))
  (push full-name vim--defined-macro-names)
  (puthash (cdr full-name)
           (make-vim-macro-def
            :keys keys
            :detailed nil)
           vim--macro-definitions))

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
           (ask-for-name? vim--current-universal-argument-provided?)
           (macro-def
            (if ask-for-name?
                (cl-destructuring-bind (rendered . name-mapping) (vim--render-macro-names vim--defined-macro-names)
                  (let* ((name (completing-read "Macro name: " rendered nil t nil 'vim--executed-macro-names-history))
                         (real-name (or (cdr (assoc name name-mapping))
                                        name)))
                    (cl-assert name)
                    (cl-assert real-name)
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
      (unless macro-def
        (error "Macro not defined: %s" name-for-user))
      (vim--prepare-buffer-undo-list!)
      (let ((last-undo buffer-undo-list)
            (keys (vim-macro-def-keys macro-def)))
        ;; Protect against overwrites by the macro.
        (let ((vim--repeat-events nil))
          (execute-kbd-macro keys count))
        (when ask-for-name?
          (vim--overwrite-repeat-events-with-keys-vector! keys))
        (vim--connect-undos! last-undo)))))

(defun vim-cmd-edit-macro ()
  "Executes either the last defined keyboard macro or asks user to pick one if universal
argument was *explicitly* provided."
  (interactive)
  (vim-cmd-edit-macro--impl nil))

(defun vim-cmd-edit-macro-other-window ()
  "Executes either the last defined keyboard macro or asks user to pick one if universal
argument was *explicitly* provided."
  (interactive)
  ;; todo:
  ;; 1. restore window configuration
  ;; 2. store textual macro definitions alongside compiled macro vectors
  ;; so that I can keep some parts commented between two editing operations.
  (vim-cmd-edit-macro--impl t))

(defun vim-cmd-edit-macro--impl (other-window?)
  (unless vim--defined-macro-names
    (error "Nothing to edit: no macros defined"))
  (cl-destructuring-bind (rendered . name-mapping) (vim--render-macro-names vim--defined-macro-names)
    (let* ((name (completing-read "Macro name: " rendered nil t nil 'vim--executed-macro-names-history))
           (real-name (or (cdr (assoc name name-mapping))
                          name)))
      (cl-assert name)
      (cl-assert real-name)
      (let ((def (gethash real-name vim--macro-definitions nil)))
        (unless def
          (error "Macro not defined: %s" real-name))

        (vim-edmacro-start real-name name def other-window?)))))

;;;; Enhance tracking of whether universal argument was supplied

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

;;;; vim-edmacro-mode

(defvar-local vim-edmacro--macro-name nil
  "Name of the macro we’re currently editing.")

(defvar-local vim-edmacro--macro-pretty-name nil
  "Name of the macro suitable for showing to the user.")

(defvar-local vim-edmacro--window-config nil)

(defvar-local vim-edmacro--macro-to-edit nil
  "Value of ‘vim-macro-def’ within ‘vim--macro-definitions’ that we will
mutate when finishing the edit.")

(defvar vim-edmacro-mode-map
  (let ((keymap (make-sparse-keymap)))
    (def-keys-for-map keymap
      ("C-c C-c" vim-edmacro-finish)
      ("C-c C-q" edmacro-insert-key)
      ("C-c C-k" vim-edmacro-cancel))
    keymap))

(defvar vim-edmacro-mode-syntax-table
  (let ((tbl (make-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\n ">" tbl)
    (modify-syntax-entry ?\; "<" tbl)
    tbl))

;;;###autoload
(define-derived-mode vim-edmacro-mode text-mode "Edit macro"
  "Major mode for editing macro definitions."
  ;; Fringe line tracking.
  (when (bound-and-true-p linum-mode)
    (linum-mode -1))
  (font-lock-mode 1)

  (setq-local comment-start ";; "
              comment-start-skip ";+ *"
              indent-tabs-mode nil
              font-lock-defaults '(nil
                                   nil ;; perform syntactic fontification (e.g. strings, comments)
                                   )))

(defun vim-edmacro-start (macro-name macro-pretty-name macro-def other-window?)
  "Initiate macro editing session."
  (cl-assert (vim-macro-def-p macro-def))
  (let ((buf (get-buffer-create (concat "macro-editing:" macro-pretty-name)))
        (win-conf (when other-window?
                    (current-window-configuration))))
    (with-current-buffer
        (funcall (if other-window?
                     #'switch-to-buffer-other-window
                   #'switch-to-buffer)
                 buf)
      (read-only-mode -1)
      (erase-buffer)
      (aif (vim-macro-def-detailed macro-def)
          (progn
            (cl-assert (stringp it))
            (insert it))
        (insert ";; Editing macro ‘" macro-pretty-name "’\n"
                ";; Finish editing with "
                (propertize "C-c C-c" 'face 'help-key-binding)
                ", insert current key with "
                (propertize "C-c C-q" 'face 'help-key-binding)
                ", cancel with "
                (propertize "C-c C-k" 'face 'help-key-binding)
                "\n\n"
                (edmacro-format-keys (vim-macro-def-keys macro-def) t)))
      (vim-edmacro-mode)
      (setq-local vim-edmacro--macro-name macro-name
                  vim-edmacro--macro-pretty-name macro-pretty-name
                  vim-edmacro--macro-to-edit macro-def
                  vim-edmacro--window-config win-conf)
      (set-buffer-modified-p nil))))

(defun vim-edmacro-cancel ()
  "Cancel editing current macro."
  (interactive)
  (kill-buffer (current-buffer)))

(defun vim-edmacro-finish ()
  "Finish editing current macro."
  (interactive)
  (let* ((str (buffer-substring (point-min) (point-max)))
         (new-keys (edmacro-parse-keys str))
         (win-conf vim-edmacro--window-config))
    (cl-assert (and vim-edmacro--macro-to-edit
                    (vim-macro-def-p vim-edmacro--macro-to-edit)))
    (setf (vim-macro-def-keys vim-edmacro--macro-to-edit) new-keys
          (vim-macro-def-detailed vim-edmacro--macro-to-edit) str)
    (message "Updated macro ‘%s’" vim-edmacro--macro-pretty-name)
    (kill-buffer (current-buffer))
    ;; Restore window configuration.
    (awhen win-conf
      (set-window-configuration it))))


(provide 'vim-macro)

;; Local Variables:
;; End:

;; vim-macro.el ends here
