;;; vim-search.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.2.0
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; TODO:
;;
;;  - searching currently uses isearch.  Although this is quite powerful,
;;    it's only usuably as interactive search and difficult to use with
;;    semi-interactive stuff like the "*" command.  The current implementation
;;    using unread-command-events is quite ugly.

(provide 'vim-search)

(defconst vim:search-mode-keymap (list 'keymap '(t . vim:search-mode-exit)))

(vim:deflocalvar vim:search-last-direction nil
  "The last search direction, either 'forward or 'backward.")

(defun vim:search-mode-activate ()
  (setq cursor-type vim:normal-mode-cursor))

(defun vim:search-mode-deactivate ()
  (isearch-exit))

(vim:defcmd vim:search-mode-exit ()
  (vim:activate-normal-mode)
  (push last-command-event unread-command-events))

;; Search mode is a very special mode being activated during a search
;; command.  Its purpose is to disable highlighting of search results
;; if something else than a repeat-search event occurs.
(vim:define-mode search "VIM search mode"
                 :ident "S"
                 :keymap vim:search-mode-keymap
                 :command-function 'vim:search-mode-command
                 :activate 'vim:search-mode-activate
                 :deactivate 'vim:search-mode-deactivate)

(defun vim:search-mode-command (command)
  "Executes a simple-command in search-mode."
  (case (vim:cmd-type command)
    ('simple (vim:normal-execute-simple-command command))
    (t (error "Only simple commands allowed in search-mode."))))

(vim:defcmd vim:search-start (nonrepeatable)
  "Starts an incremental regexp search."
  (unless (vim:search-mode-p)
    (vim:activate-search-mode))
  (setq vim:last-search-direction 'forward)
  (isearch-forward t))


(vim:defcmd vim:search-start-backward (nonrepeatable)
  "Starts an incremental regexp search."
  (unless (vim:search-mode-p)
    (vim:activate-search-mode))
  (setq vim:last-search-direction 'backward)
  (isearch-backward t))


(vim:defcmd vim:search-repeat (nonrepeatable)
  "Repeats the last incremental search."
  (unless (vim:search-mode-p)
    (vim:activate-search-mode))
  (isearch-repeat vim:last-search-direction))


(vim:defcmd vim:search-repeat-opposite (nonrepeatable)
  "Starts an incremental regexp search."
  (unless (vim:search-mode-p)
    (vim:activate-search-mode))
  (isearch-repeat (case vim:last-search-direction
		    ('forward 'backward)
		    (t 'forward))))


(defun vim:start-word-search (unbounded direction)
 
  (condition-case nil
      (goto-char (vim:motion-bwd-word-end :count 1))
    (error nil))
 
  (save-excursion
    (re-search-forward (concat "\\<[" vim:word "]+\\>")))
 
  (when (eq direction 'backward)
    (goto-char (1+ (match-end 0))))
  (let ((events (reverse (append (if (eq direction 'forward)
				     "/"
				   "?")
				 (if unbounded
				     (regexp-quote (match-string 0))
				   (concat "\\<" 
					   (regexp-quote (match-string 0))
					   "\\>"))
				 [return]
				 "n"
				 nil))))
    (while events
      (push (car events) unread-command-events)
      (setq events (cdr events)))))


(vim:defcmd vim:search-word (nonrepeatable)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search nil 'forward))
   
   
(vim:defcmd vim:search-word-backward (nonrepeatable)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search nil 'backward))
   
   
(vim:defcmd vim:search-unbounded-word (nonrepeatable)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search t 'forward))
   
   
(vim:defcmd vim:search-unbounded-word-backward (nonrepeatable)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search t 'backward))
