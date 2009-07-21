;;; vim-normal-mode.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

(provide 'vim-normal-mode)

(defcustom vim:normal-mode-cursor 'box
  "The cursor-type for normal-mode."
  :group 'vim-mode)

(defconst vim:motion-keymap (vim:make-node))
(defconst vim:normal-mode-keymap
  (vim:make-node :next-keymap vim:motion-keymap))

(defun vim:normal-mode-activate ()
  (message "-- NORMAL --")
  (setq cursor-type vim:normal-mode-cursor))

(defun vim:normal-mode-deactivate ()
  )


(defconst vim:normal-mode
  (vim:make-mode :name "Normal"
                 :activate #'vim:normal-mode-activate
                 :deactivate #'vim:normal-mode-deactivate
                 :execute-command #'vim:default-mode-exec-cmd
                 :execute-motion #'vim:default-mode-exec-motion
                 :keymap 'vim:normal-mode-keymap
                 :default-handler 'vim:default-default-handler))


(defconst vim:search-mode-keymap (vim:make-node))

(vim:deflocalvar vim:search-last-direction nil
   "The last search direction, either 'forward or 'backward.")

(defun vim:search-mode-activate ()
  (setq cursor-type vim:normal-mode-cursor))

(defun vim:search-mode-deactivate ()
  (isearch-exit))

(defun vim:search-default-handler ()
  (vim:activate-mode vim:normal-mode)
  (push last-command-event unread-command-events)
  t)

;; Search mode is a very special mode being activate during a search
;; command.  Its purpose is to disable highlighting of search results
;; if something else than a repeat-search event occurs.
(defconst vim:search-mode
  (vim:make-mode :name "Search"
		 :activate #'vim:search-mode-activate
		 :deactivate #'vim:search-mode-deactivate
		 :execute-command #'vim:default-mode-exec-cmd
		 :execute-motion #'vim:default-mode-exec-motion
		 :keymap 'vim:search-mode-keymap
		 :default-handler 'vim:search-default-handler))


(vim:define vim:search-start ()
	    :type 'simple
	    :repeatable nil
	    :count nil
  "Starts an incremental regexp search."
  (unless (eq vim:active-mode vim:search-mode)
    (vim:activate-mode vim:search-mode))
  (setq vim:last-search-direction 'forward)
  (isearch-forward t))


(vim:define vim:search-start-backward ()
	    :type 'simple
	    :repeatable nil
	    :count nil
  "Starts an incremental regexp search."
  (unless (eq vim:active-mode vim:search-mode)
    (vim:activate-mode vim:search-mode))
  (setq vim:last-search-direction 'backward)
  (isearch-backward t))


(vim:define vim:search-repeat ()
	    :type 'simple
	    :repeatable nil
	    :count nil
  "Repeats the last incremental search."
  (unless (eq vim:active-mode vim:search-mode)
    (vim:activate-mode vim:search-mode))
  (isearch-repeat vim:last-search-direction))


(vim:define vim:search-repeat-opposite ()
	    :type 'simple
	    :repeatable nil
	    :count nil
  "Starts an incremental regexp search."
  (unless (eq vim:active-mode vim:search-mode)
    (vim:activate-mode vim:search-mode))
  (isearch-repeat (case vim:last-search-direction
		    ('forward 'backward)
		    (t 'forward))))

