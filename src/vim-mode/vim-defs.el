;; vim-defs.el - Core variables. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:


(defvar-local vim--current-register nil
  "The register of the current command.")

(defvar-local vim--current-cmd-count nil
  "The count of the current command.")

(defvar-local vim--current-cmd nil
  "The node of the current command.")

(defvar-local vim--current-cmd-arg nil
  "The argument of the current command.")

(defvar-local vim--current-motion-count nil
  "The count of the current motion.")

(defvar-local vim--current-motion nil
  "The node of the current motion.")

(defvar-local vim--current-motion-arg nil
  "The argument of the current motion.")

(defvar-local vim--current-motion-type nil
  "The type of the current motion (inclusive, exclusive,
  linewise).")

(defvar-local vim--current-force-motion-type nil
  "The forced type of the motion of current command (inclusive,
  exclusive, linewise).")


(defvar vim--repeat-events nil
  "List of vectors each denoting part of key sequence for the repeat command.")

(defvar vim--reified-repeat-events nil
  "Cache for reification of ‘vim--repeat-events’ to speed up successive ‘vim:cmd-repeat’.")

(defvar-local vim--current-key-sequence nil
  "The key-sequence of the current command. Mostly filled in by the
insert mode from ‘pre-command-hook’.")

(provide 'vim-defs)

;; Local Variables:
;; End:

;; vim-defs.el ends here
