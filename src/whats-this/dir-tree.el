;;; dir-tree.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 16 November 2011
;; Keywords:
;; Requirements:
;; Status: Not ready yet (alpha stage?). Presently it doesn't displays candidates as I want

(require 'custom)

(defvar *dir-tree-root* "/home/sergey/emacs/src/Vim-mode/" ;;nil
  "Root of directory hierarchy to perform search in.")

(defvar *dir-tree-cache* nil
  "Cache of `*dir-tree-root*' directory.")

;; (dir-tree-repropertize-paths
 ;; (get-directory-contents-rec "/home/sergey/emacs/src/Vim-mode/"
                             ;; nil
                             ;; #'(lambda (p) (string-match-p ".+\\.el$" p))
                             ;; #'(lambda (p)
                                 ;; (not (string-match-p
                                       ;; "^\\(CVS\\|\\.svn\\|\\.git\\|\\.bzr\\|\\.hg\\|_darcs\\)$"
                                       ;; p)))))
;;


(defun dir-tree-divide-paths (paths)
  (loop
    for p in paths
    collect
    (cons (file-name-nondirectory p)
          p)))


(defun dir-tree-locate ()
  "Read filename with completion of candidates from
`*dir-tree-root*' and open it."
  (interactive)
  (message "YOU CHOOSE: %S"
           (icicle-completing-read "> "
                                   (dir-tree-repropertize-paths
                                    (sort
                                     (get-directory-contents-rec
                                      "/home/sergey/emacs/src/"
                                      nil
                                      #'(lambda (p) (string-match-p ".+\\.el$" p))
                                      #'(lambda (p)
                                          (not (string-match-p
                                                "^\\(CVS\\|\\.svn\\|\\.git\\|\\.bzr\\|\\.hg\\|_darcs\\)$"
                                                p))))
                                     #'string<)))))


;;; dir-tree.el ends here
