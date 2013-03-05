;; tagged-buffers.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:


(require 'datastructures)
(require 'keys-def)

;;;; internal functions

(defstruct buffer-tag
  name
  predicate)

(defmacro* make-buf-tag-pred (&key major-modes
                                   name-regexp
                                   or-expr-in-buffer
                                   and-expr-in-buffer)
  (let ((buf-var (gensym)))
    (assert (or major-modes name-regexp or-expr-in-buffer an-expr-in-buffer) nil
            "at least one argument must be non-nil")
    `(lambda (,buf-var)
       (with-current-buffer ,buf-var
         (and (or
               ,@(remq nil
                       (list (when major-modes
                               `(memq major-mode ,major-modes))
                             (when name-regexp
                               `(string-match-pure? ,name-regexp
                                                    (buffer-name ,buf-var)))
                             (when or-expr-in-buffer
                               `(progn ,or-expr-in-buffer)))))
              ,@(remq nil
                      (list (when and-expr-in-buffer
                              `(progn ,and-expr-in-buffer)))))))))

(defun tagged-buflist/buffer-tag< (tag1 tag2)
  (string< (buffer-tag-name tag1) (buffer-tag-name tag2)))

(defconst +common-buffer-tags+
          (mapcar (lambda (entry)
                    (make-buffer-tag :name (first entry) :predicate (cadr entry)))
                  (list
           (list "Clojure"
                         (make-buf-tag-pred
                          :major-modes '(clojure-mode
                                         nrepl-mode
                                         nrepl-popup-buffer-mode
                                         nrepl-macroexpansion-minor-mode
                                         nrepl-interaction-mode
                                         nrepl-popup-buffer-mode)
                          :or-expr-in-buffer
                          (and (string-match-pure? "^\\*.*nrepl.*\\*$" (buffer-name))
                               (memq major-mode (text-mode
                                                 fundamental-mode)))))
           (list "Lisp"
                         (make-buf-tag-pred
                          :major-modes '(cl-mode lisp-mode common-lisp-mode)
                          :and-expr-in-buffer (not (string-match-pure? "^\\*.+\\*$"
                                                                       (buffer-name)))))
           (list "Slime"
                         (make-buf-tag-pred
                          :major-modes '(slime-repl-mode sldb-mode)
                          :name-regexp (rx "*"
                                           (or (seq (or "slime-repl"
                                                        "sldb")
                                                    (+ " ")
                                                    (or "sbcl"
                                                        "sbcl-full"
                                                        "cmucl"
                                                        "clisp"
                                                        "ccl"
                                                        "ecl"
                                                        "clozure"
                                                        "lisp"
                                                        "scheme"
                                                        "chicken"
                                                        "bigloo"
                                                        "scheme48"
                                                        "guile"
                                                        "gambit"
                                                        "gauche"
                                                        "mit")
                                                    (? "/"
                                                       (+ digit)))
                                               (or "slime-events"
                                                   "slime-description"
                                                   "slime-trace"
                                                   "slime-compilation"
                                                   "slime-xref"
                                                   "slime-apropos"
                                                   "slime-inspector"
                                                   "slime-macroexpansion"
                                                   "inferior-lisp"
                                                   "lisp-interaction"
                                                   "fuzzy completions"))
                                           "*"
                                           ;; (? "<"
                                           ;;    (+ digit)
                                           ;;    ">")
                                           )))
           (list "Emacs Lisp"
                         (make-buf-tag-pred
                          :major-modes '(emacs-lisp-mode
                                         inferior-emacs-lisp-mode)
                          :and-expr-in-buffer (not (string-match-pure? "^\\*.+\\*$"
                                                                       (buffer-name)))))
           (list "Scheme"
                         (make-buf-tag-pred
                          :major-modes '(scheme-mode)
                          :name-regexp (rx (or (seq "*"
                                                    (? (or "chicken"
                                                           "bigloo"
                                                           "scheme48"
                                                           "guile"
                                                           "gambit"
                                                           "gauche"
                                                           "mit")
                                                       "-")
                                                    "scheme*")
                                               "* Guile REPL *")
                                           (? "<" (+ digit) ">"))))
           (list "C/C++"
                         (make-buf-tag-pred
                          :major-modes '(c-mode c++-mode glsl-mode)))
           (list "Haskell"
                         (make-buf-tag-pred
                          :major-modes '(haskell-mode
                                         inferior-haskell-mode
                                         inferior-hugs-mode
                                         haskell-hugs-mode
                                         ghc-core-mode
                                         hugs-mode)
                          :name-regexp (rx "*haskell*"
                                           (? "<" (+ digit) ">"))))
           (list "Prolog"
                         (make-buf-tag-pred
                          :major-modes '(prolog-mode)
                          :name-regexp (rx "*prolog*"
                                           (? "<" (+ digit) ">"))))
                   (list "octave"
                         (make-buf-tag-pred
                          :major-modes '(octave-mode inferior-octave-mode)
                          :name-regexp (rx "*[oO]ctave*"
                                           (? "<" (+ digit) ">"))))
           (list "Python"
                         (make-buf-tag-pred
                          :major-modes '(python-mode
                                         python-repl-mode
                                         inferior-python-mode
                                         python-run-mode)
                          :name-regexp (rx (or "*[pP]ython*"
                                               "*IPython*"
                                               "*Python Output*")
                                           (? "<" (+ digit) ">"))))
           (list "Cython"
                         (make-buf-tag-pred
                          :major-modes '(cython-mode
                                         cython-compilation-mode)))
           (list "Maxima"
                         (make-buf-tag-pred
                          :major-modes '(maxima-mode
                                         maxima-noweb-mode
                                         inferior-maxima-mode)
                          :name-regexp (rx (or "*maxima*"
                                               "*imaxima*")
                                           (? "<"
                                              (+ digit)
                                              ">"))))
           (list "Org"
                         (make-buf-tag-pred
                          :major-modes '(org-mode
                                         org-agenda-mode
                                         diary-mode
                                         calendar-mode)))
           (list "Book"
                         (make-buf-tag-pred
                          :major-modes '(doc-view-mode)
                          :name-regexp (rx bol
                                           (+ anything)
                                           (or ".pdf"
                                               ".djvu"
                                               ".ps"
                                               ".dvi")
                                           eol)))
           (list "Latex"
                         (make-buf-tag-pred
                          :major-modes '(latex-mode tex-mode LaTeX-mode)))
           (list "Web"
                         (make-buf-tag-pred
                          :major-modes '(html-mode
                                         sgml-mode
                                         nxhtml-mode
                                         nxhtml-muamo-mode
                                         nxml-mode
                                         css-mode
                                         js-mode
                                         django-nxhtml-mumamo-mode
                                         django-html-mumamo-mode
                                         rnc-mode)))
           (list "Java"
                         (make-buf-tag-pred
                          :major-modes '(java-mode)))
                   (list "vc"
                         (make-buf-tag-pred
                          :major-modes '(magit-mode
                                         magit-commit-mode
                                         magit-diff-mode
                                         magit-key-mode
                                         magit-log-edit-mode
                                         magit-log-mode
                                         magit-reflog-mode
                                         magit-show-branches-mode
                                         magit-stash-mode
                                         magit-status-mode
                                         magit-wazzup-mode
                                         gitignore-mode)
                          :name-regexp (rx bol "*magit" (* nonl) "*" eol)))
           (list "lowlevel programming"
                         (make-buf-tag-pred
                          :major-modes '(asm-mode
                                         llvm-mode
                                         tablegen-mode)))
           (list "other programming"
                         (make-buf-tag-pred
                          :major-modes '(makefile-mode
                                         makefile-automake-mode
                                         makefile-gmake-mode
                                         makefile-makepp-mode
                                         makefile-bsdmake-mode
                                         makefile-imake-mode
                                         cmake-mode
                                         shell-script-mode
                                         sh-mode
                                         sh-script-mode
                                         conf-space-mode
                                         conf-mode
                                         conf-xdefaults-mode
                                         conf-unix-mode
                                         conf-colon-mode
                                         conf-javaprop-mode
                                         conf-ppd-mode
                                         conf-windows-mode
                                         lua-mode
                                         autoconf-mode)
                          :name-regexp (rx bol
                                           (or "makefile"
                                               "Makefile"
                                               "GNUMakefile")
                                           eol)))
                   (list "utility"
                         (make-buf-tag-pred
                          :major-modes '(comint-mode
                                         compilation-mode
                                         grep-mode
                                         latex-compilation-mode
                                         haskell-compilation-mode
                                         hs-lint-mode
                                         hs-scan-mode
                                         gnuplot-run-mode
                                         eshell-mode
                                         shell-mode)
                          :name-regexp (rx bol (or "*Tags List*") eol)
                          :or-expr-in-buffer (get-buffer-process (current-buffer))))
                   (list "dired"
                         (make-buf-tag-pred
                          :major-modes '(dired-mode)))
                   (list "other"
                         (make-buf-tag-pred
                          :major-modes '(help-mode
                                         apropos-mode
                                         Info-mode
                                         Man-mode
                                         ibuffer-mode)
                          :name-regexp (rx bol
                                           (or "*scratch*"
                                               "*Messages*"
                                               "*Pp Eval Output*"
                                               "*Backtrace*")
                                   eol))))))



(defun tagged-buflist/generate-tag-group-by-git-repository-root ()
  "Create tag group specification based on each buffer's git repository root."
  (unless *have-git?*
    (error "No git installed on the system"))
  (let ((roots (remove-duplicates
                (delq nil
                      (mapcar (lambda (buf)
                                (with-current-buffer buf
                                  (git-update-file-repository)
                                  git-repository))
                              (buffer-list)))
                :test #'string=)))
    (append
     (sort
     (mapcar (lambda (repo-root)
               (let ((root repo-root))
                 (make-buffer-tag
                  :name (concat "git:" repo-root)
                  :predicate
                  (make-buf-tag-pred
                   :or-expr-in-buffer
                   (progn (git-update-file-repository)
                          (string= root git-repository))))))
             roots)
      #'tagged-buflist/buffer-tag<)
     (list (make-buffer-tag
            :name "no git repository"
            :predicate (lambda (buf)
                         (with-current-buffer buf
                           (git-update-file-repository)
                           (null git-repository))))))))



(defstruct tagged-buffer
  buf
  tags
  selected)

(defun tagged-buffer-name (tagged-buf)
  (buffer-name (tagged-buffer-buf tagged-buf)))


(defun tagged-buflist/user-buffers ()
  (filter (lambda (buf)
            (and (not (string-match-pure? "^ " (buffer-name buf)))
                 (not (invisible-buffer? buf))))
          (buffer-list)))

(defun tagged-buflist/tagged-buffers (tags)
  (mapcar (lambda (buf)
            (make-tagged-buffer
             :buf buf
             :tags (sorted-set/from-list
                    (filter (lambda (tag)
                              (assert (buffer-tag-p tag) nil
                                      "Tag should be of buffer-tag type, %s" tag)
                              (funcall (buffer-tag-predicate tag) buf))
                            tags)
                    #'tagged-buflist/buffer-tag<)
             :selected nil))
          (tagged-buflist/user-buffers)))

(defun* tagged-buflist/buffers-matching-tagset (tagged-buflist
                                                tag-set
                                                &key (exact nil))
  (filter (lambda (buf)
            ;; (message "buffer %s: intersecting %S and %S"
            ;;          (tagged-buffer-buf buf)
            ;;          (mapcar #'buffer-tag-name (sorted-set-items (tagged-buffer-tags buf)))
            ;;          (mapcar #'buffer-tag-name (sorted-set-items tag-set)))
            ;; (not (sorted-set/empty? (sorted-set/intersection (tagged-buffer-tags buf)
            ;;                                                  tag-set)))
            (= (sorted-set/length (sorted-set/intersection (tagged-buffer-tags buf)
                                                           tag-set))
               (if exact
                 ;; (sorted-set/length (tagged-buffer-tags buf))
                 (sorted-set/length tag-set)
               (min (sorted-set/length (tagged-buffer-tags buf))
                      (sorted-set/length tag-set)))))
          tagged-buflist))

(defvar *tagged-buffers-depth-faces*
  [outline-1 outline-2
   outline-3 outline-4
   outline-5 outline-6
   outline-7 outline-8])

(defvar *tagged-buffers-name-length* 32
  "Number of characters to reserve for buffer name. If buffer name exceeds this
limit it will extend past it and affect any text following.")

(defun* tagged-buflist/render-recursively (tagged-buflist
                            unexpanded-tag-hierarchy-def
                            &key
                            (sort-predicate
                             (lambda (a b)
                               (string< (tagged-buffer-name a)
                                                       (tagged-buffer-name b))))
                                           (add-full-buffer-names nil))
  "SPEC - list of tag specifications as understood by `buffer-tags'"
  (let ((tag-hierarchy-def (mapcar #'tagged-buflist/expand-tag-definitions
                                   unexpanded-tag-hierarchy-def)))
    (letrec ((render-buffers-matching-tag-set
               (lambda (tag-set depth)
                 (let ((buffers (sort (tagged-buflist/buffers-matching-tagset tagged-buflist
                                                                              tag-set
                                                                              :exact t)
                                      sort-predicate)))
                   ;; (message "tag-set: %s, buffers: %s"
                   ;;          (pp-to-string (mapcar #'buffer-tag-name (sorted-set-items tag-set)))
                   ;;          (pp-to-string (mapcar #'tagged-buffer-name buffers)))
                   (dolist (buf buffers)
                     (let* ((bufname
                              (propertize (concat ;; "[ "
                                           (tagged-buffer-name buf)
                                           ;; " ]"
                                           )
                                          'tagged-entry-type (list 'buffer-name
                                                                   buf)))
                            (bufname-padding (- *tagged-buffers-name-length*
                                                (length bufname))))
                     (insert (make-string depth ?*)
                             " "
                               bufname
                               (if (< 0 bufname-padding)
                                 (make-string bufname-padding ?\s)
                                 "")
                             " "
                               (if add-full-buffer-names
                                 (or (buffer-file-name (tagged-buffer-buf buf))
                                     "")
                                 "")
                               "\n"))))))
             (render
               (lambda (spec tag-set depth buflist)
                 ;; (message "tag-set: %s"
                 ;;          (pp-to-string (mapcar #'buffer-tag-name (sorted-set-items tag-set))))
                 (if spec
                   (dolist (tag (first spec))
                     (let* ((new-tag-set (sorted-set/add tag-set tag))
                            (new-buflist
                              (tagged-buflist/buffers-matching-tagset buflist
                                                                      new-tag-set
                                                                      :exact (null? (rest spec)))))
                       (when (not (null? new-buflist))
                         (insert (make-string depth ?*)
                                 " "
                                 (propertize (buffer-tag-name tag)
                                             'face
                                             (aref *tagged-buffers-depth-faces*
                                                   depth)
                                             'tagged-entry-type (list 'tag tag))
                                 "\n")
                         (funcall render
                                  (rest spec)
                                  new-tag-set
                                  (+ depth 1)
                                  new-buflist))))
                   (funcall render-buffers-matching-tag-set
                            tag-set
                            depth)))))
      (funcall render
               tag-hierarchy-def
               (sorted-set/empty #'tagged-buflist/buffer-tag<)
               1
               tagged-buflist))))

;;;; user-visible functions

(defvar *tagged-buflist-buffer-name* "*buflist*")

(defun tagged-buflist-show ()
  "Switch to tagged buffer list."
  (interactive)
  (let ((buf (get-buffer-create *tagged-buflist-buffer-name*)))
    (with-current-buffer buf
      (kill-all-local-variables)
      (tagged-buflist-mode)
        (font-lock-mode -1)
      ;; (insert "\n")
      (tagged-buflist/refresh)
        ;; (org-align-all-tags)
      )
    (switch-to-buffer buf)))



(defmacro tagged-buflist/with-tagged-entry-type-for-current-line (entry-type-var
                                                                  &rest body)
  (declare (indent 2))
  `(let ((,entry-type-var
           (get-text-property
            (next-single-property-change (line-beginning-position)
                                         'tagged-entry-type
                                         (current-buffer)
                                         (line-end-position))
            'tagged-entry-type)))
     ,@body))

(defun tagged-buflist/switch-to-buffer-at-point ()
  "Try to find buffer name on line where point is and switch to such buffer."
  (interactive)
  (tagged-buflist/with-tagged-entry-type-for-current-line
   entry-value
   (if (eq 'buffer-name (first-safe entry-value))
     (switch-to-buffer (tagged-buffer-buf (first (rest entry-value))))
     (error "Not on a line with buffer name"))))

(defun tagged-buflist/switch-to-buffer-at-point-other-window ()
  (interactive)
  (tagged-buflist/with-tagged-entry-type-for-current-line
   entry-value
   (if (eq 'buffer-name (first-safe entry-value))
     (switch-to-buffer-other-window (tagged-buffer-buf (first (rest entry-value))))
     (error "Not on a line with buffer name"))))


(defvar *tagged-buflist-marked-buffers* nil
  "List of buffers marked from tagged buffer list.")



(defvar *tagged-buflist-tag-hierarchy*
  (list #'tagged-buflist/generate-tag-group-by-git-repository-root
                              +common-buffer-tags+)
  "Hierarchy definition (list of lists/functions of tags) to render
tagged bufer list.")


(defun tagged-buflist/refresh ()
  "Refresh tagged buffer list in current buffer."
  (interactive)
  (let ((tags (apply #'append (mapcar #'tagged-buflist/expand-tag-definitions
                                      *tagged-buflist-tag-hierarchy*))))
    (with-current-buffer (get-buffer-create *tagged-buflist-buffer-name*)
      (erase-buffer)
      (tagged-buflist/render-recursively (tagged-buflist/tagged-buffers tags)
                                         tag-hierarchy
                                         :add-full-buffer-names nil))))

(defvar tagged-buflist-mode-map
  (let ((map (make-sparse-keymap)))
    (def-keys-for-map map
      ("t"        next-line)
      ("n"        previous-line)
      ("h"        backward-char)
      ("s"        forward-char)
      +vi-search-keys+
      +control-x-prefix+
      +vim-special-keys+
      ("<return>" tagged-buflist/switch-to-buffer-at-point)
      ("o"        tagged-buflist/switch-to-buffer-at-point-other-window)
      ("r"        tagged-buflist/refresh))
    map))


(define-derived-mode tagged-buflist-mode org-mode "Tagged buflist"
  "Major mode for displaying and manipulating list of tagged buffer.
Similar to `ibuffer-mode'."
  (font-lock-mode -1)
  (undo-tree-minor-mode -1)
  (setq-local org-pretty-entities nil)
  ;; (dolist (face '(outline-1 outline-2
  ;;                 outline-3 outline-4
  ;;                 outline-5 outline-6
  ;;                 outline-7 outline-8))
  ;;   (face-remap-add-relative face 'default))
  )



(defun tagged-buflist/expand-tag-definitions (tags)
  "If TAGS is a function then it will be called and it's result will be
treated as a list of tags; otherwise it should be list of plain tags."
  (assert (or (functionp tags) (listp tags)))
  (if (functionp tags)
    (funcall tags)
    (copy-list tags)))


(defun collect-buffers-under-subtree ()
  (save-excursion
   (org-map-entries
    (lambda ()
      )
    t
    nil
    'tree)))

(defun collect-tags ()
  (save-excursion
   (let ((tags nil))
     (org-map-entries
      (lambda ()
        (when (not (outline-invisible-p))
          (setf tags (append (org-get-tags-at) tags))))
      t
      nil
      ;; 'tree
      )
     (remove-duplicates tags :test #'equal))))



(provide 'tagged-buffers)

;; Local Variables:
;; End:

;; tagged-buffers.el ends here
