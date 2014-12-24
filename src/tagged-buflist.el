;; tagged-buflist.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'more-clojure)
(require 'more-scheme)
(require 'datastructures)
(require 'keys-def)
(require 'eproj)

;;; buffer tags and their definitions

;;;; buffer tags

(defstruct (buffer-tag
            (:conc-name buffer-tag/))
  name
  comparison-name ;; string
  other-comparison-info ;; string or nil
  predicate ;; function of one argument - buffer to test
  )

(defmacro* make-buf-tag-pred (&key major-modes
                                   name-regexp
                                   or-expr-in-buffer
                                   and-expr-in-buffer)
  (let ((buf-var (gensym "buf-var")))
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
  (let ((name1 (buffer-tag/comparison-name tag1))
        (name2 (buffer-tag/comparison-name tag2))
        (other-info1 (buffer-tag/other-comparison-info tag1))
        (other-info2 (buffer-tag/other-comparison-info tag2)))
    (or (string< name1 name2)
        (and (string= name1 name2)
             (not (null? other-info1))
             (not (null? other-info2))
             (string< other-info1 other-info2)))))

(defun tagged-buflist/expand-tag-definitions (tags)
  "If TAGS is a function then it will be called and it's result will be
treated as a list of tags; otherwise it should be list of plain tags."
  (assert (or (functionp tags) (listp tags)))
  (if (functionp tags)
    (funcall tags)
    (copy-list tags)))

;;;; tag definitions

(defparameter +common-buffer-tags+
  (let ((basic-tags
         (map (lambda (entry)
                (make-buffer-tag :name (first entry)
                                 :comparison-name (first entry)
                                 :other-comparison-info nil
                                 :predicate (cadr entry)))
              (list
               (list "Haskell"
                     (make-buf-tag-pred
                      :major-modes '(haskell-mode
                                     literate-haskell-mode
                                     haskell-cabal-mode
                                     c2hs-mode
                                     haskell-c-mode
                                     inferior-haskell-mode
                                     inferior-hugs-mode
                                     haskell-hugs-mode
                                     ghc-core-mode
                                     hugs-mode
                                     alex-mode
                                     happy-mode)
                      :name-regexp (rx "*"
                                       (or "haskell" "ghci")
                                       "*"
                                       (? "<" (+ digit) ">"))))
               (list "Proof assistants"
                     (make-buf-tag-pred
                      :major-modes '(agda2-mode
                                     coq-mode
                                     idris-mode)
                      :name-regexp (rx (or "*Agda information*"
                                           (seq "*"
                                                (or (seq "agda"
                                                         (? "2"))
                                                    "coq"
                                                    "idris")
                                                "*"
                                                (? "<" (+ digit) ">"))))))
               (list "HDL"
                     (make-buf-tag-pred
                      :major-modes '(verilog-mode
                                     vhdl-mode
                                     ucf-mode)))
               (list "Clojure"
                     (make-buf-tag-pred
                      :major-modes '(clojure-mode
                                     nrepl-mode
                                     nrepl-popup-buffer-mode
                                     nrepl-macroexpansion-minor-mode
                                     nrepl-interaction-mode
                                     nrepl-popup-buffer-mode
                                     kibit-check-mode)
                      :or-expr-in-buffer
                      (and (string-match-pure? "^\\*.*nrepl.*\\*$" (buffer-name))
                           (memq major-mode '(text-mode
                                              fundamental-mode)))))
               (list "Emacs Lisp"
                     (make-buf-tag-pred
                      :major-modes '(emacs-lisp-mode
                                     inferior-emacs-lisp-mode)
                      :and-expr-in-buffer (not (string-match-pure? "^\\*.+\\*$"
                                                                   (buffer-name)))))
               (list "C/C++"
                     (make-buf-tag-pred
                      :major-modes '(c-mode c++-mode glsl-mode)))
               (list "Ocaml"
                     (make-buf-tag-pred
                      :major-modes '(tuareg-mode
                                     tuareg-interactive-mode)
                      :name-regexp (rx "*ocaml-toplevel*"
                                       (? "<" (+ digit) ">"))))
               (list "Octave"
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
               (list "Prolog"
                     (make-buf-tag-pred
                      :major-modes '(prolog-mode)
                      :name-regexp (rx "*prolog*"
                                       (? "<" (+ digit) ">"))))
               (list "Org"
                     (make-buf-tag-pred
                      :major-modes '(org-mode
                                     org-agenda-mode
                                     diary-mode
                                     calendar-mode)))
               (list "Books"
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
               (list "Java"
                     (make-buf-tag-pred
                      :major-modes '(java-mode)))
               (list "Web"
                     (make-buf-tag-pred
                      :major-modes '(html-mode
                                     sgml-mode
                                     nxhtml-mode
                                     nxhtml-muamo-mode
                                     css-mode
                                     js-mode
                                     django-nxhtml-mumamo-mode
                                     django-html-mumamo-mode
                                     rnc-mode)))
               (list "markup"
                     (make-buf-tag-pred
                      :major-modes '(nxml-mode
                                     markdown-mode
                                     yaml-mode
                                     rst-mode)))
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
                                     tcl-mode
                                     autoconf-mode
                                     pascal-mode
                                     m2-mode ;; Modula-2
                                     dos-mode)
                      :name-regexp (rx bol
                                       (or "makefile"
                                           "Makefile"
                                           "GNUMakefile")
                                       eol)))
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
                                     gitignore-mode
                                     gitconfig-mode)
                      :name-regexp (rx bol "*magit" (* nonl) "*" eol)))
               (list "utility"
                     (make-buf-tag-pred
                      :major-modes '(comint-mode
                                     compilation-mode
                                     clojure-compilation-mode
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
               ;; (list "other"
               ;;       (make-buf-tag-pred
               ;;        :major-modes '(help-mode
               ;;                       apropos-mode
               ;;                       Info-mode
               ;;                       Man-mode
               ;;                       ibuffer-mode
               ;;
               ;;                       snippet-mode
               ;;                       text-mode
               ;;                       fundamental-mode
               ;;                       special-mode)
               ;;        :name-regexp (rx bol
               ;;                         (or "*scratch*"
               ;;                             "*Messages*"
               ;;                             "*Pp Eval Output*"
               ;;                             "*Backtrace*")
               ;;                         eol)))
               ))))
    (append basic-tags
            (list (make-buffer-tag
                   :name "other"
                   :comparison-name "other"
                   :other-comparison-info nil
                   :predicate (lambda (buf)
                                (all? (comp #'not
                                            (partial-first #'funcall buf)
                                            #'buffer-tag/predicate)
                                      basic-tags)))))))

(defparameter tagged-buflist/consider-nontracked-files-as-residing-in-repository nil
  "If set to T then all files under root of some repository will be considered
to be part of it and, therefore, their buffers will be put in the group with
tracked files for that repository.")

(defun tagged-buflist/generate-tag-groups-by-eproj ()
  "Generate tag group specifications based on each buffer's eproj project."
  (let* ((projects (remove-duplicates
                    (delq nil
                          (map (lambda (buf)
                                 (condition-case nil
                                     (eproj-get-project-for-buf buf)
                                   (error nil)))
                               (visible-buffers)))
                    :test (lambda (a b)
                            (string=? (eproj-project/root a)
                                      (eproj-project/root b))))))
    (append
     (sort
      (map (lambda (proj)
             (let ((repo-root (eproj-normalize-file-name (eproj-project/root proj)))
                   (fname (abbreviate-file-name (eproj-project/root proj)))
                   (project-type (let ((type (eproj-project-type/name
                                              (eproj-project/type proj))))
                                   (if (eq? type 'eproj-file)
                                     "eproj"
                                     (format "%s" type)))))
               (make-buffer-tag
                :name (concat project-type
                              ":"
                              fname)
                :comparison-name fname
                :other-comparison-info project-type
                :predicate
                (lambda (buffer)
                  (awhen (eproj-get-initial-project-root-for-buf buffer)
                    (let ((buffer-project-root (eproj-normalize-file-name it)))
                      (with-current-buffer buffer
                        (cond
                          ((and (null? tagged-buflist/consider-nontracked-files-as-residing-in-repository)
                                (not (null? buffer-project-root)))
                           (string=? repo-root buffer-project-root))
                          ((or (eq? major-mode 'magit-status-mode)
                               (eq? major-mode 'dired-mode))
                           (string-prefix? repo-root
                                           (eproj-normalize-file-name default-directory)))
                          ;; buffers like *Messages* do not have buffer-file-truename
                          ;; and therefore are filtered here
                          ((not (null? buffer-file-truename))
                           (string=? repo-root
                                     buffer-project-root)
                           ;; doesn't work for nested projects
                           ;; (string-prefix? repo-root
                           ;;                 (eproj-normalize-file-name buffer-file-truename))
                           )
                          (t
                           nil)))))))))
           projects)
      #'tagged-buflist/buffer-tag<)
     (list (make-buffer-tag
            :name "no repository"
            :comparison-name "no repository"
            :other-comparison-info nil
            :predicate (lambda (buffer)
                         (or (null? (eproj-get-initial-project-root-for-buf buffer))
                             (null? (with-current-buffer buffer
                                      buffer-file-truename)))))))))

;;; tagged sections

;; supported properties and their values:
;; 'visibility         = #{'visible 'invisible}
;; 'buffer             = tagged-buffer struct for buffer type section
;; 'buffer-name-bounds = (cons <buffer name beginning pos> <buffer name end pos>), bounds in buffer listing sections, e.g. *buflist*

(defstruct (tagged-section
            (:conc-name tagged-section/))
  name
  type       ;; buffer or group
  properties ;; hash table with #'eq test
  beg
  end
  children ;; list of children sections
  parent   ;; another section or nil
  optional-overlay)

;; (defun tagged-section/content-overlay (section)
;;   "Retrieve overlay for SECTION's content: for buffers overlay will
;; cover buffer name, for groups it will not cover section's name."
;;   (aif (tagged-section/optional-overlay section)
;;     it
;;     ;; (let ((ov (make-overlay (tagged-section/beg section)
;;     ;;                         (tagged-section/end section))))
;;     ;;   (setf (tagged-section/optional-overlay section) ov)
;;     ;;   ov)
;;     (pcase (tagged-section/type section)
;;       (`group
;;        (let ((ov (make-overlay (save-excursion
;;                                  (goto-char (tagged-section/beg section))
;;                                  (line-end-position))
;;                                (tagged-section/end section))))
;;          (setf (tagged-section/optional-overlay section) ov)
;;          ov))
;;       (`buffer
;;        (let ((ov (make-overlay (tagged-section/beg section)
;;                                (tagged-section/end section))))
;;          (setf (tagged-section/optional-overlay section) ov)
;;          ov)))))

(defun tagged-section/overlay (section)
  "Retrieve overlay for SECTION's content: for buffers overlay will
cover buffer's name, for groups it would not cover section's name."
  (aif (tagged-section/optional-overlay section)
    it
    ;; (let ((ov (make-overlay (tagged-section/beg section)
    ;;                         (tagged-section/end section))))
    ;;   (setf (tagged-section/optional-overlay section) ov)
    ;;   ov)
    (pcase (tagged-section/type section)
      (`group
       (let ((ov (make-overlay (save-excursion
                                 (goto-char (tagged-section/beg section))
                                 (line-end-position))
                               (tagged-section/end section))))
         (setf (tagged-section/optional-overlay section) ov)
         ov))
      (`buffer
       (destructuring-bind (start . end)
           (tagged-section/get-prop section 'buffer-name-bounds)
         (let ((ov (make-overlay start end)))
           (setf (tagged-section/optional-overlay section) ov)
           ov))))))

(defparameter tagged-buflist/toplevel-section nil)
(defparameter tagged-buflist/current-section nil)

(defun tagged-section/add-child (section child)
  "Add CHILD as last SECTION's child."
  (setf (tagged-section/children section)
        (append (tagged-section/children section)
                (list child))))

(defun tagged-section/put-prop (section key value)
  "Assign VALUE to property KEY in SECTION."
  (assert (symbol? key))
  (puthash key value (tagged-section/properties section)))

(defun tagged-section/get-prop (section key)
  "Get property KEY from SECTION or nil if not fonud."
  (assert (symbol? key))
  (gethash key (tagged-section/properties section) nil))

(defmacro tagged-buflist/with-new-section (name type section-var &rest body)
  (declare (indent 3))
  (assert (memq type '(group buffer)))
  (let ((begin-var (gensym "begin-var")))
    `(let* ((,begin-var (point))
            (,section-var
             (make-tagged-section
              :name ,name
              :type ',type
              :beg ,begin-var
              :end nil
              :properties (let ((tbl
                                 ;; use as small memory as possible
                                 (make-hash-table :test #'eq
                                                  :size 2
                                                  :rehash-size 1)))
                            (puthash 'visibility 'visible tbl)
                            tbl)
              :children (list)
              :parent tagged-buflist/current-section
              :optional-overlay nil))
            (tagged-buflist/current-section ,section-var))
       (unwind-protect
           (progn
             ,@body)
         (setf (tagged-section/end ,section-var) (point))
         (awhen (tagged-section/parent ,section-var)
           (tagged-section/add-child it
                                     ,section-var))))))

(defun tagged-buflist/invisible-section? (section)
  (eq? (tagged-section/get-prop section 'visibility) 'invisible))

(defun tagged-buflist/section-not-visible? (section)
  "Check whether SECTION is in hidden subtree but do not count its hidden state."
  (letrec ((iter
            (lambda (section)
              (cond ((null? section)
                     nil)
                    ((tagged-buflist/invisible-section? section)
                     t)
                    (t
                     (funcall iter
                              (tagged-section/parent section)))))))
    (funcall iter (tagged-section/parent section)))
  ;; implementation with loop
  ;; (let ((result nil))
  ;;   (while (and section
  ;;               (not result))
  ;;     (if (eq? 'invisible (tagged-section/get-prop section 'visibility))
  ;;       (setf result t)
  ;;       (setf (section (tagged-section/parent section))))
  ;;     result))
  )

(defun tagged-section= (section-a section-b)
  (and (string= (tagged-section/name section-a)
                (tagged-section/name section-b))
       (or (and (null? (tagged-section/parent section-a))
                (null? (tagged-section/parent section-b)))
           (tagged-section= (tagged-section/parent section-a)
                            (tagged-section/parent section-b)))))

(defun tagged-section/start= (section-a section-b)
  (= (tagged-section/beg section-a)
     (tagged-section/beg section-b)))

(defun tagged-section/start< (section-a section-b)
  (< (tagged-section/beg section-a)
     (tagged-section/beg section-b)))

(defun tagged-section/hash (section)
  (let ((name-chain nil))
    (while section
      (push (tagged-section/name section) name-chain)
      (setf section (tagged-section/parent section)))
    (sxhash (nreverse name-chain))))

(define-hash-table-test 'tagged-section-hash-test
  #'tagged-section=
  #'tagged-section/hash)

;;; tagged buffers

(defparameter tagged-buflist/buffers nil
  "List of `tagged-buffer' structs of buffers currently shown.")

;; I have no idea what this is about...
;;
;; Note that single tagged buffer may be represented in several sections
;; but section cannot be referenced from more than one tagged buffer.
(defstruct (tagged-buffer
            (:conc-name tagged-buffer/))
  buf
  tags
  sections ;; list of tagged section for this buffer
  )

(defun tagged-buffer= (buf-a buf-b)
  (eq (tagged-buffer/buf buf-a) (tagged-buffer/buf buf-b)))

(defun tagged-buffer/name (tagged-buf)
  (buffer-name (tagged-buffer/buf tagged-buf)))


(defun tagged-buflist/user-buffers ()
  (filter (lambda (buf)
            (not (string-match-pure? "^ " (buffer-name buf))))
          (visible-buffers)))

(defparameter tagged-buflist/buffer-tags-cache
  (make-hash-table :test #'eq :size 503 :weakness t)
  "Cache that associates sets of tags to buffers")

(defun tagged-buflist/tagged-buffers (tags)
  (assert (all? #'buffer-tag-p tags) nil
          "All tags should be of buffer-tag type, %s" tags)
  ;; While map here works it is assumed that no git repository for opened
  ;; files will change its HEAD reference. Since this function should not
  ;; take long it is realistic assumption. But even if some repository will
  ;; change, this change will not be visible inside git-with-temp-head-commit-cache
  ;; and therefore theoretically some files might be classified as parts
  ;; of repository even though this very moment repository deleted them or
  ;; switched branch where they've not-tracked state.
  (git-with-temp-head-commit-cache
   (map (lambda (buf)
          (let ((buffer-tags
                 (if-let (cached-tags (gethash buf tagged-buflist/buffer-tags-cache))
                   cached-tags
                   (let ((new-tags
                          (sorted-set/from-list
                           (filter (lambda (tag)
                                     (funcall (buffer-tag/predicate tag) buf))
                                   tags)
                           #'tagged-buflist/buffer-tag<)))
                     (puthash buf new-tags tagged-buflist/buffer-tags-cache)))))
            (make-tagged-buffer
             :buf buf
             :tags buffer-tags
             :sections nil)))
        (tagged-buflist/user-buffers))))

(defun* tagged-buflist/buffers-matching-tagset (tagged-buflist
                                                tagset
                                                &key (exact nil))
  "Filter out buffers with tags matching TAGSET from TAGGED-BUFLIST. If
EXACT is supplied then leave buffers with tags exactly in TAGSET otherwise
buffers that match part of tagset will be included in result."
  (let ((tagset-len (sorted-set/length tagset)))
    (filter (lambda (buf)
              (= (sorted-set/length
                  (sorted-set/intersection (tagged-buffer/tags buf)
                                           tagset))
                 (if exact
                   tagset-len
                   (min (sorted-set/length (tagged-buffer/tags buf))
                        tagset-len))))
            tagged-buflist)))

(defparameter tagged-buffers/group-faces
  [outline-1 outline-2 outline-3 outline-4 outline-5 outline-6 outline-7 outline-8])

(defparameter *tagged-buffers-name-length* 32
  "Number of characters to reserve for buffer name. If buffer name exceeds this
limit it will extend past it and affect any text following.")

;;; presentation of buffer tree

(defun* tagged-buflist/show-recursively (tagged-buflist
                                         tag-hierarchy-def
                                         &key
                                         (sort-predicate
                                          (lambda (a b)
                                            (string< (tagged-buffer/name a)
                                                     (tagged-buffer/name b))))
                                         (add-full-buffer-names nil))
  "TAGGED-BUFLIST - list of `tagged-buffer's.

TAG-HIERARCHY-DEF - list of lists of `buffer-tag' structs. Each list
in TAG-HIERARCHY-DEF would be treated as a set of tags for one level, and
could be obtained with tagged-buflist/expand-tag-definitions."
  (letrec
      ((show-buffers-matching-tag-set
        (lambda (tagset depth buflist)
          (let ((buffers (sort (tagged-buflist/buffers-matching-tagset buflist
                                                                       tagset
                                                                       :exact t)
                               sort-predicate)))
            (dolist (buf buffers)
              (tagged-buflist/with-new-section
                  (tagged-buffer/name buf)
                  buffer
                  section
                (push section (tagged-buffer/sections buf))
                (tagged-section/put-prop section 'buffer buf)
                (let* ((real-buffer (tagged-buffer/buf buf))
                       (real-buffer-file-name (buffer-file-name real-buffer))
                       (bufname (tagged-buffer/name buf))
                       (bufname-padding (- *tagged-buffers-name-length*
                                           (length bufname)))
                       (line-prefix
                        (concat (make-string (* 2 depth) ?\s)
                                (if (and real-buffer-file-name
                                         (buffer-modified? real-buffer))
                                  "(+) "
                                  "    ")))
                       (line-prefix-len (length line-prefix))
                       (line (concat line-prefix
                                     bufname
                                     (if (< 0 bufname-padding)
                                       (make-string bufname-padding ?\s)
                                       "")
                                     " "
                                     (if add-full-buffer-names
                                       (abbreviate-file-name
                                        (or real-buffer-file-name
                                            ""))
                                       "")
                                     "\n"))
                       (propertized-line
                        (propertize line 'tagged-buflist/section section)))
                  (let ((start (+ (point) line-prefix-len)))
                    (tagged-section/put-prop section
                                             'buffer-name-bounds
                                             (cons start
                                                   (+ start (length bufname)))))
                  (insert propertized-line)))))))
       (show-buffers
        (lambda (spec tagset depth buflist)
          (if spec
            (dolist (tag (first spec))
              (let* ((new-tag-set (sorted-set/add tagset tag))
                     (new-buflist
                      (tagged-buflist/buffers-matching-tagset buflist
                                                              new-tag-set
                                                              :exact (null? (rest spec)))))
                (when (not (null? new-buflist))
                  (tagged-buflist/with-new-section
                      (buffer-tag/name tag)
                      group
                      section
                    (let* ((group-face
                            (aref tagged-buffers/group-faces
                                  (rem* depth
                                        (length tagged-buffers/group-faces))))
                           (line (concat
                                  (make-string (* 2 depth) ?\s)
                                  "["
                                  (propertize (buffer-tag/name tag)
                                              'face
                                              group-face)
                                  "]\n"))
                           (propertized-line
                            (propertize line 'tagged-buflist/section section)))
                      (insert propertized-line))
                    (funcall show-buffers
                             (rest spec)
                             new-tag-set
                             (+ depth 1)
                             new-buflist)))))
            (funcall show-buffers-matching-tag-set
                     tagset
                     depth
                     buflist)))))
    (funcall show-buffers
             tag-hierarchy-def
             (sorted-set/empty #'tagged-buflist/buffer-tag<)
             0
             tagged-buflist)
    (while (char= (char-before) ?\n)
      (delete-backward-char 1))))

;;; internal functions

(defun tagged-buflist/for-single-section (pred func)
  "Call function FUNC for first section than satisfies predicate PRED."
  (letrec ((iter
            (lambda (section)
              (if (not (null? section))
                (if (funcall pred section)
                  (progn
                    (funcall func section)
                    t)
                  (mapc iter (tagged-section/children section)))
                nil))))
    (funcall iter tagged-buflist/toplevel-section)))

(defun tagged-buflist/for-multiple-sections (pred func)
  "Call function FUNC for all sections than satisfy predicate PRED."
  (letrec ((iter
            (lambda (section)
              (when section
                (when (funcall pred section)
                  (funcall func section))
                (mapc iter (tagged-section/children section))))))
    (funcall iter tagged-buflist/toplevel-section)))

(defmacro tagged-buflist/with-preserved-selection (&rest body)
  "Remember item at point, execute body and move point to remembered item.
If item was deleted after end of BODY's execution then try to return to
line the point was on."
  (declare (indent 0))
  (let ((selected-section-var (gensym "selected-section-var"))
        (selected-line-var (gensym "selected-line-var")))
    `(let ((,selected-line-var (count-lines1 (point-min) (point))))
       (tagged-buflist/with-optional-section-for-line ,selected-section-var
         (unwind-protect
             (progn
               ,@body)
           ;; find selected section
           (when ,selected-section-var
             (unless (tagged-buflist/for-single-section
                      (comp (partial #'tagged-section=
                                     ,selected-section-var))
                      #'tagged-buflist/goto-section)
               (goto-line ,selected-line-var))))))))

(defmacro tagged-buflist/with-preserved-invisible-sections (&rest body)
  "Remember hidden sections, execute body and restore hidden sections."
  (declare (indent 0))
  ;; this may be overly complicated but it tries to avoid GC which may
  ;; pay off in refresh function
  (let ((invisible-sections-var (gensym "invisible-sections-var")))
    `(let ((,invisible-sections-var nil))
       (tagged-buflist/for-multiple-sections
        (comp (partial #'eq? 'invisible)
              (partial-first #'tagged-section/get-prop
                             'visibility))

        (lambda (section)
          (tagged-buflist/change-group-visibility section 'visible)
          (push section ,invisible-sections-var)))
       (unwind-protect
           (progn
             ,@body)
         (dolist (sec ,invisible-sections-var)
           (tagged-buflist/change-group-visibility
            (gethash sec
                     tagged-buflist/tagged-sections-store)
            'invisible))))))

(defun tagged-buflist/clear-variables ()
  "Clear all tagged-buflist internal variables."
  (clrhash tagged-buflist/tagged-sections-store)
  (setf tagged-buflist/group-sections nil
        tagged-buflist/marked-buffers nil
        tagged-buflist/toplevel-section nil
        tagged-buflist/current-section nil
        tagged-buflist/buffers nil))

;;;; functions operating of current line's section

(defmacro tagged-buflist/with-section-for-line (section-var
                                                &rest body)
  (declare (indent 1))
  `(if-let (,section-var (get-text-property (point) 'tagged-buflist/section))
     (progn
       ,@body)
     (error "No tagged buflist section found for line: \"%s\""
            (current-line))))

(defmacro tagged-buflist/with-optional-section-for-line (section-var
                                                         &rest body)
  (declare (indent 1))
  `(let ((,section-var (get-text-property (point) 'tagged-buflist/section)))
     ,@body))

(defmacro tagged-buflist/with-section-type-for-line (type-var
                                                     &rest body)
  (declare (indent 1))
  (let ((section-var (gensym "section-var")))
    `(tagged-buflist/with-section-for-line ,section-var
       (let ((,type-var (tagged-section/type ,section-var)))
         ,@body))))

(defun tagged-buflist/describe-section ()
  "Describe section at point."
  (interactive)
  (tagged-buflist/with-section-for-line section
    (message "name: %s\nbegin: %s\nend: %s\ntype: %s\nproperties: %s\n"
             (tagged-section/name section)
             (tagged-section/beg section)
             (tagged-section/end section)
             (tagged-section/type section)
             (hash-table->alist (tagged-section/properties section)))))

(defun tagged-buflist/goto-section (section)
  (goto-char (tagged-section/beg section)))

(defun tagged-buflist/change-group-visibility (section new-visibility)
  (let ((ov (tagged-section/overlay section))
        (orig-visibility (tagged-section/get-prop section 'visibility)))
    (assert (not (null? orig-visibility)) nil
            "No visibility property found among properties %s"
            (hash-table->alist (tagged-section/properties section)))
    (when (not (eq? orig-visibility new-visibility))
      (cond ((eq? new-visibility 'invisible)
             (overlay-put ov 'invisible t)
             (overlay-put ov 'before-string " ...\n")
             (tagged-section/put-prop section 'visibility 'invisible))
            ((eq? new-visibility 'visible)
             (overlay-put ov 'invisible nil)
             (overlay-put ov 'before-string nil)
             (tagged-section/put-prop section 'visibility 'visible))
            (t
             (error "Unsupported visibility type %s" new-visiblity))))
    nil))

(defun tagged-buflist/cycle-group-visibility (section)
  (assert (eq? 'group (tagged-section/type section)))
  (let ((visibility (tagged-section/get-prop section 'visibility)))
    (assert (not (null? visibility)) nil
            "No visibility property found among properties %s"
            (hash-table->alist (tagged-section/properties section)))
    (cond ((eq? visibility 'visible)
           (tagged-buflist/change-group-visibility section 'invisible))
          ((eq? visibility 'invisible)
           (tagged-buflist/change-group-visibility section 'visible))
          (t
           (error "Unsupported visibility type %s" visiblity)))))

(defun* tagged-buflist/for-section-on-line (&key
                                            (if-group #'ignore)
                                            (if-buffer #'ignore))
  (tagged-buflist/with-section-for-line section
    (pcase (tagged-section/type section)
      (`group
       (funcall if-group section))
      (`buffer
       (funcall if-buffer section)))))

(defun* tagged-buflist/for-section (section
                                    &key
                                    (if-group #'ignore)
                                    (if-buffer #'ignore))
  (pcase (tagged-section/type section)
    (`group
     (funcall if-group section))
    (`buffer
     (funcall if-buffer section))))

(defun tagged-buflist/collect-tagged-buffers-under-section (section)
  (save-excursion
    (let ((buffers nil))
      (letrec ((collect
                (lambda (section)
                  (tagged-buflist/for-section
                   section
                   :if-group (comp (partial #'map collect)
                                   #'tagged-section/children)

                   :if-buffer (comp (partial-first #'push buffers)
                                    (partial-first #'tagged-section/get-prop 'buffer))))))
        (funcall collect section))
      buffers)))

;;;; group sections

(defparameter tagged-buflist/tagged-sections-store
  (make-hash-table :test #'tagged-section-hash-test)
  "Hash table of sections as keys and themselves as values.")

(defparameter tagged-buflist/group-sections nil
  "Vector of tagged sections sorted by their beginning positions within
`tagged-buflist/main-buffer-name' buffer.")

(defun tagged-buflist/refresh-section-stores ()
  "Fill `tagged-buflist/group-sections' with tagged group-sections starting from
the `tagged-buflist/toplevel-section'.
Populate `tagged-buflist/tagged-sections-store' with sections in buffer."
  (let ((sections nil))
    (clrhash tagged-buflist/tagged-sections-store)
    (letrec ((iter
              (lambda (section)
                (puthash section
                         section
                         tagged-buflist/tagged-sections-store)
                (when (eq? 'group (tagged-section/type section))
                  (push section sections))
                (dolist (child (tagged-section/children section))
                  (funcall iter child)))))
      (funcall iter tagged-buflist/toplevel-section))
    (setf tagged-buflist/group-sections
          (list->vector (reverse sections)))))

;;; user-visible functions

;;;; buffer marking and operations on marked buffers

(defparameter tagged-buflist/marked-buffers nil
  "List of buffers (tagged-buffer structs) marked from tagged buffer list.")

(defface tagged-buflist/marked-face
  `((t (:foreground ,+solarized-magenta+)))
  "Face to highlight marked buffers."
  :group 'tagged-buflist)

(defmacro tagged-buflist/with-preserved-marks (&rest body)
  "Save value of `tagged-buflist/marked-buffers', execute BODY and restore
saved buffer marks."
  (declare (indent 0))
  (let ((store-var (gensym "store-var")))
    `(let ((,store-var tagged-buflist/marked-buffers))
       (unwind-protect
           (progn
             ,@body)
         (dolist (buf ,store-var)
           (tagged-buflist/mark-buffer (find buf tagged-buflist/buffers
                                             :test #'tagged-buffer=)))))))

(defun tagged-buflist/mark-buffer (tagged-buf)
  "Place mark on TAGGED-BUF."
  (assert (tagged-buffer-p tagged-buf))
  (unless (member* tagged-buf
                   tagged-buflist/marked-buffers
                   :test #'tagged-buffer=)
    (push tagged-buf tagged-buflist/marked-buffers)
    (dolist (section (tagged-buffer/sections tagged-buf))
      (overlay-put (tagged-section/overlay section)
                   'face
                   'tagged-buflist/marked-face))))

(defun tagged-buflist/unmark-buffer (tagged-buf)
  "Remove mark from TAGGED-BUF."
  (assert (tagged-buffer-p tagged-buf))
  (when (member* tagged-buf
                 tagged-buflist/marked-buffers
                 :test #'tagged-buffer=)
    (setf tagged-buflist/marked-buffers
          (remove* tagged-buf
                   tagged-buflist/marked-buffers
                   :test #'tagged-buffer=))
    (dolist (section (tagged-buffer/sections tagged-buf))
      (overlay-put (tagged-section/overlay section)
                   'face
                   nil))))


(defun tagged-buflist/mark-buffers-at-point ()
  (interactive)
  (tagged-buflist/for-section-on-line
   :if-group (comp (partial #'map #'tagged-buflist/mark-buffer)
                   #'tagged-buflist/collect-tagged-buffers-under-section)
   :if-buffer (comp #'tagged-buflist/mark-buffer
                    (partial-first #'tagged-section/get-prop 'buffer))))

(defparameter tagged-buflist/mark-buffers-by-file-name-history nil
  "History variable for `tagged-buflist/mark-buffers-by-file-name'.")

(defun tagged-buflist/mark-buffers-by-file-name (filename-regex)
  (interactive "sfilename regex: ")
  (mapc #'tagged-buflist/mark-buffer
        (filter (comp (partial #'string-match-pure? filename-regex)
                      #'buffer-file-name
                      #'tagged-buffer/buf)
                (filter (comp #'not
                              #'null?
                              #'buffer-file-name
                              #'tagged-buffer/buf)
                        tagged-buflist/buffers))))

(defparameter tagged-buflist/mark-buffers-by-major-mode-history nil
  "History variable for `tagged-buflist/mark-buffers-by-major-mode'.")

(defun tagged-buflist/mark-buffers-by-major-mode (mmode-str)
  "Mark buffers with major mode equal to MMODE-STR that will be treated as symbol."
  (interactive
   (list (completing-read-vanilla
          "mode: "
          (map #'symbol->string
               (remove-duplicates
                (map (lambda (tagged-buf)
                       (buffer-local-value 'major-mode
                                           (tagged-buffer/buf tagged-buf)))
                     tagged-buflist/buffers)))
          nil
          t
          ;; initial input
          (tagged-buflist/for-section-on-line
           :if-group (comp #'first-safe
                           (partial #'map #'symbol->string)
                           #'remove-duplicates
                           (partial #'map (comp (partial #'buffer-local-value 'major-mode)
                                                #'tagged-buffer/buf
                                                (partial-first #'tagged-section/get-prop
                                                               'buffer))))
           :if-buffer (comp #'symbol->string
                            (partial #'buffer-local-value 'major-mode)
                            #'tagged-buffer/buf
                            (partial-first #'tagged-section/get-prop
                                           'buffer)))
          'tagged-buflist/mark-buffers-by-major-mode-history)))
  (let ((mmode (string->symbol mmode-str)))
    (mapc #'tagged-buflist/mark-buffer
          (filter (comp (partial #'eq? mmode)
                        (partial #'buffer-local-value 'major-mode)
                        #'tagged-buffer/buf)
                  tagged-buflist/buffers))))

(defun tagged-buflist/unmark-buffers-at-point ()
  (interactive)
  (tagged-buflist/for-section-on-line
   :if-group (comp (partial #'map #'tagged-buflist/unmark-buffer)
                   #'tagged-buflist/collect-tagged-buffers-under-section)
   :if-buffer (comp #'tagged-buflist/unmark-buffer
                    (partial-first #'tagged-section/get-prop 'buffer))))

(defun tagged-buflist/unmark-all ()
  (interactive)
  (mapc #'tagged-buflist/unmark-buffer tagged-buflist/marked-buffers))

(defun tagged-buflist/delete-marked-buffers ()
  (interactive)
  (tagged-buflist/with-preserved-selection
    (when tagged-buflist/marked-buffers
      (tagged-buflist/invalidate-refresh-flag!))
    (map (comp #'kill-buffer
               #'tagged-buffer/buf)
         tagged-buflist/marked-buffers)
    (setf tagged-buflist/marked-buffers nil)
    (tagged-buflist/refresh)))

(defun tagged-buflist/save-marked-buffers ()
  (interactive)
  (tagged-buflist/with-preserved-selection
    (map (lambda (tagged-buf)
           (with-current-buffer (tagged-buffer/buf tagged-buf)
             (save-buffer)))
         tagged-buflist/marked-buffers)
    (tagged-buflist/refresh)))

(defun tagged-buflist/eval-in-marked-buffers (expr)
  "Evaluate expression EXPR in marked buffers."
  (interactive (list (read-from-minibuffer "expr: "
                                           nil
                                           icicle-read-expression-map
                                           t ;; Apply `read' to string read.
                                           'read-expression-history)))
  (tagged-buflist/with-preserved-selection
    (map (lambda (tagged-buf)
           (with-current-buffer (tagged-buffer/buf tagged-buf)
             (eval expr)))
         tagged-buflist/marked-buffers)
    (tagged-buflist/refresh)))

;;;; "main" functions

(defparameter tagged-buflist/main-buffer-name "*buflist*")

(defparameter tagged-buflist/show-filenames t
  "Whether to show buffer filenames.")

(defparameter tagged-buflist/refresh-is-needed nil
  "Flag whether current state of tagged buflist is no more up to date.")

(defun tagged-buflist/invalidate-refresh-flag! ()
  (setf tagged-buflist/refresh-is-needed t))

;; yay, this hook is really good!
(add-hook 'buffer-list-update-hook #'tagged-buflist/invalidate-refresh-flag!)

(defun tagged-buflist/setup-main-buffer ()
  "Set up main buffer for showing sections and return in."
  (if-let (buf (get-buffer tagged-buflist/main-buffer-name))
    (when tagged-buflist/refresh-is-needed
      (tagged-buflist/refresh))
    (progn
      (setf buf (get-buffer-create tagged-buflist/main-buffer-name))
      (with-current-buffer buf
        (kill-all-local-variables)
        (tagged-buflist-mode)
        (font-lock-mode -1)
        (read-only-mode +1)
        (add-hook 'kill-buffer-hook
                  #'tagged-buflist/clear-variables
                  nil ;; append
                  t   ;; local
                  )
        (tagged-buflist/refresh)
        (goto-char (point-min)))))
  buf)

(defun tagged-buflist-show ()
  "Switch to tagged buffer list."
  (interactive)
  (setf tagged-buflist/marked-buffers nil)
  (switch-to-buffer (tagged-buflist/setup-main-buffer)))

(defun tagged-buflist-show-select-current-buf ()
  (interactive)
  (setf tagged-buflist/marked-buffers nil)
  (let ((orig-buffer (current-buffer))
        (buf (tagged-buflist/setup-main-buffer)))
    ;; select original buffer
    (with-current-buffer buf
      (tagged-buflist/for-single-section
       (lambda (section)
         (when-let (tbuf (tagged-section/get-prop section
                                                  'buffer))
           (eq? orig-buffer (tagged-buffer/buf tbuf))))
       #'tagged-buflist/goto-section))
    (switch-to-buffer buf)))

(defun tagged-buflist/toggle-filenames ()
  "Refresh tagged buffer list in current buffer."
  (interactive)
  (setf tagged-buflist/show-filenames (not tagged-buflist/show-filenames))
  (tagged-buflist/refresh))

(defun tagged-buflist/refresh ()
  "Refresh tagged buffer list in `tagged-buflist/main-buffer-name' buffer."
  (interactive)
  (let* ((expanded-hierarchy (map #'tagged-buflist/expand-tag-definitions
                                  tagged-buflist/tag-hierarchy))
         (tags (apply #'append expanded-hierarchy)))
    (aif (get-buffer tagged-buflist/main-buffer-name)
      (with-current-buffer it
        (with-inhibited-read-only
         (tagged-buflist/with-preserved-marks
           (tagged-buflist/with-preserved-invisible-sections
             (tagged-buflist/with-preserved-selection
               (setf tagged-buflist/refresh-is-needed nil
                     tagged-buflist/marked-buffers nil
                     tagged-buflist/buffers (tagged-buflist/tagged-buffers tags))
               (dotimes (i (length tagged-buflist/group-sections))
                 (tagged-buflist/change-group-visibility
                  (aref tagged-buflist/group-sections i)
                  'visible))
               (erase-buffer)
               (tagged-buflist/with-new-section
                   "toplevel"
                   group
                   section
                 (setf tagged-buflist/toplevel-section section)
                 (tagged-buflist/show-recursively tagged-buflist/buffers
                                                  expanded-hierarchy
                                                  :add-full-buffer-names
                                                  tagged-buflist/show-filenames))
               (tagged-buflist/refresh-section-stores))))))
      (error "No buffer %s found" tagged-buflist/main-buffer-name))))



(defun tagged-buflist/switch-to-buffer-at-point-or-cycle ()
  "Switch to buffer if on positioned on line with buffer or cycle visibility
of group if on line with group."
  (interactive)
  (tagged-buflist/for-section-on-line
   :if-group #'tagged-buflist/cycle-group-visibility
   :if-buffer (comp #'switch-to-buffer
                    #'tagged-buffer/buf
                    (partial-first #'tagged-section/get-prop 'buffer))))

(defun tagged-buflist/switch-to-buffer-at-point-other-window-or-cycle ()
  (interactive)
  (tagged-buflist/for-section-on-line
   :if-group #'tagged-buflist/cycle-group-visibility
   :if-buffer (comp #'switch-to-buffer-other-window
                    #'tagged-buffer/buf
                    (partial-first #'tagged-section/get-prop 'buffer))))



(defparameter tagged-buflist/jump-to-buffer-history nil
  "History variable for `tagged-buflist/jump-to-buffer'.")

(defun tagged-buflist/jump-to-buffer (bufname)
  "Jump to section corresponding to buffer with BUFNAME name."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read-vanilla "buffer to jump to: "
                               (map #'tagged-buffer/name tagged-buflist/buffers)
                               nil
                               t   ;; require match
                               nil ;; initial input
                               'tagged-buflist/jump-to-buffer-history))))
  (letrec ((extract-leaf-sections
            (lambda (section)
              (aif (tagged-section/children section)
                (foldr #'append nil (map extract-leaf-sections it))
                (list section)))))
    (let ((buffer-sections
           (funcall extract-leaf-sections
                    tagged-buflist/toplevel-section)))
      (assert (all? (comp #'not
                          #'null?
                          (partial-first #'tagged-section/get-prop 'buffer))
                    buffer-sections))
      (assert (all? (comp #'not
                          #'null?
                          #'tagged-buffer/buf
                          (partial-first #'tagged-section/get-prop 'buffer))
                    buffer-sections))
      (tagged-buflist/goto-section
       (find-if (lambda (section)
                  (string= bufname
                           (buffer-name
                            (tagged-buffer/buf
                             (tagged-section/get-prop section
                                                      'buffer)))))
                buffer-sections)))))

;;;; forward/backward/up selection of groups

(defun tagged-buflist/select-parent ()
  "Jump to parent of current section."
  (interactive)
  (tagged-buflist/with-section-for-line section
    (awhen (tagged-section/parent section)
      (tagged-buflist/goto-section it))))

(defun tagged-buflist/find-visible-section (index delta)
  "Find in `tagged-buflist/group-sections' non-`tagged-buflist/section-not-visible?'
section closest to START-IDX in direction depending on DELTA."
  (let* ((group-sections-len (length tagged-buflist/group-sections))
         (normalize-idx
          (lambda (idx)
            (if (< idx 0)
              (- group-sections-len 1)
              (cl-rem idx group-sections-len))))
         (iterations group-sections-len))
    (while (and (tagged-buflist/section-not-visible? (aref tagged-buflist/group-sections
                                                           index))
                (< 0 iterations))
      (setf index (funcall normalize-idx (+ index delta))
            iterations (- iterations 1)))
    (if (= iterations 0)
      nil
      index)))

(defun tagged-buflist/select-forward ()
  "Jump to next visibe section with wrapping around in buffer."
  (interactive)
  (tagged-buflist/with-section-for-line section
    (let* ((group-sections-len (length tagged-buflist/group-sections))
           (normalize-idx
            (lambda (idx)
              (cl-rem idx group-sections-len)))
           (section-idx
            ;; rightmost index may be equal to group-sections-len
            (funcall normalize-idx (bisect-rightmost section
                                                     tagged-buflist/group-sections
                                                     0
                                                     group-sections-len
                                                     #'tagged-section/start=
                                                     #'tagged-section/start<)))
           (next-idx
            (pcase (tagged-section/type section)
              (`group
               (tagged-buflist/find-visible-section
                (cl-rem (+ section-idx 1) group-sections-len)
                +1))
              (`buffer
               section-idx))))
      (tagged-buflist/goto-section (aref tagged-buflist/group-sections next-idx)))))

(defun tagged-buflist/select-backward ()
  "Jump to previous visibe section with wrapping around in buffer."
  (interactive)
  (tagged-buflist/with-section-for-line section
    (let* ((group-sections-len (length tagged-buflist/group-sections))
           (normalize-idx
            (lambda (idx)
              (if (< idx 0)
                (- group-sections-len 1)
                idx)))
           (section-idx (bisect-leftmost section
                                         tagged-buflist/group-sections
                                         0
                                         group-sections-len
                                         #'tagged-section/start=
                                         #'tagged-section/start<))
           (prev-idx
            (pcase (tagged-section/type section)
              (`group
               ;; In this case approximate index is okay because
               ;; all sections in this "group" share the same beginning
               (tagged-buflist/find-visible-section
                (funcall normalize-idx
                         (cl-rem (- section-idx 1) group-sections-len))
                -1))
              (`buffer
               (- section-idx 1)))))
      (tagged-buflist/goto-section (aref tagged-buflist/group-sections prev-idx)))))

;;; tagged-buflist-mode

(defparameter tagged-buflist/tag-hierarchy
  (list #'tagged-buflist/generate-tag-groups-by-eproj
        +common-buffer-tags+)
  "Hierarchy definition (list of lists/functions of tags) to show
tagged bufer list.")

(defparameter tagged-buflist-mode-map
  (let ((map (make-sparse-keymap)))
    (dotimes (k 128)
      (define-key map [k] nil))
    (def-keys-for-map map
      ("t"               next-line)
      ("n"               previous-line)
      ("h"               backward-char)
      ("s"               forward-char)
      ("<down>"          next-line)
      ("<up>"            previous-line)
      ("<left>"          backward-char)
      ("<right>"         forward-char)
      ;; +vi-search-keys+
      +control-x-prefix+
      +vim-special-keys+
      ("<return>"        tagged-buflist/switch-to-buffer-at-point-or-cycle)
      ("SPC"             tagged-buflist/switch-to-buffer-at-point-other-window-or-cycle)
      ("d"               tagged-buflist/delete-marked-buffers)
      ("o"               tagged-buflist/switch-to-buffer-at-point-other-window-or-cycle)
      ("r"               tagged-buflist/refresh)
      ("`"               tagged-buflist/toggle-filenames)

      ("m"               tagged-buflist/mark-buffers-at-point)
      ("* m"             tagged-buflist/mark-buffers-by-major-mode)
      ("* %"             tagged-buflist/mark-buffers-by-file-name)
      ("u"               tagged-buflist/unmark-buffers-at-point)
      ("U"               tagged-buflist/unmark-all)
      ("<f2>"            tagged-buflist/save-marked-buffers)
      ("e"               tagged-buflist/eval-in-marked-buffers)

      ("S-TAB"           tagged-buflist/select-backward)
      ("S-<tab>"         tagged-buflist/select-backward)
      ("S-<iso-lefttab>" tagged-buflist/select-backward)
      ("TAB"             tagged-buflist/select-forward)
      ("<tab>"           tagged-buflist/select-forward)
      ("'"               tagged-buflist/select-parent)
      ("/"               tagged-buflist/jump-to-buffer))
    map))


(define-derived-mode tagged-buflist-mode nil "Tagged buflist"
  "Major mode for displaying and manipulating list of tagged buffer.
Similar to `ibuffer-mode'."
  (font-lock-mode -1)
  (undo-tree-mode -1)
  (setq-local org-pretty-entities nil)
  (toggle-truncate-lines +1)
  (setq-local mode-line-format
              '(" %[%b%] "
                (:eval (when buffer-read-only
                         "(RO)"))
                ("("
                 mode-name
                 ")")
                (:eval
                 (when (buffer-narrowed?)
                   "(Narrowed)"))
                " "
                (line-number-mode
                 ("%l/"
                  (:eval (number-to-string
                          (count-lines (point-min)
                                       (point-max))))
                  "(%p)"))))

  ;; (dolist (face '(outline-1 outline-2
  ;;                 outline-3 outline-4
  ;;                 outline-5 outline-6
  ;;                 outline-7 outline-8))
  ;;   (face-remap-add-relative face 'default))
  )


;;; end

(provide 'tagged-buflist)

;; Local Variables:
;; End:

;; tagged-buflist.el ends here
