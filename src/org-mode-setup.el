;; org-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 December 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'el-patch)
(require 'org-drill)
(require 'render-formula)

;; for reveal.js presentations
(require 'htmlize)
(require 'ox-reveal)

;; tangling
(require 'ob)

;;;###autoload
(el-patch-feature ob-tangle)
;;;###autoload
(el-patch-feature org-drill)

(def-keys-for-map global-map
  ("C-c l" org-store-link)
  ("C-c a" org-agenda))

;; org mode customizations
(setf org-agenda-ndays 7
      org-deadline-warning-days 7
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-agenda-files (-filter #'file-exists?
                                (list (concat +emacs-config-path+ "/todo.org")
                                      "/home/sergey/projects/todo.org"))
      ;; notes are stored in descending date order - most recent always at top
      org-reverse-note-order t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-fontify-quote-and-verse-blocks t
      org-use-property-inheritance nil ;; '("DRILL_CARD_TYPE")

      org-highlight-latex-fragments-and-specials t
      org-latex-default-packages-alist
      '(("AUTO" "inputenc"  t)
        ("T1"   "fontenc"   t)
        (""     "fixltx2e"  nil)
        ;; amsmath should be one of the first, it appears to define \iiint in
        ;; some particular way and barfs when someone already defined it
        ;; at the time of \usepackage{amsmath}. I suspect that wasysym or
        ;; someone near it does this to \iiint and upsets amsmath, therefore
        ;; amsmath is included among the firts.
        (""     "amsmath"   t)
        (""     "graphicx"  t)
        (""     "longtable" nil)
        (""     "float"     nil)
        (""     "wrapfig"   nil)
        (""     "soul"      t)
        (""     "textcomp"  t)
        (""     "marvosym"  t)
        (""     "wasysym"   t)
        (""     "latexsym"  t)
        (""     "amssymb"   t)
        (""     "amstext"   nil)
        (""     "hyperref"  nil)
        "\\tolerance=1000")

      org-pretty-entities t

      ;; fontify code in code blocks
      org-src-fontify-natively t
      ;; if block has an active edit buffer, it `org-edit-src'
      ;; will switch to that buffer immediately
      org-src-ask-before-returning-to-edit-buffer nil

      org-use-fast-todo-selection t
      org-log-done 'time
      org-log-into-drawer nil
      org-log-state-notes-insert-after-drawers nil
      org-log-reschedule 'note

      org-drill-leech-method 'warn
      org-drill-use-visible-cloze-face-p t
      org-drill-hide-item-headings-p t
      org-drill-maximum-items-per-session 15
      org-drill-maximum-duration 15                     ;; minutes
      org-drill-save-buffers-after-drill-sessions-p nil ;; don't prompt for save
      ;; this may be useful when working with large amounts of items
      org-drill-add-random-noise-to-intervals-p t

      org-src-preserve-indentation t)

(setf org-entities-user
      '(("triangleleft"   ;; name
         "\\triangleleft" ;; LaTeX
         t                ;; math mode
         "&#x25c1;"       ;; html
         "<|"             ;; ascii
         "<|"             ;; latin1
         "◁"              ;; utf8
         )
        ("triangleright"
         "\\triangleright"
         t
         "&#x25b7;"
         "|>"
         "|>"
         "▷")
        ("lbrace"
         "\\lbrace"
         t
         "&#123;"
         "{"
         "{"
         "{")
        ("rbrace"
         "\\rbrace"
         t
         "&#125;"
         "}"
         "}"
         "}")))

(add-to-list 'org-babel-tangle-lang-exts '("haskell" . "hs"))

;;; eval-after-load's

(defface org-started
  '((t (:inherit default)))
  "Face to highlight started tasks."
  :group 'org-mode)

(eval-after-load
    "org"
  '(progn
     (setf org-todo-keywords
           '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!)")
             (sequence "|" "CANCELLED(c@/!)" "ON HOLD(h@/!)"))
           org-todo-keyword-faces
           '(("STARTED"   . org-started)
             ("CANCELLED" . org-cancelled)
             ("ON HOLD"   . org-waiting)))

     ;; customize it to receive width and height arguments of inline image
     ;; (redefun org-display-inline-images (&optional include-linked refresh beg end)
     ;;   "Display inline images.
;; Normally only links without a description part are inlined, because this
;; is how it will work for export.  When INCLUDE-LINKED is set, also links
;; with a description part will be inlined.  This can be nice for a quick
;; look at those images, but it does not reflect what exported files will look
;; like.
;; When REFRESH is set, refresh existing images between BEG and END.
;; This will create new image displays only if necessary.
;; BEG and END default to the buffer boundaries."
;;        (interactive "P")
;;        (unless refresh
;;          (org-remove-inline-images)
;;          (if (fboundp 'clear-image-cache) (clear-image-cache)))
;;        (save-excursion
;;          (save-restriction
;;            (widen)
;;            (setq beg (or beg (point-min)) end (or end (point-max)))
;;            (goto-char beg)
;;            (let ((re (concat "\\(?:#+[+ ]*INLINE:[ \t]*\\(.*\\)[ \t]*\n[ \t]*\\(?:#.*\n[ \t]*\\)*\\)?\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^\]\n]+?"
;;                              (substring (org-image-file-name-regexp) 0 -2)
;;                              "\\)\\]" (if include-linked "" "\\]")))
;;                  options old file ov img)
;;              (while (re-search-forward re end t)
;;                (when (image-type-available-p 'imagemagick)
;;                  (setq options (match-string-no-properties 1)))
;;                (setq old (get-char-property-and-overlay (match-beginning 2)
;;                                                         'org-image-overlay))
;;                (setq file (expand-file-name
;;                            (concat (or (match-string 4) "") (match-string 5))))
;;                (when (file-exists-p file)
;;                  (if (and (car-safe old) refresh)
;;                    (image-refresh (overlay-get (cdr old) 'display))
;;                    (setq img (save-match-data (create-image file)))
;;                    (when (and options
;;                               (image-type-available-p 'imagemagick))
;;                      (save-match-data
;;                        (let ((scaled? nil))
;;                          (when (string-match? "width\s*=\s*\\([0-9]+\\)" options)
;;                            (setf img (append img
;;                                              (list ':width
;;                                                    (read
;;                                                     (match-string-no-properties 1 options))))
;;                                  scaled? t)
;;                            (setf (getf (cdr img) :type) 'imagemagick))
;;                          (when (string-match? "height\s*=\s*\\([0-9]+\\)" options)
;;                            (setf img (append img
;;                                              (list ':height
;;                                                    (read
;;                                                     (match-string-no-properties 1 options))))
;;                                  scaled? t))
;;                          (when scaled?
;;                            (setf (getf (cdr img) :type) 'imagemagick)))))
;;                    (when img
;;                      (setq ov (make-overlay (match-beginning 0) (match-end 0)))
;;                      (overlay-put ov 'display img)
;;                      (overlay-put ov 'face 'default)
;;                      (overlay-put ov 'org-image-overlay t)
;;                      (overlay-put ov 'modification-hooks
;;                                   (list 'org-display-inline-modification-hook))
;;                      (push ov org-inline-image-overlays)))))))))

     (org-babel-do-load-languages
      'org-babel-load-languages
      '((C          . nil)
        (clojure    . nil)
        (dot        . nil)
        (emacs-lisp . nil)
        (haskell    . t)
        (js         . nil)
        (latex      . nil)
        (lisp       . nil)
        (ocaml      . nil)
        (octave     . nil)
        (org        . nil)
        (oz         . nil)
        (python     . nil)
        (R          . nil)
        (scheme     . nil)
        (sh         . nil)
        (sql        . nil)
        (sqlite     . nil)))))

(eval-after-load
    "org-src"
  '(progn
     (setf org-src-lang-modes
           (cons (cons "haskell" 'haskell-mode)
                 (cons (cons "scheme" 'scheme-mode)
                       (cons (cons "dot" 'graphviz-dot-mode)
                             (remove-if (lambda (entry)
                                          (member (car entry)
                                                  '("dot"
                                                    "scheme"
                                                    "haskell")))
                                        org-src-lang-modes)))))))

(eval-after-load
    "ob-tangle"
  '(progn
     (setf org-babel-tangle-lang-exts
           (cons (cons "scheme" "scm")
                 org-babel-tangle-lang-exts))

     ;; Fix issue when org-bracket-link-analytic-regexp matches ordinary link
     ;; so that (match-string 5) - which is supposed to be the source name -
     ;; becomes nil
     (el-patch-defun org-babel-detangle (&optional source-code-file)
       "Propagate changes in source file back original to Org file.
This requires that code blocks were tangled with link comments
which enable the original code blocks to be found."
       (interactive)
       (save-excursion
         (when source-code-file (find-file source-code-file))
         (goto-char (point-min))
         (let ((counter 0) new-body end)
           (while (re-search-forward org-bracket-link-analytic-regexp nil t)
             (when (el-patch-wrap 2 0
                     (and (match-string 5)
                          (re-search-forward
                           (concat " " (regexp-quote (match-string 5)) " ends here"))))
               (setq end (match-end 0))
               (forward-line -1)
               (save-excursion
                 (when (setq new-body (org-babel-tangle-jump-to-org))
                   (org-babel-update-block-body new-body)))
               (setq counter (+ 1 counter)))
             (el-patch-wrap 2 0
               (and end
                    (goto-char end))))
           (prog1 counter (message "Detangled %d code blocks" counter)))))))

;;; common org-drill's question cards with math rendering
;;; and other setup

(defparameter *org-drill-hint-tags* '("reveal" "hint" "example")
  "Subheadings with these tags will be shown revealed during question.
These tags will be inherited by all subheadings. Use like this:

** foo question         :drill:
   foo is [bar]

*** Example             :hint:
**** baz
**** quux
")

(defun org-drill-present-common-card ()
  "Similar to `org-drill-present-simple-card' but also expands latex formulas
into images."
  (with-hidden-comments
   (with-hidden-cloze-hints
    (with-hidden-cloze-text
     (render-buffer-off)
     (let ((org-use-tag-inheritance (or org-use-tag-inheritance
                                        *org-drill-hint-tags*)))
       (org-drill-hide-subheadings-if
        ;; return nil if subheading is to be revealed
        (lambda ()
          (not (--any? (member it *org-drill-hint-tags*)
                       (org-get-tags-at))))))
     (ignore-errors
       (org-display-inline-images t))
     (org-cycle-hide-drawers 'all)
     (render-buffer-on)
     (prog1 (org-drill-presentation-prompt)
       (render-buffer-off)
       (org-drill-hide-subheadings-if 'org-drill-entry-p))))))

(defun org-drill-present-common-answer (reschedule-fn)
  "If `org-drill-present-common-card' is yin then this is yang - this function
handles formula rendering during answer showing and restores original text
when question is rated."
  (org-drill-hide-subheadings-if 'org-drill-entry-p)
  (org-drill-unhide-clozed-text)
  (ignore-errors
    (org-display-inline-images t))
  (render-buffer-on)
  (prog1 (with-hidden-cloze-hints
          (funcall reschedule-fn))
    (render-buffer-off)))

(defadvice org-drill-reschedule (before
                                 org-drill-reschedule-hide-drawers
                                 activate
                                 compile)
  (org-cycle-hide-drawers 'all))

(setf org-drill-card-type-alist
      (cons (list "common"
                  #'org-drill-present-common-card
                  #'org-drill-present-common-answer)
            (cons (list nil
                        #'org-drill-present-common-card
                        #'org-drill-present-common-answer)
                  (remove-if (lambda (x)
                               (member* (car x) '(nil "common")
                                        :test (lambda (a b)
                                                (or (eq? a b) (string=? a b)))))
                             org-drill-card-type-alist))))


(eval-after-load
    "org-drill"
  '(progn
     (setf org-drill-optimal-factor-matrix
           (persistent-store-get 'org-drill-optimal-factor-matrix))

     (el-patch-defun org-drill-save-optimal-factor-matrix ()
       (el-patch-swap
         (savehist-autosave)
         (progn
           (message "Saving optimal factor matrix...")
           (persistent-store-put 'org-drill-optimal-factor-matrix

                                 org-drill-optimal-factor-matrix))))

     ;; remove unconditional hiding of sub-sublevels
     (el-patch-defun org-drill-hide-subheadings-if (test)
       "TEST is a function taking no arguments. TEST will be called for each
of the immediate subheadings of the current drill item, with the point
on the relevant subheading. TEST should return nil if the subheading is
to be revealed, non-nil if it is to be hidden.
Returns a list containing the position of each immediate subheading of
the current topic."
       (let ((drill-entry-level (org-current-level))
             (drill-sections nil))
         (org-show-subtree)
         (save-excursion
           (org-map-entries
            (lambda ()
              (when (and (not (org-invisible-p))
                         (> (org-current-level) drill-entry-level))
                (when (el-patch-splice 2 0
                        (or (/= (org-current-level) (1+ drill-entry-level))
                            (funcall test)))
                  (hide-subtree))
                (push (point) drill-sections)))
            "" 'tree))
         (reverse drill-sections)))))

;;; other functions

(defun org-mode-up-heading ()
  "Move to the the beginning of heading or one level up in heading hierarchy."
  (interactive)
  (if (= (line-beginning-position) (point))
    (outline-up-heading 1)
    (outline-previous-heading)))


(vimmize-motion org-beginning-of-line)
(vimmize-motion org-end-of-line)

(defun org-toggle-display-style ()
  (interactive)
  (org-toggle-link-display)
  ;; pretty display of \[a-z]+ latex-like entries
  (org-toggle-pretty-entities))

(defun org-toggle-inline-images-and-formulae ()
  (interactive)
  (org-toggle-inline-images)
  (render-formula-toggle-formulae))

(vim:defcmd vim:org-mode-make-revealjs-presentation (nonrepeatable)
  (save-buffer)
  (with-disabled-fci
   (org-reveal-export-to-html))
  (message "Done"))

(vim:defcmd vim:org-mode-export (nonrepeatable)
  (save-buffer)
  (with-disabled-fci
   (org-export-dispatch)))

(vim:defcmd vim:org-mode-tangle (nonrepeatable)
  (save-buffer)
  (with-disabled-fci
   (org-babel-tangle)))

(vim:defcmd vim:org-mode-make-beamer-presentation (nonrepeatable)
  (save-buffer)
  (with-disabled-fci
   (org-beamer-export-to-pdf)))

(vim:defcmd vim:org-latex-export-to-pdf (nonrepeatable)
  (save-buffer)
  (org-latex-export-to-pdf))

(defun org-mode-show-level-1 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 1))

(defun org-mode-show-level-2 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 2))

(defun org-mode-show-level-3 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 3))

(defun org-mode-show-level-4 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 4))

(defun org-mode-show-level-5 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 5))

(defun org-mode-show-level-6 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 6))

(defun org-mode-show-level-7 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 7))

(defun org-mode-show-level-8 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 8))

(defun org-mode-show-level-9 ()
  (interactive)
  (outline-hide-subtree)
  (outline-show-children 9))

(defun org-mode-setup ()
  (init-common :use-yasnippet t
               :use-render-formula nil
               :use-whitespace 'tabs-only
               :use-fci t)
  (typography-setup)
  (setup-indent-size 2)
  (bind-tab-keys #'org-cycle
                 #'org-shifttab
                 :enable-yasnippet t)
  (vim:local-emap "beamer" #'vim:org-mode-make-beamer-presentation)
  (vim:local-emap "reveal" #'vim:org-mode-make-revealjs-presentation)
  (vim:local-emap "export" #'vim:org-mode-export)
  (vim:local-emap "tangle" #'vim:org-mode-tangle)
  (vim:local-emap "pdf"    #'vim:org-latex-export-to-pdf)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("<print>" org-toggle-inline-images-and-formulae)
    ("-"       vim:org-mode-make-beamer-presentation)
    ("C-."     org-open-at-point)
    ("C-,"     org-mark-ring-goto)
    ("C-o"     org-open-at-point)

    ("'"       org-mode-up-heading)

    ("z O"     outline-show-all)
    ;; ("z o"   show-subtree)
    ("z c"     outline-hide-subtree)
    ;; hide everything except current entry and its parents
    ("z C"     outline-hide-other)

    ("j"       eval-last-sexp)

    ("C-1"     org-mode-show-level-1)
    ("C-2"     org-mode-show-level-2)
    ("C-3"     org-mode-show-level-3)
    ("C-4"     org-mode-show-level-4)
    ("C-5"     org-mode-show-level-5)
    ("C-6"     org-mode-show-level-6)
    ("C-7"     org-mode-show-level-7)
    ("C-8"     org-mode-show-level-8)
    ("C-9"     org-mode-show-level-9))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("j"       eval-region))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<f6>"       org-toggle-display-style)
    ("<f9>"       vim:org-latex-export-to-pdf)
    ("S-<f9>"     open-buffer-as-pdf)
    ("C-SPC"      pcomplete)
    ("<C-return>" org-meta-return)
    ("<C-down>"   org-metadown)
    ("<C-up>"     org-metaup)
    ("<C-left>"   org-metaleft)
    ("<C-right>"  org-metaright)
    ("C-="        input-unicode))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:operator-pending-mode-local-keymap
                     vim:motion-mode-local-keymap)
    ("0" vim:org-beginning-of-line)
    ("$" vim:org-end-of-line))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC"   abbrev+-org-self-insert-or-expand-abbrev))

  (def-keys-for-map org-mode-map
    ("C-k"   nil)
    ("C-t"   org-todo)
    ("SPC"   abbrev+-org-self-insert-or-expand-abbrev)))

(defun org-agenda-mode-setup ()
  (def-keys-for-map org-agenda-mode-map
    +vim-special-keys+
    +vim-search-keys+
    ("h"   org-agenda-next-line)
    ("t"   org-agenda-previous-line)
    ("C-t" org-agenda-todo)))

;;; epilogue

(provide 'org-mode-setup)

;; Local Variables:
;; End:

;; org-mode-setup.el ends here
