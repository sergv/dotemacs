;; org-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 December 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)

  (require 'macro-util))

(require 'common)
(require 'common-font)
(require 'dash)
(require 'el-patch)
(require 'indentation)
(require 'render-formula)
(require 'vim-setup)

;; tangling
(require 'ob)

;;;###autoload
(el-patch-feature ob-tangle)

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

      ;; Do not align node's contents to the indentation of the parent heading.
      org-adapt-indentation nil

      ;; Enable `org-indent-mode' in all files on startup.
      org-startup-indented t
      ;; Make tab indent within code blocks as if it was issued in the
      ;; proper mode for block's language.
      org-src-tab-acts-natively t

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
      '((C          . t)
        (clojure    . nil)
        (dot        . t)
        (emacs-lisp . t)
        (haskell    . t)
        (js         . nil)
        (latex      . t)
        (lisp       . t)
        (maxima     . t)
        (ocaml      . t)
        (octave     . t)
        (org        . t)
        (oz         . nil)
        (python     . t)
        (R          . nil)
        (scheme     . t)
        (shell      . t)
        (sql        . nil)
        (sqlite     . nil)))))

(eval-after-load
    "org-src"
  '(progn
     (setf org-src-lang-modes
           (cons (cons "haskell" 'haskell-mode)
                 (cons (cons "scheme" 'scheme-mode)
                       (cons (cons "dot" 'graphviz-dot-mode)
                             (cl-remove-if (lambda (entry)
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
                 org-babel-tangle-lang-exts))))

;;; other functions

(defun org-mode-up-heading ()
  "Move to the the beginning of heading or one level up in heading hierarchy."
  (interactive)
  (if (= (line-beginning-position) (point))
    (outline-up-heading 1)
    (outline-previous-heading)))

;;;###autoload
(defun org-format-buffer ()
  (interactive)
  (indent-whole-buffer))

(puthash 'org-mode
         #'org-format-buffer
         *mode-indent-functions-table*)

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

(vim-defcmd vim:org-mode-export (nonrepeatable)
  (save-buffer)
  (org-export-dispatch))

(vim-defcmd vim:org-mode-tangle (nonrepeatable)
  (save-buffer)
  (org-babel-tangle))

(vim-defcmd vim:org-mode-make-beamer-presentation (nonrepeatable)
  (save-buffer)
  (org-beamer-export-to-pdf))

(vim-defcmd vim:org-latex-export-to-pdf (nonrepeatable)
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

(defun org-mode-space-abbrev+ (&optional dont-expand)
  (interactive "P")
  (when (or dont-expand
            (not (abbrev+-expand)))
    (org-self-insert-command 1)))

(defhydra-derive org-mode-vim-normal-z-outline hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_O_: show all outlines
_c_: hide subtree
_C_: hide everything except current entry and its parents"
  ("O" outline-show-all)
  ("o" outline-show-subtree)
  ("c" outline-hide-subtree)
  ("C" outline-hide-other))

(defun org-mode-setup ()
  ;; Harfbuzz and texture healing break rendering of tables and make
  ;; some | separators disappear.
  (common-font--use-default-composition-function-table!)
  (init-common :use-yasnippet t
               :use-render-formula nil
               :use-whitespace 'tabs-only
               :use-fci t)
  (typography-setup)
  (setup-indent-size 2)
  (setq-local tab-width 8)
  (bind-tab-keys #'org-cycle
                 #'org-shifttab
                 :enable-yasnippet t)
  (vim-local-emap "beamer" #'vim:org-mode-make-beamer-presentation)
  (vim-local-emap "export" #'vim:org-mode-export)
  (vim-local-emap "tangle" #'vim:org-mode-tangle)
  (vim-local-emap "pdf"    #'vim:org-latex-export-to-pdf)
  (def-keys-for-map vim-normal-mode-local-keymap
    ("M-<up>"   org-metaup)
    ("M-<down>" org-metadown)

    ("<print>"  org-toggle-inline-images-and-formulae)
    ("C-."      org-open-at-point)
    ("C-,"      org-mark-ring-goto)
    ("C-o"      org-open-at-point)

    ("'"        org-mode-up-heading)

    ("z"        org-mode-vim-normal-z-outline/body)

    ("C-1"      org-mode-show-level-1)
    ("C-2"      org-mode-show-level-2)
    ("C-3"      org-mode-show-level-3)
    ("C-4"      org-mode-show-level-4)
    ("C-5"      org-mode-show-level-5)
    ("C-6"      org-mode-show-level-6)
    ("C-7"      org-mode-show-level-7)
    ("C-8"      org-mode-show-level-8)
    ("C-9"      org-mode-show-level-9))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("<tab>"   org-indent-region))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("<f6>"         org-toggle-display-style)
    (("C-m" "<f9>") vim:org-latex-export-to-pdf:interactive)
    ("S-<f9>"       open-buffer-as-pdf)
    ("C-SPC"        pcomplete)
    ("<C-return>"   org-meta-return)
    ("<C-down>"     org-metadown)
    ("<C-up>"       org-metaup)
    ("<C-left>"     org-metaleft)
    ("<C-right>"    org-metaright)
    ("C-="          input-unicode)
    ("<return>"     newline-and-indent))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap
                     vim-operator-pending-mode-local-keymap
                     vim-motion-mode-local-keymap)
    ("0" vim:org-beginning-of-line:interactive)
    ("$" vim:org-end-of-line:interactive))

  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC"  org-mode-space-abbrev+))

  (def-keys-for-map org-mode-map
    ("C-k"   nil)
    ("C-t"   org-todo)
    ("SPC"   org-mode-space-abbrev+)))

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
