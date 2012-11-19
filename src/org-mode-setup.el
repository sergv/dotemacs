;; org-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'set-up-paths)
(require 'common)

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/org-mode"))

(load-library "org-install")
(require 'org-drill)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; org mode customizations
(setf org-agenda-ndays 7
      org-deadline-warning-days 7
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-agenda-files (filter #'file-exist?
                               (list (concat +emacs-config-path+ "/todo.org")
                                     "/home/sergey/projects/todo.org"
                                     "/home/sergey/university/todo.org"))
      ;; notes are stored in descending date order - most recent always at top
      org-reverse-note-order t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-use-property-inheritance nil ;; '("DRILL_CARD_TYPE")

      org-highlight-latex-fragments-and-specials t
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
      org-drill-maximum-items-per-session 100
      org-drill-maximum-duration 15 ;; minutes
      org-drill-save-buffers-after-drill-sessions-p nil ;; don't prompt for save
      ;; this may be useful when working with large amounts of items
      ;; org-drill-add-random-noise-to-intervals-p t
      )

;;;; common org-drills' question cards with math rendering

(defun org-drill-present-common-card ()
  "Similar to `org-drill-present-simple-card' but also expands latex formulas
into images."
  (with-hidden-comments
   (with-hidden-cloze-hints
    (with-hidden-cloze-text
     (render-buffer-off)
     (org-drill-hide-all-subheadings-except nil)
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

;;;; eval-after-load's

(eval-after-load
 "org"
 '(progn
   (setf org-todo-keywords
         '((sequence "TODO(t)" "WAITING(w!)" "STARTED(s!)" "|" "DONE(d!)")
           (sequence "|" "CANCELLED(c@)"))
         org-todo-keyword-faces
         '(("WAITING"   . org-waiting)
           ("STARTED"   . org-started)
           ("CANCELLED" . org-cancelled)))

   ;; change cursor used in calendar from empty to filled box
   (redefun org-read-date (&optional org-with-time to-time from-string prompt
                                     default-time default-input inactive)
     "Read a date, possibly a time, and make things smooth for the user.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month, year,
hour and minute.  If this command is called to replace a timestamp at point,
or to enter the second timestamp of a range, the default time is taken
from the existing stamp.  Furthermore, the command prefers the future,
so if you are giving a date where the year is not given, and the day-month
combination is already past in the current year, it will assume you
mean next year.  For details, see the manual.  A few examples:

  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  2/15          --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  12            --> currentyear-currentmonth-12
  Fri           --> nearest Friday (today or later)
  etc.

Furthermore you can specify a relative date by giving, as the *first* thing
in the input:  a plus/minus sign, a number and a letter [hdwmy] to indicate
change in days weeks, months, years.
With a single plus or minus, the date is relative to today.  With a double
plus or minus, it is relative to the date in DEFAULT-TIME.  E.g.
  +4d           --> four days from today
  +4            --> same as above
  +2w           --> two weeks from today
  ++5           --> five days from default date

The function understands only English month and weekday abbreviations.

While prompting, a calendar is popped up - you can also select the
date with the mouse (button 1).  The calendar shows a period of three
months.  To scroll it to other months, use the keys `>' and `<'.
If you don't like the calendar, turn it off with
       \(setq org-read-date-popup-calendar nil)

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument ORG-WITH-TIME, the prompt will suggest to
also insert a time.  Note that when ORG-WITH-TIME is not set, you can
still enter a time, and this function will inform the calling routine
about this change.  The calling routine may then choose to change the
format used to insert the time stamp into the buffer to include the time.
With optional argument FROM-STRING, read from this string instead from
the user.  PROMPT can overwrite the default prompt.  DEFAULT-TIME is
the time/date that is used for everything that is not specified by the
user."
     (require 'parse-time)
     (let* ((org-time-stamp-rounding-minutes
              (if (equal org-with-time '(16)) '(0 0) org-time-stamp-rounding-minutes))
            (org-dcst org-display-custom-times)
            (ct (org-current-time))
            (org-def (or org-overriding-default-time default-time ct))
            (org-defdecode (decode-time org-def))
            (dummy (progn
                     (when (< (nth 2 org-defdecode) org-extend-today-until)
                       (setcar (nthcdr 2 org-defdecode) -1)
                       (setcar (nthcdr 1 org-defdecode) 59)
                       (setq org-def (apply 'encode-time org-defdecode)
                             org-defdecode (decode-time org-def)))))
            (calendar-frame-setup nil)
            (calendar-setup nil)
            (calendar-move-hook nil)
            (calendar-view-diary-initially-flag nil)
            (calendar-view-holidays-initially-flag nil)
            (timestr (format-time-string
                      (if org-with-time "%Y-%m-%d %H:%M" "%Y-%m-%d") org-def))
            (prompt (concat (if prompt (concat prompt " ") "")
                            (format "Date+time [%s]: " timestr)))
            ans (org-ans0 "") org-ans1 org-ans2 final)

       (cond
         (from-string (setq ans from-string))
         (org-read-date-popup-calendar
          (save-excursion
           (save-window-excursion
            (calendar)
            (org-eval-in-calendar '(setq cursor-type 'box) t)
            (unwind-protect
                 (progn
                   (calendar-forward-day (- (time-to-days org-def)
                                            (calendar-absolute-from-gregorian
                                             (calendar-current-date))))
                   (org-eval-in-calendar nil t)
                   (let* ((old-map (current-local-map))
                          (map (copy-keymap calendar-mode-map))
                          (minibuffer-local-map (copy-keymap minibuffer-local-map)))
                     (org-defkey map (kbd "RET") 'org-calendar-select)
                     (org-defkey map [mouse-1] 'org-calendar-select-mouse)
                     (org-defkey map [mouse-2] 'org-calendar-select-mouse)
                     (org-defkey minibuffer-local-map [(meta shift left)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-backward-month 1))))
                     (org-defkey minibuffer-local-map [(meta shift right)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-forward-month 1))))
                     (org-defkey minibuffer-local-map [(meta shift up)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-backward-year 1))))
                     (org-defkey minibuffer-local-map [(meta shift down)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-forward-year 1))))
                     (org-defkey minibuffer-local-map [?\e (shift left)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-backward-month 1))))
                     (org-defkey minibuffer-local-map [?\e (shift right)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-forward-month 1))))
                     (org-defkey minibuffer-local-map [?\e (shift up)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-backward-year 1))))
                     (org-defkey minibuffer-local-map [?\e (shift down)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-forward-year 1))))
                     (org-defkey minibuffer-local-map [(shift up)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-backward-week 1))))
                     (org-defkey minibuffer-local-map [(shift down)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-forward-week 1))))
                     (org-defkey minibuffer-local-map [(shift left)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-backward-day 1))))
                     (org-defkey minibuffer-local-map [(shift right)]
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(calendar-forward-day 1))))
                     (org-defkey minibuffer-local-map ">"
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(scroll-calendar-left 1))))
                     (org-defkey minibuffer-local-map "<"
                                 (lambda () (interactive)
                                   (org-eval-in-calendar '(scroll-calendar-right 1))))
                     (org-defkey minibuffer-local-map "\C-v"
                                 (lambda () (interactive)
                                   (org-eval-in-calendar
                                    '(calendar-scroll-left-three-months 1))))
                     (org-defkey minibuffer-local-map "\M-v"
                                 (lambda () (interactive)
                                   (org-eval-in-calendar
                                    '(calendar-scroll-right-three-months 1))))
                     (run-hooks 'org-read-date-minibuffer-setup-hook)
                     (unwind-protect
                          (progn
                            (use-local-map map)
                            (setq org-read-date-inactive inactive)
                            (add-hook 'post-command-hook 'org-read-date-display)
                            (setq org-ans0 (read-string prompt default-input
                                                        'org-read-date-history nil))
                            ;; org-ans0: from prompt
                            ;; org-ans1: from mouse click
                            ;; org-ans2: from calendar motion
                            (setq ans (concat org-ans0 " " (or org-ans1 org-ans2))))
                       (remove-hook 'post-command-hook 'org-read-date-display)
                       (use-local-map old-map)
                       (when org-read-date-overlay
                         (delete-overlay org-read-date-overlay)
                         (setq org-read-date-overlay nil)))))
              (bury-buffer "*Calendar*")))))

         (t ; Naked prompt only
          (unwind-protect
               (setq ans (read-string prompt default-input
                                      'org-read-date-history timestr))
            (when org-read-date-overlay
              (delete-overlay org-read-date-overlay)
              (setq org-read-date-overlay nil)))))

       (setq final (org-read-date-analyze ans org-def org-defdecode))

       (when org-read-date-analyze-forced-year
         (message "Year was forced into %s"
                  (if org-read-date-force-compatible-dates
                    "compatible range (1970-2037)"
                    "range representable on this machine"))
         (ding))

       ;; One round trip to get rid of 34th of August and stuff like that....
       (setq final (decode-time (apply 'encode-time final)))

       (setq org-read-date-final-answer ans)

       (if to-time
         (apply 'encode-time final)
         (if (and (boundp 'org-time-was-given) org-time-was-given)
           (format "%04d-%02d-%02d %02d:%02d"
                   (nth 5 final) (nth 4 final) (nth 3 final)
                   (nth 2 final) (nth 1 final))
           (format "%04d-%02d-%02d" (nth 5 final) (nth 4 final) (nth 3 final))))))))

(eval-after-load
 "org-comat"
 '(progn
   ;; add handling of vim's region
   (redefun org-region-active-p ()
     "Is `transient-mark-mode' on and the region active?
Works on both Emacs and XEmacs."
     (unless org-ignore-region
       (cond
         ((featurep 'xemacs)
          (and zmacs-regions (region-active-p)))

         ((region-active-p)
          t)
         ((run-if-fbound vim:visual-mode-p)
          t)
         ((fboundp 'use-region-p)
          (use-region-p))
         (transient-mark-mode
          mark-active))))))

(eval-after-load
 "org-list"
 '(progn
   ;; (redefun org-update-checkbox-count (&optional all)
   ;;   "Update the checkbox statistics in the current section.
   ;; This will find all statistic cookies like [57%] and [6/12] and update
   ;; them with the current numbers.  With optional prefix argument ALL,
   ;; do this for the whole buffer."
   ;;      (interactive "P")
   ;;      (save-excursion
   ;;       (let* ((buffer-invisibility-spec (org-inhibit-invisibility))
   ;;              (beg (condition-case nil
   ;;                       (progn (outline-back-to-heading) (point))
   ;;                     (error (point-min))))
   ;;              (end (move-marker
   ;;                    (make-marker)
   ;;                    (progn (or (outline-get-next-sibling) ;; (1)
   ;;                               (goto-char (point-max)))
   ;;                           (point))))
   ;;              (re "\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)")
   ;;              (re-box
   ;;                "^[ \t]*\\(*+\\|[-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
   ;;              b1 e1 f1 c-on c-off lim (cstat 0))
   ;;         (when all
   ;;           (goto-char (point-min))
   ;;           (or (outline-get-next-sibling) (goto-char (point-max))) ;; (2)
   ;;           (setq beg (point) end (point-max)))
   ;;         (goto-char beg)
   ;;         (while (re-search-forward re end t)
   ;;           (setq cstat (1+ cstat)
   ;;                 b1 (match-beginning 0)
   ;;                 e1 (match-end 0)
   ;;                 f1 (match-beginning 1)
   ;;                 lim (cond
   ;;                       ((org-on-heading-p)
   ;;                        (or (outline-get-next-sibling) ;; (3)
   ;;                            (goto-char (point-max)))
   ;;                        (point))
   ;;                       ((org-at-item-p) (org-end-of-item) (point))
   ;;                       (t nil))
   ;;                 c-on 0 c-off 0)
   ;;           (goto-char e1)
   ;;           (when lim
   ;;             (while (re-search-forward re-box lim t)
   ;;               (if (member (match-string 2) '("[ ]" "[-]"))
   ;;                 (setq c-off (1+ c-off))
   ;;                 (setq c-on (1+ c-on))))
   ;;             (goto-char b1)
   ;;             (insert (if f1
   ;;                       (format "[%d%%]" (/ (* 100 c-on)
   ;;                                           (max 1 (+ c-on c-off))))
   ;;                       (format "[%d/%d]" c-on (+ c-on c-off))))
   ;;             (and (looking-at "\\[.*?\\]")
   ;;                  (replace-match ""))))
   ;;         (when (interactive-p)
   ;;           (message "Checkbox statistics updated %s (%d places)"
   ;;                    (if all "in entire file" "in current outline entry")
   ;;                    cstat)))))
   ))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C          . t)
   (clojure    . nil)
   (dot        . t)
   (emacs-lisp . t)
   (haskell    . t)
   (js         . nil)
   (latex      . t)
   (lisp       . nil)
   (ocaml      . nil)
   (octave     . t)
   (org        . t)
   (oz         . nil)
   (python     . nil)
   (R          . nil)
   (scheme     . t)
   (sh         . nil)
   (sql        . nil)
   (sqlite     . nil)))

(eval-after-load
 "org-src"
 '(progn
   (setf org-src-lang-modes
         (cons (cons "scheme" 'scheme)
               (cons (cons "dot" 'graphviz-dot-mode)
                     (remove-if (lambda (entry)
                                  (string= (car entry) "dot"))
                                org-src-lang-modes))))))

(eval-after-load
 "ob-tangle"
 '(progn
   (setf org-babel-tangle-lang-exts
         (cons (cons "scheme" "scm")
               org-babel-tangle-lang-exts))

   ;; fix case when org-bracket-link-analytic-regexp matches ordinary link
   ;; so that (match-string 5) - which is supposed to be the source name -
   ;; becomes nil
   (redefun org-babel-detangle (&optional source-code-file)
     "Propagate changes in source file back original to Org-mode file.
This requires that code blocks were tangled with link comments
which enable the original code blocks to be found."
     (interactive)
     (save-excursion
      (when source-code-file (find-file source-code-file))
      (goto-char (point-min))
      (let ((counter 0) new-body end)
        (while (re-search-forward org-bracket-link-analytic-regexp nil t)
          (when (and (not (null? (match-string 5)))
                     (re-search-forward
                      (concat " " (regexp-quote (match-string 5)) " ends here")))
            (setq end (match-end 0))
            (forward-line -1)
            (save-excursion
             (when (setq new-body (org-babel-tangle-jump-to-org))
               (org-babel-update-block-body new-body)))
            (setq counter (+ 1 counter)))
          (goto-char end))
        (prog1 counter (message "detangled %d code blocks" counter)))))))

;;;; other functions

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

(defun org-mode-setup ()
  (init-common :use-yasnippet t :use-render-formula nil)
  (set (make-local-variable 'yas/fallback-behavior)
       '(apply org-cycle))

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("TAB"   org-cycle)
    ("<tab>" org-cycle)

    ("M-."   org-open-at-point)
    ("M-,"   org-mark-ring-goto)
    ("C-o"   org-open-at-point)
    ("g o"   org-open-at-point)

    ("="     org-mode-up-heading)
    ("<up>"  org-mode-up-heading)

    ("<f1>"  org-toggle-display-style)
    ("z O"   show-all)
    ;; ("z o"   show-subtree)
    ("z c"   hide-subtree)
    ;; hide everything except current entry and its parrents
    ("z C"   hide-other)

    ("j"     eval-last-sexp)
    ("J"     eval-print-last-sexp-unlimited-length)

    ("T"     org-forward-same-level)
    ("N"     org-backward-same-level))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("j"   eval-region))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("M-/"   pcomplete))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:operator-pending-mode-local-keymap
                     vim:motion-mode-local-keymap)
    ("0" vim:org-beginning-of-line)
    ("$" vim:org-end-of-line))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("TAB"   yas/expand)
    ("<tab>" yas/expand)
    ("SPC"   abbrev+-org-self-insert-or-expand-abbrev))

  (def-keys-for-map org-mode-map
    ("TAB"   yas/expand)
    ("<tab>" yas/expand)
    ("C-k"   nil)
    ("C-t"   org-todo)
    ("SPC"   abbrev+-org-self-insert-or-expand-abbrev)))

(add-hook 'org-mode-hook #'org-mode-setup)

(defun org-agenda-mode-setup ()
  (def-keys-for-map org-agenda-mode-map
    +control-x-prefix+
    +vim-special-keys+
    +vi-search-keys+
    ("t"   org-agenda-next-line)
    ("n"   org-agenda-previous-line)

    ("C-t" org-agenda-todo)))

(add-hook 'org-agenda-mode-hook #'org-agenda-mode-setup)

;;;; epilogue

(setf *elisp-do-not-move-files*
      (append *elisp-do-not-move-files*
              '("org.el"
                "org-agenda.el"
                "org-ascii.el"
                "org-attach.el"
                "org-archive.el"
                "org-bbdb.el"
                "org-beamer.el"
                "org-bibtex.el"
                "org-capture.el"
                "org-clock.el"
                "org-colview.el"
                "org-colview-xemacs.el"
                "org-compat.el"
                "org-pcomplete.el"
                "org-crypt.el"
                "org-ctags.el"
                "org-datetree.el"
                "org-docview.el"
                "org-entities.el"
                "org-exp.el"
                "org-exp-blocks.el"
                "org-docbook.el"
                "org-faces.el"
                "org-feed.el"
                "org-footnote.el"
                "org-freemind.el"
                "org-gnus.el"
                "org-habit.el"
                "org-html.el"
                "org-icalendar.el"
                "org-id.el"
                "org-indent.el"
                "org-info.el"
                "org-inlinetask.el"
                "org-jsinfo.el"
                "org-irc.el"
                "org-latex.el"
                "org-list.el"
                "org-lparse.el"
                "org-mac-message.el"
                "org-macs.el"
                "org-mew.el"
                "org-mhe.el"
                "org-mks.el"
                "org-mobile.el"
                "org-mouse.el"
                "org-odt.el"
                "org-publish.el"
                "org-plot.el"
                "org-protocol.el"
                "org-remember.el"
                "org-rmail.el"
                "org-special-blocks.el"
                "org-src.el"
                "org-table.el"
                "org-taskjuggler.el"
                "org-timer.el"
                "org-vm.el"
                "org-w3m.el"
                "org-wl.el"
                "org-xoxo.el"
                "ob.el"
                "ob-table.el"
                "ob-lob.el"
                "ob-ref.el"
                "ob-exp.el"
                "ob-tangle.el"
                "ob-comint.el"
                "ob-eval.el"
                "ob-keys.el"
                "ob-awk.el"
                "ob-C.el"
                "ob-calc.el"
                "ob-ditaa.el"
                "ob-haskell.el"
                "ob-perl.el"
                "ob-sh.el"
                "ob-R.el"
                "ob-dot.el"
                "ob-mscgen.el"
                "ob-latex.el"
                "ob-lisp.el"
                "ob-ledger.el"
                "ob-python.el"
                "ob-sql.el"
                "ob-asymptote.el"
                "ob-emacs-lisp.el"
                "ob-matlab.el"
                "ob-ruby.el"
                "ob-sqlite.el"
                "ob-clojure.el"
                "ob-ocaml.el"
                "ob-sass.el"
                "ob-css.el"
                "ob-gnuplot.el"
                "ob-octave.el"
                "ob-screen.el"
                "ob-plantuml.el"
                "ob-org.el"
                "ob-js.el"
                "ob-scheme.el"
                "ob-lilypond.el"
                "ob-java.el"
                "ob-shen.el"
                "ob-fortran.el"
                "ob-picolisp.el")))


(provide 'org-mode-setup)

;; Local Variables:
;; End:

;; org-mode-setup.el ends here
