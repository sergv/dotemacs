;;; org-mode-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'common)

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "org-7.8.11/lisp"))
(add-to-list 'load-path (concat +emacs-standalone-path+
                                "org-7.8.11/contrib/lisp"))

(load-library "org-install")
;; (require 'org-install)
;; (require 'org-drill)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(setf org-log-done t)

;; org mode customizations
(setf org-agenda-ndays 7
      org-deadline-warning-days 14
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      ;; notes are stored in descending date order - most recent always at top
      org-reverse-note-order t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

(eval-after-load
 "org"
 '(progn
   (setf org-todo-keywords
         '((sequence "TODO(t)" "DONE(d)")
           (sequence "|" "CANCELLED(c)"))
         org-todo-keyword-faces
         '(("CANCELLED" . org-cancelled)))   ))

(eval-after-load
 "org-comat"
 '(progn
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


;; (defalias 'org-region-active-p 'org-region-active-p+)
;; (fset 'org-region-active-p 'org-region-active-p+)


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
  (init-common)

  (setf vim:insert-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-keymap)
        vim:normal-mode-local-keymap           (make-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap))

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ("M-."  org-open-at-point)
    ("C-o"  org-open-at-point)
    ("g o"  org-open-at-point)

    ("="    org-mode-up-heading)
    ("<up>" org-mode-up-heading)

    ("<f1>" org-toggle-display-style)
    ("z O"  show-all)
    ;; ("z o"  show-subtree)
    ("z c"  hide-subtree)
    ;; hide everything except current entry and its parrents
    ("z C"  hide-other)

    ("j"    eval-last-sexp)
    ("J"    eval-print-last-sexp-unlimited-length))

  (def-keys-for-map2 vim:visual-mode-local-keymap
    ("j"   eval-region))

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap)
    ("TAB"   org-cycle)
    ("<tab>" org-cycle)
    ("M-/"   pcomplete))

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:visual-mode-local-keymap
                      vim:operator-pending-mode-local-keymap
                      vim:motion-mode-local-keymap)
    ("0" vim:org-beginning-of-line)
    ("$" vim:org-end-of-line))

  (def-keys-for-map2 vim:insert-mode-local-keymap
    ("SPC" abbrev+-org-self-insert-or-expand-abbrev))

  (def-keys-for-map2 org-mode-map
    ("C-k" nil)
    ("C-t" org-todo)
    ("SPC" abbrev+-org-self-insert-or-expand-abbrev)))

(add-hook 'org-mode-hook #'org-mode-setup)

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
;; lexical-binding: t
;; End:

;;; org-mode-setup.el ends here
