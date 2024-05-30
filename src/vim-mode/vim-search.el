;; vim-search.el - Search und substitute commands for ex-mode. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;; TODO:
;;
;;  - the substitute command should be more interactive and especially an operation
;;    without the 'g' option should highlight all future occurences

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'vim-common)
(require 'vim-macs)
(require 'vim-ex)

(defcustom vim-interactive-search-highlight 'selected-window
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'vim-ex-mode)

(defcustom vim-substitute-case 'sensitive
  "The case behaviour of the search command."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." 'sensitive)
                (const :tag "Case insensitive." 'insensitive)
                (const :tag "Smart case." 'smart))
  :group 'vim-ex-mode)

(defcustom vim-substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'vim-ex-mode)

(defcustom vim-substitute-interactive-replace t
  "If t and substitute patterns are highlighted the replacement is shown interactively."
  :type 'boolean
  :group 'vim-ex-mode)

(defface vim-search-face '((t (:inherit isearch)))
  "Face for interactive search."
  :group 'vim-ex-mode)

(defface vim-lazy-highlight-face '((t (:inherit lazy-highlight)))
  "Face for highlighting all matches in interactive search."
  :group 'vim-ex-mode)

(defface vim-substitute-face '((((supports :underline))
                                (:underline t :foreground "red")))
  "Face for interactive replacement text."
  :group 'vim-ex-mode)

;; A pattern.
(cl-defstruct (vim-pattern
               (:constructor nil)
               (:constructor vim-make-pattern
                             (&key ((:regex re))
                                   ((:case-fold ca) nil)
                                   (whole-line t)
                                   &aux (regex (vim--regex-without-case re))
                                   (case-fold (vim--regex-case re ca)))))
  regex      ;; The pattern itself.
  case-fold  ;; The case for this pattern.
  whole-line ;; If non-nil the pattern matches the whole line,
  ;; otherwise only the first occurrence.
  )

(defun vim--regex-without-case (re)
  "Returns the regular expression without all occurrences of \\c and \\C."
  (replace-regexp-in-string
   "\\\\[cC]"
   ""
   re
   t ;; fixed case
   t ;; literal
   ))

(defun vim--regex-case (re default-case)
  "Returns the case as implied by \\c or \\C in regular expression `re'.
If \\c appears anywhere in the pattern, the pattern is case
insenstive, if \\C appears the pattern is case sensitive. Only
the first occurrence of \\c or \\C is used, all others are
ignored. If neither \\c nor \\C appears in the pattern, the
case specified by `default-case' is used. `default-case' should be either
'sensitive, 'insensitive or 'smart. In the latter case the pattern will be
case-sensitive if and only if it contains an upper-case letter, otherwise it
will be case-insensitive."
  (let ((start 0)
        recase)
    (while (and (not recase)
                (string-match "\\\\." re start))
      (cl-case (aref re (1- (match-end 0)))
        (?c (setq recase 'insensitive))
        (?C (setq recase 'sensitive))
        (t (setq start (match-end 0)))))
    (or recase
        (pcase default-case
          ((or `sensitive `insensitive) default-case)
          (`smart (if (isearch-no-upper-case-p re t) 'insensitive 'sensitive))
          (_ nil)))))

;; The lazy-highlighting framework.
(defvar-local vim--active-highlights-alist nil
  "An alist of currently active highlights.")


(cl-defstruct (vim-hl
               (:constructor vim-make-highlight))
  name        ;; The name of this highlight.
  pattern     ;; The search pattern.
  face        ;; The face for this highlights.
  window      ;; The window where this highlight has been started.
  beg         ;; The minimal position for the highlighting.
  end         ;; The maximal position for the highlighting.
  update-hook ;; Hook to be called when the lazy highlighting.
  match-hook  ;; Hook to be called when a single lazy highlight pattern has been setup.
  overlays    ;; The currently active overlays.
  )

(cl-defun vim--new-highlight (name &key
                                   (face 'vim-lazy-highlight-face)
                                   (win (selected-window))
                                   (beg nil)
                                   (end nil)
                                   (update-hook nil)
                                   (match-hook nil))
  "Creates new highlighting object with a certain `name'."
  (declare (indent 1))
  (cl-assert (symbolp name) nil "Excepted symbol as name of highlight")
  (cl-assert (facep face))
  (cl-assert (or (null match-hook) (functionp match-hook)))
  (cl-assert (or (null update-hook) (functionp update-hook)))
  (when (vim-hl-active-p name)
    (vim--delete-hl name))
  (when (null vim--active-highlights-alist)
    (add-hook 'window-scroll-functions #'vim-hl-update-highlights-scroll nil t)
    (add-hook 'window-size-change-functions #'vim-hl-update-highlights-resize nil))
  (push (cons name (vim-make-highlight :name name
                                       :pattern nil
                                       :face face
                                       :overlays nil
                                       :window win
                                       :beg beg
                                       :end end
                                       :update-hook update-hook
                                       :match-hook match-hook))
        vim--active-highlights-alist))

(defun vim--delete-hl (name)
  "Removes the highlighting object with a certain `name'."
  (cl-assert (symbolp name))
  (let ((hl (cdr-safe (assq name vim--active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (vim-hl-overlays hl))
      (setq vim--active-highlights-alist
            (assq-delete-all name vim--active-highlights-alist))
      (vim-hl-update-highlights))
    (when (null vim--active-highlights-alist)
      (remove-hook 'window-scroll-functions #'vim-hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions #'vim-hl-update-highlights-resize))))


(defun vim-hl-active-p (name)
  "Returns t iff the highlight with a certain name is active."
  (cl-assert (symbolp name))
  (and (assq name vim--active-highlights-alist) t))


(defun vim-hl-change (name new-pattern)
  "Sets the regular expression of the highlighting object with
name `name' to `new-regex'."
  (cl-assert (symbolp name))
  (let ((hl (cdr-safe (assq name vim--active-highlights-alist))))
    (when hl
      (setf (vim-hl-pattern hl)
            (if (zerop (length new-pattern))
                nil
              new-pattern))
      (vim-hl-idle-update))))


(defun vim-hl-set-region (name beg end)
  (cl-assert (symbolp name))
  (let ((hl (cdr-safe (assq name vim--active-highlights-alist))))
    (when hl
      (setf (vim-hl-beg hl) beg
            (vim-hl-end hl) end)
      (vim-hl-idle-update))))


(defun vim-hl-update-highlights ()
  "Updates the overlays of all active highlights."
  (dolist (highlight vim--active-highlights-alist)
    (let* ((hl (cdr highlight))
           (old-ovs (vim-hl-overlays hl))
           new-ovs
           (pattern (vim-hl-pattern hl))
           (face (vim-hl-face hl))
           (match-hook (vim-hl-match-hook hl))
           result)
      (condition-case lossage
          (progn
            (when pattern
              (let ((case-fold-search (eq (vim-pattern-case-fold pattern) 'insensitive))
                    (regex (vim-pattern-regex pattern)))
                (dolist (win (if (eq vim-interactive-search-highlight 'all-windows)
                                 (get-buffer-window-list (current-buffer) nil t)
                               (list (vim-hl-window hl))))
                  (let ((begin (max (window-start win)
                                    (or (vim-hl-beg hl) (point-min))))
                        (end (min (window-end win)
                                  (or (vim-hl-end hl) (point-max))))
                        (last-line-end-pos nil))
                    (when (< begin end)
                      (save-excursion
                        (goto-char begin)
                        ;; set the overlays for the current highlight, reusing old overlays
                        ;; (if possible)
                        (while (re-search-forward regex end t)
                          (when (or (vim-pattern-whole-line pattern)
                                    (not (equal (line-end-position) last-line-end-pos)))
                            (unless (vim-pattern-whole-line pattern)
                              (setq last-line-end-pos (line-end-position)))
                            (push (if old-ovs
                                      (progn
                                        (move-overlay (car old-ovs)
                                                      (match-beginning 0)
                                                      (match-end 0))
                                        (overlay-put (car old-ovs) 'face face)
                                        (pop old-ovs))
                                    (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                                      (overlay-put ov 'face face)
                                      (overlay-put ov 'vim-hl (vim-hl-name hl))
                                      (overlay-put ov 'priority 1000)
                                      ov))
                                  new-ovs)
                            (when match-hook (funcall match-hook (car new-ovs)))))))))))

            (mapc #'delete-overlay old-ovs)
            (setf (vim-hl-overlays hl) new-ovs)
            (if (or (null pattern) new-ovs)
                (setq result t)
              ;; maybe the match could just not be found somewhere else?
              (save-excursion
                (goto-char (vim-hl-beg hl))
                (if (and (vim-search--find-next-pattern pattern)
                         (< (match-end 0) (vim-hl-end hl)))
                    (setq result (format "Match in line %d" (line-number-at-pos (match-beginning 0))))
                  (setq result "No match")))))

        (invalid-regexp
         (setq result (concat "Invalid regexp error: " (cadr lossage))))

        (search-failed
         (setq result (concat "Search failed error: " (nth 2 lossage))))

        (error
         (setq result (format "Other error: %s" lossage))))

      (awhen (vim-hl-update-hook hl)
        (funcall it result)))))

(defvar vim-hl-update-timer nil
  "Time used for updating highlights.")

(defun vim-hl-idle-update ()
  "Triggers the timer to update the highlights in the current buffer."
  (when (and vim-interactive-search-highlight
             vim--active-highlights-alist)
    (when vim-hl-update-timer
      (cancel-timer vim-hl-update-timer))
    (setq vim-hl-update-timer
          (run-at-time 0.1 nil
                       #'vim-hl-do-update-highlight
                       (current-buffer)))))

(defun vim-hl-do-update-highlight (&optional buffer)
  "Timer function, updating the highlights."
  (with-current-buffer buffer
    (vim-hl-update-highlights))
  (setq vim-hl-update-timer nil))

(defun vim-hl-update-highlights-scroll (_win _begin)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer)
    (vim-hl-idle-update)))

(defun vim-hl-update-highlights-resize (frame)
  "Updates highlights after resizing a window."
  (let ((buffers (delete-dups (-map #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (vim-hl-idle-update)))))

(defun vim-search--find-next-pattern (pattern)
  "Looks for the next occurrence of pattern in a certain direction."
  (let ((case-fold-search (eq (vim-pattern-case-fold pattern) 'insensitive)))
    (re-search-forward (vim-pattern-regex pattern) nil t)))

;; Substitute
(defun vim-ex--pattern-argument-activate ()
  (with-selected-window vim-ex--current-window
    (with-current-buffer vim-ex--current-buffer
      (vim--new-highlight 'vim-substitute-face
        :match-hook (when vim-substitute-interactive-replace
                      #'vim-ex--pattern-update-replacement)
        :update-hook #'vim-ex--pattern-update-substitute-command-info)
      (vim-ex--pattern-argument-update))))

(defvar vim--ex-pattern-update-overlay nil
  "Overlay in ex minibuffer that shows substitution feedback to the user,
e.g. whether regexp is malformed, not matched, etc.")

(defun vim-ex--pattern-update-substitute-command-info (result)
  (when result
    (cl-assert (or (stringp result) (eq result t)))
    (when vim-ex--minibuffer
      (with-current-buffer vim-ex--minibuffer
        (let ((after-change-functions nil))
          (unless vim--ex-pattern-update-overlay
            (setf vim--ex-pattern-update-overlay
                  (make-overlay (point-min) (point-max) (current-buffer))))
          (move-overlay vim--ex-pattern-update-overlay
                        (point-min)
                        (point-max)
                        (current-buffer))
          (if (or (eq result t)
                  (zerop (length result)))
              (overlay-put vim--ex-pattern-update-overlay 'after-string nil)
            (overlay-put vim--ex-pattern-update-overlay
                         'after-string (concat " | " result))))))))

(defun vim-ex--pattern-argument-deactivate ()
  (when vim-ex--current-window
    (with-selected-window vim-ex--current-window
      (with-current-buffer vim-ex--current-buffer
        (vim--delete-hl 'vim-substitute-face)
        (delete-overlay vim--ex-pattern-update-overlay)))))

(defun vim--construct-substitute-pattern (search-regex flags)
  (cl-assert (and search-regex (stringp search-regex)))
  (cl-assert (or (null flags) (listp flags) (-all #'stringp flags)))
  (when (memq ?g flags)
    (error "Don't use ‘g’ flag, use ‘n’ with inverted meaning instead"))
  (vim-make-pattern
   :regex search-regex
   :whole-line (not (memq ?n flags))
   :case-fold
   (cond
     ((memq ?i flags) 'insensitive)
     ((memq ?I flags) 'sensitive)
     (t               vim-substitute-case))))

(defvar vim-substitute-face-replacement nil
  "The actual replacement.")

(defun vim-ex--pattern-argument-update ()
  (when vim-substitute-highlight-all
    (cl-multiple-value-bind (pattern replacement flag-str)
        (vim--parse-substitute vim-ex--arg)
      (with-selected-window vim-ex--current-window
        (with-current-buffer vim-ex--current-buffer
          (let ((substitute-pattern
                 (when pattern
                   (vim--construct-substitute-pattern pattern
                                                      (string->list flag-str)))))
            (setf vim-substitute-face-replacement replacement)
            (vim-hl-set-region 'vim-substitute-face
                               ;; first line
                               (if (car-safe vim-ex--range)
                                   (save-excursion
                                     (goto-line-dumb (car vim-ex--range))
                                     (line-beginning-position))
                                 (line-beginning-position))
                               ;; last line
                               (if (car-safe vim-ex--range)
                                   (save-excursion
                                     (goto-line-dumb (or (cdr vim-ex--range)
                                                     (car vim-ex--range)))
                                     (line-end-position))
                                 (line-end-position)))
            (vim-hl-change 'vim-substitute-face substitute-pattern)))))))

(defun vim-ex--pattern-update-replacement (overlay)
  "Updates the replacement display."
  (let ((replacement (match-substitute-replacement vim-substitute-face-replacement)))
    (put-text-property 0 (length replacement)
                       'face 'vim-substitute-face
                       replacement)
    (overlay-put overlay 'after-string replacement)))


(vim--define-arg-handler 'substitute
  :activate 'vim-ex--pattern-argument-activate
  :deactivate 'vim-ex--pattern-argument-deactivate
  :update 'vim-ex--pattern-argument-update)


(vim-defcmd vim:cmd-substitute (motion argument:substitute nonrepeatable noninteractive)
  "The VIM substitutde command: [range]s/pattern/replacement/flags

Allowed flags are:
  n - replace only first occurrence on every line
  c - do interactive replacement confirming every match
  i - ignore case
  I - don't ignore case
"
  (save-match-data
    (vim:cmd-nohighlight--impl)
    (cl-multiple-value-bind (pattern replacement flag-str)
        (vim--parse-substitute argument)
      (let ((flags (string->list flag-str)))
        (when (memq ?g flags)
          (error "Don't use flag g, use \"n\" with inverted meaning instead"))
        (let ((confirm (memq ?c flags)))
          (vim--do-substitute motion
                              (vim--construct-substitute-pattern pattern flags)
                              replacement
                              flags
                              confirm))))))

(defun vim--do-substitute (motion pattern replacement _flags confirm)
  "Do the actual substitution in current buffer. Search for regexp
pattern and replace matches with REPLACEMENT.
"
  (unless pattern (error "No pattern given"))
  (unless replacement (error "No replacement given"))

  (let* ((current-line (line-number-at-pos (point)))
         (first-line (if motion
                         (vim-motion-first-line motion)
                       current-line))
         (last-line (if motion
                        (vim-motion-last-line motion)
                      current-line))
         (last-line-pos (if (= current-line last-line)
                            (line-end-position)
                          (save-excursion
                            (goto-line-dumb last-line)
                            (line-end-position))))
         (regex (vim-pattern-regex pattern))
         (last-point (point))
         (overlay (make-overlay (point) (point)))
         (first-line-pos (if (= current-line first-line)
                             (line-beginning-position)
                           (save-excursion
                             (goto-line-dumb first-line)
                             (line-beginning-position)))))

    (let ((case-fold-search (eq 'insensitive (vim-pattern-case-fold pattern)))
          (case-replace case-fold-search))
      (with-marker (last-line-pos-marker (copy-marker last-line-pos))
        (unwind-protect
            (cond
              ((vim-pattern-whole-line pattern)
               ;; this one is easy, just use the built in function
               (if confirm
                   (perform-replace regex
                                    replacement
                                    t
                                    t   ;; is-regexp
                                    nil ;; delimited-count
                                    nil ;; repeat-count
                                    nil ;; map
                                    first-line-pos
                                    last-line-pos-marker)
                 (progn
                   (goto-char first-line-pos)
                   (while (re-search-forward regex last-line-pos-marker t)
                     (replace-match replacement nil nil)))))
              (t
               (let ((nreplaced 0))
                 (if confirm
                     ;; Marker is needed to keep track where current line ends
                     ;; because replacement may make current line longer and computed line end
                     ;; position will not be correct any more.
                     (with-marker (next-line-pos-marker (copy-marker first-line-pos))
                       ;; this one is more difficult, we have to do the
                       ;; highlighting and questioning on our own
                       (overlay-put overlay 'face
                                    (if (facep 'isearch) 'isearch 'region))
                       (map-y-or-n-p (lambda (x)
                                       (set-match-data x)
                                       (move-overlay overlay (match-beginning 0) (match-end 0))
                                       (concat "Query replacing "
                                               (match-string-no-properties 0)
                                               " with "
                                               (match-substitute-replacement replacement
                                                                             case-fold-search)
                                               ": "))
                                     (lambda (x)
                                       (set-match-data x)
                                       (replace-match replacement case-fold-search)
                                       (cl-incf nreplaced)
                                       (setq last-point (point)))
                                     (lambda ()
                                       (goto-char next-line-pos-marker)
                                       (when (and (< (point) last-line-pos-marker)
                                                  (re-search-forward regex last-line-pos-marker t nil))
                                         (setq last-point (point))
                                         (move-marker next-line-pos-marker (1+ (line-end-position)))
                                         (match-data)))))

                   ;; just replace the first occurences per line
                   ;; without highlighting and asking
                   (progn
                     (goto-char first-line-pos)
                     (while (and (<= (point) last-line-pos-marker)
                                 (re-search-forward regex last-line-pos-marker t nil))
                       (cl-incf nreplaced)
                       (replace-match replacement)
                       (setq last-point (point))
                       (forward-line)
                       (beginning-of-line))))

                 (goto-char last-point)
                 (if (= nreplaced 1)
                     (vim-notify "Replaced 1 occurence")
                   (vim-notify "Replaced %d occurences" nreplaced)))))

          ;; clean-up the overlay
          (delete-overlay overlay))))))

(defun vim--parse-substitute (text)
  "Parse ex command line in TEXT and return triple
\(<pattern> <replacement> <flags>\)."
  (when (string-match-p "^\\s-*[/|,;:!@#]." text)
    (cl-multiple-value-bind (pattern replacement flags)
        (vim--parse-substitute-pattern-repl-flags text)
      (values pattern (vim-substitute-face-expand-escapes replacement) flags))))

(defun vim--parse-substitute-pattern-repl-flags (str)
  "Perform actual parse of substitute command. Works much better than
regular expressions."
  (let ((i 0)
        (len (length str))
        ;; ends are exclusive
        pattern-start
        pattern-end
        replacement-start
        replacement-end
        flags-start
        flags-end

        delimiter)
    (cl-symbol-macrolet ((skip-quoted
                          (while (and (< i len)
                                      (not (eq (aref str i) delimiter)))
                            ;; skip quoted character
                            (if (eq (aref str i) ?\\)
                                (cl-incf i 2)
                              (cl-incf i))))
                         (skip-flags
                          (while (and (< i len)
                                      (memq (aref str i)
                                            (eval-when-compile (string-to-list "niIcg"))))
                            (cl-incf i))))
      (while (and (< i len)
                  (not (memq (aref str i) vim-common-command-delimiters)))
        (cl-incf i))
      (setf delimiter (aref str i))
      (cl-incf i)
      (setf pattern-start i)
      skip-quoted
      (setf pattern-end i)

      (cl-incf i)
      (when (< i len)
        (setf replacement-start i)
        skip-quoted
        (setf replacement-end i)
        (cl-incf i)

        (when (< i len)
          (setf flags-start i)
          skip-flags
          (setf flags-end i)))
      (values (expand-escape-sequences!
               (substring-no-properties str pattern-start (min len pattern-end)))
              (when (and replacement-start
                         replacement-end
                         ;; hack
                         (<= replacement-end len))
                (substring-no-properties str replacement-start (min len replacement-end)))
              (when (and flags-start flags-end)
                (substring-no-properties str flags-start (min len flags-end)))))))

(defun vim-substitute-face-expand-escapes (replacement)
  "Expand escapes in the replacement string of vim substitue command."
  (let* ((result nil)
         (idx 0)
         (n (length replacement)))
    ;; handle escaped chars
    (while (< idx n)
      (let ((char (aref replacement idx)))
        (if (and (= char ?\\)
                 (< (1+ idx) n))
          (let ((next-char (aref replacement (1+ idx))))
            (cl-case next-char
              (?n (push ?\n result))
              (?t (push ?\t result))
              (?r (push ?\r result))
              ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\\)
               (push ?\\ result)
               (push next-char result))
              (t (push next-char result)))
            (cl-incf idx 2))
          (push char result)
          (cl-incf idx))))
    (apply #'string (nreverse result))))

;; Related commands.
(vim-defcmd vim:cmd-nohighlight (nonrepeatable noninteractive)
  "Disables the active search highlightings."
  (vim:cmd-nohighlight--impl))

(defun vim:cmd-nohighlight--impl ()
  (vim--delete-hl 'vim-substitute-face)
  (search-disable-highlighting))

(vim-defcmd vim:cmd-nohighlight-everywhere (nonrepeatable noninteractive)
  "Disables the active search highlightings in all buffers."
  (vim--delete-hl 'vim-substitute-face)
  (search-disable-all-highlighting))

(provide 'vim-search)

;; Local Variables:
;; End:

;; vim-search.el ends here
