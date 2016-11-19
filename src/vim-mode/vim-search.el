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

(eval-when-compile (require 'cl-lib))

(require 'vim-macs)
(require 'vim-ex)

(defcustom vim:interactive-search-highlight 'selected-window
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'vim-ex-mode)

(defcustom vim:substitute-case 'sensitive
  "The case behaviour of the search command."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." 'sensitive)
                (const :tag "Case insensitive." 'insensitive)
                (const :tag "Smart case." 'smart))
  :group 'vim-ex-mode)

(defcustom vim:substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'vim-ex-mode)

(defcustom vim:substitute-interactive-replace t
  "If t and substitute patterns are highlighted the replacement is shown interactively."
  :type 'boolean
  :group 'vim-ex-mode)

(defparameter vim:substitute-pattern nil
  "The actual replacement.")

(defparameter vim:substitute-replacement nil
  "The actual replacement.")

(defface vim:search '((t (:inherit isearch)))
  "Face for interactive search."
  :group 'vim-ex-mode)

(defface vim:lazy-highlight '((t (:inherit lazy-highlight)))
  "Face for highlighting all matches in interactive search."
  :group 'vim-ex-mode)

(defface vim:substitute '((((supports :underline))
                           (:underline t
                                       :foreground "red")))
  "Face for interactive replacement text."
  :group 'vim-ex-mode)

;; A pattern.
(defstruct (vim:pattern
            (:constructor nil)
            (:constructor vim:make-pattern
                          (&key ((:regex re))
                                ((:case-fold ca) nil)
                                (whole-line t)
                                &aux (regex (vim:regex-without-case re))
                                (case-fold (vim:regex-case re ca)))))
  regex      ;; The pattern itself.
  case-fold  ;; The case for this pattern.
  whole-line ;; If non-nil the pattern matches the whole line,
  ;; otherwise only the first occurrence.
  )

(defun vim:regex-without-case (re)
  "Returns the regular expression without all occurrences of \\c and \\C."
  (replace-regexp-in-string
   "\\\\."
   (lambda (txt)
     (if (member (aref txt 1) '(?c ?C))
       ""
       txt))
   re t t))

(defun vim:regex-case (re default-case)
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
      (case (aref re (1- (match-end 0)))
        (?c (setq recase 'insensitive))
        (?C (setq recase 'sensitive))
        (t (setq start (match-end 0)))))
    (or recase
        (pcase default-case
          ((or `sensitive `insensitive) default-case)
          (`smart (if (isearch-no-upper-case-p re t) 'insensitive 'sensitive))
          (_ nil)))))

;; The lazy-highlighting framework.
(defvar-local vim:active-highlights-alist nil
  "An alist of currently active highlights.")


(defstruct (vim:hl
            (:constructor vim:make-highlight))
  name        ;; The name of this highlight.
  pattern     ;; The search pattern.
  face        ;; The face for this highlights.
  window      ;; The window where this highlight has been started.
  beg         ;; The minimal position for the highlighting.
  end         ;; The maximal position for the highlighting.
  update-hook ;; Hook to be called when the lazy highlighting.
  match-hook ;; Hook to be called when a single lazy highlight pattern has been setup.
  overlays   ;; The currently active overlays.
  )

(defun* vim:make-hl (name &key
                          (face 'vim:lazy-highlight)
                          (win (selected-window))
                          (beg nil)
                          (end nil)
                          (update-hook nil)
                          (match-hook nil))
  "Creates new highlighting object with a certain `name'."
  (cl-assert (symbolp name) nil "Excepted symbol as name of highlight")
  (when (vim:hl-active-p name)
    (vim:delete-hl name))
  (when (null vim:active-highlights-alist)
    (add-hook 'window-scroll-functions #'vim:hl-update-highlights-scroll nil t)
    (add-hook 'window-size-change-functions #'vim:hl-update-highlights-resize nil))
  (push (cons name (vim:make-highlight :name name
                                       :pattern nil
                                       :face face
                                       :overlays nil
                                       :window win
                                       :beg beg
                                       :end end
                                       :update-hook update-hook
                                       :match-hook match-hook))
        vim:active-highlights-alist))


(defun vim:delete-hl (name)
  "Removes the highlighting object with a certain `name'."
  (cl-assert (symbol? name))
  (let ((hl (cdr-safe (assq name vim:active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (vim:hl-overlays hl))
      (setq vim:active-highlights-alist
            (assq-delete-all name vim:active-highlights-alist))
      (vim:hl-update-highlights))
    (when (null vim:active-highlights-alist)
      (remove-hook 'window-scroll-functions #'vim:hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions #'vim:hl-update-highlights-resize))))


(defun vim:hl-active-p (name)
  "Returns t iff the highlight with a certain name is active."
  (cl-assert (symbol? name))
  (and (assq name vim:active-highlights-alist) t))


(defun vim:hl-change (name new-pattern)
  "Sets the regular expression of the highlighting object with
name `name' to `new-regex'."
  (cl-assert (symbol? name))
  (let ((hl (cdr-safe (assq name vim:active-highlights-alist))))
    (when hl
      (setf (vim:hl-pattern hl)
            (if (zerop (length new-pattern))
              nil
              new-pattern))
      (vim:hl-idle-update))))


(defun vim:hl-set-region (name beg end)
  (cl-assert (symbol? name))
  (let ((hl (cdr-safe (assq name vim:active-highlights-alist))))
    (when hl
      (setf (vim:hl-beg hl) beg
            (vim:hl-end hl) end)
      (vim:hl-idle-update))))


(defun* vim:hl-update-highlights ()
  "Updates the overlays of all active highlights."
  (dolist (hl (-map #'cdr vim:active-highlights-alist))
    (let ((old-ovs (vim:hl-overlays hl))
          new-ovs
          (pattern (vim:hl-pattern hl))
          (face (vim:hl-face hl))
          (match-hook (vim:hl-match-hook hl))
          result)
      (condition-case lossage
          (progn
            (when pattern
              (dolist (win (if (eq vim:interactive-search-highlight 'all-windows)
                             (get-buffer-window-list (current-buffer) nil t)
                             (list (vim:hl-window hl))))
                (let ((begin (max (window-start win)
                                  (or (vim:hl-beg hl) (point-min))))
                      (end (min (window-end win)
                                (or (vim:hl-end hl) (point-max))))
                      last-line)
                  (when (< begin end)
                    (save-excursion
                      (goto-char begin)
                      ;; set the overlays for the current highlight, reusing old overlays
                      ;; (if possible)
                      (while (and (vim:search-find-next-pattern pattern t)
                                  (< (match-beginning 0) (match-end 0))
                                  (<= (match-end 0) end))
                        (when (or (vim:pattern-whole-line pattern)
                                  (not (equal (line-number-at-pos (match-beginning 0)) last-line)))
                          (setq last-line (line-number-at-pos (match-beginning 0)))
                          (push (if old-ovs
                                  (progn
                                    (move-overlay (car old-ovs)
                                                  (match-beginning 0)
                                                  (match-end 0))
                                    (overlay-put (car old-ovs) 'face face)
                                    (pop old-ovs))
                                  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                                    (overlay-put ov 'face face)
                                    (overlay-put ov 'vim:hl (vim:hl-name hl))
                                    (overlay-put ov 'priority 1000)
                                    ov))
                                new-ovs)
                          (when match-hook (funcall match-hook (car new-ovs)))
                          )))))))

            (mapc #'delete-overlay old-ovs)
            (setf (vim:hl-overlays hl) new-ovs)
            (if (or (null pattern) new-ovs)
              (setq result t)
              ;; maybe the match could just not be found somewhere else?
              (save-excursion
                (goto-char (vim:hl-beg hl))
                (if (and (vim:search-find-next-pattern pattern t)
                         (< (match-end 0) (vim:hl-end hl)))
                  (setq result (format "Match in line %d" (line-number-at-pos (match-beginning 0))))
                  (setq result "No match")))))

        (invalid-regexp
         (setq result (cadr lossage)))

        (search-failed
         (setq result (nth 2 lossage)))

        (error
         (setq result (format "%s" lossage))))

      (when (vim:hl-update-hook hl)
        (funcall (vim:hl-update-hook hl) result)))))

(defparameter vim:hl-update-timer nil
  "Time used for updating highlights.")

(defun vim:hl-idle-update ()
  "Triggers the timer to update the highlights in the current buffer."
  (when (and vim:interactive-search-highlight
             vim:active-highlights-alist)
    (when vim:hl-update-timer
      (cancel-timer vim:hl-update-timer))
    (setq vim:hl-update-timer
          (run-at-time 0.1 nil
                       #'vim:hl-do-update-highlight
                       (current-buffer)))))

(defun vim:hl-do-update-highlight (&optional buffer)
  "Timer function, updating the highlights."
  (with-current-buffer buffer
    (vim:hl-update-highlights))
  (setq vim:hl-update-timer nil))

(defun vim:hl-update-highlights-scroll (win begin)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer)
    (vim:hl-idle-update)))

(defun vim:hl-update-highlights-resize (frame)
  "Updates highlights after resizing a window."
  (let ((buffers (delete-dups (-map #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (vim:hl-idle-update)))))

(defun* vim:search-find-next-pattern (pattern is-forward?)
  "Looks for the next occurrence of pattern in a certain direction."
  (let ((case-fold-search (eq (vim:pattern-case-fold pattern) 'insensitive)))
    (if is-forward
        (re-search-forward (vim:pattern-regex pattern) nil t)
      (re-search-backward (vim:pattern-regex pattern) nil t))))

;; Substitute
(defun vim:ex-pattern-argument-activate ()
  (with-selected-window vim:ex-current-window
    (with-current-buffer vim:ex-current-buffer
      (vim:make-hl 'vim:substitute
                   :match-hook (and vim:substitute-interactive-replace
                                    #'vim:ex-pattern-update-replacement))
      (vim:ex-pattern-argument-update))))

(defun vim:ex-pattern-argument-deactivate ()
  (with-selected-window vim:ex-current-window
    (with-current-buffer vim:ex-current-buffer
      (vim:delete-hl 'vim:substitute))))

(defun vim:ex-pattern-argument-update ()
  (when vim:substitute-highlight-all
    (multiple-value-bind (pattern replacement flag-str)
        (vim:parse-substitute vim:ex-arg)
      (with-selected-window vim:ex-current-window
        (with-current-buffer vim:ex-current-buffer
          (setq vim:substitute-pattern
                (and pattern
                     (vim:make-pattern
                      :regex pattern
                      :whole-line (if flag-str (not (string-match-pure? "!g" flag-str)) t)
                      :case-fold (or (and (string? flag-str) (string-match-pure? "i" flag-str) 'insensitive)
                                     (and (string? flag-str) (string-match-pure? "I" flag-str) 'sensitive)
                                     vim:substitute-case)))
                vim:substitute-replacement replacement)
          (vim:hl-set-region 'vim:substitute
                             ;; first line
                             (if (car-safe vim:ex-range)
                               (save-excursion
                                 (goto-line1 (car vim:ex-range))
                                 (line-beginning-position))
                               (line-beginning-position))
                             ;; last line
                             (if (car-safe vim:ex-range)
                               (save-excursion
                                 (goto-line1 (or (cdr vim:ex-range)
                                                 (car vim:ex-range)))
                                 (line-end-position))
                               (line-end-position)))
          (vim:hl-change 'vim:substitute vim:substitute-pattern))))))

(defun vim:ex-pattern-update-replacement (overlay)
  "Updates the replacement display."
  (let ((repl (vim:match-substitute-replacement vim:substitute-replacement)))
    (put-text-property 0 (length repl)
                       'face 'vim:substitute
                       repl)
    (overlay-put overlay 'after-string repl)))


(vim:define-arg-handler 'substitute
  :activate 'vim:ex-pattern-argument-activate
  :deactivate 'vim:ex-pattern-argument-deactivate
  :update 'vim:ex-pattern-argument-update)


(vim:defcmd vim:cmd-substitute (motion argument:substitute nonrepeatable)
  "The VIM substitutde command: [range]s/pattern/replacement/flags

Allowed flags are:
  n - replace only first occurrence on every line
  c - do interactive replacement confirming every match
  i - ignore case
  I - don't ignore case
"
  (save-match-data
    (vim:cmd-nohighlight)
    (multiple-value-bind (pattern replacement flag-str)
        (vim:parse-substitute argument)
      (let* ((flags (string->list flag-str)))
        (when (memq ?g flags)
          (error "Don't use flag g, use \"n\" with inverted meaning instead"))
        (let ((whole-line (not (memq ?n flags)))
              (confirm (memq ?c flags))
              (ignore-case (memq ?i flags))
              (dont-ignore-case (memq ?I flags)))

          (vim:do-substitute :motion motion
                             :pattern pattern
                             :replacement replacement
                             :flags flags
                             :whole-line whole-line
                             :confirm confirm
                             :ignore-case ignore-case
                             :dont-ignore-case dont-ignore-case))))))

(defun* vim:do-substitute (&key
                           motion
                           pattern
                           replacement
                           flags
                           whole-line
                           confirm
                           ignore-case
                           dont-ignore-case)
  "Do the actual substitution in current buffer. Search for regexp
pattern and replace matches with REPLACEMENT.
"
  (unless pattern (error "No pattern given"))
  (unless replacement (error "No replacement given"))

  (let* ((first-line (if motion
                       (vim:motion-first-line motion)
                       (line-number-at-pos (point))))
         (last-line (if motion
                      (vim:motion-last-line motion)
                      (line-number-at-pos (point))))

         (pattern (vim:make-pattern
                   :regex pattern
                   :whole-line whole-line
                   :case-fold (or (and ignore-case 'insensitive)
                                  (and dont-ignore-case 'sensitive)
                                  vim:substitute-case)))
         (regex (vim:pattern-regex pattern))
         (last-point (point))
         (overlay (make-overlay (point) (point)))
         (next-line (line-number-at-pos (point))))

    (let ((case-fold-search (eq 'insensitive (vim:pattern-case-fold pattern)))
          (case-replace case-fold-search))
      (unwind-protect
          (cond
            (whole-line
             ;; this one is easy, just use the built in function
             (perform-replace regex replacement confirm t nil nil nil
                              (save-excursion
                                (goto-line1 first-line)
                                (line-beginning-position))
                              (save-excursion
                                (goto-line1 last-line)
                                (line-end-position))))
            (let ((nreplaced 0))
              (if confirm
                (progn
                  ;; this one is more difficult, we have to do the
                  ;; highlighting and questioning on our own
                  (overlay-put overlay 'face
                               (if (facep 'isearch) 'isearch 'region))
                  (map-y-or-n-p (lambda (x)
                                  (set-match-data x)
                                  (move-overlay overlay (match-beginning 0) (match-end 0))
                                  (concat "Query replacing "
                                          (match-string 0)
                                          " with "
                                          (vim:match-substitute-replacement replacement
                                                                            case-fold-search)
                                          ": "))
                                (lambda (x)
                                  (set-match-data x)
                                  (replace-match replacement case-fold-search)
                                  (incf nreplaced)
                                  (setq last-point (point)))
                                (lambda ()
                                  (let ((end (save-excursion
                                               (goto-line1 last-line)
                                               (line-end-position))))
                                    (goto-line1 next-line)
                                    (beginning-of-line)
                                    (when (and (> end (point))
                                               (re-search-forward regex end t nil))
                                      (setq last-point (point))
                                      (setq next-line (1+ (line-number-at-pos (point))))
                                      (match-data))))))

                ;; just replace the first occurences per line
                ;; without highlighting and asking
                (progn
                  (goto-line1 first-line)
                  (beginning-of-line)
                  (while (and (<= (line-number-at-pos (point)) last-line)
                              (re-search-forward regex (save-excursion
                                                         (goto-line1 last-line)
                                                         (line-end-position))
                                                 t nil))
                    (incf nreplaced)
                    (replace-match replacement)
                    (setq last-point (point))
                    (forward-line)
                    (beginning-of-line))))

              (goto-char last-point)
              (if (= nreplaced 1)
                (message "Replaced 1 occurence")
                (message "Replaced %d occurences" nreplaced))))

        ;; clean-up the overlay
        (delete-overlay overlay)))))

(defun vim:parse-substitute (text)
  "Parse ex command line in TEXT and return triple
(<pattern> <replacement> <flags>)."
  (when (string-match-p "^\\s-*[/|,;:!@#]." text)
    (multiple-value-bind (pattern replacement flags)
        (vim:parse-substitute-pattern-repl-flags text)
      (values pattern (vim:substitute-expand-escapes replacement) flags))))

(defun vim:parse-substitute-pattern-repl-flags (str)
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

        delimiter
        (delimiters (string-to-list "/|,;:!@#")))
    (let* ((skip-quoted
            (lambda ()
              (while (and (< i len)
                          (not (char= (aref str i) delimiter)))
                ;; skip quoted character
                (if (char= (aref str i) ?\\)
                  (incf i 2)
                  (incf i)))))
           (skip-flags
            (lambda ()
              (while (and (< i len)
                          (member* (aref str i)
                                   (string-to-list "giIc")
                                   :test #'char=))
                (incf i))))
           (pattern-expand-newlines
            (lambda (pat)
              (letrec ((expand
                        (lambda (pos result-pat)
                          (cond
                            ((= pos (length pat))
                             result-pat)
                            ((and (char= (aref pat pos) ?\\)
                                  (< (1+ pos) (length pat))
                                  (or (char= (aref pat (1+ pos)) ?n)
                                      (char= (aref pat (1+ pos)) ?t)))
                             (let ((next-c (aref pat (1+ pos))))
                               (funcall expand
                                        (+ pos 2)
                                        (cons (cond
                                                ((char= next-c ?n) ?\n)
                                                ((char= next-c ?t) ?\t))
                                              result-pat))))
                            (t
                             (funcall expand (1+ pos)
                                      (cons (aref pat pos)
                                            result-pat)))))))
                (concatenate 'string
                             (nreverse (funcall expand 0 '())))))))
      (while (and (< i len)
                  (not (member* (aref str i)
                                delimiters
                                :test #'char=))
                  ;; (not (char= (aref str i) ?/))
                  )
        (incf i))
      (setf delimiter (aref str i))
      (incf i)
      (setf pattern-start i)
      (funcall skip-quoted)
      (setf pattern-end i)

      (incf i)
      (when (< i len)
        (setf replacement-start i)
        (funcall skip-quoted)
        (setf replacement-end i)
        (incf i)

        (when (< i len)
          (setf flags-start i)
          (funcall skip-flags)
          (setf flags-end i)))
      (values (funcall pattern-expand-newlines
                       (subseq str pattern-start (min len pattern-end)))
              (when (and replacement-start
                         replacement-end
                         ;; hack
                         (<= replacement-end len))
                (subseq str replacement-start (min len replacement-end)))
              (when (and flags-start flags-end)
                (subseq str flags-start (min len flags-end)))))))

(defun vim:substitute-expand-escapes (replacement)
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
            (case next-char
              (?n (push ?\n result))
              (?t (push ?\t result))
              (?r (push ?\r result))
              ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\\)
               (push ?\\ result)
               (push next-char result))
              (t (push next-char result)))
            (incf idx 2))
          (push char result)
          (incf idx))))
    (apply #'string (nreverse result))))

;; Related commands.
(vim:defcmd vim:cmd-nohighlight (nonrepeatable)
  "Disables the active search highlightings."
  (vim:delete-hl 'vim:search)
  (vim:delete-hl 'vim:provide)
  (vim:delete-hl 'vim:substitute)
  (search-disable-highlighting))

(vim:defcmd vim:cmd-nohighlight-everywhere (nonrepeatable)
  "Disables the active search highlightings in all buffers."
  (vim:delete-hl 'vim:search)
  (vim:delete-hl 'vim:provide)
  (vim:delete-hl 'vim:substitute)
  (search-disable-all-highlighting))

(provide 'vim-search)

;; Local Variables:
;; End:

;; vim-search.el ends here
