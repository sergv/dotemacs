;; vim-motions.el - Implementation of VIM motions. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vim-mode motions can be defined with the macro vim-defmotion.
;; Similar to commands motions have several keyword-like optional
;; parameters and a view attributes. The general form is as follows.
;;
;;   (vim-defmotion motion-name ((count [count-name])
;;                               (argument [argument-name])
;;                               {inclusive,exclusive,linewise,block})
;;     body ...)
;;
;; The count and argument parameters are optional and can have a
;; special variable-name. Exactly one of the attributes inclusive,
;; exclusive, linewise, block must be specified.
;;
;; count: The number of times the motion should be repeated.
;;
;; argument: An extra character argument to be given after the motion
;;           command has been initiated. Examples are the motions f F
;;           t T of Vim.
;;
;; inclusive, exclusive, linewise, block: This is the default motion
;;                                        type of this motion command.
;;
;; Each motion command should return an object of type vim:motion \(see
;; below\). If the function does not return such an object explicitly,
;; it is automatically created implicitly based on the position of
;; (point) before and after execution of the motion and the specified
;; motion-type. This means when a motion is called from lisp code it
;; returns *always* a vim:motion object.
;;
;; For more information about the vim:motion struct and motion types
;; look at vim-core.el.

;;; Code:

;; TODO:
;;   - should motions that do not change point automatically (ding)?
;;     this is not true in general, e.g., t
;;   - alternatively operator pending mode could never ding, but then
;;     all motions have to be valid even in case of erros

(eval-when-compile
  (require 'cl)
  (require 'subr-x))

(require 'vim-defs)
(require 'vim-macs)
(require 'vim-core)
(require 'vim-compat)

(defgroup vim-motions nil
  "Motions"
  :group 'vim-mode)

(defcustom vim-word "[:word:]"
  "Regexp-set matching a word."
  :type 'string
  :group 'vim-motions)

(defcustom vim-find-skip-newlines t
  "If non-nil character find motions t,T,f,F skip over newlines."
  :type 'boolean
  :group 'vim-motions)

(defvar-local vim--last-find nil
  "The previous find command (command . arg).")

(defvar-local vim--local-marks-alist nil
  "Local marks for this buffer.")

(defvar vim--global-marks-alist nil
  "Global marks.")

;; Definitions of custom errors

(define-error 'vim/end-of-buffer "End of buffer")
(define-error 'vim/beginning-of-buffer "Beginning of buffer")
(define-error 'vim/no-such-object "No such object")

(defun vim--local-mark-p (mark-char)
  "Returns t if `mark-char' is a local mark."
  (or (and (>= mark-char ?\a) (<= mark-char ?\z))
      (memq mark-char '(?\` ?\' ?\, ?\" ?\^ ?\. ?\< ?\>))))

(defun vim--global-mark-p (mark-char)
  "Returns t if `mark-char' is a global mark."
  (or
   (and (>= mark-char ?A) (<= mark-char ?\Z))
   (and (>= mark-char ?a) (<= mark-char ?\z))))

(defun vim--special-mark-p (mark-char)
  "Returns t if `mark-char' is one of the special marks ( ) { }."
  (memq mark-char '(?\( ?\) ?\{ ?\})))

(defconst vim--special-mark-functions-alist
  '((?\( . vim:motion-bwd-sentence)
    (?\) . vim:motion-fwd-sentence)
    (?\{ . vim:motion-bwd-paragraph)
    (?\} . vim:motion-fwd-paragraph))
  "Assocative list for special marks to corresponding functions.")

(defvar vim-save-position-omit-next nil
  "Next call to ‘vim-save-position’ won’t do anything.")

(defun vim-save-position (&optional pos)
  "Save position to ' mark register."
  (if vim-save-position-omit-next
      (setq-local vim-save-position-omit-next nil)
    (vim-set-mark ?' pos)))

(advice-add 'end-of-defun :before #'vim-save-position)

(defun vim-set-mark (mark-char &optional pos)
  "Sets the mark `mark-char' to `pos' or (point)."
  (let (m)
    (cond
      ((vim--local-mark-p mark-char)
       (setq m (or (cdr-safe (assq mark-char vim--local-marks-alist))))
       (unless m
         (setq m (make-marker))
         (push (cons mark-char m) vim--local-marks-alist)))

      ((vim--global-mark-p mark-char)
       (setq m (or (cdr-safe (assq mark-char vim--global-marks-alist))))
       (unless m
         (setq m (make-marker))
         (push (cons mark-char m) vim--global-marks-alist)))
      ((vim--special-mark-p mark-char)
       (error "Can't set special mark '%c'" mark-char))
      (t (error "Unknown mark '%c'" mark-char)))
    (set-marker m (or pos (point)))))

(defun vim-get-local-mark (mark-char)
  "Returns the marker of `mark-char' if it's in the current buffer."
  (cond
    ((vim--local-mark-p mark-char)
     (let ((m (cdr-safe (assq mark-char vim--local-marks-alist))))
       (if m m
         (error "No mark '%c' defined" mark-char))))
    ((vim--global-mark-p mark-char)
     (let ((m (cdr-safe (assq mark-char vim--global-marks-alist))))
       (if m
           (if (eq (marker-buffer m) (current-buffer))
               m
             (error "Global mark '%c' not in current buffer" mark-char))
         (error "No mark '%c' defined" mark-char))))
    ((vim--special-mark-p mark-char)
     (save-excursion
       (funcall (cdr (assq mark-char vim--special-mark-functions-alist)))
       (point)))
    (t
     (error "Unknown mark: '%c'" mark-char))))

(defun vim--set-change-mark (beg _end)
  "Sets the change mark . to `beg'."
  (vim-set-mark ?. beg))

(defun vim--register-set-change-mark-function ()
  (add-hook 'before-change-functions 'vim--set-change-mark))

(defun vim--unregister-set-change-mark-function ()
  (remove-hook 'before-change-functions 'vim--set-change-mark))

(add-hook 'vim-mode-on-hook #'vim--register-set-change-mark-function)
(add-hook 'vim-mode-off-hook #'vim--unregister-set-change-mark-function)

(vim-defmotion vim:motion-left (exclusive count raw-result)
  "Move the cursor count characters left."
  (goto-char (max (line-beginning-position)
                  (- (point) (or count 1)))))

(vim-defmotion vim:motion-right (exclusive count raw-result)
  "Move the cursor count characters right."
  (goto-char
   (min (line-end-position)
        (+ (point) (or count 1)))))

(vim-defmotion vim:motion-up (linewise count raw-result)
  "Move the cursor count lines up."
  (vim--use-last-column!)
  (let ((line-move-visual nil))
    ;; don't use `forward-line' because `previous-line' skips invisible parts
    ;; of buffer while `forward-line' doesn't
    (forward-line (- (or count 1)))))

(vim-defmotion vim:motion-down (linewise count raw-result)
  "Move the cursor count lines down."
  (vim--use-last-column!)
  (let ((line-move-visual nil))
    ;; don't use `forward-line' because `next-line' skips invisible parts
    ;; of buffer while `forward-line' doesn't
    (forward-line (or count 1))))

(vim-defmotion vim:motion-lines (linewise count raw-result)
  "Moves count - 1 lines down."
  (vim--use-last-column!)
  (let (line-move-visual
        (c (1- (or count 1))))
    (if (/= 0 c)
        (forward-line c)
      (vim-make-motion :has-begin t
                       :begin (line-beginning-position)
                       :end (line-end-position)
                       :type 'linewise))))

(vim-defmotion vim:motion-current-line (linewise count raw-result)
  "Moves count - 1 lines down, properly considering the case when point is at
e.g. shell prompt.."
  (vim--use-last-column!)
  (let (line-move-visual
        (start (line-beginning-position)))
    (vim-make-motion :has-begin t
                     :begin start
                     :end (line-end-position (or count 1))
                     ;; anything but linewise or block
                     :type 'exclusive)))

(vim-defmotion vim:motion-window-first-line (linewise count raw-result)
  "Moves the cursor to the first line of the window, plus count lines, default zero."
  (vim-save-position)
  (move-to-window-line (or count 0))
  (skip-to-indentation))

(vim-defmotion vim:motion-window-middle-line (linewise raw-result)
  "Moves the cursor to the beginning of the middle line of the window.  Ignores count."
  (vim-save-position)
  (move-to-window-line (/ (window-body-height) 2))
  (skip-to-indentation))

(vim-defmotion vim:motion-window-last-line (linewise count raw-result)
  "Moves the cursor to the last line of the window, minus count lines, default zero."
  (vim-save-position)
  (move-to-window-line (- (window-body-height) (or count 0) 1))
  (skip-to-indentation))

(defsubst vim--motion-beginning-of-line-or-digit-argument-impl (beginning-of-line-func)
  (if (and current-prefix-arg
           (not (zerop (prefix-numeric-value current-prefix-arg))))
      (call-interactively #'digit-argument)
    (call-interactively beginning-of-line-func)))

(defun vim-motion-beginning-of-line-or-digit-argument ()
  "Feeds a 0 count or moves the cursor to the beginning of the line."
  (interactive)
  (vim--motion-beginning-of-line-or-digit-argument-impl 'vim:motion-beginning-of-line:interactive))

(vim-defmotion vim:motion-beginning-of-line (exclusive raw-result)
  "Move the cursor to the beginning of the current line."
  (beginning-of-line))

(vim-defmotion vim:motion-first-non-blank (exclusive raw-result)
  "Move the cursor to the first non-blank character of the current line."
  (skip-to-indentation))

(vim-defmotion vim:motion-end-of-line (inclusive raw-result)
  "Move the cursor to the end of the current line."
  (end-of-line))

(vim-defmotion vim:motion-last-non-blank (inclusive count raw-result)
  "Move the cursor to the last non-blank charactor of the current line."
  (save-match-data
    (goto-char
     (save-excursion
       (beginning-of-line count)
       (re-search-forward "[ \t]*$")
       (max (line-beginning-position)
            (1- (match-beginning 0)))))))

(vim-defmotion vim:motion-go-to-first-non-blank-beg (exclusive count raw-result)
  "Moves the cursor to the first non-blank character of line count."
  (vim-save-position)
  (if (and current-prefix-arg
           count)
      (goto-line-dumb count)
    (goto-char (point-min)))
  (skip-to-indentation))

(defun pseudovim-motion-go-to-first-non-blank-end (&optional count)
  (interactive "p")
  (vim-save-position)
  (if (and current-prefix-arg
           count)
      (goto-line-dumb count)
    (goto-char (max
                (1- (point-max))
                (point-min))))
  (skip-to-indentation))

(vim-defmotion vim:motion-go-to-first-non-blank-end (linewise count raw-result)
  "Moves the cursor to the first non-blank character of line count."
  (pseudovim-motion-go-to-first-non-blank-end count))

(defun vim-boundary--chars (direction chars)
  "A boundary selector for a sequence of `chars'."
  (save-match-data
    (save-excursion
      (pcase direction
        (`fwd
         (when (re-search-forward (concat "[" chars "]+") nil t)
           (1- (match-end 0))))
        (`bwd
         (unless (looking-at-p (concat "[" chars "]"))
           (skip-chars-backward (if (= (aref chars 0) ?^)
                                    (substring chars 1)
                                  (concat "^" chars))))
         (skip-chars-backward chars)
         (when (looking-at-p (concat "[" chars "]"))
           (point)))))))

(defun vim-boundary--syntax (direction syntax)
  "A boundary selector for a set of syntax."
  (save-excursion
    (pcase direction
      (`fwd
       (skip-syntax-forward syntax)
       (1- (point)))
      (`bwd
       (skip-syntax-backward syntax)
       (point)))))

(defun vim-boundary--lines (direction predicate)
  "A boundary selector for lines identified by an predicate.
The begin-boundary is placed at the first character of the first
line, the end-boundary is placed at the last character before the
newline character of the last line."
  (save-excursion
    (let ((dir (pcase direction
                 (`fwd +1)
                 (`bwd -1))))
      ;; The last newline on a non-empty line does not count as part
      ;; of the current line.
      (when (and (not (bolp)) (looking-at-p "\n")) (forward-char))
      (forward-line 0)
      ;; skip unmatched lines
      (while (and (not (funcall predicate)) (zerop (forward-line dir))))
      ;; skip matched lines
      (when (funcall predicate)
        (while (save-excursion
                 (and (zerop (forward-line dir))
                      (funcall predicate)))
          (forward-line dir))
        (pcase direction
          (`fwd (end-of-line)
                (when (and (not (bolp)) (looking-at-p "\n"))
                  (backward-char)))
          (`bwd (forward-line 0)))
        (point)))))

(defun vim-boundary--empty-line (direction)
  "A boundary selector for a single empty line."
  (save-excursion
    (let ((dir (pcase direction
                 (`fwd +1)
                 (`bwd -1))))
      (while (and (not (and (bolp) (eolp))) (zerop (forward-line dir))))
      (when (and (bolp) (eolp))
        (point)))))

(defun vim-boundary--ws (direction)
  "A boundary selector for whitespaces excluding newlines."
  (vim-boundary--chars direction " \r\t"))

(defun vim-boundary--wl (direction)
  "A boundary selector for whitespaces."
  (vim-boundary--lines direction (lambda () (and (bolp) (eolp)))))

(defun vim-boundary--wsnl (direction)
  "A boundary selector for all whitespaces. A newline at the
beginning or end of the object (except empty lines) is not
counted."
  (save-excursion
    (cl-block end
      (pcase direction
        (`fwd (while (let ((pos (vim-boundary--chars 'fwd " \t\r\n")))
                       (unless pos (cl-return-from end nil))
                       (goto-char pos)
                       (when (and (not (bolp))
                                  (looking-at-p "\n")
                                  (not (looking-back "[ \t\r\n]" (point-min))))
                         (forward-char) t))))
        (`bwd (let ((start (point)))
                (while (let ((pos (vim-boundary--chars 'bwd " \t\r\n")))
                         (unless pos (cl-return-from end nil))
                         (goto-char pos)
                         (when (and (not (bolp))
                                    (looking-at-p "\n"))
                           (if (and (looking-at-p "\n[ \t\r\n]")
                                    (< (point) start))
                               (progn (forward-char) nil)
                             (backward-char) t)))))))
      (point))))

(defun vim-boundary--word (direction)
  "A boundary selector for words."
  (funcall (vim--union-boundary (lambda (dir) (vim-boundary--chars dir vim-word))
                                (lambda (dir) (vim-boundary--chars dir (concat "^ \t\r\n" vim-word)))
                                (lambda (dir) (vim-boundary--empty-line dir)))
           direction))

(defun vim-boundary--WORD (direction)
  "A boundary selector for WORDs."
  (funcall (vim--union-boundary (lambda (dir) (vim-boundary--chars dir "^ \t\r\n"))
                                (lambda (dir) (vim-boundary--empty-line dir)))
           direction))

(defun vim-boundary--symbol (direction)
  "A boundary selector for words."
  (funcall (vim--union-boundary (lambda (dir) (vim-boundary--syntax dir "w_"))
                                ;; (lambda (dir) (vim-boundary--syntax dir "^w_"))
                                (lambda (dir) (vim-boundary--empty-line dir)))
           direction))

(defun vim-boundary--sentence (direction)
  "A boundary selector for sentences."
  (save-match-data
    (save-excursion
      (pcase direction
        (`fwd (when (re-search-forward "\\([.!?][])\"']*\\)\\(?:[ \t\r\n]+\\|\\'\\)" nil t)
                (1- (match-end 1))))
        (`bwd (let ((start (point))
                    dot)
                ;; search the final char of the previous sentence, check
                ;; if it is really the end of a sentence up to the
                ;; beginning of the next sentence, and ensure that this
                ;; beginning is not behind the start position
                (while (and (setq dot (re-search-backward "[.!?]" nil t))
                            (not (bobp))
                            (or (not (re-search-forward "\\=[.!?][])\"']*[ \t\r\n]+" nil t))
                                (> (match-end 0) start)))
                  (goto-char (1- dot)))
                (when dot (point))))))))

(defun vim-boundary--paragraph (direction)
  "A boundary selector for paragraphs.
      A paragraph is a non-empty sequence of non-empty lines."
  (vim-boundary--lines direction (lambda () (not (and (bolp) (eolp))))))

(defun vim--union-boundary (&rest boundaries)
  "A boundary selector returning the nearest bound out of a set of bounds."
  (let ((boundaries-loc boundaries))
    (lambda (direction)
      (let ((positions
             (delq nil
                   (-map (lambda (bnd)
                           (funcall bnd direction))
                         boundaries-loc))))
        (when positions
          (apply (pcase direction
                   (`fwd #'min)
                   (`bwd #'max))
                 positions))))))

(defun vim--union-selector (&rest boundaries)
  "A selector returns a pair of coordinates of the next (or previous) object
described by one of the given `boundaries'."
  (let ((boundaries-loc boundaries))
    (let ((find-best (lambda (get-object first-better)
                       (let (obj1)
                         (dolist (obj2 (-map get-object boundaries-loc))
                           (cl-multiple-value-bind (b1 e1) obj1
                             (cl-multiple-value-bind (b2 e2) obj2
                               (setq obj1
                                     (cond
                                       ((null obj1) obj2)
                                       ((null obj2) obj1)
                                       ((funcall first-better b1 e1 b2 e2) obj1)
                                       (t obj2))))))
                         obj1))))
      (lambda (direction)
        (pcase direction
          (`fwd (funcall find-best
                         (lambda (bnd)
                           (let ((end (funcall bnd 'fwd)))
                             (when end
                               (let ((beg (save-excursion
                                            (goto-char end)
                                            (funcall bnd 'bwd))))
                                 (values beg end)))))
                         (lambda (b1 e1 b2 e2)
                           (or (< b1 b2) (and (= b1 b2) (> e1 e2))))))

          (`bwd (funcall find-best
                         (lambda (bnd)
                           (let ((beg (funcall bnd 'bwd)))
                             (when beg
                               (let ((end (save-excursion
                                            (goto-char beg)
                                            (funcall bnd 'fwd))))
                                 (values beg end)))))
                         (lambda (b1 e1 b2 e2)
                           (or (> e1 e2) (and (= e1 e2) (< b1 b2)))))))))))

(defun vim--move-fwd-beg (n boundary &optional linewise)
  "Moves the cursor to the beginning of the `n'-th text-object
forward given by `boundary'. A boundary is a function taking one
parameter `direction' which is either 'fwd or 'bwd. If the
paramter is 'fwd the function should return the last position
contained in the first text-object after or at point. If the
parameter is 'bwd the function should return the first position
contained in the first text-object before or at point."
  (cl-block end
    (when (> n 0)
      (let ((start (point)))
        ;; can't move further if already at the end of buffer
        (when (and (not (vim-operator-pending-mode-p))
                   (>= start (1- (point-max))))
          (signal 'vim/end-of-buffer nil))
        ;; go to the end of the (possibly) current object
        (let ((pos (funcall boundary 'fwd)))
          (if pos (goto-char pos)
            ;; no such object
            (goto-char (point-max))
            (cl-return-from end nil)))
        ;; check if this object is really the current one
        (when (< start (or (funcall boundary 'bwd) (point-min)))
          ;; if not, count this object
          (cl-decf n))
        ;; search the end of the next objects
        (dotimes (_ n)
          (if linewise (forward-line) (forward-char))
          (let ((next (funcall boundary 'fwd)))
            (unless next (goto-char (point-max)) (cl-return-from end nil))
            (goto-char next)))
        ;; found the end of the object, go to its beginning
        (goto-char (or (funcall boundary 'bwd) (point-min)))))))

(defun vim--move-fwd-end (n boundary &optional linewise)
  "Moves the cursor to the end of the `n'-th text-object forward
given by `boundary'. A boundary is a function taking one
parameter `direction' which is either 'fwd or 'bwd. If the
paramter is 'fwd the function should return the last position
contained in the first text-object after or at point. If the
parameter is 'bwd the function should return the first position
contained in the first text-object before or at point."
  (when (> n 0)
    (when (and (not (vim-operator-pending-mode-p))
               (>= (point) (1- (point-max))))
      (signal 'vim/end-of-buffer nil))
    (dotimes (_ n)
      (if linewise (forward-line) (forward-char))
      (goto-char (or (funcall boundary 'fwd) (point-max))))))

(defun vim--move-bwd-beg (n boundary &optional linewise)
  "Moves the cursor to the beginning of the `n'-th text-object
backward given by `boundary'. A boundary is a function taking one
parameter `direction' which is either 'fwd or 'bwd. If the
paramter is 'fwd the function should return the last position
contained in the first text-object after or at point. If the
parameter is 'bwd the function should return the first position
contained in the first text-object before or at point."
  (when (> n 0)
    (when (bobp) (signal 'vim/beginning-of-buffer nil))
    (dotimes (_ n)
      (if linewise (forward-line -1) (backward-char))
      (goto-char (or (funcall boundary 'bwd) (point-min))))))

(defun vim--move-bwd-end (n boundary &optional linewise)
  "Moves the cursor to the end of the `n'-th text-object backward
given by `boundary'. A boundary is a function taking one
parameter `direction' which is either 'fwd or 'bwd. If the
paramter is 'fwd the function should return the last position
contained in the first text-object after or at point. If the
parameter is 'bwd the function should return the first position
contained in the first text-object before or at point."
  (cl-block end
    (when (> n 0)
      (let ((start (point)))
        ;; can't move further if already at the beginning of buffer
        (when (eobp) (signal 'vim/beginning-of-buffer nil))
        ;; go to the beginning of the (possibly) current object
        (let ((pos (funcall boundary 'bwd)))
          (if pos (goto-char pos)
            ;; no such object
            (goto-char (point-min))
            (cl-return-from end nil)))
        ;; check if this object is really the current one
        (when (> start (or (funcall boundary 'fwd) (point-min)))
          ;; if not, count this object
          (cl-decf n))
        (dotimes (_ n)
          (if linewise (forward-line -1) (forward-char -1))
          (let ((next (funcall boundary 'bwd)))
            (unless next (goto-char (point-min)) (cl-return-from end nil))
            (goto-char next)))
        (goto-char (or (funcall boundary 'fwd) (point-min)))))))

(defun vim--inner-motion (n boundary ws-boundary type)
  "Selects or extends an inner text-object given by `boundary'.
`n' is the number of text-objects to be selected (or by which the
selection should be extended), `ws-boundary' selects the
whitespace object, `type' is the type of the motion to be
returned. A boundary is a function taking one parameter
`direction' which is either 'fwd or 'bwd. If the paramter is 'fwd
the function should return the last position contained in the
first text-object after or at point. If the parameter is 'bwd the
function should return the first position contained in the first
text-object before or at point."
  (let* ((linewise (eq type 'linewise))
         (forward (if linewise #'forward-line #'forward-char))
         (sel (vim--union-selector ws-boundary boundary))
         beg end pnt)
    (if (and (vim-visual-mode-p)
             (/= (point) (mark)))
        ;; extend visual range
        (if (< (point) (mark))
            ;; extend backward
            (progn
              (dotimes (_ n)
                (funcall forward -1)
                (cl-multiple-value-bind (b _) (funcall sel 'bwd)
                  (goto-char (or b (point-min)))))
              (setq end (mark)
                    pnt (point)))
          ;; extend forward
          (dotimes (_ n)
            (funcall forward +1)
            (cl-multiple-value-bind (_ e) (funcall sel 'fwd)
              (goto-char (or e (1- (point-max))))))
          (setq end (point)
                pnt (point)))

      ;; select current ...
      (cl-multiple-value-bind (b e) (funcall sel 'fwd)
        (dotimes (_ (1- n))
          (goto-char (or e (1- (point-max))))
          (funcall forward +1)
          (cl-multiple-value-bind (_ ne) (funcall sel 'fwd)
            (setq e (or ne (1- (point-max))))))
        (setq beg b
              end e
              pnt e)))

    (goto-char pnt)
    (if beg
        (vim-make-motion :has-begin t
                         :begin beg
                         :end end
                         :type type)
      (vim-make-motion :has-begin nil
                       :end end
                       :type type))))

(defun vim--outer-motion (n boundary ws-boundary type)
  "Selects or extends an outer text-object given by `boundary'.
`n' is the number of text-objects to be selected (or by which the
selection should be extended), `ws-boundary' selects the
whitespace object, `type' is the type of the motion to be
returned. A boundary is a function taking one parameter
`direction' which is either 'fwd or 'bwd. If the paramter is 'fwd
the function should return the last position contained in the
first text-object after or at point. If the parameter is 'bwd the
function should return the first position contained in the first
text-object before or at point."
  (let* ((linewise (eq type 'linewise))
         (forward (if linewise #'forward-line #'forward-char))
         (sel (vim--union-selector boundary))
         (ws-sel (vim--union-selector ws-boundary))
         beg end pnt)
    (if (and (vim-visual-mode-p) (/= (point) (mark)))
        ;; extend visual range
        (if (< (point) (mark))
            ;; extend backward
            (progn
              (dotimes (_ n)
                (cl-multiple-value-bind (wsb wse) (save-excursion
                                                    (funcall forward -1)
                                                    (funcall ws-sel 'bwd))
                  (vim--move-bwd-beg 1 boundary linewise)
                  (when (and wsb (< wsb (point))
                             (save-excursion
                               (funcall forward -1)
                               (>= wse (point))))
                    (goto-char wsb))))
              (setq end (point)
                    pnt (point)))

          ;; extend forward
          (dotimes (_ n)
            (cl-multiple-value-bind (wsb wse) (save-excursion
                                                (funcall forward +1)
                                                (funcall ws-sel 'fwd))
              (vim--move-fwd-end 1 boundary linewise)
              (when (and wsb (> wse (point))
                         (save-excursion
                           (funcall forward +1)
                           (<= wsb (point))))
                (goto-char wse))))
          (setq end (point)
                pnt (point)))

      ;; select current ...
      (save-excursion
        (cl-multiple-value-bind (b e) (funcall sel 'fwd)
          (unless b (signal 'vim/no-such-object nil))
          (dotimes (_ (1- n))
            (goto-char e)
            (funcall forward +1)
            (cl-multiple-value-bind (_ ne) (funcall sel 'fwd)
              (when ne (setq e ne))))
          (setq beg b
                end e)))

      ;; check whitespace before object
      (cond
        ;; started at white-space
        ((cl-multiple-value-bind (wsb _) (funcall ws-sel 'fwd)
           (when (and wsb (<= wsb (point)))
             (setq beg wsb))))

        ;; whitespace behind
        ((save-excursion
           (when (< end (point-max))
             (goto-char end)
             (funcall forward +1)
             (cl-multiple-value-bind (wsb wse) (funcall ws-sel 'fwd)
               (when (and wsb (<= wsb (point)))
                 (setq end wse))))))

        ;; no whitespace behind
        ((> beg (point-min))
         (goto-char beg)
         (funcall forward -1)
         (cl-multiple-value-bind (wsb wse) (funcall ws-sel 'bwd)
           (when (and wse (>= wse (point)))
             (setq beg wsb)))))

      (setq pnt end))

    (goto-char pnt)
    (if beg
        (vim-make-motion :has-begin t
                         :begin beg
                         :end end
                         :type type)
      (vim-make-motion :end end :type type))))

(defun vim--block-select (open-re close-re match-test open-pos close-pos n)
  "Returns the position of an enclosing block."
  (let ((find-at-point (lambda (re pos begin)
                         (goto-char pos)
                         ;; start searching the object in the current
                         ;; line to see if it's at point
                         (forward-line 0)
                         (while (and (re-search-forward re
                                                        (line-end-position) t)
                                     (< (match-end 0) pos)))
                         (if (and (match-beginning 0)
                                  (<= (match-beginning 0) open-pos)
                                  (>= (match-end 0) open-pos))
                             ;; found object at cursor
                             (if begin
                                 (goto-char (match-beginning 0))
                               (goto-char (match-end 0)))
                           (goto-char pos))))

        ;; splits the match-data into parts belonging to the
        ;; open-regexp and the close-regexp
        (split-match-data (lambda (n-open)
                            (let* ((open-md (cddr (match-data)))
                                   (split-cdr (nthcdr (1+ (* 2 n-open)) open-md))
                                   (close-md (cdr split-cdr)))
                              (setcdr split-cdr nil)
                              (values open-md close-md)))))
    (cl-block end
      (save-excursion
        (let ((combined-re (concat "\\("
                                   open-re
                                   "\\)\\|\\("
                                   close-re
                                   "\\)"))
              (n-open-groups (regexp-opt-depth open-re))
              op-beg op-end cl-beg cl-end
              (cnt n)
              found-stack)
          ;; set default match-test
          (unless match-test (setq match-test (lambda (_a _b) t)))
          ;; search the opening object
          (funcall find-at-point open-re open-pos nil)
          (while (> cnt 0)
            (unless (re-search-backward combined-re nil t) (cl-return-from end nil))
            ;; split match data for open and close regexp
            (cl-multiple-value-bind (open-md close-md)
                (funcall split-match-data n-open-groups)
              (if (car open-md)
                  ;; match opening regexp
                  (if found-stack
                      (if (funcall match-test open-md (car found-stack))
                          ;; found matching opening object
                          (pop found-stack)
                        ;; found object does not match
                        (cl-return-from end nil))
                    ;; found enclosing opening object
                    (cl-decf cnt)
                    (when (zerop cnt)
                      ;; found the opening object we looked for, so
                      ;; store it as the only object in the stack
                      (push open-md found-stack)))
                ;; match closing regexp, save it
                (push close-md found-stack))))

          ;; found the opening object
          (setq op-beg (match-beginning 0)
                op-end (1- (match-end 0)))

          ;; search the closing object
          (goto-char (1+ op-end))
          (while found-stack
            (unless (re-search-forward combined-re nil t) (cl-return-from end nil))
            (cl-multiple-value-bind (open-md close-md)
                (funcall split-match-data n-open-groups)
              (if (car close-md)
                  ;; match closing regexp
                  (if (funcall match-test (car found-stack) close-md)
                      ;; found matching closing object
                      (pop found-stack)
                    ;; found object does not match
                    (cl-return-from end nil))
                ;; found opening object
                (push open-md found-stack))))

          ;; found the closing object
          (setq cl-beg (match-beginning 0)
                cl-end (1- (match-end 0)))
          (when (>= cl-end close-pos)
            (values op-beg op-end cl-beg cl-end)))))))

(defun vim--inner-block (open-re close-re match-test n)
  "Selects the next `n' enclosing blocks excluding the delimiters."
  (let (open-pos close-pos)
    (if (vim-visual-mode-p)
        (setq open-pos (min (point) (mark))
              close-pos (max (point) (mark)))
      (setq open-pos (point)
            close-pos (point)))

    ;; check if we the current inner tag is selected completely
    (cl-multiple-value-bind (op-beg op-end cl-beg _)
        (vim--block-select open-re close-re match-test open-pos close-pos 1)
      (when (and op-beg
                 (or (= (1+ op-end) open-pos)
                     (and (= (+ 2 op-end) open-pos)
                          (save-excursion
                            (goto-char (1+ op-end))
                            (and (eolp) (not (bolp))))))
                 (or (= (1- cl-beg) close-pos)
                     (and (= (- cl-beg 2) close-pos)
                          (save-excursion
                            (goto-char cl-beg)
                            (bolp)))))
        (cl-incf n)))

    (cl-multiple-value-bind (op-beg op-end cl-beg _)
        (vim--block-select open-re close-re match-test open-pos close-pos n)
      (when op-beg
        (when (save-excursion
                (goto-char (1+ op-end))
                (and (eolp) (not (bolp))))
          ;; The opening tag ended right at eol, so skip the newline
          (cl-incf op-end))
        ;; bug: mark ofter returns nil and causes problems
        ;; (goto-char (if (< (point) (mark)) (1+ op-end) (1- cl-beg)))
        (goto-char (1- cl-beg))
        (vim-make-motion :has-begin t
                         :begin (1+ op-end)
                         :end (1- cl-beg)
                         :type 'inclusive)))))

(defun vim--outer-block (open-re close-re match-test n)
  "Selects the next `n' enclosing blocks including the delimiters."
  (let (open-pos close-pos)
    (if (vim-visual-mode-p)
        (setq open-pos (min (point) (mark))
              close-pos (max (point) (mark)))
      (setq open-pos (point)
            close-pos (point)))

    ;; check if we the current inner tag is selected completely
    (cl-multiple-value-bind (op-beg _ _ cl-end)
        (vim--block-select open-re close-re match-test open-pos close-pos 1)
      (when (and op-beg
                 (= op-beg open-pos)
                 (= cl-end close-pos))
        (cl-incf n)))

    (cl-multiple-value-bind (op-beg _ _ cl-end)
        (vim--block-select open-re close-re match-test open-pos close-pos n)
      (when op-beg
        ;; see vim--inner-block for this problem
        ;; (goto-char (if (< (point) (mark)) op-beg cl-end))
        (goto-char cl-end)
        (vim-make-motion :has-begin t
                         :begin op-beg
                         :end cl-end
                         :type 'inclusive)))))

;;; motion word

(vim-defmotion vim:motion-fwd-word (exclusive count raw-result)
  "Moves the cursor beginning of the next word."
  (let ((line (line-number-at-pos (point))))
    (vim--move-fwd-beg (or count 1) #'vim-boundary--word)

    ;; In operator-pending mode, if we reached the beginning of a new
    ;; line, go back to the end of the previous line.
    (when (and (vim-operator-pending-mode-p)
               (< line (line-number-at-pos (point))) ; only if we skipped a newline
               (vim--looking-back "^[ \t]*")
               (not (save-excursion
                      (forward-visible-line -1)
                      (and (bolp) (eolp)))))
      (forward-visible-line -1)
      (end-of-line))))

(vim-defmotion vim:motion-bwd-word (exclusive count raw-result)
  "Moves the cursor beginning of the previous word."
  (vim--move-bwd-beg (or count 1) #'vim-boundary--word))

(vim-defmotion vim:motion-fwd-word-end (inclusive count raw-result)
  "Moves the cursor to the end of the next word."
  (vim--move-fwd-end (or count 1) #'vim-boundary--word))

(vim-defmotion vim:motion-bwd-word-end (inclusive count raw-result)
  "Moves the cursor to the end of the previous word."
  (vim--move-bwd-end (or count 1) #'vim-boundary--word))

(vim-defmotion vim:motion-inner-word (inclusive count motion-result)
  "Select `count' inner words."
  (vim--inner-motion (or count 1) #'vim-boundary--word #'vim-boundary--ws 'inclusive))

(vim-defmotion vim:motion-outer-word (inclusive count motion-result)
  "Select `count' outer words."
  (vim--outer-motion (or count 1) #'vim-boundary--word #'vim-boundary--ws 'inclusive))

;;; motion WORD

(vim-defmotion vim:motion-fwd-WORD (exclusive count raw-result)
  "Moves the cursor to beginning of the next WORD."
  (let ((line (line-number-at-pos (point))))
    (vim--move-fwd-beg (or count 1) #'vim-boundary--WORD)

    ;; in operator-pending mode, if we reached the beginning of a new
    ;; line, go back to the end of the previous line
    (when (and (vim-operator-pending-mode-p)
               (< line (line-number-at-pos (point))) ; only if we skipped a newline
               (vim--looking-back "^[ \t]*")
               (not (save-excursion
                      (forward-visible-line -1)
                      (and (bolp) (eolp)))))
      (forward-visible-line -1)
      (end-of-line))))

(vim-defmotion vim:motion-bwd-WORD (exclusive count raw-result)
  "Moves the cursor to beginning of the previous WORD."
  (vim--move-bwd-beg (or count 1) #'vim-boundary--WORD))

(vim-defmotion vim:motion-fwd-WORD-end (inclusive count raw-result)
  "Moves the cursor to the end of the next WORD."
  (vim--move-fwd-end (or count 1) #'vim-boundary--WORD))

(vim-defmotion vim:motion-bwd-WORD-end (inclusive count raw-result)
  "Moves the cursor to the end of the next WORD."
  (vim--move-bwd-end (or count 1) #'vim-boundary--WORD))

(vim-defmotion vim:motion-inner-WORD (inclusive count motion-result)
  "Select `count' inner WORDs."
  (vim--inner-motion (or count 1) #'vim-boundary--WORD #'vim-boundary--ws 'inclusive))

(vim-defmotion vim:motion-outer-WORD (inclusive count motion-result)
  "Select `count' outer WORDs."
  (vim--outer-motion (or count 1) #'vim-boundary--WORD #'vim-boundary--ws 'inclusive))

;;; motion symbol

(vim-defmotion vim:motion-fwd-symbol (inclusive count raw-result)
  (vim--move-fwd-beg (or count 1) #'vim-boundary--symbol))

(vim-defmotion vim:motion-fwd-symbol-end (inclusive count raw-result)
  (vim--move-fwd-end (or count 1) #'vim-boundary--symbol))

(vim-defmotion vim:motion-bwd-symbol (inclusive count raw-result)
  (vim--move-bwd-beg (or count 1) #'vim-boundary--symbol))

(vim-defmotion vim:motion-inner-symbol (inclusive count motion-result)
  "Select `count' inner symbol."
  (vim--inner-motion (or count 1)
                     #'vim-boundary--symbol
                     #'vim-boundary--ws
                     'inclusive))

(vim-defmotion vim:motion-outer-symbol (inclusive count motion-result)
  "Select `count' outer symbols."
  (vim--outer-motion (or count 1)
                     #'vim-boundary--symbol
                     #'vim-boundary--ws
                    'inclusive))

;;; motion sentence

(vim-defmotion vim:motion-fwd-sentence (exclusive count raw-result)
  "Move the cursor `count' sentences forward."
  (dotimes (_ (or count 1))
    (goto-char (min (save-excursion
                      (vim--move-fwd-beg 1 #'vim-boundary--sentence)
                      (point))
                    (save-excursion
                      (vim:motion-fwd-paragraph)
                      (point))))))

(vim-defmotion vim:motion-bwd-sentence (exclusive count raw-result)
  "Move the cursor `count' sentences backward."
  (vim--move-bwd-beg (or count 1)
                    (vim--union-boundary #'vim-boundary--sentence #'vim-boundary--paragraph)))

(vim-defmotion vim:motion-inner-sentence (inclusive count motion-result)
  "Select `count' inner words."
  (vim--inner-motion (or count 1)
                    (vim--union-boundary #'vim-boundary--sentence #'vim-boundary--paragraph)
                    #'vim-boundary--wsnl 'inclusive))

(vim-defmotion vim:motion-outer-sentence (inclusive count motion-result)
  "Select `count' outer words."
  (vim--outer-motion (or count 1)
                    (vim--union-boundary #'vim-boundary--sentence #'vim-boundary--paragraph)
                    #'vim-boundary--wsnl 'inclusive))

;;; motion paragraph

(vim-defmotion vim:motion-fwd-paragraph (exclusive count raw-result)
  "Move the cursor `count' paragraphs forward."
  (if (eobp) (signal 'vim/end-of-buffer nil)
    (dotimes (_ (or count 1))
      (goto-char (or (vim-boundary--paragraph 'fwd) (point-max)))
      (forward-line))))

(vim-defmotion vim:motion-bwd-paragraph (exclusive count raw-result)
  "Move the cursor `count' paragraphs backward."
  (if (bobp) (signal 'vim/beginning-of-buffer nil)
    (dotimes (_ (or count 1))
      (goto-char (or (vim-boundary--paragraph 'bwd) (point-min)))
      (forward-line -1))))

(vim-defmotion vim:motion-inner-paragraph (linewise count motion-result)
  "Select `count' inner words."
  (vim--inner-motion (or count 1) #'vim-boundary--paragraph #'vim-boundary--wl 'linewise))

(vim-defmotion vim:motion-outer-paragraph (linewise count motion-result)
  "Select `count' outer words."
  (vim--outer-motion (or count 1) #'vim-boundary--paragraph #'vim-boundary--wl 'linewise))

;;; motion parentheses

(vim-defmotion vim:motion-inner-parentheses (inclusive count motion-result)
  "Select `count' enclosing pairs of () exclusive."
  (vim--inner-block "(" ")" nil (or count 1)))

(vim-defmotion vim:motion-outer-parentheses (inclusive count motion-result)
  "Select `count' enclosing pairs of () inclusive."
  (vim--outer-block "(" ")" nil (or count 1)))

;;; motion brackets

(vim-defmotion vim:motion-inner-brackets (inclusive count motion-result)
  "Select `count' enclosing pairs of [] exclusive."
  (vim--inner-block "\\[" "\\]" nil (or count 1)))

(vim-defmotion vim:motion-outer-brackets (inclusive count motion-result)
  "Select `count' enclosing pairs of [] inclusive."
  (vim--outer-block "\\[" "\\]" nil (or count 1)))

;;; motion braces

(vim-defmotion vim:motion-inner-braces (inclusive count motion-result)
  "Select `count' enclosing pairs of {} exclusive."
  (vim--inner-block "{" "}" nil (or count 1)))

(vim-defmotion vim:motion-outer-braces (inclusive count motion-result)
  "Select `count' enclosing pairs of {} inclusive."
  (vim--outer-block "{" "}" nil (or count 1)))

;;; motion angles

(vim-defmotion vim:motion-inner-angles (inclusive count motion-result)
  "Select `count' enclosing pairs of <> exclusive."
  (vim--inner-block "<" ">" nil (or count 1)))

(vim-defmotion vim:motion-outer-angles (inclusive count motion-result)
  "Select `count' enclosing pairs of <> inclusive."
  (vim--outer-block "<" ">" nil (or count 1)))

;;; motion xml tags

(defun vim--compare-blocks-match1 (open-md close-md)
  "Helper functions, compares two blocks by the regular
expression subgroup 1."
  (zerop (compare-buffer-substrings
          nil (nth 2 open-md) (nth 3 open-md)
          nil (nth 2 close-md) (nth 3 close-md))))

(defun vim--generic-motion-xml-blocks (block-function count)
  "Calls a block selection function with regular expressions
matching xml tags. `block-function' should be either
#'vim--inner-block or #'vim:outer-block."
  (funcall block-function
           "<\\([^/>]+?\\)>" "</\\([^/>]+?\\)>"
           #'vim--compare-blocks-match1
           (or count 1)))

(vim-defmotion vim:motion-inner-xml-tags (inclusive count raw-result)
  "Select `count' enclosing pairs of <tag> </tag> exclusive."
  (vim--generic-motion-xml-blocks #'vim--inner-block count))

(vim-defmotion vim:motion-outer-xml-tags (inclusive count raw-result)
  "Select `count' enclosing pairs of <tag> </tag> inclusive."
  (vim--generic-motion-xml-blocks #'vim--outer-block count))

;;; motion quote

(defun vim--bounds-of-generic-quote (open-qt close-qt &optional side)
  "Returns the start and end points of a text enclosed in some quotes,
but only on the current line."
  (save-excursion
    (let* ((p (point))
           (end (line-end-position))
           (re (concat open-qt "\\(?:[^\\\\]\\|\\\\.\\)*?" close-qt))
           md
           (retry t))
      (beginning-of-line)
      (while retry
        (if (not (re-search-forward re end t))
            (setq retry nil)
          (pcase side
            (`nil
             (when (> (match-end 0) p)
               (setq retry nil)
               (setq md (match-data))))
            (`before
             (if (>= (match-beginning 0) p)
                 (setq retry nil)
               (setq md (match-data))))
            (`after
             (when (> (match-end 0) (1+ p))
               (setq retry nil
                     md (match-data)))))))

      (when md
        (set-match-data md)
        (cons (match-beginning 0) (1- (match-end 0)))))))

(defun vim--inner-quote (count open-qt &optional close-qt)
  "Select text between two quotes."
  (let ((bounds (vim--bounds-of-generic-quote open-qt
                                              (or close-qt open-qt))))
    (cond
      ;; no quote found
      ((not bounds) (signal 'vim/no-such-object nil))
      ;; point is in visual mode on one of both quotes
      ;; or quoted text is empty
      ((or (>= 1 (- (cdr bounds) (car bounds)))
           (eq count 2)
           (and (vim-visual-mode-p)
                (= (min (point) (mark)) (1+ (car bounds)))
                (= (max (point) (mark)) (1- (cdr bounds)))))
       (goto-char
        (if (and (vim-visual-mode-p) (< (point) (mark)))
            (car bounds)
          (cdr bounds)))
       (vim-make-motion :has-begin t
                        :begin (car bounds)
                        :end (cdr bounds)
                        :type 'inclusive))
      ;; visual mode an point is on at leas one of both quotes
      ((and (vim-visual-mode-p)
            (not (= (point) (mark)))
            (or (>= (car bounds) (min (point) (mark)))
                (<= (cdr bounds) (max (point) (mark)))))
       (signal 'vim/no-such-object nil))
      (t
       (goto-char
        (if (and (vim-visual-mode-p) (< (point) (mark)))
            (1+ (car bounds))
          (1- (cdr bounds))))
       (vim-make-motion :has-begin t
                        :begin (1+ (car bounds))
                        :end (1- (cdr bounds))
                        :type 'inclusive)))))

(defun vim--outer-quote (_count open-qt &optional close-qt)
  "Select text between two quotes including the quotes."
  (if (and (vim-visual-mode-p)
           (/= (point) (mark)))
      ;; visual mode so extend the region
      (let* ((to-right (>= (point) (mark)))
             (bounds (vim--bounds-of-generic-quote
                      open-qt (or close-qt open-qt)
                      (if to-right 'after 'before)))
             (beg (min (point) (mark) (car bounds)))
             (end (max (point) (mark) (cdr bounds)))
             (pnt (if to-right end beg)))
        (goto-char pnt)
        (when to-right
          (forward-char)
          (skip-chars-forward " \t\r")
          (backward-char)
          (setq end (point)))
        (vim-make-motion :has-begin t
                         :begin beg
                         :end end
                         :type 'inclusive))

    (let ((bounds (vim--bounds-of-generic-quote open-qt
                                                (or close-qt open-qt))))
      (cond
        ;; nothing found
        ((not bounds) (signal 'vim/no-such-object nil))
        ;; extend whitespaces to the right
        ((save-excursion
           (goto-char (1+ (cdr bounds)))
           (looking-at "[ \t\r]"))
         (let ((end (save-excursion
                      (goto-char (1+ (cdr bounds)))
                      (skip-chars-forward " \t\r")
                      (1- (point)))))
           (goto-char end)
           (vim-make-motion :has-begin t
                            :begin (car bounds)
                            :end end
                            :type 'inclusive)))
        (t
         ;; extend whitespaces to the left
         (goto-char (cdr bounds))
         (vim-make-motion :has-begin t
                          :begin (save-excursion
                                   (goto-char (car bounds))
                                   (skip-chars-backward " \t\r")
                                   (point))
                          :end (cdr bounds)
                          :type 'inclusive))))))

(defun vim--bounds-of-string--guess-via-beginning-of-defun ()
  ;; Find beginning current “function” to limit parsing
  ;; scope. The “function” here is defined as any
  ;; non-whitespace entity which starts at column 0.
  (save-excursion
    (beginning-of-defun)
    (point)))

(defun vim--bounds-of-string--guess-via-comint-prompt ()
  "Take closest comint prompt as a reasonable guess."
  (or (save-excursion
        (and (comint-previous-prompt 1)
             (point)))
      (point-min)))

(defvar vim-bounds-of-string-guess-start #'vim--bounds-of-string--guess-via-beginning-of-defun
  "Function that should produce point in current buffer that would
be a good start for ‘parse-partial-sexp’ region for the purpose
of detecting strings.

The function shouldn’t move point.")

(defun vim--bounds-of-string (p)
  "Return beginning and end of string at poith P."
  (save-excursion
    (let* ((start (funcall vim-bounds-of-string-guess-start))
           (end p)
           (state (parse-partial-sexp start end))
           (inside-stringp (elt state 3))
           (string-start (elt state 8)))
      (if inside-stringp
          (progn
            ;; Continue parsing to find the end of string.
            (parse-partial-sexp end
                                (point-max)
                                nil
                                nil
                                state
                                'syntax-table ;; Stop after string end.
                                )
            (list string-start (- (point) 1)))
        nil))))

(defun vim--inner-doubled-quote (count)
  "Select text between two quotes."
  (let ((bounds (vim--bounds-of-string (point))))
    (if (not bounds)
        (signal 'vim/no-such-object nil)
      (cl-multiple-value-bind (beg end) bounds
        (cond
          ;; point is in visual mode on one of both quotes
          ;; or quoted text is empty
          ((or (>= 1 (- end beg))
               (eq count 2)
               (and (vim-visual-mode-p)
                    (= (min (point) (mark)) (1+ beg))
                    (= (max (point) (mark)) (1- end))))
           (goto-char
            (if (and (vim-visual-mode-p) (< (point) (mark)))
                beg
              end))
           (vim-make-motion :has-begin t
                            :begin beg
                            :end end
                            :type 'inclusive))
          ;; visual mode an point is on at leas one of both quotes
          ((and (vim-visual-mode-p)
                (not (= (point) (mark)))
                (or (>= beg (min (point) (mark)))
                    (<= end (max (point) (mark)))))
           (signal 'vim/no-such-object nil))
          (t
           (goto-char
            (if (and (vim-visual-mode-p) (< (point) (mark)))
                (1+ beg)
              (1- end)))
           (vim-make-motion :has-begin t
                            :begin (1+ beg)
                            :end (1- end)
                            :type 'inclusive)))))))

;; TODO: find out how to support count here
(defun vim--outer-doubled-quote (_count)
  "Select text between two quotes including the quotes."
  (if (and (vim-visual-mode-p)
           (/= (point) (mark)))
      ;; visual mode so extend the region
      (let* ((to-right (>= (point) (mark)))
             (bounds (vim--bounds-of-string (point))))
        (when bounds
          (let* ((beg (min (point) (mark) (first bounds)))
                 (end (max (point) (mark) (second bounds)))
                 (pnt (if to-right end beg)))
            (goto-char pnt)
            (when to-right
              (forward-char)
              (skip-chars-forward " \t\r")
              (backward-char)
              (setq end (point)))
            (vim-make-motion :has-begin t
                             :begin beg
                             :end end
                             :type 'inclusive))))

    (if-let (bounds (vim--bounds-of-string (point)))
        (cl-multiple-value-bind (beg end) bounds
          (cond
            ;; extend whitespaces to the right
            ((save-excursion
               (goto-char (1+ end))
               (looking-at "[ \t\r]"))
             (let ((end (save-excursion
                          (goto-char (1+ end))
                          (skip-chars-forward " \t\r")
                          (1- (point)))))
               (goto-char end)
               (vim-make-motion :has-begin t
                                :begin beg
                                :end end
                                :type 'inclusive)))
            (t
             ;; extend whitespaces to the left
             (goto-char end)
             (vim-make-motion :has-begin t
                              :begin (save-excursion
                                       (goto-char (car bounds))
                                       (skip-chars-backward " \t\r")
                                       (point))
                              :end end
                              :type 'inclusive))))
      ;; nothing found
      (signal 'vim/no-such-object nil))))

(defconst vim--motion-single-quote-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" tbl)
    tbl))

(vim-defmotion vim:motion-inner-single-quote (inclusive count motion-result)
  "Select text between two single quotes without the quotes."
  (with-syntax-table vim--motion-single-quote-syntax-table
    (vim--inner-doubled-quote count)))

(vim-defmotion vim:motion-outer-single-quote (inclusive count motion-result)
  "Select text between two single quotes including the quotes."
  (with-syntax-table vim--motion-single-quote-syntax-table
    (vim--outer-doubled-quote count)))

(defconst vim--motion-double-quote-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" tbl)
    tbl))

(vim-defmotion vim:motion-inner-double-quote (inclusive count motion-result)
  "Select text between two double quotes without the quotes."
  (with-syntax-table vim--motion-double-quote-syntax-table
    (vim--inner-doubled-quote count)))

(vim-defmotion vim:motion-outer-double-quote (inclusive count motion-result)
  "Select text between two double quotes including the quotes."
  (with-syntax-table vim--motion-double-quote-syntax-table
    (vim--outer-doubled-quote count)))

(defconst vim--motion-back-quote-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" tbl)
    tbl))

(vim-defmotion vim:motion-inner-back-quote (inclusive count motion-result)
  "Select text between two back quotes without the quotes."
  (with-syntax-table vim--motion-back-quote-syntax-table
    (vim--inner-doubled-quote count)))

(vim-defmotion vim:motion-outer-back-quote (inclusive count motion-result)
  "Select text between two back quotes including the quotes."
  (with-syntax-table vim--motion-back-quote-syntax-table
    (vim--outer-doubled-quote count)))

(vim-defmotion vim:motion-find (inclusive count (argument:char arg) raw-result)
  "Move the cursor to the next count’th occurrence of arg."
  (forward-char)
  (let ((case-fold-search nil))
    (unless (search-forward (char-to-string arg)
                            (unless vim-find-skip-newlines (line-end-position))
                            t (or count 1))
      (backward-char)
      (error (format "Can't find %c" arg)))
    (setq vim--last-find (cons #'vim:motion-find arg))
    (backward-char)))

(vim-defmotion vim:motion-find-back (exclusive count (argument:char arg) raw-result)
  "Move the cursor to the previous count'th occurrence of arg."
  (let ((case-fold-search nil))
    (unless (search-backward (char-to-string arg)
                             (unless vim-find-skip-newlines (line-beginning-position))
                             t (or count 1))
      (error (format "Can't find %c" arg)))
    (setq vim--last-find (cons #'vim:motion-find-back arg))))

(vim-defmotion vim:motion-find-to (inclusive count (argument:char arg) raw-result)
  "Move the cursor to the character before the next count'th\
   occurence of arg."
  (vim:motion-find :count count :argument arg)
  (backward-char)
  (setq vim--last-find (cons #'vim:motion-find-to arg)))

(vim-defmotion vim:motion-find-back-to (exclusive count (argument:char arg) raw-result)
  "Move the cursor to the character after the previous count'th\
   occurence of arg."
  (vim:motion-find-back :count count :argument arg)
  (forward-char)
  (setq vim--last-find (cons #'vim:motion-find-to arg)))

(vim-defmotion vim:motion-repeat-last-find (inclusive count raw-result)
  "Repeats the last find command."
  (unless vim--last-find
    (error "No previous find command"))
  (funcall (car vim--last-find)
           :count count
           :argument (cdr vim--last-find)))

(vim-defmotion vim:motion-repeat-last-find-opposite (inclusive count raw-result)
  "Repeats the last find command."
  (unless vim--last-find
    (error "No previous find command"))
  (let ((func
         (pcase (car vim--last-find)
           (`vim:motion-find         'vim:motion-find-back)
           (`vim:motion-find-back    'vim:motion-find)
           (`vim:motion-find-to      'vim:motion-find-back-to)
           (`vim:motion-find-back-to 'vim:motion-find-to)
           (_                        (error (format "Unexpected find command %s"
                                                    (car vim--last-find))))))
        (arg (cdr vim--last-find)))
    (let ((vim--last-find nil))
      (funcall func :count count :argument arg))))

(vim-defmotion vim:motion-jump-item (inclusive raw-result)
  "Find the next item in this line after or under the cursor and
jumps to the corresponding one."
  (pseudovim-motion-jump-item))

(defun vim:forward-end-of-block (open-close-re count)
  "Go to the `count'-th next unmatched end of block."
  (let ((cnt (or count 1)))
    (save-match-data
      (save-excursion
        (while (and (> cnt 0)
                    (re-search-forward open-close-re nil t))
          (if (match-beginning 1)
              (cl-incf cnt)
            (cl-decf cnt))))
      (if (zerop cnt)
          (goto-char (match-beginning 0))
        (signal 'vim/no-such-object (list "No closing of block found."))))))

(defun vim:backward-beginning-of-block (open-close-re count)
  "Go to the `count'-th previous unmatched beginning of block."
  (let ((cnt (or count 1)))
    (save-match-data
      (save-excursion
        (while (and (> cnt 0)
                    (re-search-backward open-close-re nil t))
          (if (match-beginning 1)
              (cl-decf cnt)
            (cl-incf cnt))))
      (if (zerop cnt)
          (goto-char (match-beginning 0))
        (signal 'vim/no-such-object (list "No opening of block found."))))))

(vim-defmotion vim:motion-forward-closing-parenthesis (exclusive count raw-result)
  "Go to the `count'-th next unmatched closing )."
  (vim:forward-end-of-block (rx (or (group-n 1 "(") ")")) count))

(vim-defmotion vim:motion-backward-opening-parenthesis (exclusive count raw-result)
  "Go to the `count'-th previous unmatched opening (."
  (vim:backward-beginning-of-block (rx (or (group-n 1 "(") ")")) count))

(vim-defmotion vim:motion-forward-closing-bracket (exclusive count raw-result)
  "Go to the `count'-th next unmatched closing ]."
  (vim:forward-end-of-block (rx (or (group-n 1 "[") "]")) count))

(vim-defmotion vim:motion-backward-opening-bracket (exclusive count raw-result)
  "Go to the `count'-th previous unmatched opening [."
  (vim:backward-beginning-of-block (rx (or (group-n 1 "[") "]")) count))

(vim-defmotion vim:motion-forward-closing-brace (exclusive count raw-result)
  "Go to the `count'-th next unmatched closing }."
  (vim:forward-end-of-block (rx (or (group-n 1 "{") "}")) count))

(vim-defmotion vim:motion-backward-opening-brace (exclusive count raw-result)
  "Go to the `count'-th previous unmatched opening {."
  (vim:backward-beginning-of-block (rx (or (group-n 1 "{") "}")) count))

(vim-defmotion vim:motion-backward-opening-comment (exclusive count raw-result)
  "Go to the `count'-th previous unmatched opening /*."
  (save-match-data
    (when (save-excursion
            (re-search-backward "/\\*" nil t count))
      (goto-char (match-beginning 0)))))

(vim-defmotion vim:motion-forward-closing-comment (exclusive count raw-result)
  "Go to the `count'-th next unmatched closing ]."
  (save-match-data
    (when (save-excursion
            (re-search-forward "\\*/" nil t count))
      (goto-char (match-beginning 0)))))

(vim-defmotion vim:motion-mark (exclusive (argument:char mark-char) raw-result)
  "Moves to the position of `mark-char'."
  (let ((pos (point)))
    (goto-char (vim-get-local-mark mark-char))
    (vim-save-position pos)))

(vim-defmotion vim:motion-mark-line (linewise (argument:char mark-char) raw-result)
  "Moves to the first non-blank char in the line of `mark-char'."
  (let ((pos (point)))
    (goto-char (vim-get-local-mark mark-char))
    (vim:motion-first-non-blank)
    (vim-save-position pos)
    t))

(provide 'vim-motions)

;; Local Variables:
;; End:

;; vim-motions.el ends here
