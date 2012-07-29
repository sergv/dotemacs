;;; sexpy-highlight.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  8 January 2012
;; Keywords: fontification
;; Requirements:
;; Status: in development

;; Oh, there's lots of stuff left to make this usable and, first of all,
;; manageable - reduce is useful and necessary but by no means sufficient
;; for this kind of task.
;; In short:
;; a. better abstractions required
;; b. fontification description language (FDL) development and
;; specification requried
;; c. everything should be based on that specification
;; d. btw set of features of FDL needs much more attention - it's necessary
;; to redo it form scratch


(defun unfontify-buffer ()
  (interactive)
  (remove-text-properties (point-min) (point-max) '(face nil)))



(defun sexpy-interpreter (point limit fontification)
  "Take list of FDL code - FONTIFICATION argument and tries to collect
match-data starting from POINT using FONTIFICATION and not to exceed LIMIT."
  (goto-char point)
  (let ((result nil))
    (catch 'done
      )))

(defun sexpy-one-step (fontification bounds-so-far)
  "Perform one step according to FONTIFICATION, move point and
return bounds with resulting fontification
 (new-font (list of (start end) pairs)). List of (start end) pairs
would be in reversed order."
  (when (or (not fontification)
            (not (every #'consp fontification)))
    (error "Bad fontification format"))
  (let ((begin (point))
        (parse-sexp-ignore-comments t))
    (catch 'unknown-reader-macro
      (case (caar fontification)
        (face
         (multiple-value-bind (sexp-beg _ sexp-end)
             (bounds-of-sexp begin)
           (goto-char sexp-end)
           (values (cdr fontificanion)
                   (cons (list sexp-beg sexp-end)
                         bounds-so-far))))
        (skip
         (goto-char (end-of-sexp begin))
         (values (cdr fontificanion)
                 bounds-so-far))
        (list
         (let ((font (cdar fontification))
               (result bounds-so-far))
           (while (not (null font))
             (multiple-value-bind (new-font bounds)
                 (sexpy-one-step font result)
               (setf font new-font
                     result bounds)))
           (values (cdr fontification)
                   result)))
        (last
         (let ((font (cdar fontification)))
           (multiple-value-bind (new-font bounds)
               (sexpy-one-step font bounds-so-far)
             (values fontification
                     bounds))))
        (alt
         (let ((atom-case (cadar fontification))
               (list-case (caddar fontification)))
           (multiple-value-bind (sexp-begin sexp-no-prefix sexp-end)
               (bounds-of-sexp begin)
             (multiple-value-bind (_ bounds)
                 (if (beginning-of-sexp? sexp-no-prefix)
                   (sexpy-one-step atom-case bounds-so-far)
                   (sexpy-one-step list-case bounds-so-far))
               (values (cdr fontification)
                       bounds)))))))))

(defun end-of-sexp (pos)
  "Return end of sexp or after (skipping whitespace) POS."
  (multiple-value-bind (sexp-begin prefixless-begin sexp-end)
      (bounds-of-sexp begin)
    sexp-end))

(defun bounds-of-sexp (pos)
  "Return triple (begin prefixless-begin end) of sexp at or
after (skipping whitespace) POS."
  (goto-char pos)
  (skip-syntax-forward " <>")
  (let ((begin (point)))
    (cond
      ;; some heuristics to handle reader macro
      ((char= ?\# (char-after))
       (skip-syntax-forward "^ <>(")
       (if (beginning-of-sexp? (point))
         ;; if it's sexp after reader macro then everything is ok -
         ;; it's closing paren is probably the end
         (values begin (point) (scan-sexps (point) 1))
         ;; however if there's no sexp then we generally don't know
         ;; where this macro ends, but we (probably) know that we're
         ;; in some sort of sexp and we abandon that sexp if unknown
         ;; reader macro encountered
         (let ((macro-dispatcher (buffer-substring-no-properties (1+ pos)
                                                                 (point))))
           (cond
             ((member* macro-dispatcher
                       '("x" "b" "o")
                       :test #'string=)
              (values begin (1+ pos) (point)))
             ;; aye, unknown reader macro met
             (t
              (throw 'unknown-reader-macro nil)))))
       (t
        (skip-syntax-forward "'")
        (let ((prefixless-begin (point)))
          (values begin prefixless-begin (scan-sexps prefixless-begin 1))))))))





























;; First attempt at sexp-based highlighting, failed badly
;; (defun lhl-highlight-sexp (pos pattern)
;;   "Highlight sexp starting at POS (i.e. \( char at POS) as descirbed in PATTERN,
;; pattern being like this
;; \((face face1) default skip (repeat (face face3) skip (face face4)) default\)."
;;   (goto-char pos)
;;   (forward-char 1)
;;   ;; skip spaces
;;   (skip-syntax-forward " >")
;;   (let* ((sexp-end (save-excursion
;;                     (forward-list)
;;                     (point)))
;;          (sexp-length (- sexp-end (point))))
;;     (unless (= 1 sexp-length)
;;       (let ((prev (point))
;;             (char (char-after)))
;;         ;; skip out of loop when end of current sexp will be encountered
;;         (condition-case nil
;;             (while t
;;               (forward-sexp 1)
;;               ;; so now region between prev and (point) contains sexp
;;               ;; to be highlighted
;;
;;               (cond
;;                 ((char= char ?\()
;;                  ;; beginning of next sexp
;;                  (lhl-highlight-func-application prev))
;;                 (t
;;                  ))
;;
;;               (skip-syntax-forward " >")
;;               (setf prev (point)
;;                     char (char-after)))
;;           (error nil))))))


;; ;; and this stuff seems to be really cool
;;
;; (defun reduce-over-buffer-sexp (pos func start)
;;   "Reduce function FUNC over all elements of sexp which
;; starts at point POS. Reduce from left to right. FUNC is a
;; function of four arguments, second and fourth being start
;; and exclusive end of current sexp (i.e. buffer positions),
;; and third being start of sexp without prefix characters (#', etc)."
;;   (assert (beginning-of-sexp? pos))
;;   (goto-char pos)
;;   (move-by-char 'forward)
;;   (skip-syntax-forward " <>")
;;   (unless (end-of-sexp? (point))
;;     (let ((arg start)
;;           (parse-sexp-ignore-comments t))
;;       (catch 'reduce-over-buffer-sexp-done
;;         ;; scan-sexps may throw error
;;         ;; f may throw something interesting
;;         (while t
;;           (multiple-value-bind (begin prefixless-begin end)
;;               (bounds-of-sexp (point))
;;             (assert (not (or (= begin end)
;;                              (= prefixless-begin end))))
;;             (setf arg
;;                   (funcall func arg begin prefixless-begin end))
;;             (goto-char end)
;;             (skip-syntax-forward " <>")
;;             (when (end-of-sexp? (point)) ;; (char= ?\) (char-after))
;;               (throw 'reduce-over-buffer-sexp-done arg)))
;;
;;           ;; (goto-char (scan-sexps (point) 1))
;;           ;; (let ((end (point)))
;;           ;;   (goto-char (scan-sexps (point) -1))
;;           ;;   (let ((prefixless-begin (point)))
;;           ;;     (backward-prefix-chars)
;;           ;;     (let ((begin (point)))
;;           ;;       (assert (not (or (= begin end)
;;           ;;                        (= prefixless-begin end))))
;;           ;;       (setf arg
;;           ;;             (funcall func arg begin prefixless-begin end))
;;           ;;       (goto-char end)
;;           ;;       (skip-syntax-forward " <>")
;;           ;;       (when (end-of-sexp? (point)) ;; (char= ?\) (char-after))
;;           ;;         (throw 'reduce-over-buffer-sexp-done nil)))))
;;           )
;;         ;; (condition-case nil
;;         ;;
;;         ;;     (error (error "Unbalanced expression encountered")))
;;         ))))
;;
;;
;; (defun bounds-of-sexp (pos)
;;   (goto-char pos)
;;   (skip-syntax-forward " <>")
;;   (cond
;;     ;; some heuristics to handle reader macro
;;     ((char= ?\# (char-after))
;;      (skip-syntax-forward "^ <>(")
;;      (if (beginning-of-sexp? (point))
;;        ;; if it's sexp after reader macro then everything is ok -
;;        ;; it's closing paren is probably the end
;;        (values pos (point) (scan-sexps (point) 1))
;;        ;; however if there's no sexp then we generally don't know
;;        ;; where this macro ends, but we (probably) know that we're
;;        ;; in some sort of sexp and we abandon that sexp if unknown
;;        ;; reader macro encountered
;;        (let ((macro-dispatcher (buffer-substring-no-properties (1+ pos)
;;                                                                (point))))
;;          (cond
;;            ((member* macro-dispatcher
;;                      '("x" "b" "o")
;;                      :test #'string=)
;;             (values pos (1+ pos) (point)))
;;            ;; aye, unknown reader macro met
;;            (t
;;             (throw 'reduce-over-buffer-sexp-done nil))))))
;;     (t
;;      (skip-syntax-forward "'")
;;      (let ((prefixless-begin (point)))
;;        (values pos prefixless-begin (scan-sexps prefixless-begin 1))))))
;;
;;
;; ;; (face x) - use x face only for that place
;; ;; (list x ...) - list argument expected, fontify it's elements with x ...
;; ;; (repeat x) - here list argument expected, fontify with x
;; ;; every element of that list
;; ;; (as x) - search for x in *fontification-alist* and apply
;; ;; found fontification instead, as must be first and single element
;; ;; (rest x) - must be last form in list, zero or more elements expected there,
;; ;; fontify every one of them with x
;; ;; &determine - investigate this argument and determine
;; ;; fontification from it's structure - if it's sexp then
;; ;; infer fontification using sexps' head
;; ;; ;; &rest - many arguments expected, fontify every one of them
;; ;; ;; with &determine
;; ;; &atoms-in-sexp - iterate over sexp and fontify atoms where appropriate,
;; ;; recurse into subsexps and fontify atoms there too
;; ;; &atom - here atom expected, probably with prefix. fontify it as other atoms
;; ;; nil - do nothing
;; (defface sexpy-test-1
;;     '((t (:foreground "#cb4b16"))) "")
;; (defface sexpy-test-2
;;     '((t (:foreground "#2aa198"))) "")
;; (defface sexpy-test-3
;;     '((t (:foreground "#d33682"))) "")
;;
;;
;; (defvar *fontification-alist*
;;   nil)
;;
;; (setf *fontification-alist*
;;       '((defun
;;             (face font-lock-keyword-face)
;;             (face font-lock-variable-name-face)
;;           (repeat (face font-lock-warning-face))
;;           (face font-lock-string-face)
;;           (rest &determine))
;;         (let
;;             (face sexpy-test-3)
;;           (repeat (list nil (face sexpy-test-2) ;; &determine
;;                         ))
;;           (rest &determine))
;;         (foo (rest &determine))
;;         (defun* as defun)))
;;
;;
;; (defun fontify-form-at-point ()
;;   (interactive)
;;   (save-excursion
;;    (determine-and-fontify (point))))
;;
;; (defun head-of-sexp (pos)
;;   "Return first element of sexp starting at point POS as string or
;; nil if there's no sexp or that sexp is empty."
;;   (save-excursion
;;    (unless (end-of-sexp? (1+ pos))
;;      (multiple-value-bind (begin prefixless-begin end)
;;          (bounds-of-sexp (1+ pos))
;;        (buffer-substring-no-properties prefixless-begin
;;                                        end)))))
;;
;;
;; (defun fontify-part-with (fontify-command begin end)
;;   (cond
;;     ((null fontify-command)
;;      )
;;     ((listp fontify-command)
;;      (let ((tag (car fontify-command)))
;;        (cond
;;          ((eq tag 'face)
;;           (put-text-property begin end 'face (cadr fontify-command)))
;;          ((eq tag 'repeat)
;;           (assert (beginning-of-sexp? begin))
;;           (reduce-over-buffer-sexp begin
;;                                    #'(lambda (fontification begin prefixless-begin end)
;;                                        (fontify-part-with fontification
;;                                                           prefixless-begin
;;                                                           end)
;;                                        fontification)
;;                                    (cadr fontify-command)))
;;          ((eq tag 'list)
;;           (assert (beginning-of-sexp? begin))
;;           (reduce-over-buffer-sexp begin
;;                                    #'fontify-list-linearly
;;                                    (cdr fontify-command)))
;;          ((eq tag 'rest)
;;           (fontify-part-with (cadr fontify-command)
;;                              begin
;;                              end))
;;          (t
;;           (error "unknows fontification tag: %s" tag)))))
;;     ((eq fontify-command '&determine)
;;      (determine-and-fontify begin))
;;     ;; ((eq fontify-command '&rest)
;;     ;;  (determine-and-fontify begin))
;;     ((eq fontify-command '&atoms-in-sexp)
;;      (reduce-over-buffer-sexp begin
;;                               #'(lambda (_ begin prefixless-begin end)
;;                                   (if (beginning-of-sexp? prefixless-begin)
;;                                     (fontify-part-with fontify-command
;;                                                        prefixless-begin
;;                                                        end)
;;                                     (fontify-atom begin
;;                                                   prefixless-begin
;;                                                   end)))
;;                               nil))
;;     ((eq fontify-command '&atom)
;;      (multiple-value-bind (atom-begin atom-prefixless-begin atom-end)
;;          (bounds-of-sexp begin)
;;        (fontify-atom atom-begin
;;                      atom-prefixless-begin
;;                      atom-end)))
;;     (t
;;      (error "unknown fontification command: %s" fontify-command))))
;;
;; (defun determine-and-fontify (pos)
;;   (if (beginning-of-sexp? pos)
;;     (let ((head (head-of-sexp pos)))
;;       (when (and head (> (length head) 0))
;;         (reduce-over-buffer-sexp
;;          pos
;;          #'fontify-list-linearly
;;          (determine-fontification-for-form (intern head)))))
;;     (progn
;;       (goto-char pos)
;;       (skip-syntax-forward "'")
;;       (let ((prefixless-begin (point))
;;             (parse-sexp-ignore-comments t))
;;         (fontify-atom pos prefixless-begin (scan-sexps prefixless-begin 1))))))
;;
;; (defun determine-fontification-for-form (head-symbol)
;;   (let ((fontification (cdr (assoc* head-symbol
;;                                     *fontification-alist*
;;                                     :test #'eq))))
;;     ;; if no fontification found then this sexp isn't a special form
;;     ;; and therefore skip over it's atoms without recursing
;;     ;; into subsexps
;;     (cond
;;       ((null fontification)
;;        '((rest &determine)))
;;       ((and (listp fontification)
;;             (eq 'as (car-safe fontification)))
;;        (determine-fontification-for-form (cadr fontification)))
;;       (t fontification))))
;;
;; (defun rest-command? (fontification)
;;   (and (listp fontification)
;;        (eq 'rest (car fontification))))
;;
;; (defun fontify-list-linearly (fontification begin prefixless-begin end)
;;   (fontify-part-with (car fontification) prefixless-begin end)
;;   (if (rest-command? (car fontification))
;;     fontification
;;     (cdr fontification)))
;;
;; (defun fontify-atom (begin prefixless-begin end)
;;   )

;; (defun iterate-over-sexp-at-point ()
;;   (interactive)
;;   (save-excursion
;;    (reduce-over-buffer-sexp
;;     (point)
;;     (lambda (_ beg prefixless-begin end)
;;       (message "observing subsexp: %s"
;;                (buffer-substring-no-properties prefixless-begin end)))
;;     nil)))
;;
;; '( foo bar baz-baz
;;   ( quux
;;    'quux    )  ()
;;   '(  "  foo"
;;     )
;;   #'(((((hello)))))
;;
;;   world
;;
;;   )


;; (let ((hello world)
;;       (foo (bar)))
;;   a
;;   b
;;   (c d)
;;   (() e))




;; (defun test-head-of-sexp (text)
;;   (with-temp-buffer
;;     (set-syntax-table lisp-mode-syntax-table)
;;     (insert text)
;;     (message "HEAD OF SEXP: %S" (head-of-sexp (point-min)))
;;     nil))
;;
;;
;; (defun test-bounds-of-sexp (text)
;;   (with-temp-buffer
;;     (set-syntax-table lisp-mode-syntax-table)
;;     (insert text)
;;     (goto-char (point-min))
;;     (skip-syntax-forward " <>")
;;     (multiple-value-bind (begin prefixless-begin end)
;;         (bounds-of-sexp (point))
;;       (message "PREFIX: %S" (buffer-substring-no-properties begin
;;                                                             prefixless-begin))
;;       (message "BODY: %S" (buffer-substring-no-properties prefixless-begin
;;                                                           end)))
;;     nil))
;;
;; (setf debug-on-error t)
;;
;;
;; (setf *fontification-alist*
;;       '((defun
;;             (face font-lock-keyword-face)
;;             (face font-lock-variable-name-face)
;;           (repeat (face font-lock-warning-face))
;;           (face font-lock-string-face)
;;           (rest &determine))
;;         (let
;;             (face sexpy-test-3)
;;           (repeat (list (face sexpy-test-1) (face sexpy-test-2) ;; &determine
;;                         ))
;;           (rest (face sexpy-test-3)))))
;;

(provide 'sexpy-highlight)


;; Local Variables:
;; lexical-binding: t
;; End:

;;; sexpy-highlight.el ends here
