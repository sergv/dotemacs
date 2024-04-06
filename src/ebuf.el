;; ebuf.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 20 February 2022
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'bisect)
(require 'common)
(require 'buffer-groups)
(require 'dash)
(require 'eproj)
(require 'keys-def)
(require 'macro-util)
(require 'nix-prettify-mode)
(require 'v)

;;;; Grouping

(defun ebuf--group-buffers (buffers get-key)
  (let ((grouped (make-hash-table :test #'equal)))
    (dolist (buf buffers)
      (let ((key (funcall get-key buf)))
        (puthash key
                 (cons buf
                       (gethash key grouped))
                 grouped)))
    grouped))

(cl-defstruct ebuf--buffer-classifier
  by-name      ;; list of (<regexp string> . <name>)
  by-mode      ;; hash table <mode> -> <name>
  by-predicate ;; list of (<function called within buffer> . <name>)
  names-order  ;; list of <name>
  )

(defun ebuf--classify-buffer (classifier buf)
  (cl-assert (bufferp buf))
  (or (gethash (buffer-local-value 'major-mode buf)
               (ebuf--buffer-classifier-by-mode classifier))
      (let ((name (buffer-name buf)))
        (cdr
         (--find (string-match-p (car it) name)
                 (ebuf--buffer-classifier-by-name classifier))))
      (with-current-buffer buf
        (cdr
         (--find (funcall (car it))
                 (ebuf--buffer-classifier-by-predicate classifier))))))

(defun ebuf--make-classifier (buffer-groups)
  (let ((by-name nil)
        (by-mode (make-hash-table :test #'eq))
        (by-predicate nil))
    (dolist (entry buffer-groups)
      (cl-assert (consp entry))
      (let ((name (car entry))
            (spec (cadr entry)))
        (when (eq (car spec) 'or)
          (setq spec (cdr spec)))
        (dolist (entry2 spec)
          (cl-assert (consp entry2))
          (pcase (car entry2)
            (`mode
             (puthash (cdr entry2) name by-mode))
            (`name
             (push (cons (cdr entry2) name) by-name))
            (`predicate
             (let ((expr (cdr entry2)))
               (push (cons (byte-compile `(lambda () ,expr)) name) by-predicate)))
            (_
             (error "Unrecognized entry: %s" entry2))))))
    (make-ebuf--buffer-classifier :by-name (nreverse by-name)
                                  :by-mode by-mode
                                  :by-predicate (nreverse by-predicate)
                                  :names-order (-map #'car buffer-groups))))

(defconst ebuf--buffer-classifier (eval-when-compile (ebuf--make-classifier +buffer-groups+)))

(defun ebuf--group-and-sort (buffers)
  (with-current-buffer (setq ebuf--main-buffer (get-buffer-create "*buffers*"))
    (let ((by-project-root
           (ebuf--group-buffers buffers
                                (lambda (buf)
                                  (when-let (proj (eproj-get-project-for-buf-lax buf))
                                    (eproj-project/root proj)))))
          (grouped
           (make-hash-table :test #'equal))
          (classifier ebuf--buffer-classifier))
      (cl-loop
       for k being the hash-keys of by-project-root using (hash-values buf-group)
       do
       (puthash k
                (ebuf--group-buffers buf-group
                                     (lambda (buf)
                                       (ebuf--classify-buffer classifier buf)))
                grouped))
      (-map (lambda (x)
              (let ((proj-root (car x))
                    (classified (cdr x)))
                (cons proj-root
                      (mapcan (lambda (y)
                                (awhen (gethash y classified)
                                  (list (cons y (funcall ebuf--buffer-sorting-method it)))))
                              (ebuf--buffer-classifier-names-order classifier)))))
            (sort (hash-table->alist grouped)
                  (lambda (a b)
                    (string< (car a) (car b))))))))

;;;; Rendering

(cl-defstruct (ebuf-section
               (:conc-name ebuf-section-))
  name
  depth       ;; levels to root
  buf         ;; buffer object or nil
  beg         ;; fixnum position
  caption-end ;; fixnum position
  end         ;; fixnum position
  children    ;; list of children sections
  parent      ;; another section or nil
  visible?    ;; t or nil
  overlay     ;; either nil or overlay object spanning caption-end and end positions.
  )

(defvar ebuf--main-buffer nil)

(defvar ebuf--toplevel-sections nil
  "Root sections of the ‘ebuf--main-buffer’.")

(defvar ebuf--all-sections nil
  "All sections of the ‘ebuf--main-buffer’ stored in a sorted vector.")

(defvar ebuf--marked-buffers (make-hash-table :test #'equal)
  "Hash table mapping buffers to t.")

(defvar ebuf--buffer-sorting-method #'ebuf--sort-by-recency
  "Function that takes a list of buffers and returns a list of buffers in some order.")

(defun ebuf--cleanup-on-buffer-kill ()
  (setf ebuf--main-buffer nil
        ebuf--toplevel-sections nil
        ebuf--all-sections nil)
  (clrhash ebuf--marked-buffers))

(defface ebuf-group-1-face
  `((t (:inherit default :bold t)))
  "Face for project names"
  :group 'tagged-buflist)

(defface ebuf-group-2-face
  `((t (:inherit default :bold t)))
  "Face to highlight marked buffers."
  :group 'tagged-buflist)

(defface ebuf-marked-buffer-face
  `((t (:foreground ,+solarized-orange+ :bold t)))
  "Face to highlight marked buffers."
  :group 'tagged-buflist)

(defface ebuf-regular-buffer-face
  `((t (:foreground ,+solarized-orange+)))
  "Face to highlight marked buffers."
  :group 'tagged-buflist)

(defface ebuf-read-only-buffer-face
  `((t (:foreground ,+solarized-cyan+)))
  "Face to highlight read-only buffers."
  :group 'tagged-buflist)

(defface ebuf-invisible-buffer-face
  `((t (:foreground ,+solarized-violet+)))
  "Face to highlight read-only buffers."
  :group 'tagged-buflist)

(defconst ebuf--depth-mult 2)

(defun ebuf--sort-by-recency (bufs)
  "Sort buffers in order from most recently used to the least recently used."
  (sort bufs
        (lambda (x y)
          (time-less-p (with-current-buffer y
                         (or buffer-display-time 0))
                       (with-current-buffer x
                         (or buffer-display-time 0))))))

(defun ebuf--sort-alphabetically (bufs)
  "Sort buffers by name in lexicographic order."
  (sort bufs
        (lambda (x y)
          (string< (buffer-local-value 'buffer-file-name x)
                   (buffer-local-value 'buffer-file-name y)))))

(defun ebuf--add-face (str face)
  (propertize str 'face face 'font-lock-face face))

(defun ebuf--render-buffers! (marked-bufs buffers)
  (cl-assert (hash-table-p marked-bufs))
  (let ((hidden-sections (make-hash-table :test #'equal)))
    (when ebuf--all-sections
      (dovector (section ebuf--all-sections)
        (unless (ebuf-section-visible? section)
          (puthash (ebuf--section-trail section) t hidden-sections))
        (awhen (ebuf-section-overlay section)
          (delete-overlay it))))
    (let ((grouped (ebuf--group-and-sort buffers))
          (toplevel-sections nil)
          (all-sections nil))
      (dolist (proj-entry grouped)
        (let* ((proj-root (car proj-entry))
               (proj-name (or proj-root
                              "no project"))
               (proj-depth 0)
               (proj-beg (point))
               (proj-caption-end nil)
               (proj-end nil)
               (proj-children nil)
               (proj-caption (ebuf--add-face (concat "[" proj-name "]")
                                             'ebuf-group-1-face)))
          (insert proj-caption)
          (setf proj-caption-end (point))
          (insert-char ?\n)
          (dolist (subgroup (cdr proj-entry))
            (let* ((group-name (car subgroup))
                   (group-depth (+ proj-depth 1))
                   (group-beg (point))
                   (group-caption-end nil)
                   (group-end nil)
                   (group-children nil)
                   (group-caption (ebuf--add-face (concat "[" group-name "]")
                                                  'ebuf-group-2-face))
                   (bufs (cdr subgroup)))
              (insert-char ?\s (* ebuf--depth-mult group-depth))
              (insert group-caption)
              (setf group-caption-end (point))
              (insert-char ?\n)
              (dolist (buf bufs)
                (let* ((buf-name (buffer-name buf))
                       ;; May consider using ‘buffer-file-truename’ to resolve symbolic
                       ;; links. But there seems to be no reason to do so yet.
                       (buf-file (buffer-local-value 'buffer-file-name buf))
                       (buf-ro? (buffer-local-value 'buffer-read-only buf))
                       (buf-caption (if buf-file
                                        (if proj-root
                                            (file-relative-name buf-file proj-root)
                                          (abbreviate-file-name buf-file))
                                      buf-name))
                       (buf-depth (+ group-depth 1))
                       (buf-beg (point))
                       (buf-caption-end nil)
                       (buf-end nil)
                       (buf-line
                        (let* ((is-marked? (gethash buf marked-bufs))
                               (mark (if is-marked?
                                         ">"
                                       " "))
                               (mod (if (and buf-file
                                             (buffer-modified-p buf))
                                        "*"
                                      " "))
                               (ro (if buf-ro?
                                       "%"
                                     " "))
                               (line (concat mark
                                             mod
                                             ro
                                             (make-string (max 0
                                                               (- (* buf-depth ebuf--depth-mult)
                                                                  (length mark)
                                                                  (length mod)
                                                                  (length ro)))
                                                          ?\s)
                                             (cond
                                               (buf-ro?
                                                (ebuf--add-face buf-caption
                                                                'ebuf-read-only-buffer-face))
                                               ((invisible-buffer? buf)
                                                (ebuf--add-face buf-caption
                                                                'ebuf-invisible-buffer-face))
                                               (t
                                                buf-caption)))))
                          (if is-marked?
                              (ebuf--add-face line 'ebuf-marked-buffer-face)
                            line))))
                  (insert buf-line)
                  (setf buf-end (point)
                        buf-caption-end (point))
                  (insert-char ?\n)
                  (let ((buf-section (make-ebuf-section
                                      :name buf-caption
                                      :depth buf-depth
                                      :buf buf
                                      :beg buf-beg
                                      :caption-end buf-caption-end
                                      :end buf-end
                                      :children nil
                                      :parent nil
                                      :visible? t
                                      :overlay nil)))
                    (push buf-section group-children)
                    (push buf-section all-sections))))
              (setf group-end (point)
                    group-children (nreverse group-children))
              (let ((group-section (make-ebuf-section
                                    :name group-caption
                                    :depth group-depth
                                    :buf nil
                                    :beg group-beg
                                    :caption-end group-caption-end
                                    :end group-end
                                    :children group-children
                                    :parent nil
                                    :visible? t
                                    :overlay nil)))
                (dolist (child group-children)
                  (setf (ebuf-section-parent child) group-section))
                (push group-section proj-children)
                (push group-section all-sections))))
          (setf proj-end (point)
                proj-children (nreverse proj-children))
          (let ((proj-section (make-ebuf-section
                               :name proj-caption
                               :depth proj-depth
                               :buf nil
                               :beg proj-beg
                               :end proj-end
                               :caption-end proj-caption-end
                               :children proj-children
                               :parent nil
                               :visible? t
                               :overlay nil)))
            (dolist (child proj-children)
              (setf (ebuf-section-parent child) proj-section))
            (push proj-section toplevel-sections)
            (push proj-section all-sections))))
      (setf ebuf--toplevel-sections (nreverse toplevel-sections)
            ebuf--all-sections (sort (list->vector all-sections)
                                     (lambda (x y)
                                       (< (ebuf-section-beg x)
                                          (ebuf-section-beg y)))))
      (dovector (section ebuf--all-sections)
        (when (gethash (ebuf--section-trail section) hidden-sections)
          (ebuf--make-section-invisible! section)))))
  (read-only-mode +1))

(defun ebuf--get-section-overlay (section)
  (cl-assert (ebuf-section-p section))
  (aif (ebuf-section-overlay section)
      it
    (setf (ebuf-section-overlay section)
          (make-overlay (ebuf-section-caption-end section)
                        (ebuf-section-end section)))))

(defun ebuf--for-section-buffers (section f)
  "Call F on all buffers under SECTION."
  (cl-assert (ebuf-section-p section))
  (cl-assert (functionp f))
  (aif (ebuf-section-buf section)
      (funcall f it)
    (dolist (child (ebuf-section-children section))
      (ebuf--for-section-buffers child f))))

(defun ebuf--section-idx-at-point ()
  (when ebuf--all-sections
    (let* ((pt (point))
           (len (length ebuf--all-sections))
           (idx (bisect-find ebuf--all-sections
                             0
                             len
                             (lambda (section)
                               (and (<= (ebuf-section-beg section)
                                        pt)
                                    (<= pt
                                        (ebuf-section-caption-end section))))
                             (lambda (section)
                               (< pt
                                  (ebuf-section-beg section))))))
      ;; Don’t let result go past the last section.
      (min idx (1- len)))))

(defun ebuf--section-at-point ()
  (awhen (ebuf--section-idx-at-point)
    (aref ebuf--all-sections it)))

;;;###autoload
(defun ebuf-start ()
  (interactive)
  (with-current-buffer (setq ebuf--main-buffer (get-buffer-create "*buffers*"))
    (ebuf-refresh))
  (switch-to-buffer ebuf--main-buffer))

;;;###autoload
(defun ebuf-start-other-window ()
  (interactive)
  (with-current-buffer (setq ebuf--main-buffer (get-buffer-create "*buffers*"))
    (ebuf-refresh))
  (switch-to-buffer-other-window ebuf--main-buffer))

(defun ebuf--section-trail (section)
  (let ((names nil)
        (p section))
    (while p
      (push (ebuf-section-name p) names)
      (setf p (ebuf-section-parent p)))
    names))

(defun ebuf--locate-with-trail (sections trail)
  (when trail
    (awhen (--find (equal (ebuf-section-name it)
                          (car trail))
                   sections)
      (if-let (trail-rest (cdr trail))
          (ebuf--locate-with-trail (ebuf-section-children it) trail-rest)
        it))))

(defun ebuf-refresh ()
  (interactive)
  (let* ((idx (ebuf--section-idx-at-point))
         (selected-trail (and idx
                              (ebuf--section-trail (aref ebuf--all-sections idx))))
         (win (selected-window))
         (start (and idx
                     (window-start win))))

    (save-current-column
      (with-inhibited-read-only
       (delete-region (point-min) (point-max))
       (unless (eq major-mode 'ebuf-mode)
         (ebuf-mode))
       (ebuf--render-buffers! ebuf--marked-buffers
                              (ebuf--interesting-buffers))
       (let ((previously-selected-section
              (or (and selected-trail
                       (ebuf--locate-with-trail ebuf--toplevel-sections
                                                selected-trail))
                  (and idx
                       (aref ebuf--all-sections
                             ;; After update some sections may have gone missing so index
                             ;; may be too large now - select the last section since it
                             ;; will be closest to the original index.
                             (let ((len (length ebuf--all-sections)))
                               (if (< idx len)
                                   idx
                                 (1- len))))))))

         (if previously-selected-section
             (progn
               (set-window-start win start)
               (goto-char (ebuf-section-beg previously-selected-section)))
           (goto-char (point-min))))))))

(defconst ebuf--invisible-buffer-re
  (rx bos
      (or (or "*buffers*"
              "*magit-process*"
              "*Ibuffer*"
              "*Kill Ring*"
              "*Pp Eval Output*"
              "*Async Shell Command*"
              "*Completions*"
              "*Help*"
              "*P4 update status*")
          (or " *code-conversion-work*"
              " *code-converting-work*"
              " *nix-repl completions redirect*"
              " *server*"
              " *Compiler Input*"
              " *Compiler Output*"
              " *DOC*"
              " *LV*"
              " *RNC Input*")
          (seq (or " *eldoc for "
                   " *Echo Area "
                   " *Minibuf"
                   " markdown-mode-fontification:")
               (* anything))
          (seq "#" (+ anything) "#"))
      eos))

(defun ebuf--interesting-buffers ()
  "Return a list of buffers that we care about and want to see rendered."
  (--filter (not (string-match-p ebuf--invisible-buffer-re
                                 (buffer-name it)))
            (buffer-list)))

(defun ebuf--section-part-of-hidden-parent? (section)
  (setf section (ebuf-section-parent section))
  (let ((is-hidden? nil))
    (while (and section
                (not is-hidden?))
      (setf is-hidden? (not (ebuf-section-visible? section)))
      (setf section (ebuf-section-parent section)))
    is-hidden?))

(defun ebuf-select-next-entry ()
  "Select next visible buffer entry."
  (interactive)
  (let ((idx (ebuf--section-idx-at-point))
        (len (length ebuf--all-sections)))
    (save-current-column
      (setf idx (mod (1+ idx) len))
      (while (ebuf--section-part-of-hidden-parent? (aref ebuf--all-sections idx))
        (setf idx (mod (1+ idx) len)))
      (goto-char (ebuf-section-beg (aref ebuf--all-sections idx))))))

(defun ebuf-select-prev-entry ()
  "Select previous visible buffer entry."
  (interactive)
  (let ((idx (ebuf--section-idx-at-point))
        (len (length ebuf--all-sections)))
    (save-current-column
      (setf idx (mod (1- idx) len))
      (while (ebuf--section-part-of-hidden-parent? (aref ebuf--all-sections idx))
        (setf idx (mod (1- idx) len)))
      (goto-char (ebuf-section-beg (aref ebuf--all-sections idx))))))


(defun ebuf-select-next-section ()
  "Select next visible non-atomic section."
  (interactive)
  (let ((idx (ebuf--section-idx-at-point))
        (len (length ebuf--all-sections)))
    (save-current-column
      (setf idx (mod (1+ idx) len))
      (let ((section (aref ebuf--all-sections idx)))
        (while (or (ebuf--section-part-of-hidden-parent? section)
                   (ebuf-section-buf section))
          (setf idx (mod (1+ idx) len)
                section (aref ebuf--all-sections idx))))
      (goto-char (ebuf-section-beg (aref ebuf--all-sections idx))))))

(defun ebuf-select-prev-section ()
  "Select previous visible non-atomic section."
  (interactive)
  (let ((idx (ebuf--section-idx-at-point))
        (len (length ebuf--all-sections)))
    (save-current-column
      (setf idx (mod (1- idx) len))
      (let ((section (aref ebuf--all-sections idx)))
        (while (or (ebuf--section-part-of-hidden-parent? section)
                   (ebuf-section-buf section))
          (setf idx (mod (1- idx) len)
                section (aref ebuf--all-sections idx))))
      (goto-char (ebuf-section-beg (aref ebuf--all-sections idx))))))


(defun ebuf--make-section-invisible! (section)
  (unless (ebuf-section-buf section)
    (let ((ov (ebuf--get-section-overlay section)))
      (overlay-put ov 'invisible t)
      (overlay-put ov 'before-string " ...\n")
      (setf (ebuf-section-visible? section) nil))))

(defun ebuf--make-section-visible! (section)
  (unless (ebuf-section-buf section)
    (let ((ov (ebuf--get-section-overlay section)))
      (overlay-put ov 'invisible nil)
      (overlay-put ov 'before-string nil)
      (setf (ebuf-section-visible? section) t))))

(defun ebuf--section-visibility-state (section)
  (if (ebuf-section-visible? section)
      (if (--all? (ebuf-section-visible? it) (ebuf-section-children section))
          'fully-visible
        'visible)
    'hidden))

(defmacro ebuf--fold-visibility-state (state on-fully-visible on-visible on-hidden)
  (declare (indent 1))
  `(pcase ,state
     (`fully-visible ,on-fully-visible)
     (`visible       ,on-visible)
     (`hidden        ,on-hidden)
     (_
      (error "Invalid visibility state: %s" state))))

(defun ebuf--section-cycle-visibility-state (state is-forward?)
  (if is-forward?
      (ebuf--fold-visibility-state state 'hidden 'fully-visible 'visible)
    (ebuf--fold-visibility-state state 'visible 'hidden 'fully-visible)))

(defun ebuf--make-section-fully-visible! (section)
  "Show SECTION and its children."
  (ebuf--make-section-visible! section)
  (dolist (child (ebuf-section-children section))
    (ebuf--make-section-visible! child)))

(defun ebuf--make-section-partially-visible! (section)
  "Show SECTION but hide its children."
  (ebuf--make-section-visible! section)
  (dolist (child (ebuf-section-children section))
    (ebuf--make-section-invisible! child)))

(defun ebuf--apply-visibility-state! (section state)
  (ebuf--fold-visibility-state
      state
    (ebuf--make-section-fully-visible! section)
    (ebuf--make-section-partially-visible! section)
    (ebuf--make-section-invisible! section)))

(defun ebuf--cycle-section! (section is-forward?)
  (unless (ebuf-section-buf section)
    (ebuf--apply-visibility-state! section
                                   (ebuf--section-cycle-visibility-state (ebuf--section-visibility-state section)
                                                                         is-forward?))))

(defun ebuf-section-show-level-1 ()
  (interactive)
  (let ((section (ebuf--section-at-point)))
    (unless (ebuf-section-buf section)
      (ebuf--make-section-invisible! section))))

(defun ebuf-section-show-level-2 ()
  (interactive)
  (ebuf--make-section-partially-visible! (ebuf--section-at-point)))

(defun ebuf-section-show-level-3 ()
  (interactive)
  (let ((section (ebuf--section-at-point)))
    (ebuf--make-section-visible! section)
    (dolist (child (ebuf-section-children section))
      (ebuf--make-section-partially-visible! child))))

(defun ebuf-section-show-level-4 ()
  (interactive)
  (let ((section (ebuf--section-at-point)))
    (ebuf--make-section-visible! section)
    (dolist (child (ebuf-section-children section))
      (ebuf--make-section-fully-visible! child))))

(defun ebuf--get-buffer-at-point-or-cycle ()
  (let ((section (ebuf--section-at-point)))
    (aif (ebuf-section-buf section)
        it
      (ebuf--cycle-section! section t))))

(defun ebuf-cycle-section-at-point ()
  (interactive)
  (ebuf--cycle-section! (ebuf--section-at-point) t))

(defvar ebuf-global-cycle-state nil)

(defun ebuf-cycle-all-toplevel-sections ()
  (interactive)
  (setf ebuf-global-cycle-state
        (if ebuf-global-cycle-state
            (ebuf--section-cycle-visibility-state ebuf-global-cycle-state t)
          'hidden))
  (cl-loop
   for section in ebuf--toplevel-sections
   do
   (ebuf--apply-visibility-state! section ebuf-global-cycle-state)))

(defun ebuf-switch-to-buffer-at-point-or-cycle ()
  (interactive)
  (awhen (ebuf--get-buffer-at-point-or-cycle)
    (switch-to-buffer it)))

(defun ebuf-switch-to-buffer-at-point-other-window-or-cycle ()
  (interactive)
  (awhen (ebuf--get-buffer-at-point-or-cycle)
    (switch-to-buffer-other-window it)))

(defun ebuf-mark-buffers-at-point ()
  (interactive)
  (let* ((start (point))
         (idx (ebuf--section-idx-at-point))
         (section (aref ebuf--all-sections idx)))
    (ebuf--for-section-buffers section
                               (lambda (buf)
                                 (puthash buf t ebuf--marked-buffers)))
    (goto-char (ebuf-section-end section))
    (skip-chars-forward "\n")
    (if (eobp)
        (goto-char start)
      ;; [Redisplay to preserve selected window line]
      ;; Redisplay to make sure new position is visible since we
      ;; preserve offset from window start (i.e. selected window line)
      ;; when refreshing.
      (redisplay t))
    (ebuf-refresh)))

(defun ebuf-select-parent ()
  (interactive)
  (let ((section (ebuf--section-at-point)))
    (awhen (ebuf-section-parent section)
      (goto-char (ebuf-section-beg it)))))

(defun ebuf-unmark-buffers-at-point ()
  (interactive)
  (let ((start (point))
        (section (ebuf--section-at-point)))
    (ebuf--for-section-buffers section
                               (lambda (buf)
                                 (remhash buf ebuf--marked-buffers)))
    (goto-char (ebuf-section-end section))
    (skip-chars-forward "\n")
    (if (eobp)
        (goto-char start)
      ;; See [Redisplay to preserve selected window line]
      (redisplay t))
    (ebuf-refresh)))

(defun ebuf-unmark-all ()
  (interactive)
  (clrhash ebuf--marked-buffers)
  (ebuf-refresh))

(defun ebuf-delete-marked-buffers ()
  (interactive)
  (let ((removed nil))
    (ebuf-with-marked-buffers (lambda (buf)
                                (kill-buffer buf)
                                (push buf removed))
                              (lambda ()
                                (error "No buffers marked")))
    (when removed
      (dolist (buf removed)
        (remhash buf ebuf--marked-buffers))
      (ebuf-refresh))))

(defun ebuf-with-marked-buffers (f if-none-selected)
  "Invoke function F on each marked buffer."
  (if (zerop (hash-table-count ebuf--marked-buffers))
      (funcall if-none-selected)
    (progn
      (maphash (lambda (buf _)
                 (cl-assert (bufferp buf))
                 (funcall f buf))
               ebuf--marked-buffers)
      (ebuf-refresh))))

(defun ebuf-eval-in-marked-buffers ()
  (interactive)
  (let ((expr-read? nil)
        (expr nil))
    (ebuf-with-marked-buffers
     (lambda (buf)
       (unless expr-read?
         (setf expr (read--expression "Eval in marked buffers: ")
               expr-read? t))
       (with-current-buffer buf
         (eval expr)))
     (lambda ()
       (error "No buffers marked")))))

(defun ebuf-sort-buffers-alphabetically ()
  (interactive)
  (setf ebuf--buffer-sorting-method #'ebuf--sort-alphabetically)
  (ebuf-refresh))

(defun ebuf-sort-buffers-by-recency ()
  (interactive)
  (setf ebuf--buffer-sorting-method #'ebuf--sort-by-recency)
  (ebuf-refresh))

(defhydra-ext hydra-ebuf-sorting (:exit t :foreign-keys nil :hint nil)
  "
_a_lphabetic
_r_ecency
"
  ("a" ebuf-sort-buffers-alphabetically)
  ("r" ebuf-sort-buffers-by-recency) ;; aka least recently used
  )

(defvar ebuf-mode-map
  (let ((keymap (make-sparse-keymap)))
    (def-keys-for-map keymap
      +vim-special-keys+
      +vim-search-keys+
      (("h" "<down>")    ebuf-select-next-entry)
      (("t" "<up>")      ebuf-select-prev-entry)
      ("<escape>"        quit-window)
      ("<return>"        ebuf-switch-to-buffer-at-point-or-cycle)
      (("SPC" "o")       ebuf-switch-to-buffer-at-point-other-window-or-cycle)
      (("H" "<f5>")      ebuf-refresh)

      ("m"               ebuf-mark-buffers-at-point)
      (","               ebuf-delete-marked-buffers)
      ("e"               ebuf-eval-in-marked-buffers)
      ("S"               hydra-ebuf-sorting/body)
      ("k"               ebuf-unmark-buffers-at-point)
      ("K"               ebuf-unmark-all)
      ("'"               ebuf-select-parent)
      ("C-h"             ebuf-select-next-section)
      ("C-t"             ebuf-select-prev-section)

      ("1"               ebuf-section-show-level-1)
      ("2"               ebuf-section-show-level-2)
      ("3"               ebuf-section-show-level-3)
      ("4"               ebuf-section-show-level-4)

      (("TAB" "<tab>")                       ebuf-cycle-section-at-point)
      (("S-TAB" "S-<tab>" "S-<iso-lefttab>") ebuf-cycle-all-toplevel-sections))
    keymap))

(define-derived-mode ebuf-mode fundamental-mode "Ebuf"
  "Major mode for queries in auxiliary buffer."
  ;; Fringe line tracking.
  (when (eval-when-compile (bound-and-true-p linum-mode))
    (linum-mode -1))
  (hl-line-mode +1)
  (nix-prettify-mode +1)
  ;; Disable font-lock-mode so that strings propertized with 'face property
  ;; will display correctly. Also there’s nothing for font-lock to do anyway.
  (when font-lock-mode
    (font-lock-mode -1))
  (add-hook 'kill-buffer-hook #'ebuf--cleanup-on-buffer-kill nil t))

(provide 'ebuf)

;; Local Variables:
;; End:

;; ebuf.el ends here
