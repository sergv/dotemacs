;;; vim-compat.el - Layer for interfacing different Emacsen

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(provide 'vim-compat)

;; Check emacs and xemacs

(defconst vim:xemacs-p (string-match "XEmacs" emacs-version))
(defconst vim:emacs-p (not vim:xemacs-p))

(defconst vim:default-region-face (if vim:xemacs-p 'zmacs-region 'region))
(defconst vim:deactivate-region-hook (if vim:xemacs-p
					 'zmacs-deactivate-region-hook
				       deactivate-mark-hook))

(defmacro vim:emacsen (&rest impls)
  "Defines some body depending in emacs version."
  `(progn ,@(cdr (or (find-if #'(lambda (x) (eval (car x))) impls)
                     '(t (error "Not implemented for this Emacs version"))))))


(defun vim:set-keymap-default-binding (keymap command)
  "Sets the default binding of a keymap."
  (vim:emacsen
   (vim:emacs-p
    (define-key keymap t command))
   
   (vim:xemacs-p
    (set-keymap-default-binding keymap command))))

(defmacro vim:called-interactively-p ()
  "Returns t iff the containing function has been called interactively."
  (vim:emacsen
   (vim:emacs-p '(called-interactively-p))
   (vim:xemacs-p '(let (executing-macro) (interactive-p)))))

(vim:emacsen
 (vim:emacs-p (defalias 'vim:minibuffer-p 'minibufferp))
 (vim:xemacs-p (defalias 'vim:minibuffer-p 'active-minibuffer-window)))

(vim:emacsen
 (vim:emacs-p (defalias 'vim:this-command-keys 'this-command-keys-vector))
 (vim:xemacs-p (defalias 'vim:this-command-keys 'this-command-keys)))

(vim:emacsen
 (vim:emacs-p (defalias 'vim:deactivate-mark 'deactivate-mark))
 (vim:xemacs-p (defalias 'vim:deactivate-mark 'zmacs-deactivate-region)))

(vim:emacsen
 (vim:emacs-p (defalias 'vim:char-p 'integerp))
 (vim:xemacs-p (defalias 'vim:char-p 'characterp)))

(defun vim:test-completion (string collection &optional predicate)
  "Returns non-nil if `string' is a valid completion."
  (vim:emacsen
   (vim:emacs-p (test-completion string collection predicate))
   (vim:xemacs-p (eq (try-completion string collection predicate) t))))


(defun vim:looking-back (regexp &optional limit greedy)
  "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.
If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
  (vim:emacsen
   (vim:emacs-p (looking-back regexp limit greedy))
   
   (vim:xemacs-p 
    (let ((start (point))
          (pos
           (save-excursion
             (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                  (point)))))
      (if (and greedy pos)
          (save-restriction
            (narrow-to-region (point-min) start)
            (while (and (> pos (point-min))
                        (save-excursion
                          (goto-char pos)
                          (backward-char 1)
                          (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
              (setq pos (1- pos)))
            (save-excursion
              (goto-char pos)
              (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
      (not (null pos))))))
  

(defun vim:initialize-keymaps (enable)
  "Initialize keymaps when vim-mode is enabled."
  (vim:emacsen
   (vim:emacs-p
    (if enable
        (add-to-list 'emulation-mode-map-alists 'vim:emulation-mode-alist)
      (setq emulation-mode-map-alists
            (delq 'vim:emulation-mode-alist emulation-mode-map-alists))))
   
   (vim:xemacs-p
    (if enable
	(vim:normalize-minor-mode-map-alist)
      (setq minor-mode-map-alist (set-difference minor-mode-map-alist
						 vim:emulation-mode-alist
						 :key 'car))))))


(when vim:xemacs-p
  
  (defun vim:normalize-minor-mode-map-alist ()
    (make-local-variable 'minor-mode-map-alist)
    (setq minor-mode-map-alist (append vim:emulation-mode-alist
				       (set-difference minor-mode-map-alist
						       vim:emulation-mode-alist
						       :key 'car))))

  (defmacro define-globalized-minor-mode (global-mode mode turn-on &rest keys)
    "Make a global mode GLOBAL-MODE corresponding to buffer-local minor MODE.
TURN-ON is a function that will be called with no args in every buffer
  and that should try to turn MODE on if applicable for that buffer.
KEYS is a list of CL-style keyword arguments.  As the minor mode
  defined by this function is always global, any :global keyword is
  ignored.  Other keywords have the same meaning as in `define-minor-mode',
  which see.  In particular, :group specifies the custom group.
  The most useful keywords are those that are passed on to the
  `defcustom'.  It normally makes no sense to pass the :lighter
  or :keymap keywords to `define-globalized-minor-mode', since these
  are usually passed to the buffer-local version of the minor mode.

If MODE's set-up depends on the major mode in effect when it was
enabled, then disabling and reenabling MODE should make MODE work
correctly with the current major mode.  This is important to
prevent problems with derived modes, that is, major modes that
call another major mode in their body."

    (let* ((global-mode-name (symbol-name global-mode))
	   (pretty-name (easy-mmode-pretty-mode-name mode))
	   (pretty-global-name (easy-mmode-pretty-mode-name global-mode))
	   (group nil)
	   (extra-keywords nil)
	   (MODE-buffers (intern (concat global-mode-name "-buffers")))
	   (MODE-enable-in-buffers
	    (intern (concat global-mode-name "-enable-in-buffers")))
	   (MODE-check-buffers
	    (intern (concat global-mode-name "-check-buffers")))
	   (MODE-cmhh (intern (concat global-mode-name "-cmhh")))
	   (MODE-major-mode (intern (concat (symbol-name mode) "-major-mode")))
	   keyw)

      ;; Check keys.
      (while (keywordp (setq keyw (car keys)))
	(setq keys (cdr keys))
	(case keyw
	  (:group (setq group (nconc group (list :group (pop keys)))))
	  (:global (setq keys (cdr keys)))
	  (t (push keyw extra-keywords) (push (pop keys) extra-keywords))))

      (unless group
	;; We might as well provide a best-guess default group.
	(setq group
	      `(:group ',(intern (replace-regexp-in-string
				  "-mode\\'" "" (symbol-name mode))))))

      `(progn
         (defvar ,MODE-major-mode nil)
         (make-variable-buffer-local ',MODE-major-mode)
         ;; The actual global minor-mode
         (define-minor-mode ,global-mode
           ,(format "Toggle %s in every possible buffer.
With prefix ARG, turn %s on if and only if ARG is positive.
%s is enabled in all buffers where `%s' would do it.
See `%s' for more information on %s."
                    pretty-name pretty-global-name pretty-name turn-on
                    mode pretty-name)
           :global t ,@group ,@(nreverse extra-keywords)

           ;; Setup hook to handle future mode changes and new buffers.
           (if ,global-mode
               (progn
                 (add-hook 'after-change-major-mode-hook
                           ',MODE-enable-in-buffers)
                 (add-hook 'find-file-hook ',MODE-check-buffers)
                 (add-hook 'change-major-mode-hook ',MODE-cmhh))
             (remove-hook 'after-change-major-mode-hook ',MODE-enable-in-buffers)
             (remove-hook 'find-file-hook ',MODE-check-buffers)
             (remove-hook 'change-major-mode-hook ',MODE-cmhh))

           ;; Go through existing buffers.
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (if ,global-mode (,turn-on) (when ,mode (,mode -1))))))

         ;; Autoloading define-globalized-minor-mode autoloads everything
         ;; up-to-here.
         :autoload-end

         ;; List of buffers left to process.
         (defvar ,MODE-buffers nil)

         ;; The function that calls TURN-ON in each buffer.
         (defun ,MODE-enable-in-buffers ()
           (dolist (buf ,MODE-buffers)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (if ,mode
                     (unless (eq ,MODE-major-mode major-mode)
                       (,mode -1)
                       (,turn-on)
                       (setq ,MODE-major-mode major-mode))
                   (,turn-on)
                   (setq ,MODE-major-mode major-mode))))))
         (put ',MODE-enable-in-buffers 'definition-name ',global-mode)

         (defun ,MODE-check-buffers ()
           (,MODE-enable-in-buffers)
           (setq ,MODE-buffers nil)
           (remove-hook 'post-command-hook ',MODE-check-buffers))
         (put ',MODE-check-buffers 'definition-name ',global-mode)

         ;; The function that catches kill-all-local-variables.
         (defun ,MODE-cmhh ()
           (add-to-list ',MODE-buffers (current-buffer))
           (add-hook 'post-command-hook ',MODE-check-buffers))
         (put ',MODE-cmhh 'definition-name ',global-mode))))

  ;; This is a hack written by Hovav Shacham, author of the windmove package, so that 
  ;; windmove will work in xemacs
  ;;--- begin hack ---
 
  ;; simulate `window-edges' using `window-pixel-edges'; from
  ;; Nix , based on tapestry.el.
  (defun window-edges (&optional window)
    (let ((edges (window-pixel-edges window))
	  tmp)
      (setq tmp edges)
      (setcar tmp (/ (car tmp) (face-width 'default)))
      (setq tmp (cdr tmp))
      (setcar tmp (/ (car tmp) (face-height 'default)))
      (setq tmp (cdr tmp))
      (setcar tmp (/ (car tmp) (face-width 'default)))
      (setq tmp (cdr tmp))
      (setcar tmp (/ (car tmp) (face-height 'default)))
      edges))
  
  ;; simulate `window-at' with `walk-windows'
  (defun window-at (x y &optional frame)
    (let ((f (if (null frame)
		 (selected-frame)
	       frame)))
      (let ((guess-wind nil))
	(walk-windows (function (lambda (w)
				  (let ((w-edges (window-edges w)))
				    (when (and (eq f (window-frame w))
					       (<= (nth 0 w-edges) x)
					       (>= (nth 2 w-edges) x)
					       (<= (nth 1 w-edges) y)
					       (>= (nth 3 w-edges) y))
				      (setq guess-wind w)))))
		      t ; walk minibuffers
		      t) ; walk all frames
	guess-wind)))
  
  ;; redo `windmove-coordinates-of-position' without compute-motion
  (defun walk-screen-lines (lines goal)
    (cond
     ((< (window-point) goal) (1- lines))
     ((= (window-point) goal) lines)
     (t (vertical-motion 1)
	(walk-screen-lines (1+ lines) goal))))
  (defun windmove-coordinates-of-position (pos &optional window)
    (let* ((w (if (null window)
		  (selected-window)
		window))
	   (b (window-buffer w)))
      (save-selected-window
	(select-window w)
	(save-excursion
	  (let* ((y (progn (goto-char (window-start))
			   (walk-screen-lines 0 pos)))
		 (x (- (progn (goto-char pos)
			      (current-column))
		       (progn (goto-char (window-start))
			      (vertical-motion y)
			      (current-column)))))
	    (cons x y))))))            
  
  ;; for some reason, XEmacs is more conservative in reporting `frame-width'
  ;; and `frame-height'; we apparently need to get rid of the 1- in each.
  (defun windmove-frame-edges (window)
    (let ((frame (if window
		     (window-frame window)
		   (selected-frame))))
      (let ((x-min 0)
	    (y-min 0)
	    (x-max (frame-width frame))
	    (y-max (frame-height frame)))
	(list x-min y-min x-max y-max))))
  
  ;; --- end hack ---

  (defun window-tree (&optional frame)
    "Return the window tree for frame `frame'."
    (let ((root (frame-root-window frame))
	  (mini (minibuffer-window frame)))
      (labels
	  ((subwindows (win)
	     (cond
	      ((window-first-hchild win)
	       (let (w-list
		     (child (window-first-vchild win)))
		 (while child
		   (push child w-list)
		   (setq child (window-next-child child)))
		 (cons t
		       (cons (window-edges win)
			     (mapcar #'subwindows (reverse w-list))))))
	      ((window-first-vchild win)
	       (let (w-list
		     (child (window-first-vchild win)))
		 (while child
		   (push child w-list)
		   (setq child (window-next-child child)))
		 (cons nil
		       (cons (window-edges win)
			     (mapcar #'subwindows (reverse w-list))))))
	      (t win))))
	(list (subwindows root) mini))))
  )
      


;;; vim-compat.el ends here
