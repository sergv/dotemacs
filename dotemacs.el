

(defconst +emacs-config-path+ "/home/sergey/emacs"
  "Path to folder which is the root for emacs configuration.")

(defconst +emacs-standalone-path+ "/home/sergey/emacs/standalone"
  "Path to folder which is the root for emacs configuration.")

(defconst +emacs-documentation-path+ "/home/sergey/emacs/doc"
  "Path to folder which is the root for emacs configuration.")

(defconst +prog-data-path+ (concat +emacs-config-path+ "/prog-data")
  "Path to folder where programs can store their auxiliary files")

(defconst +bytecode-lib+ (concat +emacs-config-path+ "/lib")
  "Path to *.elc files")

(defconst +color-themes-path+ (concat +prog-data-path+ "/themes")
  "Path to color themes")

(defconst +slime-path+ (concat +emacs-standalone-path+ "/slime" ;;"-2012-01-15"
                               ))
(defconst +tmp-path+ (concat +prog-data-path+ "/tmp")
  "Path to temporary directory, contents of which may be removed on
system restars.")


(require 'cl)

;; (setf debug-on-error t)

(defun strip-trailing-slash (path)
  (if (char= ?\/ (aref path (1- (length path))))
    (subseq path 0 -1)
    path))

(defun get-directory-contents (dir)
  (remove-if #'(lambda (x) (member (file-name-nondirectory x) '("." "..")))
             (directory-files dir t)))

(defun* find-rec-special (path
                          &key
                          (filep #'(lambda (p) t))
                          (dirp  #'(lambda (p) nil))
                          (do-not-visitp
                              #'(lambda (p)
                                  (member* (file-name-nondirectory (strip-trailing-slash p))
                                           '("SCCS" "RCS" "CVS" "MCVS" ".svn"
                                             ".git" ".hg" ".bzr" "_MTN" "_darcs"
                                             "{arch}")
                                           :test #'string=))))
  "Collect files and/or directories under PATH recursively.

Collect files and directories which satisfy FILEP and
DIRP respectively in directories which don't satisfy DO-NOT-VISITP.
By default, version-control specific directories are omitted, e.g. .git etc."
  (when (stringp filep)
    (setf filep
          (lexical-let ((regular-expression filep))
                       #'(lambda (p) (string-match-p regular-expression p)))))
  (when (stringp dirp)
    (setf dirp
          (lexical-let ((regular-expression dirp))
                       #'(lambda (p) (string-match-p regular-expression p)))))
  (when (stringp do-not-visitp)
    (setf do-not-visitp
          (lexical-let ((regular-expression do-no-visitp))
                       #'(lambda (p) (string-match-p regular-expression p)))))

  (labels ((collect-rec (path accum)
             (cond
               ((and (file-directory-p path)
                     (not (funcall do-not-visitp path)))
                (reduce #'(lambda (acc p)
                            (collect-rec p acc))
                        (get-directory-contents path)
                        :initial-value (if (funcall dirp path)
                                         (cons path accum)
                                         accum)))
               ((funcall filep path)
                (cons path accum))
               (t
                accum))))
    (collect-rec path nil)))


(setf load-path
      (remove-duplicates
       (append (find-rec-special (concat +emacs-config-path+ "/src")
                                 :filep #'(lambda (x) nil)
                                 :dirp #'(lambda (x) t))
               (find-rec-special (concat +emacs-config-path+ "/third-party")
                                 :filep #'(lambda (x) nil)
                                 :dirp #'(lambda (x) t))
               load-path)
       :test #'string=))

;; (add-to-list 'load-path +bytecode-lib+)
(load-library "reasonable-elisp")
(load-library "more-scheme")
(load-library "custom")

(add-to-list 'load-path +color-themes-path+)

;; ******************************************************************

;; parts of custom
(load-library "win-buf-utils")
(load-library "cycle-on-lines")

(load-library "common")
(load-library "search")
(load-library "persistent-store")
(persistent-store-init)

(load-library "set-up-platform")
(load-library "set-up-environment-variables")
(load-library "backups")
(load-library "emacs-general-conf")


;; ******************************************************************

(setq compilation-auto-jump-to-first-error nil
      case-fold-search nil ;;turn off ignorance of case during search
      whitespace-line-column 81
      whitespace-style '(face lines-tail tabs))

(defconst *do-not-track-long-lines-modes*
  '(lisp-interaction-mode
    inferior-scheme-mode
    prolog-inferior-mode
    comint-mode
    org-mode
    inferior-octave-mode
    python-repl-mode
    inferior-haskell-mode

    makefile-automake-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-mode
    makefile-makepp-mode))


(defun* init-common (&key (use-yasnippet t)
                          (use-nxhtml-menu nil)
                          (use-comment t)
                          (use-whitespace t)
                          (use-render-formula nil))
  (linum-mode 1)
  (when use-comment
    (comment-util-mode 1))

  ;; (autopair-mode)
  ;; (set-buffer-file-coding-system 'utf-8-unix)

  (when use-yasnippet
    (yas/minor-mode-on))

  ;; it's usually already enabled by nxhtml autoloads
  ;; so action should be taken to turn it off
  (nxhtml-menu-mode (if use-nxhtml-menu 1 -1))

  (setq undo-tree-visualizer-timestamps    t
        undo-tree-visualizer-parent-buffer t)

  (when use-whitespace
    (whitespace-mode
     (if (member major-mode
                 *do-not-track-long-lines-modes*)
       -1
       +1)))

  (when use-render-formula
    (render-formula-mode 1)))

(defun* init-repl (&key (show-directory nil))
  (use-repl-modeline)
  (setf *vim:do-not-adjust-point* t)
  (emacs-forget-buffer-process))


(load-library "all-lisp-setup")
(load-library "org-mode-setup")

(load-library "c-c++-setup")
(load-library "haskell-autoload")
(load-library "awk-setup")
(load-library "shell-autoloads")
(load-library "snippet-autoloads")
(load-library "eshell-setup")
(load-library "gnuplot-setup")
(load-library "html-setup")
(load-library "latex-autoloads")
(load-library "debsources-setup")
(load-library "markdown-setup")
(load-library "prolog-setup")
(load-library "octave-setup")
(load-library "sql-setup")
(load-library "python-setup")
(load-library "cython-setup")
(load-library "graphviz-setup")
(load-library "csv-setup")
(load-library "rst-setup")
(load-library "texinfo-setup")
(load-library "maxima-setup")
(load-library "d-mode-setup")
(load-library "yaml-mode-setup")
(load-library "doc-view-setup")

(load-library "icicles-setup")
(load-library "compilation-setup")
(load-library "completion-setup")
(load-library "auto-insert-setup")
(load-library "emms-setup")
(load-library "ibuffer-setup")
(load-library "hl-paren")
(load-library "spell-setup")
(load-library "abbrev+")
(load-library "grep+")
(load-library "comment-util")
(load-library "comint-setup")
(load-library "dired-setup")
(load-library "remember-win-config")
(load-library "yasnippet-setup")
;; (load-library "cedet-setup")
(load-library "git-setup")
(load-library "tabbar-buffer-groups")
(load-library "visit-files")
(load-library "hideshow-setup")
(load-library "render-formula")
(load-library "image-mode-setup")
(load-library "tabbar-setup")



(autoload 'sunrise
          "sunrise-commander"
          "MC-like two-pane commander."
          t)


;; load keys after everything to ensure that all is bound as expected
(load-library "keys")
(load-library "vim-init")

(require 'fortunes)
(random t)
(setq initial-scratch-message nil)
(fortune-init-scratch-buf)


;; test faces for readability
;; (progn
;;   (load-file "~/emacs/color-lab.elc")
;;   (load-file "~/emacs/shr-color.elc")
;;   (mapc #'(lambda (entry)
;;             (let* ((colors (list
;;                             (cadr entry)
;;                             (caddr entry)))
;;                    (result (shr-color-visible (car colors) (cadr colors) t)))
;;               (when (not (equal colors result))
;;                 (message "face %S transform from %S to %S" (car entry) colors result))))
;;         '((face-name "bg" "fg")))
;;   nil)

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


;; do it manually if need arises
;; (desktop-read)


(ex-commands-re-cache-update)


(require 'package)
(let ((package-load-list '((melpa t))))
  (package-initialize))


;; Local Variables:
;; lexical-binding: t
;; End:

