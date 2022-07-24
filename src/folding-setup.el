;; folding-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  1 April 2020
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'set-up-platform)
  (require 'macro-util))

(require 'base-emacs-fixes)
(require 'comment-util)
(require 'el-patch)
(require 'hideshow)
(require 'yafolding)
(require 'vim-setup)

(provide 'folding-setup)

;;;; Yafolding

(defadvice yafolding-go-parent-element
    (after
     yafolding-go-parent-element/skip-whitespace
     activate
     compile)
  (skip-to-indentation))

(setf yafolding-show-fringe-marks nil)

;;;; Hideshow

;;;###autoload
(el-patch-feature hideshow)


;;;###autoload
(eval-after-load "hideshow" '(require 'folding-setup))

(defun byte-compile-file--file-hideshow-off (old-byte-compile-file &rest args)
  (let ((hs-minor-mode-hook nil))
    (apply old-byte-compile-file args)))

(advice-add 'byte-compile-file :around #'byte-compile-file--file-hideshow-off)

;; hideshow works badly with these
(add-hook 'ediff-prepare-buffer-hook 'turn-off-hideshow)
(add-hook 'vc-before-checkin-hook 'turn-off-hideshow)

;;;###autoload
(defun hs-show-sexps-in-region (begin end)
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (<= (point) end)
      (forward-sexp 1)
      (backward-sexp 1)
      (hs-show-block t)        ;; show and reposition
      (skip-syntax-forward ")" ;; close delimiters
                           )))
  ;; turn visual mode off
  (when (region-active-p)
    (deactivate-mark)
    (vim:visual-mode-exit)))

;;;###autoload
(defun hs-hide-sexps-in-region (begin end)
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (<= (point) end)
      (forward-sexp 1)
      (backward-sexp 1)
      (hs-hide-block t)        ;; hide and reposition at the end
      (skip-syntax-forward ")" ;; skip close delimiters
                           )))
  ;; turn visual mode off
  (when (region-active-p)
    (deactivate-mark)
    (vim:visual-mode-exit)))

;; todo: these two are quite similar to `hs-hide-sexps-in-region' and
;; `hs-show-sexps-in-region'
;;;###autoload
(defun hs-show-c-sexps-in-region (begin end)
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (<= (point) end)
      (skip-chars-forward "^{" end)
      (when (char=? (char-after) ?\{)
        (hs-show-block t)
        (skip-chars-forward "}"))))
  ;; turn visual mode off
  (when (region-active-p)
    (deactivate-mark)
    (vim:visual-mode-exit)))

;;;###autoload
(defun hs-hide-c-sexps-in-region (begin end)
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (progn
             (skip-chars-forward "^{" end)
             (and (char=? (char-after) ?\{)
                  (<= (point) end)))
      (hs-hide-block t)        ;; hide and reposition at the end
      (skip-chars-forward "}") ;; close delimiters
      ))
  ;; turn visual mode off
  (when (region-active-p)
    (deactivate-mark)
    (vim:visual-mode-exit)))

(el-patch-defun hs-show-all ()
  "Show everything then run `hs-show-hook'.  See `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (el-patch-remove
     (message "Showing all blocks ..."))
   (let ((hs-allow-nesting nil))
     (hs-discard-overlays (point-min) (point-max)))
   (el-patch-remove
     (message "Showing all blocks ... done"))
   (run-hooks 'hs-show-hook)))

(el-patch-defun hs-forward-sexp (match-data arg)
  "Adjust point based on MATCH-DATA and call `hs-forward-sexp-func' w/ ARG.
Original match data is restored upon return."
  (save-match-data
    (set-match-data match-data)
    (el-patch-wrap 2 0
      (when hs-block-start-mdata-select
        (goto-char (match-beginning hs-block-start-mdata-select))))
    (funcall hs-forward-sexp-func arg)))

(when-emacs-version (or (= it 25) (= it 26))
  (el-patch-defun hs-grok-mode-type ()
    "Set up hideshow variables for new buffers.
If `hs-special-modes-alist' has information associated with the
current buffer's major mode, use that.
Otherwise, guess start, end and `comment-start' regexps; `forward-sexp'
function; and adjust-block-beginning function."
    (el-patch-splice 2 2
      (if (and (boundp 'comment-start)
               (boundp 'comment-end)
               comment-start comment-end)
          (let* ((lookup (assoc major-mode hs-special-modes-alist))
                 (start-elem (or (nth 1 lookup) "\\s(")))
            (if (listp start-elem)
                ;; handle (START-REGEXP MDATA-SELECT)
                (setq hs-block-start-regexp (car start-elem)
                      hs-block-start-mdata-select (cadr start-elem))
              ;; backwards compatibility: handle simple START-REGEXP
              (setq hs-block-start-regexp start-elem
                    hs-block-start-mdata-select 0))
            (setq hs-block-end-regexp (or (nth 2 lookup) "\\s)")
                  hs-c-start-regexp (or (nth 3 lookup)
                                        (el-patch-wrap 2 1
                                          (if comment-start
                                              (let ((c-start-regexp
                                                     (regexp-quote comment-start)))
                                                (if (string-match " +$" c-start-regexp)
                                                    (substring c-start-regexp
                                                               0 (1- (match-end 0)))
                                                  c-start-regexp))
                                            (el-patch-wrap 3 0
                                              (if (memq major-mode '(select-mode text-mode flycheck-error-message-mode))
                                                  "\\(?:#\\|//\\)"
                                                (progn
                                                  (setq hs-minor-mode nil)
                                                  (error "%s Mode doesn't support Hideshow Minor Mode"
                                                         (format-mode-line mode-name))))))))
                  hs-forward-sexp-func (or (nth 4 lookup) 'forward-sexp)
                  hs-adjust-block-beginning (nth 5 lookup)))
        (setq hs-minor-mode nil)
        (error "%s Mode doesn't support Hideshow Minor Mode"
               (format-mode-line mode-name))))))

(when-emacs-version (<= 27 it)
  (el-patch-defun hs-grok-mode-type ()
    "Set up hideshow variables for new buffers.
If `hs-special-modes-alist' has information associated with the
current buffer's major mode, use that.
Otherwise, guess start, end and `comment-start' regexps; `forward-sexp'
function; and adjust-block-beginning function."
    (el-patch-splice 2 2
      (if (and (bound-and-true-p comment-start)
               (bound-and-true-p comment-end))
          (let* ((lookup (assoc major-mode hs-special-modes-alist))
                 (start-elem (or (nth 1 lookup) "\\s(")))
            (if (listp start-elem)
                ;; handle (START-REGEXP MDATA-SELECT)
                (setq hs-block-start-regexp (car start-elem)
                      hs-block-start-mdata-select (cadr start-elem))
              ;; backwards compatibility: handle simple START-REGEXP
              (setq hs-block-start-regexp start-elem
                    hs-block-start-mdata-select 0))
            (setq hs-block-end-regexp (or (nth 2 lookup) "\\s)")
                  hs-c-start-regexp (or (nth 3 lookup)
                                        (el-patch-wrap 2 1
                                          (if comment-start
                                              (let ((c-start-regexp
                                                     (regexp-quote comment-start)))
                                                (if (string-match " +$" c-start-regexp)
                                                    (substring c-start-regexp
                                                               0 (1- (match-end 0)))
                                                  c-start-regexp))
                                            (if (memq major-mode '(select-mode text-mode flycheck-error-message-mode))
                                                "\\(?:#\\|//\\)"
                                              (progn
                                                (setq hs-minor-mode nil)
                                                (error "%s Mode doesn't support Hideshow Minor Mode"
                                                       (format-mode-line mode-name)))))))
                  hs-forward-sexp-func (or (nth 4 lookup) #'forward-sexp)
                  hs-adjust-block-beginning (or (nth 5 lookup) #'identity)))
        (setq hs-minor-mode nil)
        (error "%s Mode doesn't support Hideshow Minor Mode"
               (format-mode-line mode-name))))))

;;;; Outline

(defvar-local outline-headers/min-header-length nil
  "Minimum number of `outline-headers/section-symbol''s allowed in header.")

(defvar-local outline-headers/section-start
  "^"
  "Beginning part of `outline-headers/header-re'.")

(defvar-local outline-headers/section-symbol
  nil
  "Main part of `outline-headers/header-re' that defines headers of different length.")

(defvar-local outline-headers/section-end
  "\\(?: \\|$\\)"
  "End part of `outline-headers/header-re'.")


(defvar-local outline-headers/header-re
  nil
  "Regular expression that defines headers")

;;;###autoload
(cl-defun setup-outline-headers (&key
                                 (header-start "^")
                                 (header-symbol nil)
                                 (header-end "\\(?: \\|$\\)")
                                 (length-min 3))
  (unless header-symbol
    (setf header-symbol
          (assq 'one-line
                (v-assq major-mode
                        +comment-util-comment-format-alist+)))
    (when (< 1 (length header-symbol))
      (error "setup-outline-headers: error: fetched header-symbol from comment-util but it's length is greater than 1: \"%s\" and no other header-symbol was provided"
             header-symbol)))
  (cl-assert (and (string? header-symbol)
                  (= 1 (length header-symbol)))
             nil
             "header-symbol must be string of length 1")
  (cl-assert (string? header-start)
             nil
             "header-start must be string")
  (cl-assert (string? header-end)
             nil
             "header-end must be string")
  (cl-assert (and (integer? length-min)
                  (>= length-min 1))
             nil
             "length-min must be integer >= 1")

  (setf outline-headers/section-start     header-start
        outline-headers/section-symbol    (regexp-quote header-symbol)
        outline-headers/section-end       header-end
        outline-headers/min-header-length length-min)

  (setf outline-headers/header-re
        (format "%s\\(?1:%s\\{%d,\\}\\)%s"
                outline-headers/section-start
                outline-headers/section-symbol
                outline-headers/min-header-length
                outline-headers/section-end))

  (setf buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))

  (setq-local outline-regexp outline-headers/header-re
              outline-heading-end-regexp
              (concat "\\(?:"
                      outline-headers/header-re
                      ".*?"
                      "\\(?:\\\\\n.*\\)?"
                      "\n"
                      "\\)+"))

  (outline-minor-mode +1))

;;;; Combined hideshow and yafolding

(vim-defcmd vim:folding-hide-indented-or-sexp-or-commented ()
  (if (and hs-minor-mode
           (folding-outline-on-sexp-or-commented?))
      (hs-hide-block)
    (yafolding-hide-element)))

(vim-defcmd vim:folding-show-indented-or-sexp-or-commented ()
  (if (and hs-minor-mode
           (folding-outline-on-sexp-or-commented?))
      (hs-show-block)
    (yafolding-show-element)))

(defun folding-outline-on-sexp-or-commented? ()
  (when-let (next (char-after))
    (let ((syn (char-syntax next)))
      (or (eq syn ?\()
          (eq syn ?\))
          (eq syn ?\<)))))

;;;; Hydras and setups

(defhydra-derive hydra-vim-normal-z-hideshow hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide block
_o_: show block
_C_: hide all blocks
_O_: show all blocks"
  ("o" vim:hs-show-block:interactive)
  ("c" vim:hs-hide-block:interactive)
  ("C" vim:hs-hide-all:interactive)
  ("O" vim:hs-show-all:interactive))

(defhydra-derive hydra-vim-normal-z-outline hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_f_: hide outline block
_u_: show outline block
_F_: hide all outline blocks leaving all headings visible
_U_: show all outline blocks"
  ("F" vim:outline-hide-body:interactive)
  ("f" vim:outline-hide-subtree:interactive)
  ("U" vim:outline-show-all:interactive)
  ("u" vim:outline-show-subtree:interactive))

(defhydra-derive hydra-vim-normal-z-hideshow-and-outline hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide block       _f_: hide outline block
_o_: show block       _u_: show outline block
_C_: hide all blocks  _F_: hide all outline blocks leaving all headings visible
_O_: show all blocks  _U_: show all outline blocks"
  ("o" vim:hs-show-block:interactive)
  ("c" vim:hs-hide-block:interactive)
  ("C" vim:hs-hide-all:interactive)
  ("O" vim:hs-show-all:interactive)

  ("F" vim:outline-hide-body:interactive)
  ("f" vim:outline-hide-subtree:interactive)
  ("U" vim:outline-show-all:interactive)
  ("u" vim:outline-show-subtree:interactive))

(defhydra-derive hydra-vim-normal-z-hideshow-yafolding hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide indented or sexp
_o_: show indented or sexp
_C_: hide all indented
_O_: show all indented
_T_: toggle all indented"
  ("c" vim:folding-hide-indented-or-sexp-or-commented:interactive)
  ("o" vim:folding-show-indented-or-sexp-or-commented:interactive)
  ("C" vim:yafolding-hide-all:interactive)
  ("O" vim:yafolding-show-all:interactive)
  ("T" vim:yafolding-toggle-all:interactive))

(defhydra-derive hydra-vim-visual-z-yafolding hydra-vim-visual-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: yafolding hide region"
  ("c" yafolding-hide-region))

(defhydra-derive hydra-vim-normal-z-hideshow-yafolding-and-outline hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide indented or sexp  _f_: hide outline block
_o_: show indented or sexp  _u_: show outline block
_C_: hide all indented      _F_: hide all outline blocks leaving all headings visible
_O_: show all indented      _U_: show all outline blocks
_T_: toggle all indented"
  ("c" vim:folding-hide-indented-or-sexp-or-commented:interactive)
  ("o" vim:folding-show-indented-or-sexp-or-commented:interactive)
  ("C" vim:yafolding-hide-all:interactive)
  ("O" vim:yafolding-show-all:interactive)
  ("T" vim:yafolding-toggle-all:interactive)

  ("F" vim:outline-hide-body:interactive)
  ("f" vim:outline-hide-subtree:interactive)
  ("U" vim:outline-show-all:interactive)
  ("u" vim:outline-show-subtree:interactive))

;;;###autoload
(defun setup-folding (enable-hideshow? outline-params)
  (if enable-hideshow?
      (progn
        (hs-minor-mode +1)
        (if outline-params
            (progn
              (apply #'setup-outline-headers outline-params)
              (def-keys-for-map vim-normal-mode-local-keymap
                ("z" hydra-vim-normal-z-hideshow-and-outline/body)))
          (def-keys-for-map vim-normal-mode-local-keymap
            ("z" hydra-vim-normal-z-hideshow/body))))
    (when outline-params
      (apply #'setup-outline-headers outline-params)
      (def-keys-for-map vim-normal-mode-local-keymap
        ("z" hydra-vim-normal-z-outline/body)))))

;;;###autoload
(defun setup-hideshow-yafolding (enable-hideshow outline-params)
  (setup-folding enable-hideshow nil)
  (yafolding-mode +1)
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))
  (if outline-params
      (def-keys-for-map vim-normal-mode-local-keymap
        ("z" hydra-vim-normal-z-hideshow-yafolding-and-outline/body))
    (def-keys-for-map vim-normal-mode-local-keymap
      ("z" hydra-vim-normal-z-hideshow-yafolding/body)))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("z" hydra-vim-visual-z-yafolding/body)))

;;;; Vimmized functions

(vimmize-function hs-hide-block :name vim:hs-hide-block :has-count nil)
(vimmize-function hs-show-block :name vim:hs-show-block :has-count nil)
(vimmize-function hs-hide-all   :name vim:hs-hide-all   :has-count nil)
(vimmize-function hs-show-all   :name vim:hs-show-all   :has-count nil)

(vimmize-function hs-hide-block :name vim:hs-hide-block :has-count nil)
(vimmize-function hs-show-block :name vim:hs-show-block :has-count nil)

(vimmize-function yafolding-toggle-all :name vim:yafolding-toggle-all :has-count nil)
(vimmize-function yafolding-hide-all   :name vim:yafolding-hide-all   :has-count nil)
(vimmize-function yafolding-show-all   :name vim:yafolding-show-all   :has-count nil)

(vimmize-function outline-hide-body    :name vim:outline-hide-body    :has-count nil)
(vimmize-function outline-hide-subtree :name vim:outline-hide-subtree :has-count nil)
(vimmize-function outline-show-all     :name vim:outline-show-all     :has-count nil)
(vimmize-function outline-show-subtree :name vim:outline-show-subtree :has-count nil)

;;;; End

(provide 'folding-setup)

;; Local Variables:
;; End:

;; folding-setup.el ends here
