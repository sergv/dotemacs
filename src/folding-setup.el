;; folding-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  1 April 2020
;; Description:

(require 'base-emacs-fixes)
(require 'comment-util)
(require 'el-patch)
(require 'hideshow)
(require 'vim-setup)

(provide 'folding-setup)

;;;; Hideshow

;;;###autoload
(el-patch-feature hideshow)


;;;###autoload
(eval-after-load "hideshow" '(require 'folding-setup))

(defadvice byte-compile-file (around
                              byte-compile-file-hideshow-off
                              activate
                              compile)
  (let ((hs-minor-mode-hook nil))
    ad-do-it))

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

(cond
  ((eval-when-compile
     (base-emacs-fixes--is-version 25 26))
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
  (t
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
                (format-mode-line mode-name)))))))

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
(defun* setup-outline-headers (&key
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

;;;; Combined

(defhydra-derive hydra-vim-normal-z-hideshow hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide block
_o_: show block
_C_: hide all blocks
_O_: show all blocks"
  ("o" hs-show-block)
  ("c" hs-hide-block)
  ("C" hs-hide-all)
  ("O" hs-show-all))

(defhydra-derive hydra-vim-normal-z-outline hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_f_: hide outline block
_u_: show outline block
_F_: hide all outline blocks leaving all headings visible
_U_: show all outline blocks"
  ("F" outline-hide-body)
  ("f" outline-hide-subtree)
  ("U" outline-show-all)
  ("u" outline-show-subtree))

(defhydra-derive hydra-vim-normal-z-hideshow-and-outline hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide block       _f_: hide outline block
_o_: show block       _u_: show outline block
_C_: hide all blocks  _F_: hide all outline blocks leaving all headings visible
_O_: show all blocks  _U_: show all outline blocks"
  ("o" hs-show-block)
  ("c" hs-hide-block)
  ("C" hs-hide-all)
  ("O" hs-show-all)

  ("F" outline-hide-body)
  ("f" outline-hide-subtree)
  ("U" outline-show-all)
  ("u" outline-show-subtree))

;;;###autoload
(defun setup-folding (enable-hideshow? outline-params)
  (if enable-hideshow?
      (progn
        (hs-minor-mode +1)
        (if outline-params
            (progn
              (apply #'setup-outline-headers outline-params)
              (def-keys-for-map vim:normal-mode-local-keymap
                ("z" hydra-vim-normal-z-hideshow-and-outline/body)))
          (def-keys-for-map vim:normal-mode-local-keymap
            ("z" hydra-vim-normal-z-hideshow/body))))
    (when outline-params
      (apply #'setup-outline-headers outline-params)
      (def-keys-for-map vim:normal-mode-local-keymap
        ("z" hydra-vim-normal-z-outline/body)))))

;; Local Variables:
;; End:

;; folding-setup.el ends here
