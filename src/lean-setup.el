;; lean-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 21 June 2019
;; Description:

(eval-when-compile
  (require 'macro-util))

(declare-function lean-find-definition "lean-info")
(declare-function lean-find-definition-cont "lean-info")
(declare-function lean-server-send-synchronous-command "lean-server")

(require 'dash)
(require 'select-mode)

(defun lean-setup--get-definitions-candidates (pat)
  (let* ((response (lean-server-send-synchronous-command 'search (list :query pat)))
         (results (plist-get response :results))
         (results (-filter (lambda (c) (plist-get c :source)) results))
         (candidates (-map 'helm-lean-definitions-format-candidate results)))
    candidates))

(defvar lean-find-symbol-patterns-history nil)

(cl-defun lean-find-definition-other-window (&key file line column)
  (when (fboundp 'xref-push-marker-stack) (xref-push-marker-stack))
  (when file
    (find-file-other-window file))
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char column))

(defun lean-find-symbol (&optional use-pat)
  (interactive "P")
  (if use-pat
      (let* ((pat (read-string "Glob pattern: " nil 'lean-find-symbol-patterns-history))
             (candidates (lean-setup--get-definitions-candidates pat))
             (kmap (make-sparse-keymap)))
        (def-keys-for-map kmap
          ("SPC" (lambda () (interactive)
                   (let ((candidate (elt candidates (select-mode-get-selected-index))))
                     (apply #'lean-find-definition-other-window
                            (plist-get candidate :source))))))
        (select-mode-start-selection
         candidates
         :buffer-name "Lean definitions"
         :after-init (lambda ()
                       (select-mode-setup)
                       (select-mode-extend-keymap-with kmap))
         :on-selection
         (lambda (_ candidate _selection-type)
           (select-mode-exit)
           (apply #'lean-find-definition-cont
                  (plist-get candidate :source)))
         :item-show-function
         (lambda (x) (concat (car x) "\n"))
         :preamble "Choose definition\n\n"))
    (lean-find-definition)))

;;;###autoload
(defun lean-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci t
               :use-whitespace 'tabs-only)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("<f9>" lean-std-exe)
    ("C-."  lean-find-symbol)
    ("C-,"  xref-pop-marker-stack)))

;;;###autoload
(add-hook 'lean-mode-hook #'lean-setup)

(provide 'lean-setup)

;; Local Variables:
;; End:

;; lean-setup.el ends here
