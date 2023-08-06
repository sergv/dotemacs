;; elisp-slime-nav.el --- Make M-. and M-, work in elisp like they do in slime
;;
;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: navigation slime elisp emacs-lisp
;; URL: https://github.com/purcell/elisp-slime-nav
;; Version: DEV
;;
;;; Commentary
;;
;; This package provides Slime's convenient "M-." and "M-," navigation
;; in `emacs-lisp-mode'.
;;
;; Additionally, C-u M-. will prompt for the symbol to which to jump.
;;
;; Usage:
;;
;;   (require 'elisp-slime-nav)
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
;;
;; Known issues:
;;
;;   When navigating into Emacs' C source, "M-," will not be bound to
;;   the same command, but "M-*" will typically do the trick.
;;
;;; Code

(eval-when-compile
  (require 'cl))

(require 'eproj)

(defun elisp-slime-nav--all-navigable-symbol-names ()
  "Return a list of strings for the symbols to which navigation is possible."
  (cl-loop for x being the symbols
           if (or (fboundp x) (boundp x) (symbol-plist x) (facep x))
           collect (symbol-name x)))

;;;###autoload
(defun elisp-slime-nav-find-elisp-thing-at-point (sym-name)
  "Jump to the elisp thing at point, be it a function,variable, library or face.

With a prefix arg, prompt for the symbol to jump to.

Argument SYM-NAME thing to find."
  (interactive
   (list
    (let* ((sym-at-point (symbol-at-point))
           (at-point (and sym-at-point (symbol-name sym-at-point))))
      (if current-prefix-arg
          (ivy-completing-read "Symbol: "
                               (elisp-slime-nav--all-navigable-symbol-names)
                               nil t at-point)
        at-point))))
  (when sym-name
    (let ((sym (intern sym-name)))
      (message "search for %s" (pp-to-string sym))
      (push (eproj-symbnav-current-home-entry)
            eproj-symbnav/previous-homes)
      (cond
        ((fboundp sym) (find-function sym))
        ((boundp sym) (find-variable sym))
        ((or (featurep sym) (locate-library sym-name))
         (find-library sym-name))
        ((facep sym)
         (find-face-definition sym))
        (t
         (pop-tag-mark)
         (error "Don't know how to find '%s'" sym))))))


(provide 'elisp-slime-nav)
;; elisp-slime-nav.el ends here
