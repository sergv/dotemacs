;; early-init.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 26 July 2026

(defvar function-key-map)
(defvar x-alternatives-map)

(setf x-alternatives-map (make-keymap))

;; Remove this ‘iso-lefttab’ key abomination once and for all.
(define-key x-alternatives-map (vector 'iso-lefttab) (vector 'tab))
(define-key x-alternatives-map (vector 'S-iso-lefttab) (vector 'S-tab))
(define-key x-alternatives-map (vector 'C-iso-lefttab) (vector 'C-tab))
(define-key x-alternatives-map (vector 'M-iso-lefttab) (vector 'M-tab))
(define-key x-alternatives-map (vector 's-iso-lefttab) (vector 's-tab))
(define-key x-alternatives-map (vector 'C-S-iso-lefttab) (vector 'C-S-tab))
(define-key x-alternatives-map (vector 'M-S-iso-lefttab) (vector 'M-S-tab))
(define-key x-alternatives-map (vector 's-S-iso-lefttab) (vector 's-S-tab))
(define-key x-alternatives-map (vector 'C-s-S-iso-lefttab) (vector 'C-s-S-tab))
(define-key x-alternatives-map (vector 'M-s-S-iso-lefttab) (vector 'M-s-S-tab))
(define-key x-alternatives-map (vector 'C-M-S-iso-lefttab) (vector 'C-M-S-tab))

;; This one want to translate into <backtab>.
(define-key function-key-map (vector 'S-tab) nil t)

;; Without this S-<iso-lefttab> may translate to ‘TAB’ if there’s
;; binding for ‘TAB’. Yes, that’s right, shift modifier could get
;; lost. Control modifier doesn’t get lost.
(define-key function-key-map (vector 'tab) nil t)

;; Local Variables:
;; End:

;; early-init.el ends here
