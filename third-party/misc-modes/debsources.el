;;; debsources.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 25 November 2011
;; Keywords:
;; Requirements:
;; Status:

(defvar debsources-font-lock-keywords
  (list
   ;; comments
   '("#.*$" . font-lock-comment-face)

   ;; keywords
   (cons (rx bow
             (or "deb-src"
                 "deb"
                 "main"
                 "contrib"
                 "non-free"
                 "restricted"
                 "universe"
                 "multiverse")
             eow)
         font-lock-keyword-face)

   (cons (rx bow
             (or "http://"
                 "ftp://"
                 "rsh://"
                 "ssh://"
                 "debtorrent://"
                 "cdrom:"
                 "copy:"
                 "file:")
             (+ (not (any "'<>\" "))))
         font-lock-constant-face)

   (cons (rx (* (any alnum "_./"))
             (or "etch"
                 "lenny"
                 "squeeze"
                 (seq (? "old")
                      "stable")
                 "testing"
                 "unstable"
                 "sid"
                 "rc-buggy"
                 "experimental"
                 "dapper"
                 "hardy"
                 "jaunty"
                 "karmic"
                 "lucid"
                 "maverick")
             (* (any alnum "-_./")))
         font-lock-type-face)))

(define-derived-mode debsources-mode text-mode "Debsources"
                     "Major mode for editing debian /etc/apt/sources.list file."

                     (set (make-local-variable 'comment-start) "# ")
                     (set (make-local-variable 'comment-end) "")
                     (set (make-local-variable 'comment-column) 32)
                     (set (make-local-variable 'comment-start-skip) "#[ \t]*")

                     (turn-on-font-lock)
                     (set (make-local-variable 'font-lock-defaults)
                          '(debsources-font-lock-keywords t t))
                     )



;;; debsources.el ends here
