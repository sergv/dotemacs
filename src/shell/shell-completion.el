;; shell-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 December 2012
;; Description:

(require 'pcomplete)


(setf shell-completion--git-commands
      '("add" "branch" "checkout" "diff" "reset" "rm" "show" "status"))

(defun pcomplete/git ()
  "Completion for git."
  (pcomplete-here shell-completion--git-commands)
  (cond ((pcomplete-match (rx (or "add" "rm")) 1)
         (while t (pcomplete-here (pcomplete-entries))))))

(defun pcomplete/ls ()
  "Completion for ls."
  (let ((pcomplete-help "helpful message"))
    (while t (pcomplete-here
              (funcall (lambda (f)
                         (lambda (string pred action)
                           (remove-if (lambda (x)
                                        (member x '("." ".." "./" "../")))
                                      (funcall f string pred action))))
                       (pcomplete-entries))))))

(defun pcomplete/cat ()
  "Completion for cat"
  (while t
    (let ((pcomplete-help (identity "helpful message")))
      (pcomplete-here
       (funcall (lambda (f)
                  (let ((func f))
                    (lambda (string pred action)
                      (remove-if (lambda (x)
                                   (member x '("." ".." "./" "../")))
                                 (funcall func string pred action)))))
                (pcomplete-entries))))))

(defun pcomplete/cd ()
  "Completion for cd."
  (while t
    (let ((pcomplete-help (identity "helpful message")))
      (pcomplete-here
       (funcall (lambda (f)
                  (let ((func f))
                    (lambda (string pred action)
                      (remove-if (lambda (x)
                                   (member x '("." ".." "./" "../")))
                                 (funcall func string pred action)))))
                (pcomplete-entries))))))

;; (defun pcomplete/hoge ()
;;   "Completion rules for the `hoge' command."
;;   (let ((pcomplete-help "(hoge)Invoking hoge"))
;;     (pcomplete-opt "hv") ; hoge コマンドは -h, -v というオプションを持つ
;;     (pcomplete-here '("foo"))
;;     (cond ((pcomplete-test "foo")
;;            (setq pcomplete-help "(hoge)Call foo")
;;            (while (pcomplete-here (pcomplete-entries))))
;;           (t
;;            (while (pcomplete-here (pcomplete-entries)))))))


(provide 'shell-completion)

;; Local Variables:
;; End:

;; shell-completion.el ends here
