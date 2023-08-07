;; advices-util.el --- -*- lexical-binding: nil; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'macro-util)

(defmacro make-synchronizing-advice (func
                                     adv-name
                                     lock-var
                                     acquire-pred
                                     acquire-action
                                     release-pred
                                     release-action)
  "Define advice ADV-NAME around FUNC that will surround calls to FUNC
with locking over LOCK-VAR"
  (let ((acquired-var '#:is-acquired))
    `(defadvice ,func (around ,adv-name activate compile)
       (let ((,acquired-var (and (not ,lock-var)
                                 ,acquire-pred)))
         (when ,acquired-var
           (setq ,lock-var t)
           ,acquire-action)

         ad-do-it

         (when (and ,acquired-var
                    ,release-pred)
           (setq ,lock-var nil)
           ,release-action)))))

(defmacro make-light-synchronizing-advice (func adv-name lock-var acquire-pred release-action)
  `(make-synchronizing-advice
    ,func
    ,adv-name
    ,lock-var
    ,acquire-pred
    nil
    t
    ,release-action))

;;;

(defvar make-advice-expand-on-search--lock nil)

(defmacro make-advice-expand-on-search (func expand-func &optional modes)
  "Define expand-on-search advice that will call EXPAND-FUNC after FUNC
returns in major modes from list MODES. Do nothing if MODES is empty.
Also perform synchronization such that no recursive calls of EXPAND-FUNC
will be possible."
  (let ((adv-name (string->symbol (concat (symbol->string func) "-expand-on-search")))
        (mode-list (-flatten
                    (util/eval-if-symbol modes))))
    (when (not (null mode-list))
      `(make-light-synchronizing-advice
        ,func
        ,adv-name
        make-advice-expand-on-search--lock
        (memq major-mode ',mode-list)
        ,expand-func))))

(provide 'advices-util)

;; Local Variables:
;; End:

;; advices-util.el ends here
