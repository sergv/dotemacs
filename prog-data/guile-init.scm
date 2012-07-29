
(use-modules (ice-9 history) (ice-9 format) (system repl common))
(disable-value-history!)

(add-to-load-path "/home/sergey/projects/scheme/srfi/")

(repl-default-option-set! 'value-history #f)
(repl-default-option-set! 'prompt "> ")
(repl-default-option-set! 'on-error 'backtrace)

;; don't enter debugger on error, just print terse report
;; (repl-default-option-set! 'debug 'report)

