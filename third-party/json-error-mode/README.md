json-error - An Emacs minor mode that provides json syntax error checking
=========================================================================

This is an Emacs minor mode that provides syntax error checking and
highlighting for json buffers. It implements a json parser in elisp
to provide accurate checking.

Currently the parser is only used to check for syntax errors. While
it could also be used for normal (font-lock) syntax highlighting and
indenting, other javascript major modes handle that fairly well
(like js-mode). Since the main feature I wanted was error checking,
that was the first thing to be implemented.

To enable `js-mode` and `json-error-mode` for .json files add something
like this to your config:

    (require 'js-mode)
    (require 'json-error)

    (add-to-list 'auto-mode-alist '(".json$" . js-mode))
    ;; load json-error-mode when looking at json files
    (add-hook 'js-mode-hook
          (lambda ()
            (when (string-match "\\.json$" (buffer-file-name))
              (json-error-mode))))

## Internals

The json parser is a port of the go json parser to elisp. The
buffer updating and reparsing is mostly borrowed from js2-mode.

Steve Yegge gets a huge shout-out for showing us how awesome really
good emacs modes can be. He also gets a small fist-shake for not
just implementing this in js2-mode in 2008.
