json-error - An Emacs minor mode that provides json syntax error checking
=========================================================================

This is an Emacs minor mode that provides syntax error checking and
highlighting for json buffers. It implements a json parser in elisp
to provide accurate checking.

The parser is only used to check for syntax errors. It is intended
to be used in conjunction with js-mode.

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

## Configuration

### Error Highlighting Style

You can customize how errors are highlighted by setting `json-error-highlighting-style`:

- `'context` (default) - Highlights only the line containing the error
- `'following` - Highlights everything from the error position to the end of the buffer

Example configuration:

    (setq json-error-highlighting-style 'following)

## Running Tests

To run the test suite, navigate to the test directory and run:

    cd test
    emacs -Q -batch -L .. -l ert -l json-error-test.el -l json-error-highlighting-test.el -f ert-run-tests-batch-and-exit

## Internals

json-error-mode uses Emacs' native `json-parse-buffer` function for
fast validation of JSON syntax. When an error is detected, it extracts the error
position from the native parser to provide accurate error highlighting. This mode
requires Emacs 27.1 or later.

### History

The original json parser was a port of the go json parser to elisp. The
buffer updating and reparsing is mostly borrowed from js2-mode.

Steve Yegge gets a huge shout-out for showing us how awesome really
good emacs modes can be. He also gets a small fist-shake for not
just implementing this in js2-mode in 2008.
