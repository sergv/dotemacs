[![MELPA](https://melpa.org/packages/session-async-badge.svg)](https://melpa.org/#/session-async)

# session-async.el

Package for doing asynchronous processing in Emacs, just like [`async.el`](https://github.com/jwiegley/emacs-async)

Unlike `async.el`, the API for this package provides a separate Emacs session that can stay alive across async calls.

# Why another async processing package?

`async.el` is completely functional and has a lot of track. I never stop recommending it.

I made this package because I use Tramp everyday and using `async.el` is counterproductive because the connection startup overhead takes so long.

Also, check [this discussion about `ob-async` supporting `:session`](https://github.com/astahlman/ob-async/issues/1)

I'd also add that this package is meant for package developers, rather than end user ("devs using Emacs") due to the complexity of maintaining the state of the separate Emacs session.

# Install

I recommend installing [using `straight.el`](https://ubolonton.github.io/emacs-tree-sitter/installation/#installing-with-straight-dot-el).

Do remember that this package is targeted to package developers and not end users.

## Using straight.el

Make sure you have `straight.el` up and running. Then add the following to your init.el file:

```elisp
(straight-use-package 'session-async)
```

That's basically it.

## Using MELPA

Follow the [instructions to install MELPA](https://melpa.org/#/getting-started). The run `M-x package-install` → `session-async`.

## Manual
Download this code to somewhere wher Emacs can see it. Then load this `session-async.el` file

```elisp
(add-to-list 'load-path "~/some/path/session-async.el") ;; add repo directory, not the file
(require 'session-async)
```

# Async usage

This the current interface that tries to mimic that of `async.el`

## session-async-start

    session-async-start START-FUNC FINISH-FUNC SESSION

Execute START-FUNC (often a *quoted* `lambda` sexp) in a separate Emacs session. When done executing, the return value is passed to FINISH-FUNC. Example

```elisp
(session-async-start
 `(lambda ()
    ;; this is executed in a separate process
    (sleep-for 3)
    (cons 222 (emacs-pid)))
 (pcase-lambda (`(,twotwotwo . ,separate-process-pid))
   (message "Async process done, result shoul be 222: %d, and %d ≠ %d"
            twotwotwo
            separate-process-pid
            (emacs-pid))))
```

Differences with `async`:

- If FINISH-FUNC is not provided, it will be set to `'ignore` and once the code is done executed, you won't have any kind of memory leak (buffer, session, process).
- If SESSION is not provided, a one-shot session will be created and destroyed after FINISH-FUNC is called.
- An Emacs session object is returned, not a future value.
- For handling future values, use 

## session-async-future

    session-async-future START-FUNC SESSION
    
Execute START-FUNC (often a *quoted* `lambda` sexp) in a separate Emacs session. Calling this function will return an iterator whose value can be fetched using `iter-next`.

```elisp
(let ((future (session-async-future
               `(lambda ()
                  ;; this is executed in separate process
                  (sleep-for 3)
                  222))))

  (message "I'm going to do some work here") ;; ....

  (message "Waiting on async process, result shoul be 222: %d"
           (iter-next future)))

```

## session-async-new

    session-async-new SESSION-NAME

Create a new session that can be used with several `session-async-future` or `session-async-start` calls.

This wraps an Emacs process running in the background. Be mindful that it will preserve state between `session-async-start` calls. 

Don't forget to shut it down with `session-async-shutdown` or you will have a lingering process.

```elisp
(let ((session (session-async-new)))
  (iter-next
   (session-async-future
    `(lambda ()
       (setenv "SOMETHING" "SOME VALUE"))
    session))
  (message "Current process env value: %s (nil), session: %s (SOME VALUE)"
           (getenv "SOMETHING")
           (iter-next
            (session-async-future
             `(lambda ()
                (getenv "SOMETHING"))
             session)))
  (session-async-shutdown session))

```
## session-async-shutdown

    session-async-shutdown SESSION
    
Shutdown SESSION and kill its corresponding Emacs process.

# Improving this package

Merge requests are welcome. If you're doing one, please provide unit tests for it. If you feel kinda lost, you can ask for help.

You can also open bugs.

## Code of Conduct

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

# Thanks

Thanks [`undo-fu`](https://gitlab.com/ideasman42/emacs-undo-fu) for the code of `session-async--with-advice` (which I copied from `undo-fu--with-advice`).
