# Intro

This is a fairly successful experiment in checking how much can be accomplished
by using only lexical structure of a Haskell program, without parsing.

## What’s the problem with parsing?

Parsing is solved problem, you might object. Why can’t we parse Haskell reliably
and write a great tool based on Haskell AST?

One *major* problem: preprocessor. It must be either expanded or included into
AST and both options cause too much headache:

    1. Let’s just parse it then! That seems to be an order of
       magnitude harder that parsing haskell, because preprocessor
       pragma can “break” *any* AST node in between. We’d have to
       design an AST that can have conditional splits anywhere, and
       it’s not clear how to do that.

    2. Let’s expand it! Unfortunately, that causes a whole different
       set of issues, this time with build systems. We’ll need a
       *reliable* way of working with Haskell projects, but it
       existing tools, e.g. `ghc-mod`, have too high probability of
       failure in my experience and are prone to fail when they are
       most needed.

    3. Lets ignore it! This will work, but it will sacrifice some of the
       precision, which I’d rather avoid as much as possible.

## How

It’s messy. I’ll describe it some later time.

## No dependency on Cabal or Stack

It’s a blessing and a curse at the same time. But it guarantees that
tags server will work even for projects that have no `cabal` package
and are built with e.g. `make`. You may ask, where on Earth such
projects exist? Apart from *GHC* I don’t know other open source
Haskell projects that have no cabal package, but proprietary projects
may very well don’t have one!
