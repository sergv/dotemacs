# taken from https://github.com/abo-abo/tiny/blob/master/Makefile
# see http://sachachua.com/blog/2015/02/continuous-integration-code-coverage-emacs-packages-travis-coveralls/

all: bytec lint test

# Use LC_ALL=C to avoid locale dependencies in the dates!
test:
	LC_ALL=C ./semacs \
	    --no-package --no-refresh-packages --no-org-repo -- \
	    -l tests/setup.el \
	    -l tests/session-async-tests.el \
	    -f buttercup-run

lint:
	LC_ALL=C ./semacs \
	    --install project \
	    --install package-lint \
	    -- \
	    -f package-lint-batch-and-exit \
	    session-async.el

bytec:
	LC_ALL=C ./semacs \
	    --no-package --no-refresh-packages --no-org-repo -- \
	    --eval '(setq byte-compile-error-on-warn t)' \
	    -f batch-byte-compile *.el


.PHONY:	all lint test
