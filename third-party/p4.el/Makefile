emacs ?= emacs

p4.elc: p4.el
	"$(emacs)" -Q -batch -f batch-byte-compile p4.el

clean:
	rm -f p4.elc
