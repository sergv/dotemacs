TOP := $(dir $(lastword $(MAKEFILE_LIST)))

## User options ######################################################
#
# You can override these settings in "config.mk" or on the command
# line.
#
# You might also want to set LOAD_PATH.  If you do, then it must
# contain "-L .".
#
# If you don't do so, then the default is set in the "Load-Path"
# section below.  The default assumes that all dependencies are
# installed either at "../<DEPENDENCY>", or when using package.el
# at "ELPA_DIR/<DEPENDENCY>-<HIGHEST-VERSION>".

PREFIX   ?= /usr/local
sharedir ?= $(PREFIX)/share
lispdir  ?= $(sharedir)/emacs/site-lisp/magit
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/magit

CP       ?= install -p -m 644
MKDIR    ?= install -p -m 755 -d
RMDIR    ?= rm -rf
TAR      ?= tar
SED      ?= sed

EMACS      ?= emacs
EMACS_ARGS ?= --eval "(progn \
  (put 'if-let 'byte-obsolete-info nil) \
  (put 'when-let 'byte-obsolete-info nil))"
BATCH       = $(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH)

LISP_EXTRA_TARGETS ?= check-declare

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

GITSTATS      ?= gitstats
GITSTATS_DIR  ?= $(TOP)docs/stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css \
                 -c max_authors=180 -c graph_max_authors=7

## Files #############################################################

PKG       = magit
PKGSTEXI  = magit magit-section
PACKAGES  = magit magit-section git-commit

ORGPAGES  = $(addsuffix .org,$(PKGSTEXI))
TEXIPAGES = $(addsuffix .texi,$(PKGSTEXI))
INFOPAGES = $(addsuffix .info,$(PKGSTEXI))
HTMLFILES = $(addsuffix .html,$(PKGSTEXI))
HTMLTOPS  = $(addsuffix /index.html,$(PKGSTEXI))
HTMLDIRS  = $(PKGSTEXI)
PDFFILES  = $(addsuffix .pdf,$(PKGSTEXI))
EPUBFILES = $(addsuffix .epub,$(PKGSTEXI))

# When making changes here, also update "<nongnu.git>/elpa-packages".

ELS  = git-commit.el
ELS += magit-section.el
ELS += magit-base.el
ELS += magit-git.el
ELS += magit-mode.el
ELS += magit-margin.el
ELS += magit-process.el
ELS += magit-transient.el
ELS += magit-autorevert.el
ELS += magit-core.el
ELS += magit-diff.el
ELS += magit-log.el
ELS += magit-wip.el
ELS += magit-reflog.el
ELS += magit-apply.el
ELS += magit-repos.el
ELS += magit.el
ELS += magit-status.el
ELS += magit-refs.el
ELS += magit-files.el
ELS += magit-reset.el
ELS += magit-branch.el
ELS += magit-merge.el
ELS += magit-tag.el
ELS += magit-worktree.el
ELS += magit-notes.el
ELS += magit-sequence.el
ELS += magit-commit.el
ELS += magit-remote.el
ELS += magit-clone.el
ELS += magit-fetch.el
ELS += magit-pull.el
ELS += magit-push.el
ELS += magit-patch.el
ELS += magit-bisect.el
ELS += magit-stash.el
ELS += magit-blame.el
ELS += magit-sparse-checkout.el
ELS += magit-submodule.el
ELS += magit-subtree.el
ELS += magit-ediff.el
ELS += magit-gitignore.el
ELS += magit-bundle.el
ELS += magit-extras.el
ELS += magit-dired.el
ELS += git-rebase.el
ELS += magit-bookmark.el
ELCS = $(ELS:.el=.elc)
ELMS = magit.el $(filter-out $(addsuffix .el,$(PACKAGES)),$(ELS))
ELGS = magit-autoloads.el magit-version.el

## Versions ##########################################################

VERSION ?= $(shell \
  test -e $(TOP).git && \
  git describe --tags --abbrev=0 --always | cut -c2-)
REVDESC := $(shell test -e $(TOP).git && git describe --tags)

EMACS_VERSION = 28.1

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

## Load-Path #########################################################

# Remember to also update magit-emacs-Q-command!

ifndef LOAD_PATH

USER_EMACS_DIR = $(HOME)/.emacs.d
ifeq "$(wildcard $(USER_EMACS_DIR))" ""
  XDG_CONFIG_DIR = $(or $(XDG_CONFIG_HOME),$(HOME)/.config)
  ifneq "$(wildcard $(XDG_CONFIG_DIR)/emacs)" ""
    USER_EMACS_DIR = $(XDG_CONFIG_DIR)/emacs
  endif
endif

ELPA_DIR ?= $(USER_EMACS_DIR)/elpa

COMPAT_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/compat-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(COMPAT_DIR)" ""
  COMPAT_DIR = $(TOP)../compat
endif

COND_LET_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/cond-let-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(COND_LET_DIR)" ""
  COND_LET_DIR = $(TOP)../cond-let
endif

LLAMA_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/llama-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(LLAMA_DIR)" ""
  LLAMA_DIR = $(TOP)../llama
endif

SEQ_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/seq-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(SEQ_DIR)" ""
  SEQ_DIR = $(TOP)../seq
endif

TRANSIENT_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/transient-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(TRANSIENT_DIR)" ""
  TRANSIENT_DIR = $(TOP)../transient/lisp
endif

WITH_EDITOR_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/with-editor-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(WITH_EDITOR_DIR)" ""
  WITH_EDITOR_DIR = $(TOP)../with-editor/lisp
endif

MAGIT_SECTION_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/magit-section-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)

SYSTYPE := $(shell $(EMACS) -Q --batch --eval "(princ system-type)")
ifeq ($(SYSTYPE), windows-nt)
  CYGPATH := $(shell cygpath --version 2>/dev/null)
endif

LOAD_PATH = -L $(TOP)lisp

# When making changes here, then don't forget to adjust DEPS below,
# ".github/ISSUE_TEMPLATE/bug_report.md", `magit-emacs-Q-command'
# and the "Installing from the Git Repository" info node accordingly.
# Also run "rgrep \b<another-package\b", to find other places where
# a newly added dependency might have to be mentioned as well.  Also
# remember that DEPS of packages that depend on Magit also have to
# be updated.

ifdef CYGPATH
  LOAD_PATH += -L $(shell cygpath --mixed $(COMPAT_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(COND_LET_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(LLAMA_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(SEQ_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(TRANSIENT_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(WITH_EDITOR_DIR))
  ifneq "$(MAGIT_SECTION_DIR)" ""
    LOAD_PATH += -L $(shell cygpath --mixed $(MAGIT_SECTION_DIR))
  endif
else
  LOAD_PATH += -L $(COMPAT_DIR)
  LOAD_PATH += -L $(COND_LET_DIR)
  LOAD_PATH += -L $(LLAMA_DIR)
  LOAD_PATH += -L $(SEQ_DIR)
  LOAD_PATH += -L $(TRANSIENT_DIR)
  LOAD_PATH += -L $(WITH_EDITOR_DIR)
  ifneq "$(MAGIT_SECTION_DIR)" ""
    LOAD_PATH += -L $(MAGIT_SECTION_DIR)
  endif
endif

endif # ifndef LOAD_PATH

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH = -L ../../org/lisp
endif

## Dependencies ######################################################

# This isn't used by make, but is needed for the Compile ci workflow.

DEPS  = compat
DEPS += cond-let
DEPS += llama
DEPS += seq
DEPS += transient/lisp
DEPS += with-editor/lisp

## Publish ###########################################################

DOMAIN      ?= magit.vc
CFRONT_DIST ?= E2LUHBKU1FBV02

DOCBOOK_XSL ?= /usr/share/xml/docbook/stylesheet/docbook-xsl/epub/docbook.xsl

EPUBTRASH = epub.xml META-INF OEBPS
