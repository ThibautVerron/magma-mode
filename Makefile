CASK ?= cask
EMACS ?= emacs

ECUKESFLAGS = --script

ELFILES = magma-completion.el \
	magma-font-lock.el \
	magma-interactive.el \
	magma-mode.el \
	magma-extra.el \
	magma-smie.el \
	magma-scan.el \
	magma-q.el \
	magma-vars.el

ELCFILES = $(subst .el,.elc,$(ELFILES))

ECUKESFOLDER = features

STEPFILES = $(ECUKESFOLDER)/step-definitions/magma-mode-steps.el

ENVFILES = $(ECUKESFOLDER)/support/env.el

FEATFILES = $(ECUKESFOLDER)/magma-completion.feature \
	$(ECUKESFOLDER)/magma-extras-electric-newline.feature \
	$(ECUKESFOLDER)/magma-extras-hideshow.feature \
	$(ECUKESFOLDER)/magma-indentation.feature \
	$(ECUKESFOLDER)/magma-interaction.feature

ECUKESFILES = $(STEPFILES) $(ENVFILES) $(FEATFILES)

export LANG=C

all: test

test: unit ecukes

unit: $(ELCFILES)
	${CASK} exec ert-runner

.ecukes-failing-scenarios:
	${CASK} exec ecukes $(ECUKESFLAGS)

ecukes-debug: $(ELCFILES) .ecukes-failing-scenarios
	${CASK} exec ecukes $(ECUKESFLAGS) --only-failing --debug

ecukes-fail: $(ELCFILES) .ecukes-failing-scenarios 
	${CASK} exec ecukes $(ECUKESFLAGS) --only-failing

ecukes-all: $(ELCFILES) $(ECUKESFILES)
	${CASK} exec ecukes $(ECUKESFLAGS)

ecukes: ecukes-all

%.elc : %.el
	${CASK} build

install:
	${CASK} install

.PHONY:	all test unit ecukes install ecukes-all ecukes-fail ecukes-debug
