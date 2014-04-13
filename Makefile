CASK ?= cask
EMACS ?= emacs

ECUKESFLAGS = --no-win

ELFILES = magma-completion.el \
	magma-electric-newline.el \
	magma-font-lock.el \
	magma-interactive.el \
	magma-mode.el \
	magma-extra.el \
	magma-smie.el

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

unit: $(ELFILES)
	${CASK} exec ert-runner

.ecukes-failing-scenarios: ecukes-all

ecukes-debug: .ecukes-failing-scenarios
	${CASK} exec ecukes $(ECUKESFLAGS) --only-failing --debug

ecukes-fail: .ecukes-failing-scenarios 
	${CASK} exec ecukes $(ECUKESFLAGS) --only-failing

ecukes-all: $(ELFILES) $(ECUKESFILES)
	${CASK} exec ecukes $(ECUKESFLAGS)

ecukes: ecukes-all


install:
	${CASK} install

.PHONY:	all test unit ecukes install ecukes-all ecukes-fail ecukes-debug
