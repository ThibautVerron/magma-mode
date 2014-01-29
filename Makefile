CASK ?= cask
EMACS ?= emacs

ECUKESFLAGS = #--no-win

ELFILES = magma-completion.el \
	magma-electric-newline.el \
	magma-font-lock.el \
	magma-interactive.el \
	magma-mode.el \
	magma-smie.el

STEPFILES = features/step-definitions/magma-mode-steps.el

ENVFILES = features/support/env.el

FEATFILES = features/magma-interaction.feature \
	features/magma-mode-electric-newline.feature \
	features/magma-mode-indentation.feature

ECUKESFILES = $(STEPFILES) $(ENVFILES) $(FEATFILES)

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
