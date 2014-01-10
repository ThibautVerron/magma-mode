CASK ?= cask
EMACS ?= emacs

all: test

test: ecukes

unit:
	${CASK} exec ert-runner

ecukes-debug:
	${CASK} exec ecukes --only-failing --debug

ecukes-fail:
	${CASK} exec ecukes --only-failing

ecukes-all:
	${CASK} exec ecukes

ecukes: ecukes-fail ecukes-all

install:
	${CASK} install

.PHONY:	all test unit ecukes install
