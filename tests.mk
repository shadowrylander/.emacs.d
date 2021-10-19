.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs
test := emacs --bg-daemon=test
killTest := emacsclient -s test -e "(kill-emacs)"
profile = $(shell cat $(mkfileDir)/default.aiern.org)

pre-test: subinit

pest: pre-test
|emacs -p

no-config-test:
|emacs -Q

test-and-kill-pre: pre-test
|-emacsclient -s test -e "(kill-emacs)"

test-new-and-kill: test-and-kill-pre
|$(test) -Q
|$(killTest)

test-new-nw-and-kill: test-and-kill-pre
|$(test) -Q -nw
|$(killTest)

clean-all:
|fd . $(mkfileDir) -HIe elc -x rm

pre-clean:
|fd . $(mkfileDir) -HId 1 -e elc -x rm

clean: pre-clean
|fd . $(mkfileDir)/profiles/$(profile) -HIe elc -x rm

test: pre-test
|emacs

nw-test: pre-test
|emacs -nw

test-and-kill: test-and-kill-pre
|$(test)
|$(killTest)

test-nw-and-kill: test-and-kill-pre
|$(test) -nw
|$(killTest)

test-update-and-kill: test-and-kill-pre
|$(test) --update
|$(killTest)

test-nw-update-and-kill: test-and-kill-pre
|$(test) -nw --update
|$(killTest)

bootstrap: test-and-kill-pre
|$(test) --bootstrap
|$(killTest)

emacs: test
emacs-nw: nw-test
