.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs
test := -emacs --bg-daemon=test
killTest := -emacsclient -s test -e "(kill-emacs)"
cleanAll := -fd . $(mkfileDir) -HIe elc -x rm

clean-all:
|$(cleanAll)

clean:
|-fd . $(mkfileDir) -HId 1 -e elc -x rm

pre-test: clean-all subinit

super-subinit: subinit
|git -C $(mkfileDir) submodule update --init --recursive --remote --force

init: super-subinit clean-all tangle

soft-init: subinit clean-all tangle

no-config-test: pre-test
|emacs -Q

test-and-kill-pre: pre-test
|$(killTest)

bootstrap: test-and-kill-pre
|$(test) --bootstrap
|$(killTest)

force-bootstrap: test-and-kill-pre
|$(test) --force-bootstrap
|$(killTest)

test-new-and-kill: test-and-kill-pre
|$(test) -Q
|$(killTest)

test-new-nw-and-kill: test-and-kill-pre
|$(test) -Q -nw
|$(killTest)

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

emacs: test
emacs-nw: nw-test
