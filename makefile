.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))

init:
|-fd . $(mkfileDir) -HIt d -t e -x rm -rf

tangle-setup:
|cp $(mkfileDir)/settings/org-tangle.sh $(mkfileDir)/settings/backup-tangle.sh
|chmod +x $(mkfileDir)/settings/org-tangle.sh $(mkfileDir)/settings/backup-tangle.sh

tangle: tangle-setup
|yes yes | fd . $(mkfileDir) \
    -HId 1 -e org \
    -E testing.aiern.org \
    -E resting.aiern.org \
    -x $(mkfileDir)/settings/backup-tangle.sh
|fd . $(mkfileDir)/settings \
    -HIe sh\
    -x chmod +x

subinit: init
|-git clone --depth 1 https://github.com/emacsmirror/epkgs.git $(mkfileDir)/epkgs
|-git clone --depth 1 https://github.com/emacsmirror/epkgs.git $(mkfileDir)/var/epkgs
|git -C $(mkfileDir) submodule sync --recursive
# |git -C $(mkfileDir) submodule foreach 'git -C $$toplevel config submodule.$$name.ignore all'

pull: subinit
|git -C $(mkfileDir) pull

add:
|git -C $(mkfileDir) add .

commit:
|-git -C $(mkfileDir) commit --allow-empty-message -am ""

cammit: init add commit

push: cammit
|-git -C $(mkfileDir) push

super-push: tangle push

include $(mkfileDir)/tests.mk
