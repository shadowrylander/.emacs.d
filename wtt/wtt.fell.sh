#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(pop argv)
(setv name (pop argv))
(load (concat (getenv "HOME") "/.emacs.d/early-init.el"))
(meq/up markdown-mode :mode ("\\.md\\'")
    :use-package-postconfig (yasnippet)
    :upnsd-preconfig (valiant :custom (meq/var/valiant-snippets-dir (meq/ued-lib "valiant" "snippets"))))
(meq/upnsd fell
    :custom (meq/var/fell-snippets-dir (meq/ued-lib "fell" "snippets"))
    :mode ("\\.fell\\.md\\'" . fell-markdown-mode))
(find-file (concat name "." (meq/named-uuid name) ".fell.md"))
(meq/insert-snippet "markdown valiant template")
(save-buffer)
