;; [[file:~/.emacs.d/protean.aiern.org::*init.el][init.el:2]]
;;; $EMACSDIR/init.el -*- lexical-binding: t; -*-
(when (version< emacs-version "27") (load (concat (file-name-directory load-file-name) "early-init.el")))
(defun meq/reload-first-init nil (interactive) (meq/cl (meq/ued "protean.aiern.org")))
(meq/reload-first-init)
;; init.el:2 ends here
