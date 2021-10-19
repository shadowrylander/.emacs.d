;; Adapted From: https://github.com/raxod502/straight.el#how-do-i-pin-package-versions-or-use-only-tagged-releases
;; And: https://www.reddit.com/r/emacs/comments/dppmqj/do_i_even_need_to_leverage_earlyinitel_if_i_have/?utm_source=amp&utm_medium=&utm_content=post_body
;; And: https://github.com/hartzell/straight.el/commit/882649137f73998d60741c7c8c993c7ebbe0f77a#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R1649

(setq straight-profiles
      '((nil . "default.el")
        (pinned . "pinned.el")))

(with-no-warnings
    (setq straight-vc-git-default-clone-depth 1)
    (setq straight-base-dir (concat user-emacs-directory ".local/"))
    (setq straight-repository-branch "develop")
    (setq straight-build-dir (format "build-%s" emacs-version))
    (setq straight-cache-autoloads t)
    (setq straight-check-for-modifications '(check-on-save find-when-checking))
    ;; (setq straight-use-package-by-default t)
    (setq straight-disable-byte-compilation (member "--no-byte-compilation" command-line-args)))

(delete "--no-byte-compilation" command-line-args)

(eval-and-compile
  (setq straight-recipes-gnu-elpa-use-mirror t)
  (setq straight-recipes-emacsmirror-use-mirror t)
  (setq bootstrap-version 5)
  (setq bootstrap-file (concat straight-base-dir "straight/repos/straight.el/bootstrap.el")))

(unless (file-exists-p bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))

(load bootstrap-file nil 'nomessage)

(autoload #'straight-x-pull-all "straight-x")
(autoload #'straight-x-freeze-versions "straight-x")
