;; [[file:~/.emacs.d/README.org::*early-init.el][early-init.el:2]]
;;; $EMACSDIR/early-init.el -*- lexical-binding: t; -*- no-byte-compile: t -*-
(defvar user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))
(defvar meq/var/bootstrap (member "--bootstrap" command-line-args)) (delete "--bootstrap" command-line-args)
(defvar meq/var/force-bootstrap (member "--force-bootstrap" command-line-args)) (delete "--force-bootstrap" command-line-args)
(setq package-enable-at-startup nil)
(defvar meq/var/windows (member system-type '(windows-nt ms-dos)))
(defvar meq/var/slash (if meq/var/windows "\\" "/"))
(defvar meq/var/phone (ignore-errors (string-match-p (regexp-quote "Android") (shell-command-to-string "uname -a"))))
(defvar meq/var/wsl (ignore-errors (string-match-p (regexp-quote "microsoft-standard-WSL") (shell-command-to-string "uname -a"))))
(defvar meq/var/nixos (ignore-errors (string-match-p (regexp-quote "nixos") (shell-command-to-string "uname -a"))))
(setq borg-drones-directory-prefix (concat "lib" meq/var/slash))
(setq borg-drones-directory (concat user-emacs-directory borg-drones-directory-prefix))
(defun meq/require-and-load (pkg)
    (add-to-list 'load-path (concat user-emacs-directory "lib" meq/var/slash pkg) t)
    (require (intern pkg)))
(mapc 'meq/require-and-load '("emacsql" "emacsql-sqlite" "closql"))

(add-to-list 'load-path (concat user-emacs-directory "lib" meq/var/slash "epkg" meq/var/slash "lisp") t)
(require 'epkg)

(add-to-list 'load-path (concat user-emacs-directory "lib" meq/var/slash "borg") t)
(require 'borg)

;; (unless (or
;;           meq/var/phone
;;           ;; meq/var/windows
;;           ) (meq/require-and-load "epkg"))
;; (meq/require-and-load "borg")
;; (defun meq/borg--call-git-advice (pkg &rest args)
;;   (let ((process-connection-type nil)
;;         (buffer (generate-new-buffer
;;                  (concat " *Borg Git" (and pkg (concat " " pkg)) "*"))))
;;     (if (eq (apply #'call-process "git" nil buffer nil args) 0)
;;         (kill-buffer buffer)
;;       (with-current-buffer buffer
;;         (special-mode))
;;       (pop-to-buffer buffer)
;;       (error "Borg Git: %s %s:\n\n%s" pkg args (buffer-string)))))
(defun meq/borg--call-git-advice (pkg &rest args)
  (let ((process-connection-type nil)
        (buffer (generate-new-buffer
                 (concat " *Borg Git" (and pkg (concat " " pkg)) "*"))))
    (with-current-buffer buffer
        (pop-to-buffer buffer)
        (if (eq (apply #'call-process "git" nil buffer nil args) 0)
            (unwind-protect (format "\n\n%s\n\n" (buffer-string)) (kill-buffer buffer))
            (error "Borg Git: %s %s:\n\n%s" pkg args (buffer-string))))))
(advice-add #'borg--call-git :override #'meq/borg--call-git-advice)
(defun meq/borg-build-advice (clone &optional activate)
  "Build the clone named CLONE.
Interactively, or when optional ACTIVATE is non-nil,
then also activate the clone using `borg-activate'."
  (interactive (list (borg-read-clone "Build drone: ") t))
  (borg--build-noninteractive clone)
  (when activate (borg-activate clone)))
(advice-add #'borg-build :override #'meq/borg-build-advice)
(defun meq/borg-drones-advice (func &rest args)
  (let* ((barg (pop args))
          (assimilating (pop args)))
    (seq-filter #'(lambda (pkg*) (interactive)
      (let* ((pkg (car pkg*))
              (path* (cl-getf (cdr pkg*) 'path))
              (path (cond ((listp path*) (car path*))
                          ((stringp path*) path*)))
              (exists (file-exists-p (borg-worktree pkg)))
              (no-back-slash (not (string-match-p (regexp-quote "\\") pkg)))
              (no-forward-slash (not (string-match-p (regexp-quote "/") pkg)))
              (xane (xor assimilating exists)
                    ;; (or
                    ;;     (and assimilating (not exists))
                    ;;     (and exists (not assimilating)))
                    )
              (same-borg-prefix (string= (string-remove-suffix pkg path)
                                    (string-remove-prefix borg-user-emacs-directory borg-drones-directory)))
              (result (and no-back-slash no-forward-slash xane same-borg-prefix)))
            result))
      (funcall func barg))))
(advice-add #'borg-drones :around #'meq/borg-drones-advice)
(defun meq/borg-assimilate-advice (package url &optional partially)
  "Assimilate the package named PACKAGE from URL.
If `epkg' is available, then only read the name of the package
in the minibuffer and use the url stored in the Epkg database.
If `epkg' is unavailable, the package is not in the database, or
with a prefix argument, then also read the url in the minibuffer.
With a negative prefix argument only add the submodule but don't
build and activate the drone."
  (interactive
   (nconc (borg-read-package "Assimilate package: " current-prefix-arg)
          (list (< (prefix-numeric-value current-prefix-arg) 0))))
  (message "Assimilating %s..." package)
  (unless (equal (borg-get package "s8472") "true")
      (borg--maybe-reuse-gitdir package)
      (borg--call-git
        package
        "-C" borg-top-level-directory
        "submodule"
        "add"
        "-f"
        "--depth" "1"
        "--name" package
        url
        (or
          (borg-get package "path")
          (concat (string-remove-prefix borg-user-emacs-directory borg-drones-directory) meq/var/slash package)))
      (borg--sort-submodule-sections ".gitmodules")
      (borg--call-git package "add" ".gitmodules")
      (borg--maybe-absorb-gitdir package))
  (unless partially
    (borg-build package)
    (borg-activate package))
  (borg--refresh-magit)
  (message "Assimilating %s...done" package))
(advice-add #'borg-assimilate :override #'meq/borg-assimilate-advice)
(defvar meq/var/update (member "--update" command-line-args)) (delete "--update" command-line-args)
(when meq/var/update (mapc #'borg-build (mapcar #'car (borg-drones t))))
(setq load-prefer-newer t)
(mapc #'(lambda (pkg*) (interactive)
  (let* ((pkg (symbol-name pkg*)))
    (ignore-errors (borg-activate pkg))
    (unless (require pkg* nil t)
        (borg-assimilate pkg (borg-get pkg "url"))))) '(packed auto-compile no-littering gcmh))
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(gcmh-mode 1)
(global-auto-revert-mode t) (auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
auto-revert-use-notify nil)
(if (file-exists-p (concat user-emacs-directory "lib" meq/var/slash "org"))
  (if (file-exists-p (concat user-emacs-directory "lib" meq/var/slash "org" meq/var/slash "lisp" meq/var/slash "org-loaddefs.el"))
    (borg-activate "org")
    (borg-build "org" t))
  (borg-assimilate "org" (borg-get "org" "url")))
(require 'org-loaddefs)
(defun meq/call (program buffer-name &rest args)
  (let ((process-connection-type nil)
        (buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
        (pop-to-buffer buffer)
        (if (eq (apply #'call-process program nil buffer nil args) 0)
            (unwind-protect (format "\n\n%s\n\n" (buffer-string)) (kill-buffer buffer))
            (error "%s: %s:\n\n%s" program args (buffer-string))))))
(defun meq/call-tangle (file) (meq/call (concat user-emacs-directory "settings" meq/var/slash "org-tangle.sh") "*literally-configuring*" file))
(defun meq/org-babel-load-file-advice (file &optional compile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let ((tangled-file (concat (file-name-sans-extension file) ".el")))
    ;; Tangle only if the Org file is newer than the Elisp file.
    (unless (org-file-newer-than-p
                tangled-file
                (file-attribute-modification-time
                    (file-attributes (file-truename file))))
        (meq/call-tangle file))
    (if compile
        (progn
            (byte-compile-file tangled-file)
            (load tangled-file)
            (message "Compiled and loaded %s" tangled-file))
        (load-file tangled-file)
        (message "Loaded %s" tangled-file))))
(advice-add #'org-babel-load-file :override #'meq/org-babel-load-file-advice)
(defun meq/reload-early-init nil (interactive) (org-babel-load-file (concat user-emacs-directory "README.org") t))
(meq/reload-early-init)
;; early-init.el:2 ends here
