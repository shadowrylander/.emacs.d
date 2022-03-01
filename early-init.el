;; [[file:README.org::*early-init.el][early-init.el:2]]
;;; $EMACSDIR/early-init.el -*- lexical-binding: t; -*- no-byte-compile: t -*-
(defvar user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))
(setq meq/var/initial-directory default-directory)
(defun meq/message (func &rest args) (interactive)
    (let* ((*message (apply #'format args)))

        ;; NOTE: THE SPACE BEFORE `CREATING' IS MEANT TO BE THERE!
        (unless (or (string-prefix-p " Creating" *message)

                    (string-prefix-p "Configuring" *message)
                    (string-prefix-p "Loading" *message)
                    (string-prefix-p "Library is file" *message))
            (apply func args))))
(advice-add #'message :around #'meq/message)
(add-hook 'after-init-hook #'(lambda nil (interactive) (advice-remove #'message #'meq/message)))
(defun meq/*item-in-cla (item) (unwind-protect (member item command-line-args) (delete item command-line-args)))
(defun meq/*get-next-in-cla (item)
    (let* ((index (seq-position command-line-args item))
            (value (when index (nth (1+ index) command-line-args))))
        (when value (unwind-protect value (delete value command-line-args)))))
(defun meq/*two-items-in-list (item) (unwind-protect (when (member item command-line-args) (meq/*get-next-in-cla item)) (delete item command-line-args)))
(defvar meq/var/bootstrap (meq/*item-in-cla "--bootstrap"))
(defvar meq/var/force-bootstrap (meq/*item-in-cla "--force-bootstrap"))
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
(defun meq/borg--maybe-absorb-gitdir (pkg)
  (let* ((ver (nth 2 (split-string (car (process-lines "git" "version")) " ")))
         (ver (and (string-match "\\`[0-9]+\\(\\.[0-9]+\\)*" ver)
                   (match-string 0 ver))))
    (if (version< ver "2.12.0")
        (let ((default-directory (borg-worktree pkg))
              (gitdir (borg-gitdir pkg)))
          (make-directory gitdir t)
          (borg--call-git pkg "init" "--separate-git-dir" gitdir)
          (borg--link-gitdir pkg))
      (borg--call-git pkg "-C" borg-top-level-directory "submodule" "absorbgitdirs" "--" (borg-worktree pkg)))))
(advice-add #'borg--maybe-absorb-gitdir :override #'meq/borg--maybe-absorb-gitdir)
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
      (borg--sort-submodule-sections (concat borg-top-level-directory ".gitmodules"))
      (borg--call-git package "-C" borg-top-level-directory "add")
      (borg--maybe-absorb-gitdir package))
  (unless partially
    (borg-build package)
    (borg-activate package))
  (borg--refresh-magit)
  (message "Assimilating %s...done" package))
(advice-add #'borg-assimilate :override #'meq/borg-assimilate-advice)
(defvar meq/var/update (meq/*item-in-cla "--update"))
(when meq/var/update (mapc #'borg-build (mapcar #'car (borg-drones t))))
(defvar meq/var/update-norg (meq/*item-in-cla "--update-norg"))
(when meq/var/update-norg (mapc #'borg-build (remove "org" (mapcar #'car (borg-drones t)))))
(defvar meq/var/update-this (meq/*two-items-in-list "--update-this"))
(when meq/var/update-this (borg-build meq/var/update-this))
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
(defun meq/call-tangle (file)
    (setq tangle-script (concat user-emacs-directory "settings" meq/var/slash "org-tangle.sh"))

    ;; TODO
    ;; (meq/call "chmod" "*making-tangle-script-executable*" "+x" tangle-script)

    (meq/call tangle-script "*literally-configuring*" file))
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
