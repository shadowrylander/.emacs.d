(setq borg-user-emacs-directory user-emacs-directory
    borg-drones-directory-prefix (concat "lib" meq/var/slash)
    borg-drones-directory (concat borg-user-emacs-directory borg-drones-directory-prefix)
    borg-gitmodules-file (expand-file-name ".gitmodules" user-emacs-directory))
(defun meq/require-and-load (pkg)
    (add-to-list 'load-path (concat user-emacs-directory "lib" meq/var/slash pkg) t)
    (require (intern pkg)))
(mapc 'meq/require-and-load '("compat" "emacsql" "emacsql-sqlite" "closql"))

(add-to-list 'load-path (concat user-emacs-directory "lib" meq/var/slash "epkg" meq/var/slash "lisp") t)
(require 'epkg)

(add-to-list 'load-path (concat user-emacs-directory "lib" meq/var/slash "borg") t)
(require 'borg)
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
              (not-excluded (not (member pkg '())))
              (result (and no-back-slash no-forward-slash xane same-borg-prefix not-excluded)))
            result))
      (funcall func barg))))
(advice-add #'borg-drones :around #'meq/borg-drones-advice)
(defun meq/borg-activate (clone)
  "Activate the clone named CLONE.

Add the appropriate directories to `load-path' and
`Info-directory-list', and load the autoloads file,
if it exists."
  (interactive (list (borg-read-clone "Activate clone: ")))
  (cl-flet
      ((activate (dir part)
         (let ((file (expand-file-name (format "%s-%s.el" clone part) dir)))
           (and (file-exists-p file)
                (with-demoted-errors "Error loading autoloads: %s"
                  (load file nil t))
                ;; We cannot rely on the autoloads file doing that.
                (add-to-list 'load-path dir)))))
    (dolist (dir (borg-load-path clone))
      (or ;; (activate dir "autoloads")
          ;; (activate dir "loaddefs")       ; `org' uses a different name.
          (add-to-list 'load-path dir)))) ; There might be no autoloads file.
  (dolist (dir (borg-info-path clone))
    (push  dir Info-directory-list)))
(advice-add #'borg-activate :override #'meq/borg-activate)
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
(if (file-exists-p (concat user-emacs-directory "lib" meq/var/slash "org"))
  (if (file-exists-p (concat user-emacs-directory "lib" meq/var/slash "org" meq/var/slash "lisp" meq/var/slash "org-loaddefs.el"))
    (borg-activate "org")
    (borg-build "org" t))
  (borg-assimilate "org" (borg-get "org" "url")))
(let* ((gitmodules (borg-drones t t))
        (command)

        ;; Adapted From:
        ;; Answer: https://superuser.com/a/927832/1154755
        ;; User: https://superuser.com/users/265996/jackson
        (inhibit-message t))
    (mapc #'(lambda (pkg) (interactive)
                (apply #'borg-assimilate pkg))
        ;; Adapted From:
        ;; Answer: https://stackoverflow.com/a/9366300/10827766
        ;; User: https://stackoverflow.com/users/267442/spec
        (remove nil (mapcar #'(lambda (pkg) (interactive)
            (list (car pkg) (cl-getf (cdr pkg) 'url))) gitmodules))))
(mapc #'borg-update-autoloads (mapcar #'car (borg-drones t)))
(setq borg-rewrite-urls-alist '(("git@github.com:" . "https://github.com/")
                                ("git@gitlab.com:" . "https://gitlab.com/")))
(borg-initialize)
