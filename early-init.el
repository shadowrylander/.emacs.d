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
(defvar meq/var/windows (member system-type '(windows-nt ms-dos)))
(defvar meq/var/slash (if meq/var/windows "\\" "/"))
(defvar meq/var/phone (ignore-errors (string-match-p (regexp-quote "Android") (shell-command-to-string "uname -a"))))
(defvar meq/var/wsl (ignore-errors (string-match-p (regexp-quote "microsoft-standard-WSL") (shell-command-to-string "uname -a"))))
(defvar meq/var/nixos (ignore-errors (string-match-p (regexp-quote "nixos") (shell-command-to-string "uname -a"))))
(defvar meq/var/we-are-borg (or (getenv "WEAREBORG") (meq/*item-in-cla "--we-are-borg")))
(setq package-enable-at-startup nil)
(setq meq/var/package-manager (if meq/var/we-are-borg "borg" "nix"))
(load (concat user-emacs-directory "siluam/"
    (pcase meq/var/package-manager
        ("package" "package-config.el")
        ("straight" "straight-config.el")
        ("quelpa" "quelpa.el")
        ("borg" "borg-cube.el")
        (_ "nix-config.el"))))
(setq load-prefer-newer t)
(mapc #'(lambda (pkg) (interactive)
  (let* ((pkg-name (symbol-name pkg)))
    (when meq/var/we-are-borg (ignore-errors (borg-activate pkg-name)))
    (unless (require pkg nil t)
        (pcase meq/var/package-manager
            ("package" (progn (add-to-list 'package-selected-packages pkg) (package-install pkg)))
            ("straight" (straight-use-package pkg))
            ("quelpa" (quelpa pkg))
            ("borg" (borg-assimilate pkg-name (borg-get pkg-name "url")))))
    (require pkg))) '(auto-compile no-littering gcmh))
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(gcmh-mode 1)
(global-auto-revert-mode t) (auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      auto-revert-use-notify nil)
(require 'org-loaddefs)
(defun meq/call (program buffer-name &rest args)
  (let ((process-connection-type nil)
        (buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
        (pop-to-buffer buffer)
        (if (eq (apply #'call-process program nil buffer nil args) 0)
            (unwind-protect (format "\n\n%s\n\n" (buffer-string)) (kill-buffer buffer))
            (error "%s: %s:\n\n%s" program args (buffer-string))))))
(defun meq/call-tangle (file) (meq/call "org-tangle" "*literally-configuring*" file))
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
