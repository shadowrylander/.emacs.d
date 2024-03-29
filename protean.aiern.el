;;; $EMACSDIR/protean.aiern.el -*- lexical-binding: t; -*-

(defvar meq/var/exwm nil)
(when (get-buffer "*window-manager*")
  (kill-buffer "*window-manager*"))
(when (get-buffer "*window-manager-error*")
  (kill-buffer "*window-manager-error*"))
(when (executable-find "wmctrl")
  (shell-command "wmctrl -m ; echo $?" "*window-manager*" "*window-manager-error*"))

  ;; if there was an error detecting the window manager, initialize EXWM
  (when (and (get-buffer "*window-manager-error*")
             (eq window-system 'x))
    ;; exwm startup goes here
    (setq meq/var/exwm t))
(meq/up exwm :if meq/var/exwm

    :init/defun* (post-exwm nil (interactive)
                (unless (get-buffer "Alacritty") (meq/run "alacritty"))
                (unless (get-buffer "obsidian") (meq/run "obsidian")))
    :hook (exwm-init . #'post-exwm)

    :upnsd-preconfig (dmenu) (fringe :config
                        ;; (fringe-mode (quote (1 . 1)) nil (fringe))
                        ;; (fringe-mode '(3 . 0))
                        ;; (fringe-mode 'none)
                        ;; (fringe-mode 1)
                        )

    :config
        (require 'scroll-bar)
        ;; Adapted From: https://github.com/ch11ng/exwm/blob/master/exwm-config.el#L34
        (require 'exwm-config)
        ;; Set the initial workspace number.
        (unless (get 'exwm-workspace-number 'saved-value)
            (setq exwm-workspace-number 4))
        ;; Make class name the buffer name
        (add-hook 'exwm-update-class-hook #'(lambda nil (interactive)
                    (exwm-workspace-rename-buffer exwm-class-name)))
        ;; Global keybindings.
        (unless (get 'exwm-input-global-keys 'saved-value)
            (setq exwm-input-global-keys
                `(

                    ([?\s-p] . uru)
                    ([?\s-`] . uru)
                    ([?\s-z] . uru)

                    (,(naked "s-tab") . next-buffer)
                    (,(naked "s-<iso-lefttab>") . previous-buffer)
                    (,(naked "M-s-tab") . previous-buffer)

                    ([?\s-q] . deino-buffer/body)

                    ;; 's-N': Switch to certain workspace.
                    ,@(mapcar (lambda (i)
                                `(,(kbd (format "s-%d" i)) .
                                (lambda ()
                                    (interactive)
                                    (exwm-workspace-switch-create ,i))))
                            (number-sequence 0 9)))))
        ;; Line-editing shortcuts
        (unless (get 'exwm-input-simulation-keys 'saved-value)
            (setq exwm-input-simulation-keys
                '(([?\C-b] . [left])
                    ([?\C-f] . [right])
                    ([?\C-p] . [up])
                    ([?\C-n] . [down])
                    ([?\C-a] . [home])
                    ([?\C-e] . [end])
                    ([?\M-v] . [prior])
                    ([?\C-v] . [next])
                    ([?\C-d] . [delete])
                    ([?\C-k] . [S-end delete]))))

        (exwm-enable)
        ;; Configure Ido
        (exwm-config-ido)
        ;; Other configurations
        (exwm-config-misc)

        ;; (exwm-config-default)
        ;; (exwm-enable)

    :which-key-change-ryo ("e" "exwm" 'exwm-init-hook)

    :uru (exwm-mode t deino-exwm-global (:color blue) "e g"
            ("`" nil "cancel")
            ("c" exwm-input-release-keyboard "char mode")
            ("l" exwm-input-grab-keyboard "line mode")
            ("r" exwm-reset "reset")
            ("w" exwm-workspace-switch "workspace switch")
            ("i" meq/run-interactive "run")
            ("b" deino-buffer/body "buffers"))

    :deino (deino-exwm nil "e e"
            ("`" nil "cancel")
            ("XF86PowerOff" deino-exwm/power/body "power")
            ("s" deino-exwm/shells/body "shells"))

        (deino-exwm/power (:color blue) "e p"
            ("r" (meq/run "reboot") "reboot")
            ("q" (meq/run "poweroff") "poweroff")
            ("XF86PowerOff" (meq/run "systemctl suspend" "suspend") "suspend"))

        (deino-exwm/shells (:color blue) "e s" ("a" (meq/run "alacritty") "alacritty"))

    :demon ((naked "XF86PowerOff") 'deino-exwm/body)

    :gsetq (exwm-manage-force-tiling t)
        ;; Adapted From: https://www.reddit.com/r/emacs/comments/8yf6dx/key_chords_in_exwm/
        ;; (exwm-input-line-mode-passthrough t)
)

(when (or meq/var/bootstrap meq/var/force-bootstrap)

(defvar meq/var/not-in-terminal (meq/item-in-cla "--not-in-terminal"))
(defvar meq/var/not-minimal (meq/item-in-cla "--not-minimal"))
(defvar meq/var/neither (meq/item-in-cla "--neither"))

(defvar meq/var/nix-env-channel (meq/if-two-items-in-cla
                                    "--channel"
                                    t
                                    ;; (if meq/var/nixos "nixos" "nixpkgs")
                                    "master"))
(defvar meq/var/package-managers
    `(("pkg" . (:manager "pkg" :install "install" :query "list-installed" :separator "/"))
        ("nix-env" . (:manager "nix-env" :install ("--install" "--attr") :query "--query" :separator "-" :channel ,meq/var/nix-env-channel))
        ("pip" . (:manager "pip" :install "install" :query "list --pre" :separator " "))))
(defvar meq/var/default-package-manager (if meq/var/phone "pkg" "nix-env"))
(defun meq/pm-details (pm detail) (cl-getf (cdr (assoc pm meq/var/package-managers)) detail))

(defun meq/get-packages (pm)
    (mapcar #'(lambda (pkg) (interactive) (string-trim (string-join
                                    (butlast (split-string pkg (meq/pm-details pm :separator)))
                                    (meq/pm-details pm :separator))))
    (butlast (split-string (shell-command-to-string (string-join (list (meq/pm-details pm :manager)
                                                        (meq/pm-details pm :query)) " ")) "\n"))))

(require 'a)
(defun meq/get-pipx-packages nil
    (let* ((pipx-list (shell-command-to-string "pipx list --json"))
            (json-array-type 'list)
            (json-false) (json-null)
            (pipx-alist (ignore-errors (json-parse-string pipx-list :object-type 'alist)))
            (venvs (when pipx-alist (cdr (assoc 'venvs pipx-alist))))
            (apps (when venvs (a-keys venvs)))
            (get-vaxed (lambda (app) (interactive) (when venvs
                (a-keys (cdr (assoc 'injected_packages (cdr (assoc 'metadata (cdr (assoc app venvs)))))))))))
        (list :apps (when apps (mapcar #'symbol-name apps))
            :libs (when apps (-flatten-n 1 (mapcar #'(lambda (app) (interactive) (list
                                    (meq/inconcat ":" (symbol-name app))
                                    (mapcar #'symbol-name (funcall get-vaxed app)))) apps))))))

(defun meq/concat-pkg (attr pkg*)
    (let* ((pkg-is-list (listp pkg*)) (pkg (meq/rs pkg* t)))
        (if pkg-is-list `(,(concat attr pkg) ,@pkg*) (concat attr pkg))))

(defvar meq/var/packages
    (list :installed (-flatten-n 2 (list (mapcar #'(lambda (pm) (interactive)
                                                        (let* ((pkgs (meq/get-packages pm)))
                                                            (list
                                                                (meq/inconcat ":" pm)
                                                                (if (string-prefix-p "pip" pm)
                                                                    (nthcdr 2 pkgs)
                                                                    pkgs))))
                                (mapcar #'car meq/var/package-managers))
                            (list (list :pipx (meq/get-pipx-packages)))))
        :pipx (list :apps '(black
                            (jupyterlab jupyter-lab))
                    :libs (let* ((hy '(("https://github.com/hylang/hy/archive/master/hy.tar.gz" hy))))
                            (list :xonsh (-flatten-n 1 (list hy)) :hy (-flatten-n 1 (list hy)))))
        :pkg '(python)
        :pip '(pipx)
        :base (-flatten-n 1 (list '((ddar ignore-this)
                                    (jupyter jupyter-notebook)
                                    (ripgrep rg)
                                    (silver-searcher ag)
                                    zsh)
                                (mapcar #'(lambda (pkg) (interactive)
                                            (meq/concat-pkg "gitAndTools." pkg))
                                    '(git-extras git-hub gitflow gh hub lab))))
        :extras '()
        :not-in-terminal '()
        :not-minimal (-flatten-n 1 (list '()
                                        (mapcar #'(lambda (pkg) (interactive)
                                                    (meq/concat-pkg "nix-prefetch-" pkg))
                                            '(github docker scripts))))
        :neither '()))
(defun meq/gfp (pkgs) (cl-getf meq/var/packages pkgs))

(defun meq/pipx-package-installed* (pkg)
    (not (-all? #'not (mapcar #'(lambda (pkg*) (interactive)
                        (let* ((pkg (meq/rs pkg*)))
                            (or
                                (member pkg (cl-getf (cl-getf (meq/gfp :installed) :pipx) :apps))
                                (executable-find pkg))))
        (meq/rl pkg)))))
(defun meq/pipx-package-installed (pkg &optional app)
    (if app
        (let* ((app-installed (meq/pipx-package-installed* app)))
            (when app-installed
                (not (-all? #'not (mapcar #'(lambda (pkg*) (interactive)
                        (let* ((pkg (meq/rs pkg*)))
                            (member (meq/rs pkg) (cl-getf (cl-getf (cl-getf
                                (meq/gfp :installed) :pipx) :libs) (meq/inconcat ":" (meq/rs app))))))
                (meq/rl pkg))))))
        (meq/pipx-package-installed* pkg)))
(defun meq/package-installed (pkg pm)
    (not (-all? #'not (mapcar #'(lambda (pkg**) (interactive)
                        (let* ((pkg* (meq/rs pkg**))
                                (pkg (if (and
                                            (string= pm "nix-env")
                                            (s-contains? "." pkg*))
                                        (car (last (split-string pkg* "\\.")))
                                        pkg*)))
                            (or (member pkg (cl-getf (meq/gfp :installed) (meq/inconcat ":" pm))) (executable-find pkg))))
        (meq/rl pkg)))))
(defun meq/install-pipx-package (pkg* &optional injection-env*)
    (let* ((pkg (meq/rs pkg*))
            (injection-env (when injection-env* (meq/rs injection-env*)))
            (pkg-buffer-name (format "*Installing %s%s With pipx*" pkg (if injection-env*
                                                                        (concat " in " injection-env)
                                                                        "")))
            (injection-env-buffer-name (when injection-env* (format "*Installing %s With pipx*" injection-env)))
            (pkg*-list (list pkg*))
            (pkg-installed (apply #'meq/pipx-package-installed (if injection-env*
                                                                    (-snoc pkg*-list injection-env*)
                                                                    pkg*-list)))
            (injection-env-installed (when injection-env* (meq/pipx-package-installed injection-env*))))

        (when injection-env*
            (if (and injection-env-installed (not meq/var/force-bootstrap))
                (message "Not %s; already installed" injection-env-buffer-name)
                (if (member "ignore-this" (mapcar #'meq/rs (meq/rl injection-env*)))
                    (message "Not %s; ignored" injection-env-buffer-name)
                    (message injection-env-buffer-name)
                    (meq/call "pipx" injection-env-buffer-name "install" "--force" injection-env)
                    (message "%s...done" injection-env-buffer-name))))
        (if (and pkg-installed (not meq/var/force-bootstrap))
            (message "Not %s; already installed" pkg-buffer-name)
            (if (member "ignore-this" (mapcar #'meq/rs (meq/rl pkg*)))
                (message "Not %s; ignored" pkg-buffer-name)
                (message pkg-buffer-name)
                (apply #'meq/call "pipx" pkg-buffer-name (if injection-env*
                    (list "inject" "--force" injection-env pkg)
                    (list "install" "--force" pkg)))
                (message "%s...done" pkg-buffer-name)))))
(defun meq/install-package (pkg** &optional pm*)
    (let* ((pm (or pm* meq/var/default-package-manager))
            (pkg* (meq/rs pkg**))
            (pkg (if (string= pm "nix-env") (concat (meq/pm-details pm :channel) "." pkg*) pkg*))
            (buffer-name (format "*Installing %s With %s*" pkg pm)))
        (if (and (meq/package-installed pkg** pm) (not meq/var/force-bootstrap))
            (message "Not %s; already installed" buffer-name)
            (if (member "ignore-this" (mapcar #'meq/rs (meq/rl pkg**)))
                (message "Not %s; ignored" buffer-name)
                (message buffer-name)
                (let* ((install (meq/pm-details pm :install)))
                    (apply #'meq/call (meq/pm-details pm :manager) buffer-name (append (meq/rl install)
                                                        (remove nil (list pkg (when (string= pm "nix-env") "--show-trace"))))))
                (message "%s...done" buffer-name)))))

(defun meq/wrapped-call (buffer-name &rest args)
    (message buffer-name)
    (apply #'meq/call (pop args) (format "*%s*" buffer-name) args)
    (message "%s...done" buffer-name))

(cond (meq/var/phone (let* ((pm* "pkg")
                            (install (meq/pm-details pm* :install))
                            (pm (meq/pm-details pm* :manager)))
                        (meq/wrapped-call "Adding root repo" pm install "root-repo")
                        (meq/wrapped-call "Updating pkgs" pm "update" "-y")
                        (mapc #'meq/install-package (meq/gfp :pkg))
                        (mapc #'(lambda (pkg) (interactive) (meq/install-package pkg "pip")) (meq/gfp :pip))))
    (meq/var/wsl (mapc #'meq/install-package (meq/gfp :base)))
    (t (progn
            (mapc #'meq/install-package (-flatten-n 1 (list
                                                    (meq/gfp :base)
                                                    (meq/gfp :extras))))
            (when meq/var/not-in-terminal (mapc #'meq/install-package (meq/gfp :not-in-terminal)))
            (when meq/var/not-minimal (mapc #'meq/install-package (meq/gfp :not-minimal)))
            (when meq/var/neither (mapc #'meq/install-package (meq/gfp :neither))))))

(message (meq/call "pipx" "*Ensuring Pipx Path*" "ensurepath"))
(mapc #'meq/install-pipx-package (cl-getf (meq/gfp :pipx) :apps))
(let* ((libs (cl-getf (meq/gfp :pipx) :libs)))
    (mapc #'(lambda (lib*) (interactive)
        (mapc #'(lambda (lib) (interactive)
            (meq/install-pipx-package
                lib (meq/keyword-to-symbol-name lib*))) (cl-getf libs lib*))) (map-keys libs))))

(when (or meq/var/bootstrap meq/var/force-bootstrap)

(defvar meq/var/not-in-terminal (meq/item-in-cla "--not-in-terminal"))
(defvar meq/var/not-minimal (meq/item-in-cla "--not-minimal"))
(defvar meq/var/neither (meq/item-in-cla "--neither"))

(defvar meq/var/nix-env-channel (meq/if-two-items-in-cla
                                    "--channel"
                                    t
                                    ;; (if meq/var/nixos "nixos" "nixpkgs")
                                    "master"))
(defvar meq/var/package-managers
    `(("pkg" . (:manager "pkg" :install "install" :query "list-installed" :separator "/"))
        ("nix-env" . (:manager "nix-env" :install ("--install" "--attr") :query "--query" :separator "-" :channel ,meq/var/nix-env-channel))
        ("pip" . (:manager "pip" :install "install" :query "list --pre" :separator " "))))
(defvar meq/var/default-package-manager (if meq/var/phone "pkg" "nix-env"))
(defun meq/pm-details (pm detail) (cl-getf (cdr (assoc pm meq/var/package-managers)) detail))

(defun meq/get-packages (pm)
    (mapcar #'(lambda (pkg) (interactive) (string-trim (string-join
                                    (butlast (split-string pkg (meq/pm-details pm :separator)))
                                    (meq/pm-details pm :separator))))
    (butlast (split-string (shell-command-to-string (string-join (list (meq/pm-details pm :manager)
                                                        (meq/pm-details pm :query)) " ")) "\n"))))

(require 'a)
(defun meq/get-pipx-packages nil
    (let* ((pipx-list (shell-command-to-string "pipx list --json"))
            (json-array-type 'list)
            (json-false) (json-null)
            (pipx-alist (ignore-errors (json-parse-string pipx-list :object-type 'alist)))
            (venvs (when pipx-alist (cdr (assoc 'venvs pipx-alist))))
            (apps (when venvs (a-keys venvs)))
            (get-vaxed (lambda (app) (interactive) (when venvs
                (a-keys (cdr (assoc 'injected_packages (cdr (assoc 'metadata (cdr (assoc app venvs)))))))))))
        (list :apps (when apps (mapcar #'symbol-name apps))
            :libs (when apps (-flatten-n 1 (mapcar #'(lambda (app) (interactive) (list
                                    (meq/inconcat ":" (symbol-name app))
                                    (mapcar #'symbol-name (funcall get-vaxed app)))) apps))))))

(defun meq/concat-pkg (attr pkg*)
    (let* ((pkg-is-list (listp pkg*)) (pkg (meq/rs pkg* t)))
        (if pkg-is-list `(,(concat attr pkg) ,@pkg*) (concat attr pkg))))

(defun meq/fl (&rest args) (-flatten-n (if (integerp (car args)) (pop args) 1) (list args)))

(defvar meq/var/pipx-libs
    '(("https://github.com/hylang/hy/archive/master/hy.tar.gz" hy)
        addict gitpython fuckit))

(defvar meq/var/packages
    (list :installed (meq/fl 2 (mapcar #'(lambda (pm) (interactive)
                                                        (let* ((pkgs (meq/get-packages pm)))
                                                            (list
                                                                (meq/inconcat ":" pm)
                                                                (if (string-prefix-p "pip" pm)
                                                                    (nthcdr 2 pkgs)
                                                                    pkgs))))
                                (mapcar #'car meq/var/package-managers))
                            (list (list :pipx (meq/get-pipx-packages))))
        :pipx (list :apps '(black black-macchiato borgmatic
                            legit
                            poetry ;; pyls-black
                            (jupyterlab jupyter-lab))
                    :libs (list
                            :xonsh (meq/fl meq/var/pipx-libs)
                            :hy (meq/fl meq/var/pipx-libs)))
        :pkg '(borgbackup et libffi libzmq llvm python rust)
        :pip '(pipx)
        :base (meq/fl '(asdf-vm assh autojump autossh
                                    bat bc borgbackup byobu
                                    cascadia-code coreutils ctop
                                    (ddar ignore-this) direnv diskus dos2unix duf
                                    elvish entr eternal-terminal exa
                                    fasd fff ffmpeg figlet filet fish fzf
                                    gitoxide git-crypt git-fire git-lfs glances gotop
                                    inetutils
                                    (jupyter jupyter-notebook)
                                    libffi lolcat lorri
                                    micro mkpasswd monkeysphere mosh mtr
                                    neo-cowsay neovim niv nix-direnv nnn nodePackages.prettier nox
                                    pandoc par2cmdline peru pfetch python39Packages.pipx pypy python310
                                    ranger (ripgrep rg) rsync
                                    sd shellcheck (silver-searcher ag) spacevim starship sysstat
                                    thefuck tmux tmuxp tree
                                    uutils-coreutils
                                    vim
                                    wget wtf
                                    xfce.thunar xz
                                    zenith)
                                (mapcar #'(lambda (pkg) (interactive)
                                            (meq/concat-pkg "gitAndTools." pkg))
                                    '(git-extras git-hub gitflow gh hub lab)))
        :extras '(acpilight
                    btrfs-progs ;; bcachefs-tools
                    copyq
                    darling-dmg
                    exfat
                    gptfdisk
                    ntfs3g nixos-shell
                    parted pmutils
                    snapper
                    libguestfs
                    ;; thermald
                    udftools
                    vagrant
                    win-qemu
                    xclip
                    yubico-pam yubico-piv-tool yubikey-manager yubikey-agent
                    yubikey-personalization yubioath-flutter)
        :not-in-terminal '(alacritty atom
                            firefox
                            gnome3.gnome-disk-utility gparted
                            keybase-gui kitty
                            libsForQt5.qtstyleplugin-kvantum
                            shadowfox
                            vlc vscode
                            ;; woeusb
                            xclip
                            yubikey-manager-qt yubikey-personalization-gui)
        :not-minimal (meq/fl '(extra-container
                                            ;; haskellPackages.hocker
                                            refind)
                                        (mapcar #'(lambda (pkg) (interactive)
                                                    (meq/concat-pkg "nix-prefetch-" pkg))
                                            '(github docker scripts)))
        :neither '(gnome3.gnome-boxes gnome3.gnome-tweaks
                    google-chrome google-chrome-beta google-chrome-dev
                    vivaldi vivaldi-ffmpeg-codecs vivaldi-widevine
                    vscodium)))
(defun meq/gfp (pkgs) (cl-getf meq/var/packages pkgs))

(defun meq/pipx-package-installed* (pkg)
    (not (-all? #'not (mapcar #'(lambda (pkg*) (interactive)
                        (let* ((pkg (meq/rs pkg*)))
                            (or
                                (member pkg (cl-getf (cl-getf (meq/gfp :installed) :pipx) :apps))
                                (executable-find pkg))))
        (meq/rl pkg)))))
(defun meq/pipx-package-installed (pkg &optional app)
    (if app
        (let* ((app-installed (meq/pipx-package-installed* app)))
            (when app-installed
                (not (-all? #'not (mapcar #'(lambda (pkg*) (interactive)
                        (let* ((pkg (meq/rs pkg*)))
                            (member (meq/rs pkg) (cl-getf (cl-getf (cl-getf
                                (meq/gfp :installed) :pipx) :libs) (meq/inconcat ":" (meq/rs app))))))
                (meq/rl pkg))))))
        (meq/pipx-package-installed* pkg)))
(defun meq/package-installed (pkg pm)
    (not (-all? #'not (mapcar #'(lambda (pkg**) (interactive)
                        (let* ((pkg* (meq/rs pkg**))
                                (pkg (if (and
                                            (string= pm "nix-env")
                                            (s-contains? "." pkg*))
                                        (car (last (split-string pkg* "\\.")))
                                        pkg*)))
                            (or (member pkg (cl-getf (meq/gfp :installed) (meq/inconcat ":" pm))) (executable-find pkg))))
        (meq/rl pkg)))))
(defun meq/install-pipx-package (pkg* &optional injection-env*)
    (let* ((pkg (meq/rs pkg*))
            (injection-env (when injection-env* (meq/rs injection-env*)))
            (pkg-buffer-name (format "*Installing %s%s With pipx*" pkg (if injection-env*
                                                                        (concat " in " injection-env)
                                                                        "")))
            (injection-env-buffer-name (when injection-env* (format "*Installing %s With pipx*" injection-env)))
            (pkg*-list (list pkg*))
            (pkg-installed (apply #'meq/pipx-package-installed (if injection-env*
                                                                    (-snoc pkg*-list injection-env*)
                                                                    pkg*-list)))
            (injection-env-installed (when injection-env* (meq/pipx-package-installed injection-env*))))

        (when injection-env*
            (if (and injection-env-installed (not meq/var/force-bootstrap))
                (message "Not %s; already installed" injection-env-buffer-name)
                (if (member "ignore-this" (mapcar #'meq/rs (meq/rl injection-env*)))
                    (message "Not %s; ignored" injection-env-buffer-name)
                    (message injection-env-buffer-name)
                    (meq/call "pipx" injection-env-buffer-name "install" "--force" injection-env)
                    (message "%s...done" injection-env-buffer-name))))
        (if (and pkg-installed (not meq/var/force-bootstrap))
            (message "Not %s; already installed" pkg-buffer-name)
            (if (member "ignore-this" (mapcar #'meq/rs (meq/rl pkg*)))
                (message "Not %s; ignored" pkg-buffer-name)
                (message pkg-buffer-name)
                (apply #'meq/call "pipx" pkg-buffer-name (if injection-env*
                    (list "inject" "--force" injection-env pkg)
                    (list "install" "--force" pkg)))
                (message "%s...done" pkg-buffer-name)))))
(defun meq/install-package (pkg** &optional pm*)
    (let* ((pm (or pm* meq/var/default-package-manager))
            (pkg* (meq/rs pkg**))
            (pkg (if (string= pm "nix-env") (concat (meq/pm-details pm :channel) "." pkg*) pkg*))
            (buffer-name (format "*Installing %s With %s*" pkg pm)))
        (if (and (meq/package-installed pkg** pm) (not meq/var/force-bootstrap))
            (message "Not %s; already installed" buffer-name)
            (if (member "ignore-this" (mapcar #'meq/rs (meq/rl pkg**)))
                (message "Not %s; ignored" buffer-name)
                (message buffer-name)
                (let* ((install (meq/pm-details pm :install)))
                    (apply #'meq/call (meq/pm-details pm :manager) buffer-name (append (meq/rl install)
                                                        (remove nil (list pkg (when (string= pm "nix-env") "--show-trace"))))))
                (message "%s...done" buffer-name)))))

(defun meq/wrapped-call (buffer-name &rest args)
    (message buffer-name)
    (apply #'meq/call (pop args) (format "*%s*" buffer-name) args)
    (message "%s...done" buffer-name))

(cond (meq/var/phone (let* ((pm* "pkg")
                            (install (meq/pm-details pm* :install))
                            (pm (meq/pm-details pm* :manager)))
                        (meq/wrapped-call "Adding root repo" pm install "root-repo")
                        (meq/wrapped-call "Updating pkgs" pm "update" "-y")
                        (mapc #'meq/install-package (meq/gfp :pkg))
                        (mapc #'(lambda (pkg) (interactive) (meq/install-package pkg "pip")) (meq/gfp :pip))))
    (meq/var/wsl (mapc #'meq/install-package (meq/gfp :base)))
    (t (progn
            (mapc #'meq/install-package (-flatten-n 1 (list
                                                    (meq/gfp :base)
                                                    (meq/gfp :extras))))
            (when meq/var/not-in-terminal (mapc #'meq/install-package (meq/gfp :not-in-terminal)))
            (when meq/var/not-minimal (mapc #'meq/install-package (meq/gfp :not-minimal)))
            (when meq/var/neither (mapc #'meq/install-package (meq/gfp :neither))))))

(message (meq/call "pipx" "*Ensuring Pipx Path*" "ensurepath"))
(mapc #'meq/install-pipx-package (cl-getf (meq/gfp :pipx) :apps))
(let* ((libs (cl-getf (meq/gfp :pipx) :libs)))
    (mapc #'(lambda (lib*) (interactive)
        (mapc #'(lambda (lib) (interactive)
            (meq/install-pipx-package
                lib (meq/keyword-to-symbol-name lib*))) (cl-getf libs lib*))) (map-keys libs))))

(when (> (length command-line-args) 1) (let* ((last-dab (car (last command-line-args))))
                                            (defvar meq/var/last-dab last-dab) ;; substitute-in-file-name
                                            (delete last-dab command-line-args)))
(let* ((testing (meq/ued "testing.aiern.org"))
        (resting (meq/ued "resting.aiern.org"))
        (early-init (meq/ued "early-init.org"))
        (init (meq/ued "init.org"))
        (early-aiern-init (meq/ued "early-init.aiern.org"))
        (aiern-init (meq/ued "init.aiern.org")))
    (if (bound-and-true-p meq/var/last-dab)
        (pcase meq/var/last-dab
            ("--fTest" (setq initial-buffer-choice testing))
            ("--fRest" (setq initial-buffer-choice resting))
            ("--fEarly" (setq initial-buffer-choice early-init))
            ("--fAEarly" (setq initial-buffer-choice early-aiern-init))
            ("--fInit" (setq initial-buffer-choice init))
            ("--fAInit" (setq initial-buffer-choice aiern-init))
            (t (setq initial-buffer-choice (f-full meq/var/last-dab))))
        (setq initial-buffer-choice testing))
    (eval `(add-hook 'kill-emacs-hook #'(lambda nil (interactive)
        ;; Adapted From: http://ergoemacs.org/emacs/elisp_file_name_dir_name.html
        (when (get-file-buffer ,testing) (delete-file ,testing) (copy-file ,resting ,testing))))))
