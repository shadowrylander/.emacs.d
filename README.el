(when (string= (car (last command-line-args)) "--") (delete "--" command-line-args))

(setq gc-cons-threshold most-positive-fixnum)

(setq meq/var/file-name-handler-alist file-name-handler-alist)
(unless (or (daemonp) noninteractive)

    (setq-default file-name-handler-alist nil)

    (defun meq/reset-file-handler-alist-h ()
      (setq file-name-handler-alist

            (delete-dups (append file-name-handler-alist
                                 meq/var/file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'meq/reset-file-handler-alist-h 101))

(defvar meq/var/gc-cons-percentage gc-cons-percentage)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-percentage meq/var/gc-cons-percentage)

            (defun meq/gc-on-lose-focus ()
              (unless (frame-focus-state)
                (garbage-collect)))

            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function #'meq/gc-on-lose-focus))))

(setq-default gc-cons-percentage 0.6)

(setq-default auto-window-vscroll nil
    frame-inhibit-implied-resize t
    inhibit-compacting-font-caches t)
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'view-hello-file 'ignore)
(fset 'display-startup-echo-area-message 'ignore)

(put 'narrow-to-region 'disabled nil)
(put 'up-case-rgion 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . nil) default-frame-alist)
(push '(internal-border . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

(let* ((default-directory (concat user-emacs-directory "siluam")))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

(ignore-errors
    (setq native-comp-async-report-warnings-errors nil)

    (add-to-list 'native-comp-eln-load-path (meq/ued-local "eln-cache/")))

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

;; (setq borg-rewrite-urls-alist '(("git@github.com:" . "https://github.com/")
;;                                 ("git@gitlab.com:" . "https://gitlab.com/")))
(borg-initialize)

(require 'meq)

(setq custom-file (meq/ued "custom.el"))
(meq/cl custom-file)
(setq auto-save-list-file-prefix user-emacs-directory)

(byte-recompile-directory (meq/ued "themes") nil)
(add-to-list 'custom-theme-load-path (meq/ued "themes"))
(setq custom-safe-themes t)

(with-no-warnings
  (setq use-package-verbose t)
  (setq use-package-enable-imenu-support t))
(require 'use-package)

(setq use-package-always-demand (or (meq/item-in-cla "--always-demand") (daemonp)))

(setq use-package-always-defer (meq/item-in-cla "--always-defer"))

(use-package leaf :demand t
    :init (defmacro meq/leaf (&rest args) `(leaf ,@args :require ,(cl-getf args :require t)))
    :config (use-package leaf-keywords :demand t))

(use-package use-package-extras :demand t
    :config (meq/up use-package-ensure-system-package))

(meq/up hydra
    :custom (hydra-hint-display-type 'lv)
    :bind (:map hydra-base-map ("~" . hydra--universal-argument))

    :use-package-preconfig (janus) (use-package-hydra)
    :use-package-postconfig (use-package-deino) (deino :custom (deino-hint-display-type 'lv)))

(meq/up alloy

    :use-package-preconfig (command-log-mode)
        ;; Important: https://github.com/noctuid/general.el/issues/53#issuecomment-307262154
        (use-package-chords)

    :config (alloy-auto-unbind-keys)

        (alloy-def :keymaps demon-run
                ;; Adapted From:
                ;; Answer: https://stackoverflow.com/a/4557027/10827766
                ;; User: https://stackoverflow.com/users/387076/gilles-so-stop-being-evil
                "\eOA" [up]
                "\e[A" [up]
                "\eOB" [down]
                "\e[B" [down]
                "\eOD" [left]
                "\e[D" [left]
                "\eOC" [right]
                "\e[C" [right]
                "M-x" 'meq/M-x
                (alloy-chord "jj") 'universal-argument
                (naked "<tab>") 'org-cycle
                (naked "backtab") 'org-shifttab
            :keymaps 'universal-argument-map (alloy-chord "jj") 'universal-argument-more)

    :custom (alloy-implicit-naked t))

(meq/up uru :config (with-eval-after-load 'prime (prime "u u" uru "uru") (prime "u m" minoru "minoru")))

(meq/up which-key :deino (deino/which-key (:color blue :columns 4) "w"
        ("`" nil "cancel")
        ("a" cosmoem-any-popup-showing-p "any popup showing")
        ("h" meq/which-key--hide-popup "hide-popup")
        ("s" meq/which-key--show-popup "show-popup")
        ("r" meq/which-key--refresh-popup "refresh-popup")
        ("t" meq/toggle-which-key "toggle")
        ("l" meq/which-key-show-top-level "meq/toplevel")
        ("L" which-key-show-top-level "toplevel"))
    :gsetq
        (which-key-enable-extended-define-key t)
        (which-key-idle-delay 0.1)
        (which-key-idle-secondary-delay nil)
        (which-key-allow-evil-operators t)

        ;; NOTE: This will cause the which-key maps for the operator states to show up,
        ;; breaking functionality such as `d 13 <arrow-down>', etc.
        ;; (which-key-show-operator-state-maps t)

        ;; TODO: Choose a fun one!
        (which-key-separator " × ")
        ;; (which-key-separator " |-> ")

        (which-key-popup-type 'side-window)
        (which-key-side-window-location '(right bottom left top))

        ;; If this percentage is too small, the keybindings frame will appear at the bottom
        (which-key-side-window-max-width 0.5)

        (which-key-side-window-max-height 0.25))

(meq/up cosmoem

    :config (with-eval-after-load 'prime
                (prime ", m" map-of-infinity/body "map-of-infinity")
                (meq/which-key-change-ryo "," "damascus"))

    :deino (map-of-infinity nil ", m"
        ("`" nil "cancel")
        ("w" deino/which-key/body "which-key")
        ("h" deino/cosmoem/body "cosmoem")
        ("d" meq/disable-all-modal-modes "disable all modal modes" :color blue)
        ("t" toggles/body "toggles")
        ("k" all-keymaps/body "all keymaps"))

        (deino/cosmoem (:color blue) ", c"
            ("`" nil "cancel")
            ("h" cosmoem-hide-all-modal-modes "hide all modal modes"))

        (toggles (:color blue) ", t" ("`" nil "cancel"))

        (all-keymaps (:color blue) ", k" ("`" nil "cancel")))

(meq/up sorrow :config (with-eval-after-load 'prime (primer+ "t" "toggles"))
    ;; From: https://github.com/shadowrylander/sorrow#which-key-integration
    (push '((nil . "sorrow:.*:") . (nil . "")) which-key-replacement-alist))

(unless (meq/windows-p)
    (meq/up exec-path-from-shell
        :gsetq
            (exec-path-from-shell-check-startup-files nil)
            (exec-path-from-shell-variables '("PATH" "MANPATH" "CACHE_HOME" "FPATH" "PYENV_ROOT"))
            (exec-path-from-shell-arguments '("-l"))
        :config
            (exec-path-from-shell-initialize)))

(meq/up undo-fu :deino (deino-undo nil "u"
        ("u" undo-fu-only-undo "undo")
        ("r" undo-fu-only-redo "redo")
        ("R" undo-fu-only-redo-all "redo all"))
    :upnsd-postconfig
        (undo-fu-session
            :gsetq
              (undo-fu-session-directory (meq/ued-local "undo-fu-session"))
              (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
            :hook (after-init . global-undo-fu-session-mode)))

(meq/up lode)

(meq/up meta)

(meq/up prime)

(meq/up aiern
    :gsetq (aiern-undo-system 'undo-fu aiern-move-beyond-eol t)
    :hook (after-init . (lambda nil (interactive) (meq/add-to-ignored-modal-modes aiern (setq state (list aiern-default-state)))))

    :use-package-preconfig (bind-map)

    :use-package-postconfig (aiern-aiernhide-state)

    :meta-aiern (aiern-normal-state-map) (aiern-insert-state-map)

    ;; :demon
        ;; ((alloy-chord "") 'meq/toggle-aiern-ex-cosmoem)

        ;; TODO
        ;; ((alloy-chord "''") 'aiern-ex)
    :config
        ;; TODO: How do I create a keymap `aiern-ex-keymap' out of the `aiern-ex-commands' alist?

        ;; (cosmoem-def :show-funs #'meq/aiern-ex-cosmoem-show
        ;;     :hide-funs #'meq/aiern-ex-cosmoem-hide
        ;;     :toggle-funs #'meq/aiern-ex-cosmoem-toggle
        ;;     :keymap 'aiern-ex-keymap
        ;;     ;; :transient t
        ;; )

        ;; (defun meq/aiern-ex-cosmoem-toggle nil (interactive))
        ;; (defun meq/aiern-ex-show-top-level nil (interactive)
        ;;     (meq/which-key-show-top-level 'aiern-ex-keymap))

        ;; (defun meq/toggle-aiern-ex (ua) (interactive "p")
        ;;     (if (= ua 4)
        ;;         (funcall 'meq/toggle-inner 'aiern-mode "aiern-ex" (meq/fbatp aiern-mode) 'aiern-ex-keymap nil t)
        ;;         (funcall 'meq/toggle-inner 'aiern-mode "aiern-ex" (meq/fbatp aiern-mode) 'aiern-ex-keymap)))
        ;; (defun meq/toggle-aiern-ex-cosmoem (ua) (interactive "p")
        ;;     (if (= ua 4)
        ;;         (funcall 'meq/toggle-inner 'aiern-mode "aiern-ex" (meq/fbatp aiern-mode) 'aiern-ex-keymap t t)
        ;;         (funcall 'meq/toggle-inner 'aiern-mode "aiern-ex" (meq/fbatp aiern-mode) 'aiern-ex-keymap t)))

    :sorrow ("l" :deino
                '(aiern-exits (:color blue) "e"
                    ;; From: https://github.com/emacs-evil/evil/blob/master/evil-maps.el#L449
                    "A deino for getting the fuck outta' here!"
                    ("`" nil "cancel")
                    ("l" aiern-save-and-quit ":wq")
                    ("p" aiern-quit ":q")
                    ("o" aiern-write ":w")
                    ("O" aiern-write-all ":wa")
                    ;; ("q" (funcall (alloy-simulate-key ":q! <RET>")) ":q!"))
                    ("q" (aiern-quit t) ":q!"))
                :name "aiern exits"))

(meq/up all-the-icons :config
    (ignore-errors
        (set-face-attribute 'default nil :font "Cartograph CF Extra Bold-12")
        (set-face-attribute 'mode-line nil :font "Cartograph CF Extra Bold-12")
        (set-face-attribute 'mode-line-inactive nil :font "Cartograph CF Extra Bold-12")))

(alloy-def :keymaps demon-run "C-tab" 'next-buffer "C-<iso-lefttab>" 'previous-buffer)

(sorrow-key "b" :deino '(deino-buffer (:color red :columns 3) "b"
  "
                Buffers :
  "
  ("`" nil "cancel")
  ("<right>" next-buffer "next")
  ("n" next-buffer "next")
  ("b" ivy-switch-buffer "switch" :color blue)
  ("B" ibuffer "ibuffer" :color blue)
  ("<left>" previous-buffer "prev")
  ("p" previous-buffer "prev")
  ("C-b" buffer-menu "buffer menu" :color blue)
  ("N" evil-buffer-new "new" :color blue)
  ("d" kill-this-buffer "delete")
  ;; don't come back to previous buffer after delete
  ("D" (progn (kill-this-buffer) (next-buffer)) "Delete")
  ("S" save-buffer "save")
  ("s" deino-window/body "window" :color blue)))

(meq/up ivy :sorrow ("x" :deino '(deino-execute (:color blue) "x" "A deino for launching stuff!"
        ("`" nil "cancel")
        ("e" execute-extended-command "M-x")) :name "execute order 65")
    :use-package-preconfig (smex)
    :gsetq (ivy-use-virtual-buffers t))

(meq/up counsel
    :use-package-postconfig (prescient) (ivy-prescient)
    :hook (after-init . (lambda nil (interactive)
                            (ivy-mode 1)
                            (counsel-mode 1)
                            (ivy-prescient-mode 1)
                            (prescient-persist-mode 1)))
    :deino+ (deino-execute nil ("c" counsel-M-x "counsel"))

    ;; Adapted From: https://www.reddit.com/r/emacs/comments/7o1sjq/exwm_rofidmenu_replacement_for_starting/dt0lvkm?utm_source=share&utm_medium=web2x&context=3
    :config (push (concat (getenv "HOME") "/.local/share/applications/") counsel-linux-apps-directories)
    :config/defun* (meq/counsel-linux-app-format-function (name comment exec)
                        "Default Linux application name formatter.
                    NAME is the name of the application, COMMENT its comment and EXEC
                    the command to launch it."
                        (format "% -45s %s"
                            (propertize name 'face 'font-lock-builtin-face)
                            (or comment "")))
   :gsetq (counsel-linux-app-format-function #'meq/counsel-linux-app-format-function))

(meq/upnsd damascus :use-package-postconfig (rainbow-mode :config (rainbow-mode 1)) (help-fns+)

    :deino (deino-universal/shortcuts (:color blue) "d u s"
            "A deino for universal shortcuts!"
            ("`" nil "cancel")
            (";" aiern-ex "aiern-ex")
            (":" evil-ex "evil-ex")
            ("u" uru "uru")
            ("m" minoru "minoru")
            ("w" deino-wb/body "window-buffer deino")
            ;; ("s" meq/shell "shell")
            ("'" aiern-write "save")
            ("RET" aiern-save-and-quit "save and quit"))
        (deino-universal/modal-modes (:color blue) "d u m"
            "A deino for modal modes!"
            ("a" meq/aiern-execute-with-current-bindings "aiern execute")
            ("s" meq/sorrow-execute-with-current-bindings "sorrow execute")
            ("g" meq/god-execute-with-current-bindings "god execute")
            ("r" meq/ryo-execute-with-current-bindings "ruo execute")
            ("`" nil "cancel")
        (deino-universal/major-minor-modes (:color blue) "d u M"
            "A deino for major and minor modes!"
            ("`" nil "cancel")))
        (deino-universal/everything-else (:color blue) "d u e"
            "A deino for everything else!"
            ("`" nil "cancel")
            ("u" deino-undo/body "deino-undo")
            ("RET" meq/end-of-line-and-indented-new-line "indented new line")
            ("r" deino-restart/body "deino-restart"))

    :alloy (:keymaps demon-run
            (alloy-chord ";'") 'deino-universal/shortcuts/body
            (alloy-chord "l;") 'deino-universal/modal-modes/body
            (alloy-chord "kl") 'deino-universal/major-minor-modes/body
            (alloy-chord "jk") 'deino-universal/everything-else/body
            (alloy-chord "hj") 'aiern-exits/body

            "¡" 'ignore "¹" 'ignore "½" 'ignore "⅓" 'ignore "¼" 'ignore "⅛" 'ignore "²" 'ignore "⅜" 'ignore
            "¾" 'ignore "³" 'ignore "⁴" 'ignore "⅚" 'ignore "⁵" 'ignore "⅝" 'ignore "⁶" 'ignore "⅞" 'ignore
            "⁷" 'ignore "⁸" 'ignore "⁹" 'ignore "∅" 'ignore "ⁿ" 'ignore "⁰" 'ignore "·" 'ignore "—" 'ignore
            "∞" 'ignore "≠" 'ignore "≈" 'ignore "ê" 'ignore "é" 'ignore
            "è" 'universal-argument "ë" 'ignore "ē" 'ignore
            "ū" 'ignore "ü" 'ignore "ú" 'ignore "û" 'ignore "ù" 'ignore "ì" 'ignore "ï" 'ignore "í" 'ignore
            "î" 'ignore "ī" 'ignore "ō" 'ignore "œ" 'ignore "ø" 'ignore "õ" 'ignore "ö" 'ignore "ó" 'ignore
            "ô" 'ignore "ò" 'ignore "à" 'ignore "á" 'ignore "â" 'ignore "ä" 'ignore "æ" 'ignore "ã" 'ignore
            "å" 'ignore "ā" 'ignore "ß" 'ignore "ç" 'ignore "ñ" 'ignore "¿" 'ignore

        :keymaps '(override aiern-insert-state-map evil-insert-state-map)
            (naked "C-backspace") 'meq/delete-white-or-word
            (naked "RET") 'newline-and-indent)

    :load-siluam-file-postconfig ("help+20")
    :gsetq
        (indent-tabs-mode nil
            inhibit-startup-screen t
            confirm-kill-emacs nil
            delete-selection-mode 1
            echo-keystrokes .1
            column-number-mode t
            size-indicator-mode t
            user-full-name "Jeet Ray"
            user-mail-address "titaniumfiles@outlook.com"
            scroll-step 1
            scroll-conservatively most-positive-fixnum
            vc-follow-symlinks t)

        (byte-compile-warnings nil)

    :init

        ;; TODO: Use the previous implementation of this to create a version which will use command-line arguments
        ;;       to open specific files, such as this README, protean.aiern.org, meta.aiern.org, settings/README.org, etc. ,
        ;;       in addition to any files called from the command-line itself.
        (let* ((testing (meq/ued "testing.aiern.org"))
                (resting (meq/ued "resting.aiern.org")))
            ;; (setq initial-buffer-choice testing)

            ;; Adapted From:
            ;; Answer: https://emacs.stackexchange.com/a/66329
            ;; User: https://emacs.stackexchange.com/users/26541/hettomei
            (eval `(add-hook 'after-init-hook #'(lambda nil (interactive) (unless (buffer-file-name) (find-file ,testing)))))
            
            (eval `(add-hook 'kill-emacs-hook #'(lambda nil (interactive)
                ;; Adapted From: http://ergoemacs.org/emacs/elisp_file_name_dir_name.html
                (when (get-file-buffer ,testing) (delete-file ,testing) (copy-file ,resting ,testing))))))

        (let* ((loaddefs (meq/ued-lib "org" "lisp" "org-loaddefs.el"))) (when (get-file-buffer loaddefs) (kill-buffer (get-file-buffer loaddefs))))
        (when (get-buffer "*Compile-Log*") (kill-buffer "*Compile-Log*"))
        (when (get-buffer "*Shell Command Output*") (kill-buffer "*Shell Command Output*"))

        ;; This determines the style of line numbers in effect. If set to `nil', line
        ;; numbers are disabled. For relative line numbers, set this to `relative'.
        ;; Adapted From: https://www.reddit.com/r/emacs/comments/8fz6x2/relative_number_with_line_folding/dy7lmh7?utm_source=share&utm_medium=web2x&context=3
        ;; (display-line-numbers-mode t)
        (setq display-line-numbers-type 'relative)

        ;; Adapted From:
        ;; Answer: https://stackoverflow.com/a/50716229/10827766
        ;; User: https://stackoverflow.com/users/1482346/muro
        (global-display-line-numbers-mode t)

        (setq initial-scratch-message "")

        (defun meq/remove-scratch-buffer nil (interactive)
            (when (get-buffer "*scratch*") (kill-buffer "*scratch*")))
        (add-hook 'after-change-major-mode-hook 'meq/remove-scratch-buffer)

        (add-hook 'minibuffer-exit-hook
            '(lambda nil
                (let ((buffer "*Completions*"))
                (and (get-buffer buffer)
                        (kill-buffer buffer)))))

        (setq inhibit-startup-buffer-menu t)

        (add-hook 'window-setup-hook 'delete-other-windows)

        (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

(meq/up dired-sidebar :demon ((alloy-chord "\\\\") 'meq/backslash-toggle)

    :upnsd-preconfig (dired+ :gsetq (diredp-bind-problematic-terminal-keys (display-graphic-p)))

        (dired-details :if (version< emacs-version "24.4"))
        (dired-details+ :if (version< emacs-version "24.4"))

    :gsetq (dired-sidebar-close-sidebar-on-file-open t)
        (dired-details-hidden-string "")

    :uru (dired-mode t deino-dired-mode (:color blue) "d d"
            ("`" nil "cancel")
            ("f" (meq/dired-create-and-open-fell-markdown) "create & edit fell file")
            ("d" (meq/dired-create-and-open-doc-markdown) "create & edit doc file")
            ("F" (meq/dired-create-fell-markdown) "create fell file" :color red)
            ("D" (meq/dired-create-doc-markdown) "create doc file" :color red)))

(use-package doom-aiern-modeline
    :hook (after-init . doom-aiern-modeline-mode)

    :use-package-preconfig (shrink-path)
            (god-mode :upnsd-postconfig (aiern-god-state) (evil-god-state)
                      :config (which-key-enable-god-mode-support))

    :gsetq
        ;; How tall the mode-line should be. It's only respected in GUI.
        ;; If the actual char height is larger, it respects the actual height.
        (doom-aiern-modeline-height 25)

        ;; How wide the mode-line bar should be. It's only respected in GUI.
        (doom-aiern-modeline-bar-width 3)

        ;; The limit of the window width.
        ;; If `window-width' is smaller than the limit, some information won't be displayed.
        (doom-aiern-modeline-window-width-limit fill-column)

        ;; How to detect the project root.
        ;; The default priority of detection is `ffip' > `projectile' > `project'.
        ;; nil means to use `default-directory'.
        ;; The project management packages have some issues on detecting project root.
        ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
        ;; to hanle sub-projects.
        ;; You can specify one if you encounter the issue.
        (doom-aiern-modeline-project-detection 'project)

        ;; Determines the style used by `doom-aiern-modeline-buffer-file-name'.
        ;;
        ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
        ;;   auto => emacs/lisp/comint.el (in a project) or comint.el
        ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
        ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
        ;;   truncate-with-project => emacs/l/comint.el
        ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
        ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
        ;;   truncate-all => ~/P/F/e/l/comint.el
        ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
        ;;   relative-from-project => emacs/lisp/comint.el
        ;;   relative-to-project => lisp/comint.el
        ;;   file-name => comint.el
        ;;   buffer-name => comint.el<2> (uniquify buffer name)
        ;;
        ;; If you are experiencing the laggy issue, especially while editing remote files
        ;; with tramp, please try `file-name' style.
        ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
        (doom-aiern-modeline-buffer-file-name-style 'auto)

        ;; Whether display icons in the mode-line.
        ;; While using the server mode in GUI, should set the value explicitly.
        (doom-aiern-modeline-icon (display-graphic-p))

        ;; Whether display the icon for `major-mode'. It respects `doom-aiern-modeline-icon'.
        (doom-aiern-modeline-major-mode-icon t)

        ;; Whether display the colorful icon for `major-mode'.
        ;; It respects `all-the-icons-color-icons'.
        (doom-aiern-modeline-major-mode-color-icon t)

        ;; Whether display the icon for the buffer state. It respects `doom-aiern-modeline-icon'.
        (doom-aiern-modeline-buffer-state-icon t)

        ;; Whether display the modification icon for the buffer.
        ;; It respects `doom-aiern-modeline-icon' and `doom-aiern-modeline-buffer-state-icon'.
        (doom-aiern-modeline-buffer-modification-icon t)

        ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
        (doom-aiern-modeline-unicode-fallback nil)

        ;; Whether display the minor modes in the mode-line.
        (doom-aiern-modeline-minor-modes nil)

        ;; If non-nil, a word count will be added to the selection-info modeline segment.
        (doom-aiern-modeline-enable-word-count nil)

        ;; Major modes in which to display word count continuously.
        ;; Also applies to any derived modes. Respects `doom-aiern-modeline-enable-word-count'.
        ;; If it brings the sluggish issue, disable `doom-aiern-modeline-enable-word-count' or
        ;; remove the modes from `doom-aiern-modeline-continuous-word-count-modes'.
        (doom-aiern-modeline-continuous-word-count-modes '(
            markdown-mode
            gfm-mode
            org-mode
            outline-mode))

        ;; Whether display the buffer encoding.
        (doom-aiern-modeline-buffer-encoding t)

        ;; Whether display the indentation information.
        (doom-aiern-modeline-indent-info nil)

        ;; If non-nil, only display one number for checker information if applicable.
        (doom-aiern-modeline-checker-simple-format t)

        ;; The maximum number displayed for notifications.
        (doom-aiern-modeline-number-limit 99)

        ;; The maximum displayed length of the branch name of version control.
        (doom-aiern-modeline-vcs-max-length 12)

        ;; Whether display the workspace name. Non-nil to display in the mode-line.
        (doom-aiern-modeline-workspace-name t)

        ;; Whether display the perspective name. Non-nil to display in the mode-line.
        (doom-aiern-modeline-persp-name t)

        ;; If non nil the default perspective name is displayed in the mode-line.
        (doom-aiern-modeline-display-default-persp-name nil)

        ;; If non nil the perspective name is displayed alongside a folder icon.
        (doom-aiern-modeline-persp-icon t)

        ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
        (doom-aiern-modeline-lsp t)

        ;; Whether display the GitHub notifications. It requires `ghub' package.
        (doom-aiern-modeline-github nil)

        ;; The interval of checking GitHub.
        (doom-aiern-modeline-github-interval (* 30 60))

        ;; Whether display the modal state icon.
        ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
        ;; From: https://www.reddit.com/r/emacs/comments/gqc9fm/visual_indication_of_the_mode_of_editing_with_evil/frt8trg?utm_source=share&utm_medium=web2x&context=3
        (doom-aiern-modeline-modal-icon nil)

        ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
        (doom-aiern-modeline-mu4e nil)

        ;; Whether display the gnus notifications.
        (doom-aiern-modeline-gnus t)

        ;; Wheter gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
        (doom-aiern-modeline-gnus-timer 2)

        ;; Wheter groups should be excludede when gnus automatically being updated.
        (doom-aiern-modeline-gnus-excluded-groups '("dummy.group"))

        ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
        (doom-aiern-modeline-irc t)

        ;; Function to stylize the irc buffer names.
        (doom-aiern-modeline-irc-stylize 'identity)

        ;; Whether display the environment version.
        (doom-aiern-modeline-env-version t)
        ;; Or for individual languages
        (doom-aiern-modeline-env-enable-python t)
        (doom-aiern-modeline-env-enable-ruby t)
        (doom-aiern-modeline-env-enable-perl t)
        (doom-aiern-modeline-env-enable-go t)
        (doom-aiern-modeline-env-enable-elixir t)
        (doom-aiern-modeline-env-enable-rust t)

        ;; Change the executables to use for the language version string
        (doom-aiern-modeline-env-python-executable "python") ; or `python-shell-interpreter'
        (doom-aiern-modeline-env-ruby-executable "ruby")
        (doom-aiern-modeline-env-perl-executable "perl")
        (doom-aiern-modeline-env-go-executable "go")
        (doom-aiern-modeline-env-elixir-executable "iex")
        (doom-aiern-modeline-env-rust-executable "rustc")

        ;; What to dispaly as the version while a new one is being loaded
        (doom-aiern-modeline-env-load-string "...")

        ;; Hooks that run before/after the modeline version string is updated
        (doom-aiern-modeline-before-update-env-hook nil)
        (doom-aiern-modeline-after-update-env-hook nil))

(meq/up evil :use-package-preconfig (bind-map)

    :use-package-postconfig (evil-evilified-state)

    :gsetq (evil-escape-key-sequence nil evil-undo-system 'undo-fu evil-move-beyond-eol t)

    :leaf (evil :advice
        (:override evil-insert-state (lambda (&rest args) (interactive)
            (meq/disable-all-modal-modes))))

    ;; :demon
        ;; TODO
        ;; ((alloy-chord "") 'meq/toggle-evil-ex-cosmoem)
    :config
        ;; From: https://www.reddit.com/r/emacs/comments/lp45zd/help_requested_in_configuring_ryomodal/gp3rfx9?utm_source=share&utm_medium=web2x&context=3
        ;; Kept for documentation porpoises
        ;; (eval
        ;;       `(ryo-modal-keys
        ;;             ("l l" ,(alloy-simulate-key ":wq <RET>") :first '(evil-normal-state) :name "wq")
        ;;             ("l p" ,(alloy-simulate-key ":q <RET>") :first '(evil-normal-state) :name "q")
        ;;             ("l o" ,(alloy-simulate-key ":w <RET>") :first '(evil-normal-state) :name "w")
        ;;             ("l q" ,(alloy-simulate-key ":q! <RET>") :first '(evil-normal-state) :name "q!")))

        ;; Use to get command name:
        ;; Eg: (cdr (assoc "q" evil-ex-commands))
        ;; Then "C-x C-e" (eval-last-sexp)

        ;; TODO: How do I create a keymap `evil-ex-keymap' out of the `evil-ex-commands' alist?

        ;; (cosmoem-def :show-funs #'meq/evil-ex-cosmoem-show
        ;;     :hide-funs #'meq/evil-ex-cosmoem-hide
        ;;     :toggle-funs #'meq/evil-ex-cosmoem-toggle
        ;;     :keymap 'evil-ex-keymap
        ;;     ;; :transient t
        ;; )

        ;; (defun meq/evil-ex-cosmoem-toggle nil (interactive))
        ;; (defun meq/evil-ex-show-top-level nil (interactive)
        ;;     (meq/which-key-show-top-level 'evil-ex-keymap))

        ;; (defun meq/toggle-evil-ex (ua) (interactive "p")
        ;;     (if (= ua 4)
        ;;         (funcall 'meq/toggle-inner 'evil-mode "evil-ex" (meq/fbatp evil-mode) 'evil-ex-keymap nil t)
        ;;         (funcall 'meq/toggle-inner 'evil-mode "evil-ex" (meq/fbatp evil-mode) 'evil-ex-keymap)))
        ;; (defun meq/toggle-evil-ex-cosmoem (ua) (interactive "p")
        ;;     (if (= ua 4)
        ;;         (funcall 'meq/toggle-inner 'evil-mode "evil-ex" (meq/fbatp evil-mode) 'evil-ex-keymap t t)
        ;;         (funcall 'meq/toggle-inner 'evil-mode "evil-ex" (meq/fbatp evil-mode) 'evil-ex-keymap t)))
    )

(meq/up olivetti :gsetq (olivetti-body-width 0.60))

(meq/up rainbow-identifiers
    ;; Adapted From:
    ;; Answer: https://stackoverflow.com/a/31253253/10827766
    ;; User: https://stackoverflow.com/users/2698552/chillaranand
    ;; :hook ((buffer-list-update window-configuration-change) . (lambda nil (interactive)
    ;;                                                             (rainbow-identifiers-mode 1)))
    ;; :upnsd-preconfig (xxh)
   )

(meq/up vlf :gsetq (vlf-application 'always))

(meq/up doom-themes
    :deino (deino-themes-light (:color blue) nil "A deino for light themes!" ("`" nil "cancel"))
        (deino-themes-dark (:color blue) nil "A deino for dark themes!" ("`" nil "cancel"))
    :sorrow ("t" :deino '(deino-themes nil "t" "A deino for themes!"
                ("s" meq/switch-theme-mode "switch to light / dark")
                ("l" deino-themes-light/body "light themes")
                ("d" deino-themes-dark/body "dark themes")
                ("`" nil "cancel")))
    :gsetq
        (doom-themes-enable-bold t)
        (doom-themes-enable-italic t)
        (meq/var/default-theme-override nil)
        (meq/var/default-default-theme 'dracula-purple-dark)
    ;; :use-package-postconfig
    ;;     (doom-themes-ext-neotree :config (doom-themes-neotree-config))
    ;;     (doom-themes-ext-org :config (doom-themes-org-config))
    :config
        (unless (meq/which-theme) (cond
            ((member "--purple" command-line-args)
                (delete "--purple" command-line-args)
                (meq/load-theme 'dracula-purple-dark))
            ((member "--orange" command-line-args)
                (delete "--orange" command-line-args)
                (meq/load-theme 'dracula-orange-dark))
            ((member "--red" command-line-args)
                (delete "--red" command-line-args)
                (meq/load-theme 'exo-ui-red-dark))
            ((member "--flamingo" command-line-args)
                (delete "--flamingo" command-line-args)
                (meq/load-theme 'herschel-flamingo-pink-dark))
            ((member "--blue" command-line-args)
                (delete "--blue" command-line-args)
                (meq/load-theme 'st-giles-blue-dark))
            (meq/var/phone (meq/load-theme 'orange-purple-light))
            (meq/var/default-theme-override (meq/load-theme meq/var/default-theme-override))
            (meq/var/current-theme (meq/load-theme meq/var/current-theme))
            ((meq/exwm-p) (meq/load-theme meq/var/default-default-theme))
            (t (meq/load-theme meq/var/default-default-theme))))

        (mapc #'(lambda (theme) (interactive)
            (let* ((name (symbol-name (car theme)))
                    (prefix (symbol-name (cdr theme)))
                    (light (concat name "-light"))
                    (dark (concat name "-dark")))
                (eval `(defdeino+ deino-themes-light nil
                    (,prefix (funcall #'meq/load-theme ',(intern light)) ,light)))
                (eval `(defdeino+ deino-themes-dark nil
                    (,prefix (funcall #'meq/load-theme ',(intern dark)) ,dark)))))
          '((dracula-purple . p)
            (dracula-orange . o)
            (exo-ui-red . r)
            (herschel-flamingo-pink . f)
            (st-giles-blue . b)
            (lio-fotia . l)
            (orange-purple . C-o)
            (flamingo-pink-purple . C-p)
            (ghostfreak-green . g))))

(meq/up windmove
    :config (winner-mode)
    :deino (deino-wb nil nil ("b" deino-buffer/body "buffer") ("w" deino-window/body "window"))

    :sorrow ("w" :deino '(deino-window (:columns 5) "w"
        ("`" nil "cancel")
        ("s" deino-buffer/body "buffer" :color blue)
        ("B" balance-windows "balance-windows")
        ("t" toggle-window-spilt "toggle-window-spilt")
        ("H" shrink-window-horizontally "shrink-window-horizontally")
        ("K" shrink-window "shrink-window")
        ("J" enlarge-window "enlarge-window")
        ("L" enlarge-window-horizontally "enlarge-window-horizontally")
        ("R" reverse-windows "reverse-windows")
        ("h" windmove-left "←")
        ("j" windmove-down "↓")
        ("k" windmove-up "↑")
        ("l" windmove-right "→")
        ("q" deino-move-splitter-left "X←")
        ("w" deino-move-splitter-down "X↓")
        ("e" deino-move-splitter-up "X↑")
        ("r" deino-move-splitter-right "X→")
        ("F" follow-mode "Follow")
        ("v" (lambda nil (interactive) (split-window-right) (windmove-right)) "vertical")
        ("x" (lambda nil (interactive) (split-window-below) (windmove-down)) "horizontal")
        ("d" delete-window "delete")
        ("O" delete-other-windows "only this")
        ("z" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
        ("Z" winner-redo "reset")
        ("o" other-window "other-window"))))

(meq/up ace-window
    :deino+ (deino-window (:color red)
        ("a" (lambda nil (interactive) (ace-window 1) (add-hook 'ace-window-end-once-hook
                                                                'deino-window/body)) "ace 1")
        ("S" (lambda nil (interactive) (ace-window 4) (add-hook 'ace-window-end-once-hook
                                                                'deino-window/body)) "swap")
        ("D" (lambda nil (interactive) (ace-window 16) (add-hook 'ace-window-end-once-hook
                                                                'deino-window/body)) "Delete Other")
        ("E" ace-swap-window "ace-swap-window")
        ("W" ace-delete-window "ace-delete-window" :exit t)))

(meq/upnsd alamode)

(meq/upnsd cosmog :prime ("c" deino-cosmog/body "cosmog"))

(meq/up helm
    ;; :commands (helm-M-x helm-mini helm-mode)
    :deino+ (deino-execute nil
                ("h" helm-smex-major-mode-commands "helm smex major mode")
                ("s" helm-smex "helm smex"))
            (deino-window nil ("B" helm-mini "helm-mini")
                ("f" helm-find-files "helm-find-files"))
    :use-package-postconfig ;; Adapted From: https://github.com/clemera/helm-ido-like-guide
        (helm-smex)
        (helm-flx)
        (helm-swoop)
        (helm-ido-like))

(meq/up magit :deino (deino-magit (:color blue :columns 8) "g"
  "It's just like magit!"
  ("s" magit-status "status")
  ("c" magit-checkout "checkout")
  ("b" magit-branch-manager "branch manager")
  ("m" magit-merge "merge")
  ("l" magit-log "log")
  ("c" magit-git-command "command")
  ("p" magit-process "process")
  ("`" nil "cancel")))

(meq/up modalka)

(meq/up objed)

(meq/up projectile
    :use-package-preconfig (counsel-projectile :config (counsel-projectile-mode 1)) (helm-projectile)
    ;; Adapted From: https://codeberg.org/dr.ops/medusa/src/branch/main/medusa.org#headline-16
    :deino (deino-projectile-other-window (:color teal) "p o"
        "projectile-other-window"
        ("f"  projectile-find-file-other-window        "file")
        ("g"  projectile-find-file-dwim-other-window   "file dwim")
        ("d"  projectile-find-dir-other-window         "dir")
        ("b"  projectile-switch-to-buffer-other-window "buffer")
        ("`"  nil                                      "cancel" :color blue))
    :sorrow ("p" :deino '(deino-projectile
                (:color teal :columns 4) "p p"
                ("a"   counsel-projectile-ag "counsel-projectile-ag")
                ("g"   counsel-projectile-rg "counsel-projectile-rg")
                ("c"   counsel-projectile "counsel-projectile")
                ("b"   counsel-projectile-switch-to-buffer "switch to buffer")
                ("C"   projectile-invalidate-cache "cache clear")
                ("d"   counsel-projectile-find-dir "find-dir")
                ("f"   counsel-projectile-find-file "find-file")
                ("F"   counsel-projectile-find-file-dwim "find-file-dwim")
                ("C-f" projectile-find-file-in-directory "find-file-in-dir")
                ("G"   ggtags-update-tags "update gtags")
                ("i"   projectile-ibuffer "Ibuffer")
                ("K"   projectile-kill-buffers "kill all buffers")
                ("o"   projectile-multi-occur "multi-occur")
                ("p"   counsel-projectile-switch-project "switch project")
                ("r"   projectile-recentf "recent file")
                ("x"   projectile-remove-known-project "remove known project")
                ("X"   projectile-cleanup-known-projects "cleanup non-existing projects")
                ("z"   projectile-cache-current-file "cache current file")
                ("h"   deino-helm-projectile/body "deino-helm-projectile")
                ("O"   deino-projectile-other-window/body "deino-projectile-other-window")
                ("`"   nil "cancel")))
            ("P" :deino '(deino-helm-projectile
                (:color teal :columns 4) "p h"
                ("h"   helm-projectile "helm-projectile")
                ("c"   helm-projectile-switch-project "switch-project")
                ("f"   helm-projectile-find-file "find-file")
                ("F"   helm-projectile-find-file-dwim "find-file-dwim")
                ("d"   helm-projectile-find-dir "find-dir")
                ("r"   helm-projectile-recentf "recent file")
                ("b"   helm-projectile-switch-to-buffer "switch to buffer")
                ("a"   helm-projectile-ag "helm-projectile-ag")
                ("g"   helm-projectile-rg "helm-projectile-rg")
                ("C-f" helm-projectile-find-file-in-known-projects "find file in known projects")
                ("`"   nil "cancel"))))

(meq/up pyvenv :hook (after-init . pyvenv-mode)
               :config (pyvenv-activate (meq/ued ".local" "venv"))
               :gsetq (meq/var/python "python3")
                      (meq/var/hy "hy")
                      (pyvenv-post-activate-hooks (list (lambda () (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")
                                                                         org-babel-hy-command (concat pyvenv-virtual-env "bin/hy")))))
                      (pyvenv-post-deactivate-hooks (list (lambda () (setq python-shell-interpreter meq/var/python
                                                                           org-babel-hy-command meq/var/hy)))))

(meq/up restart-emacs
    :deino (deino-restart (:color blue) "r"
            ("`" nil "cancel")
            ("l" meq/reload-emacs "reload")
            ("s" restart-emacs "restart")))

(meq/up ryo-modal
    :config ;; From: https://github.com/Kungsgeten/ryo-modal#which-key-integration
        (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

(meq/up xah-fly-keys
    :commands xah-fly-keys
    :sorrow ("m" :deino
                '(modal-modes (:color blue) "m"
                    "A modal deino!"
                    ("`" nil "cancel")
                    ("x" meq/toggle-xah "xah-fly-keys")) :name "modal modes"))

(setq show-paren-delay 0)
(add-hook 'after-init-hook #'show-paren-mode)

(meq/up emacs-lisp-mode :use-package-preconfig  (lispy) (sly))

(meq/up titan :gsetq (meq/var/titan-snippets-dir (meq/ued-lib "titan" "snippets")))

(use-package caddyfile-mode :mode ("\\caddyfile\\'"))

(use-package dockerfile-mode :mode ("\\Dockerfile\\'"))

(use-package hy-mode
    :commands (org-babel-execute:hy)
    :mode ("\\.hy\\'")
    :use-package-preconfig (ob-hy :commands (org-babel-execute:hy)))

(use-package systemd-mode :mode ("\\.service\\'"))

(eval `(use-package markdown-mode :mode ,(meq/titan-append-modes "markdown" "\\.md\\'")))

(use-package nix-mode
    :commands (org-babel-execute:nix)
    :mode ("\\.nix\\'")
    :init/defun*
        (org-babel-execute:nix (body params)
            "Execute a block of Nix code with org-babel."
            (message "executing Nix source code block")
            (let ((E (cdr (assoc :E params)))
                (in-file (unless E (org-babel-temp-file "n" ".nix")))
                (show-trace (cdr (assoc :show-trace params)))
                (json (cdr (assoc :json params)))
                (xml (cdr (assoc :xml params))))
            (unless E (with-temp-file in-file (insert body)))
            (org-babel-eval
                (format "nix-instantiate --read-write-mode --eval %s %s %s %s"
                    (if show-trace "--show-trace" "")
                    (if json "--json" "")
                    (if xml "--xml" "")
                    (if E (format "-E '%s'" body) (org-babel-process-file-name in-file)))
            ""))))

(use-package ob-python :commands (org-babel-execute:python))

(use-package ob-shell :commands (org-babel-execute:shell))

(use-package vimrc-mode
    :commands (org-babel-execute:vimrc)
    :mode "\\.vim\\(rc\\)?\\'")

(use-package xonsh-mode
    :commands (org-babel-execute:xonsh)
    :mode ("\\.xonshrc\\'" "\\.xsh\\'")
    :init/defun*
        (org-babel-execute:xonsh (body params)
            "Execute a block of Xonsh code with org-babel."
            (message "executing Xonsh source code block")
            (let ((in-file (org-babel-temp-file "x" ".xsh"))
                (opts (or (cdr (assoc :opts params)) nil))
                (args (or (cdr (assoc :args params)) nil)))
            (with-temp-file in-file
                (insert body))
            (org-babel-eval
                (format "xonsh %s %s %s"
                    (if (eq opts nil) "" opts)
                    (if (eq args nil) "" args)
                    (org-babel-process-file-name in-file))
            ""))))

(use-package text-mode
    :commands (org-babel-execute:text)
    :mode ("\\.txt\\'")
    :init/defun*
        (org-babel-execute:text (body params)
            "Return a block of text with org-babel."
            (message "returning text source code block")
            (let ((in-file (org-babel-temp-file "t" ".txt")))
            (with-temp-file in-file (insert body))
            (org-babel-eval (format "%s %s"
                                    (if meq/var/windows "type" "cat")
                                    (org-babel-process-file-name in-file)) ""))))

(use-package doc
    :commands (meq/dired-create-doc-markdown meq/dired-create-and-open-doc-markdown)
    :gsetq (meq/var/doc-snippets-dir (meq/ued-lib "doc" "snippets"))
    :uru (doc-org-mode nil deino-doc-org (:color blue :inherit (deino-org-usually/heads)) "t d o"
            ("d" (meq/insert-snippet "org titan template") "template")))

(use-package fell
    :commands (meq/dired-create-fell-markdown meq/dired-create-and-open-fell-markdown)
    :gsetq (meq/var/fell-snippets-dir (meq/ued-lib "fell" "snippets"))
    :uru (fell-org-mode nil deino-fell-org (:color blue :inherit (deino-org-usually/heads)) "t f o"
            ("f" (meq/insert-snippet "org titan template") "template")))

(eval `(use-package org

    :mode ,(meq/titan-append-modes "org" '("\\.org\\'" . org-mode))

    :use-package-postconfig (org-contrib)

        (ox-pandoc :upnsd-postconfig (riot :if (not (meq/item-in-cla "--anti-riot")))
            :deino (deino-ob-export-as (:color blue) "o e a"
                    ("`" nil "cancel")
                    ("a" org-pandoc-export-as-asciidoc "asciidoc")
                    ("g" org-pandoc-export-as-gfm "gfm")
                    ("h" org-pandoc-export-as-html5 "html5")
                    ("l" org-pandoc-export-as-latex "latex"))
                (deino-ob-export-to (:color blue) "o e t"
                    ("`" nil "cancel")
                    ("a" org-pandoc-export-to-asciidoc "asciidoc")
                    ("d" org-pandoc-export-to-docx "docx")
                    ("o" org-pandoc-export-to-odt "odt")
                    ("g" org-pandoc-export-to-gfm "gfm")
                    ("h" org-pandoc-export-to-html5 "html5")
                    ("l" org-pandoc-export-to-latex "latex"))
                (deino-ob-export-and-open (:color blue) "o e o"
                    ("`" nil "cancel")
                    ("a" org-pandoc-export-to-asciidoc-and-open "asciidoc")
                    ("g" org-pandoc-export-to-gfm-and-open "gfm")
                    ("h" org-pandoc-export-to-html5-and-open "html5")
                    ("l" org-pandoc-export-to-latex-and-open "latex"))
                (deino-ob-export (:color blue) "o e e"
                    ("`" nil "cancel")
                    ("a" deino-ob-export-as/body "export as")
                    ("t" deino-ob-export-to/body "export to")
                    ("o" deino-ob-export-and-open/body "export and open"))

            :config/defun* (meq/org-pandoc-export-advice (format a s v b e &optional buf-or-open)
                "General interface for Pandoc Export.
                If BUF-OR-OPEN is nil, output to file.  0, then open the file.
                t means output to buffer."
                (unless (derived-mode-p 'org-mode)
                    (error "You must run this command in org-mode or its derived major modes."))
                (unless (executable-find org-pandoc-command)
                    (error "Pandoc (version 1.12.4 or later) can not be found."))
                (setq org-pandoc-format format)
                (org-export-to-file 'pandoc (org-export-output-file-name
                                            (concat (make-temp-name ".tmp") ".org") s)
                    a s v b e (lambda (f) (org-pandoc-run-to-buffer-or-file f format s buf-or-open))))
            :leaf (ox-pandoc :advice (:override org-pandoc-export meq/org-pandoc-export-advice)))

        (yasnippet :config (add-to-list 'yas-snippet-dirs (meq/ued "snippets") t)
            :deino (deino-yasnippet (:color blue :hint nil) "y"
                "
                            ^YASnippets^
                --------------------------------------------
                Modes:    Load/Visit:    Actions:

                _g_lobal  _d_irectory    _i_nsert
                _m_inor   _f_ile         _t_ryout
                _e_xtra   _l_ist         _n_ew
                        _a_ll
                "
                ("d" yas-load-directory)
                ("e" yas-activate-extra-mode)
                ("i" yas-insert-snippet)
                ("f" yas-visit-snippet-file :color blue)
                ("n" yas-new-snippet)
                ("t" yas-tryout-snippet)
                ("l" yas-describe-tables)
                ("g" yas/global-mode)
                ("m" yas/minor-mode)
                ("a" yas-reload-all)))

    :config (load (meq/ued-settings "org-export-functions"))
            ;; (setq toggle-debug-on-error t)
        ;; (setq auto-mode-alist (append auto-mode-alist (meq/titan-append-modes org ("\\.org\\'" . org-mode))))

            (defun meq/org-html-export-to-as-html (func &rest args) (let (org-confirm-babel-evaluate) (apply func args)))
            (advice-add #'org-html-export-to-html :around #'meq/org-html-export-to-as-html)
            (advice-add #'org-html-export-as-html :around #'meq/org-html-export-to-as-html)

    :meta (org-mode-map)
    :meta-rename (org-mode-map "ESC" "org-metadir")
    :minoru (org-src-mode deino-edit-spc (:color blue) "o s"
            ("`" nil "cancel")
            ("i" meq/narrow-or-widen-dwim "narrow")
            ("x" org-edit-special "org edit special")

            ("s" org-edit-src-save "save")
            ("e" org-edit-src-exit "exit")
            ("a" org-edit-src-abort "abort"))

    :uru (org-mode nil deino-org (:color blue) "o o"
            "A deino for org-mode!"
            ("`" nil "cancel")
            ("t" org-babel-tangle "tangle")
            ("a" meq/org-babel-tangle-append "tangle append")
            ("F" org-babel-tangle-file "tangle file")
            ("n" meq/narrow-or-widen-dwim "narrow")
            ("s" org-edit-special "org edit special")
            ("e" deino-ob-export/body "export")
            ("g" meq/go-to-parent "go to parent")
            ("l" org-toggle-link-display "toggle link display")
            ("c" meq/org-custom-id-get-create "create uuid for heading CUSTOM_ID property")
            ("i" org-id-get-create "create uuid for heading ID property"))
    :gsetq
        ;; I'm using ox-pandoc
        ;; (org-export-backends '(md gfm latex odt org))
        (org-directory "/tmp")
        (org-roam-directory org-directory)
        (org-descriptive-links nil)
        (org-startup-folded t)
        ;; (org-src-window-setup 'current-window)
        ;; (org-cycle-emulate-tab 'whitestart)
        (org-support-shift-select t)
        ;; (org-src-tab-acts-natively t)

        (org-edit-src-content-indentation 0)))
