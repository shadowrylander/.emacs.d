;; EXWM

;; This sets up and requires the [[https://github.com/ch11ng/exwm][Emacs X Window Manager]] by
;; [[https://github.com/ch11ng][Chris Feng]]:


;; [[file:protean.aiern.org::*EXWM][EXWM:1]]
(meq/up exwm
;; EXWM:1 ends here



;; I want to run the following applications on ~EXWM~ startup:
;; - [[https://github.com/alacritty/alacritty][Alacritty]]
;; - [[https://obsidian.md/][Obsidian]]


;; [[file:protean.aiern.org::*EXWM][EXWM:2]]
:init/defun* (post-exwm nil (interactive)
            (unless (get-buffer "Alacritty") (meq/run "alacritty"))
            (unless (get-buffer "obsidian") (meq/run "obsidian")))
:hook (exwm-init . #'post-exwm)
;; EXWM:2 ends here



;; Load the ~fringe~ package and
;; [[https://github.com/lujun9972/el-dmenu/blob/e8cc9b27c79d3ecc252267c082ab8e9c82eab264/dmenu.el][dmenu]]:


;; [[file:protean.aiern.org::*EXWM][EXWM:3]]
:upnsd-preconfig (dmenu) (fringe :config
                    ;; (fringe-mode (quote (1 . 1)) nil (fringe))
                    ;; (fringe-mode '(3 . 0))
                    ;; (fringe-mode 'none)
                    ;; (fringe-mode 1)
                    )
;; EXWM:3 ends here

;; [[file:protean.aiern.org::*EXWM][EXWM:4]]
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
;; EXWM:4 ends here



;; These keybindings will run my major-mode-based deino, i.e. the ~exwm-global~ deino in this case:


;; [[file:protean.aiern.org::*EXWM][EXWM:5]]
([?\s-p] . uru)
([?\s-`] . uru)
([?\s-z] . uru)
;; EXWM:5 ends here



;; Switch buffers quickly:


;; [[file:protean.aiern.org::*EXWM][EXWM:6]]
(,(naked "s-tab") . next-buffer)
(,(naked "s-<iso-lefttab>") . previous-buffer)
(,(naked "M-s-tab") . previous-buffer)
;; EXWM:6 ends here



;; ~[s]uper-q~ will activate my buffer deino:


;; [[file:protean.aiern.org::*EXWM][EXWM:7]]
([?\s-q] . deino-buffer/body)
;; EXWM:7 ends here

;; [[file:protean.aiern.org::*EXWM][EXWM:8]]
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
;; EXWM:8 ends here



;; Here's the global ~exwm~ deino, triggered by [[https://github.com/shadowrylander/uru][uru]]:


;; [[file:protean.aiern.org::*EXWM][EXWM:9]]
:uru (exwm-mode t deino-exwm-global (:color blue) "e g"
        ("`" nil "cancel")
        ("c" exwm-input-release-keyboard "char mode")
        ("l" exwm-input-grab-keyboard "line mode")
        ("r" exwm-reset "reset")
        ("w" exwm-workspace-switch "workspace switch")
        ("i" meq/run-interactive "run")
        ("b" deino-buffer/body "buffers"))
;; EXWM:9 ends here



;; This is my general ~exwm~ deino, leading to all the other ~exwm~ deinos:


;; [[file:protean.aiern.org::*EXWM][EXWM:10]]
:deino (deino-exwm nil "e e"
        ("`" nil "cancel")
        ("XF86PowerOff" deino-exwm/power/body "power")
        ("s" deino-exwm/shells/body "shells"))
;; EXWM:10 ends here



;; I can reboot, power off, and suspend using this one:


;; [[file:protean.aiern.org::*EXWM][EXWM:11]]
(deino-exwm/power (:color blue) "e p"
    ("r" (meq/run "reboot") "reboot")
    ("q" (meq/run "poweroff") "poweroff")
    ("XF86PowerOff" (meq/run "systemctl suspend" "suspend") "suspend"))
;; EXWM:11 ends here



;; And run my shells and terminals using this one:


;; [[file:protean.aiern.org::*EXWM][EXWM:12]]
(deino-exwm/shells (:color blue) "e s" ("a" (meq/run "alacritty") "alacritty"))
;; EXWM:12 ends here



;; I use the power button to trigger the general ~exwm~ deino:


;; [[file:protean.aiern.org::*EXWM][EXWM:13]]
:demon ((naked "XF86PowerOff") 'deino-exwm/body)
;; EXWM:13 ends here



;; And finally: no tiling:


;; [[file:protean.aiern.org::*EXWM][EXWM:14]]
:gsetq (exwm-manage-force-tiling t)
        ;; Adapted From: https://www.reddit.com/r/emacs/comments/8yf6dx/key_chords_in_exwm/
        ;; (exwm-input-line-mode-passthrough t)
)
;; EXWM:14 ends here

;; Startup


;; [[file:protean.aiern.org::*Startup][Startup:1]]
(when (> (length command-line-args) 1) (let* ((last-dab (car (last command-line-args))))
                                            (defvar meq/var/last-dab last-dab)
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
;; Startup:1 ends here
