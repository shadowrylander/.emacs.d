;; Startup

;; Remove ~--~ from scripts:


;; [[file:~/.emacs.d/README.org::*Startup][Startup:1]]
(when (string= (car (last command-line-args)) "--") (delete "--" command-line-args))
;; Startup:1 ends here

;; Optimizations
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Startup optimizations from [[https://github.com/hlissner][Henrik Lissner's]] [[https://github.com/hlissner/doom-emacs/blob/develop/early-init.el][Doom Emacs' ~early-init.el~]]:

;; #+begin_quote
;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.
;; #+end_quote

;; #+begin_quote
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*Optimizations][Optimizations:1]]
(setq gc-cons-threshold most-positive-fixnum)
;; Optimizations:1 ends here



;; And for the ~file-name-handler-alist~:


;; [[file:~/.emacs.d/README.org::*Optimizations][Optimizations:2]]
(setq meq/var/file-name-handler-alist file-name-handler-alist)
(unless (or (daemonp) noninteractive)
;; Optimizations:2 ends here



;; #+begin_quote
;; `file-name-handler-alist' is consulted on each `require', `load' and
;; various path/io functions. You get a minor speed up by unsetting this.
;; Some warning, however: this could cause problems on builds of Emacs where
;; its site lisp files aren't byte-compiled and we're forced to load the
;; *.el.gz files (e.g. on Alpine).
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*Optimizations][Optimizations:3]]
(setq-default file-name-handler-alist nil)
;; Optimizations:3 ends here



;; #+begin_quote
;; ...but restore `file-name-handler-alist' later, because it is needed for
;; handling encrypted or compressed files, among other things.
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*Optimizations][Optimizations:4]]
(defun meq/reset-file-handler-alist-h ()
  (setq file-name-handler-alist
;; Optimizations:4 ends here



;; #+begin_quote
;; Merge instead of overwrite because there may have bene changes to
;; `file-name-handler-alist' since startup we want to preserve.
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*Optimizations][Optimizations:5]]
(delete-dups (append file-name-handler-alist
                             meq/var/file-name-handler-alist))))
(add-hook 'emacs-startup-hook #'meq/reset-file-handler-alist-h 101))
;; Optimizations:5 ends here



;; If I ever need it, this will give me the initial directory I was in; the code is adapted from [[https://emacs.stackexchange.com/users/1979/stefan][Stefan's]] answer [[https://emacs.stackexchange.com/a/31662/31428][here]]:


;; [[file:~/.emacs.d/README.org::*Optimizations][Optimizations:6]]
(setq meq/var/initial-directory default-directory)
;; Optimizations:6 ends here



;; The next few bits are adapted from [[https://www.reddit.com/r/emacs/comments/dppmqj/do_i_even_need_to_leverage_earlyinitel_if_i_have/?utm_source=amp&utm_medium=&utm_content=post_body][here]], with a few quotes from myself and others scattered here and there,
;; such as this bit [[https://www.reddit.com/r/emacs/comments/41m7x3/why_are_you_changing_gcconsthreshold/cz3t775?utm_source=share&utm_medium=web2x&context=3][about ~gc-cons-percentage~]]:

;; #+begin_quote
;; ... There's also gc-cons-percentage which performs a gc if the amount of new memory used as a percentage
;; of the total has increased by a certain amount.
;; If you set gc-cons-threshold to a large number that effectively puts gc-cons-percentage into the driving seat.
;; The default gc-cons-threshold is 400000 bytes, not 800000. ...
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*Optimizations][Optimizations:7]]
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
;; Optimizations:7 ends here

;; [[file:~/.emacs.d/README.org::*Optimizations][Optimizations:8]]
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
;; Optimizations:8 ends here

;; Libraries
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Byte-compile the library directories and add them to the load-path now; the following bits are adapted from [[https://emacs.stackexchange.com/users/14825/nickd][NickD's]] answer [[https://emacs.stackexchange.com/a/55415/31428][here]],
;; and [[https://www.emacswiki.org/emacs/LoadPath#h5o-2][from this section of the Emacs Wiki]].


;; [[file:~/.emacs.d/README.org::*Libraries][Libraries:1]]
(let* ((default-directory (concat user-emacs-directory "siluam")))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
;; Libraries:1 ends here

;; Native Comp
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; These are two settings I like for ~native compilation~, adapted from [[https://github.com/daviwil/dotfiles/blob/master/Emacs.org#native-compilation][here]]:

;; #+begin_quote
;; Silence compiler warnings as they can be pretty disruptive
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*Native Comp][Native Comp:1]]
(ignore-errors
    (setq native-comp-async-report-warnings-errors nil)
;; Native Comp:1 ends here



;; #+begin_quote
;; Set the right directory to store the native comp cache
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*Native Comp][Native Comp:2]]
(add-to-list 'native-comp-eln-load-path (meq/ued-local "eln-cache/")))
;; Native Comp:2 ends here

;; We are Borg.
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Assimilate the packages from my ~.gitmodules~ file:

;; # with the help of [[https://stackoverflow.com/a/28686228/10827766][this]], by [[https://stackoverflow.com/users/596361/mirzhan-irkegulov][Mirzhan Irkegulov]]:


;; [[file:~/.emacs.d/README.org::*We are Borg.][We are Borg.:1]]
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
;; We are Borg.:1 ends here



;; Wake up the borg:


;; [[file:~/.emacs.d/README.org::*We are Borg.][We are Borg.:2]]
(setq borg-rewrite-urls-alist '(("git@github.com:" . "https://github.com/")
                                ("git@gitlab.com:" . "https://gitlab.com/")))
(borg-initialize)
;; We are Borg.:2 ends here



;; /Finally/ activate my function library:


;; [[file:~/.emacs.d/README.org::*We are Borg.][We are Borg.:3]]
(require 'meq)
;; We are Borg.:3 ends here

;; Custom
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; As adapted from [[https://emacs.stackexchange.com/users/2731/ebpa][ebpa's]] answer [[https://emacs.stackexchange.com/a/18682/31428][here]]:


;; [[file:~/.emacs.d/README.org::*Custom][Custom:1]]
(setq custom-file (meq/ued "custom.el"))
(meq/cl custom-file)
(setq auto-save-list-file-prefix user-emacs-directory)
;; Custom:1 ends here

;; Themes
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*Themes][Themes:1]]
(byte-recompile-directory (meq/ued "themes") nil)
(add-to-list 'custom-theme-load-path (meq/ued "themes"))
(setq custom-safe-themes t)
;; Themes:1 ends here

;; use-package
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; [[https://github.com/jwiegley/use-package][use-package]] with [[https://github.com/jwiegley][John Wiegley]]:


;; [[file:~/.emacs.d/README.org::*use-package][use-package:1]]
(with-no-warnings
  (setq use-package-verbose t)
  (setq use-package-enable-imenu-support t))
(require 'use-package)
;; use-package:1 ends here



;; Search the ~command-line-args~ list for the ~--always-demand~ argument and set ~use-package-always-demand~ accordingly,
;; then delete the argument from the list; also set the variable if Emacs is running as a daemon.


;; [[file:~/.emacs.d/README.org::*use-package][use-package:2]]
(setq use-package-always-demand (or (meq/item-in-cla "--always-demand") (daemonp)))
;; use-package:2 ends here

;; Sometimes defer package loading

;; Quoted from [[https://github.com/jwiegley/use-package#loading-packages-in-sequence][Use-Package's Loading packages in sequence]]:

;; #+begin_quote
;; NOTE: pay attention if you set use-package-always-defer to t, and also use the :after keyword, as you will need to specify how the
;; declared package is to be loaded: e.g., by some :bind. If you're not using one of the mechanisms that registers autoloads, such as
;; :bind or :hook, and your package manager does not provide autoloads, it's possible that without adding :defer 2 to those declarations,
;; your package will never be loaded.
;; #+end_quote

;; Quoted from [[https://github.com/jwiegley/use-package#notes-about-lazy-loading][Use-Package's Notes about lazy loading]]:

;; #+begin_quote
;; In almost all cases you don't need to manually specify :defer t. This is implied whenever :bind or :mode or :interpreter is used.
;; Typically, you only need to specify :defer if you know for a fact that some other package will do something to cause your package to
;; load at the appropriate time, and thus you would like to defer loading even though use-package isn't creating any autoloads for you.
;; You can override package deferral with the :demand keyword. Thus, even if you use :bind, using :demand will force loading to occur
;; immediately and not establish an autoload for the bound key.
;; #+end_quote

;; Quoted from [[https://github.com/jwiegley/use-package#modes-and-interpreters][Use-Package's Modes and interpreters]]:

;; #+begin_quote
;; Similar to :bind, you can use :mode and :interpreter to establish a deferred binding within the auto-mode-alist and interpreter-mode-alist variables.
;; ...
;; If you aren't using :commands, :bind, :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter, or :hook
;; (all of which imply :defer; see the docstring for use-package for a brief description of each), you can still defer loading with the :defer keyword...
;; #+end_quote

;; Quoted from [[https://github.com/jwiegley/use-package#magic-handlers][Use-Package's Magic handlers]]:

;; #+begin_quote
;; Similar to :mode and :interpreter, you can also use :magic and :magic-fallback to cause certain function to be run if the beginning of a file matches
;; a given regular expression.
;; ...
;; This registers an autoloaded command for pdf-view-mode, defers loading of pdf-tools, and runs pdf-view-mode if the beginning of a buffer matches the string "%PDF".
;; #+end_quote

;; Quoted from [[https://github.com/Kungsgeten/ryo-modal#use-package-keyword][RYO-Modal's Use-package keyword]]:

;; #+begin_quote
;; Ryo-modal also provides a use-package keyword: :ryo, which is similar to :bind in that it implies :defer t and create autoloads for the bound commands.
;; The keyword is followed by one or more key-binding commands, using the same syntax as used by ryo-modal-keys...
;; #+end_quote

;; Quoted from [[https://github.com/noctuid/general.el#use-package-keywords][General's Use-package Keywords]]:

;; #+begin_quote
;; :general is similar to :bind in that it implies :defer t whenever there are bound commands that can be autoloaded
;; (e.g. it will not imply :defer t if the only bound command is to a lambda, for example). Whenever autoloadable commands are bound,
;; use-package will create autoloads for them (though this is usually not necessary).
;; #+end_quote

;; Quoted from [[https://github.com/noctuid/general.el#ghook-keyword][General's :ghook Keyword]]:

;; #+begin_quote
;; :ghook is intended to be used to add a package’s minor mode enabling function to a user-specified hook, so that when hook is run,
;; the package will be loaded and the mode enabled. This means that :ghook will usually imply :defer t. While it does not always imply :defer t,
;; it will add any non-lambda functions to :commands (this is the same behavior as :hook).
;; Though this is usually unnecessary (the commands probably already have autoloads), it will in turn imply :defer t.
;; #+end_quote

;; Quoted from [[https://github.com/noctuid/general.el#gfhook-keyword][General's :gfhook Keyword]]:

;; #+begin_quote
;; Unlike :ghook, :gfhook never adds functions to :commands and therefore never implies :defer t.
;; This is because the functions specified are ones that should be run when turning on (or toggling) the mode(s) the package provides.
;; The specified functions are external to the package, could be called elsewhere, and therefore should not trigger the package to load.
;; #+end_quote

;; Also see [[https://github.com/jwiegley/use-package/issues/738#issuecomment-447631609][this comment]].

;; Note that I assume that [[https://github.com/jwiegley/use-package#use-package-chords][chords]] also defer and create autoloads.

;; And in my experience... Not a good idea; much too confusing. Use [[https://www.reddit.com/r/emacs/comments/j2xezg/usepackage_best_practices/][the arguments here]] to decide whether to use this or ~:defer <n>~ instead.


;; [[file:~/.emacs.d/README.org::*Sometimes defer package loading][Sometimes defer package loading:1]]
(setq use-package-always-defer (meq/item-in-cla "--always-defer"))
;; Sometimes defer package loading:1 ends here

;; extras

;; This sets up [[https://github.com/conao3/leaf.el][leaf.el]] by [[https://github.com/conao3][Naoya Yamashita]]:


;; [[file:~/.emacs.d/README.org::*extras][extras:1]]
(use-package leaf :demand t
    :init (defmacro meq/leaf (&rest args) `(leaf ,@args :require ,(cl-getf args :require t)))
    :config (use-package leaf-keywords :demand t))
;; extras:1 ends here



;; And then [[https://github.com/shadowrylander/use-package-extras][use-package-extras]] by yours truely:


;; [[file:~/.emacs.d/README.org::*extras][extras:2]]
(use-package use-package-extras :demand t
    :config (meq/up use-package-ensure-system-package))
;; extras:2 ends here

;; hydra
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; This sets up [[https://github.com/abo-abo/hydra][hydra]] by [[https://github.com/abo-abo][Oleh Krehel]], as well as its ~use-package~ keywords:


;; [[file:~/.emacs.d/README.org::*hydra][hydra:1]]
(meq/up hydra
    :custom (hydra-hint-display-type 'lv)
    :bind (:map hydra-base-map ("~" . hydra--universal-argument))
;; hydra:1 ends here



;; This bit sets up the following:
;; - [[https://github.com/shadowrylander/janus][janus]] by yours truely
;; - [[https://gitlab.com/to1ne/use-package-hydra][use-package-hydra]] by [[https://gitlab.com/to1ne][to1ne]]
;; - [[https://github.com/shadowrylander/use-package-deino][use-package-deino]] by yours truely
;; - [[https://github.com/shadowrylander/deino][deino]], forked from
;; [[https://github.com/abo-abo/hydra][hydra]] by [[https://github.com/abo-abo][Oleh Krehel]]


;; [[file:~/.emacs.d/README.org::*hydra][hydra:2]]
:use-package-preconfig (janus) (use-package-hydra)
:use-package-postconfig (use-package-deino) (deino :custom (deino-hint-display-type 'lv)))
;; hydra:2 ends here

;; alloy
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Here is the configuration for [[https://github.com/shadowrylander/alloy][alloy]], forked from [[https://github.com/noctuid/general.el][general.el]] by [[https://github.com/noctuid][Fox Kiester]]:


;; [[file:~/.emacs.d/README.org::*alloy][alloy:1]]
(meq/up alloy
;; alloy:1 ends here



;; This sets up the following:
;; - [[https://github.com/shadowrylander/lode][lode]] by yours truely! :D
;; - [[https://github.com/shadowrylander/prime][prime]] by yours truely! :D
;; - [[https://github.com/lewang/command-log-mode][command-log-mode]] by [[https://github.com/lewang][Le Wang]]
;; - [[https://github.com/waymondo/use-package-chords][use-package-chords]] by [[https://github.com/waymondo][justin talbott]]


;; [[file:~/.emacs.d/README.org::*alloy][alloy:2]]
:use-package-preconfig (command-log-mode)
    ;; Important: https://github.com/noctuid/general.el/issues/53#issuecomment-307262154
    (use-package-chords)
;; alloy:2 ends here



;; I don't like having to unbind keys before reassigning them:


;; [[file:~/.emacs.d/README.org::*alloy][alloy:3]]
:config (alloy-auto-unbind-keys)
;; alloy:3 ends here

;; [[file:~/.emacs.d/README.org::*alloy][alloy:5]]
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
    (alloy-chord "  ") 'universal-argument)
;; alloy:5 ends here



;; And finally, this allows ~alloy~ to assume ~kbd~ is being used, or in this case, [[https://www.emacswiki.org/emacs/naked.el][naked]]:


;; [[file:~/.emacs.d/README.org::*alloy][alloy:6]]
:custom (alloy-implicit-naked t))
;; alloy:6 ends here

;; uru
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; This sets up my package [[https://github.com/shadowrylander/uru][uru]], which activates ~deinos~ based on the current major-mode, as mentioned in [[https://codeberg.org/dr.ops/medusa/src/branch/main/medusa.org#headline-4][magic medusa hydra]], by [[https://codeberg.org/dr.ops][Andy Drop]]:


;; [[file:~/.emacs.d/README.org::*uru][uru:1]]
(meq/up uru :demon ((alloy-chord "uu") 'uru (alloy-chord "ii") 'minoru)
    :config (with-eval-after-load 'prime (prime "u u" uru "uru") (prime "u m" minoru "minoru")))
;; uru:1 ends here

;; which-key
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; The incredible [[https://github.com/justbur/emacs-which-key][emacs-which-key]] by the incredible [[https://github.com/justbur][Justin Burkett]]:


;; [[file:~/.emacs.d/README.org::*which-key][which-key:1]]
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
;; which-key:1 ends here

;; cosmoem
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; [[https://bulbapedia.bulbagarden.net/wiki/Nebby][Nebby]]
;; [[./nebby.webp]]

;; Meet the [[https://gitlab.com/shadowrylander/cosmoem][cosmoem]] named [[https://bulbapedia.bulbagarden.net/wiki/Nebby][Nebby]], forked from [[https://gitlab.com/jjzmajic/hercules.el][hercules.el]] by [[https://gitlab.com/jjzmajic][jjzmajic]]:


;; [[file:~/.emacs.d/README.org::*cosmoem][cosmoem:1]]
(meq/up cosmoem
;; cosmoem:1 ends here

;; [[file:~/.emacs.d/README.org::*cosmoem][cosmoem:2]]
:config (with-eval-after-load 'prime
            (prime ", m" map-of-infinity/body "map-of-infinity")
            (meq/which-key-change-ryo "," "damascus"))
;; cosmoem:2 ends here



;; This ~deino~ leads to a bunch of other useful ~deinos~, as well as a few useful functions:


;; [[file:~/.emacs.d/README.org::*cosmoem][cosmoem:3]]
:deino (map-of-infinity nil ", m"
    ("`" nil "cancel")
    ("w" deino/which-key/body "which-key")
    ("h" deino/cosmoem/body "cosmoem")
    ("d" meq/disable-all-modal-modes "disable all modal modes" :color blue)
    ("t" toggles/body "toggles")
    ("k" all-keymaps/body "all keymaps"))
;; cosmoem:3 ends here



;; The ~deino~ for this package:


;; [[file:~/.emacs.d/README.org::*cosmoem][cosmoem:4]]
(deino/cosmoem (:color blue) ", c"
    ("`" nil "cancel")
    ("h" cosmoem-hide-all-modal-modes "hide all modal modes"))
;; cosmoem:4 ends here



;; A ~deino~ for all my modal-mode toggles:


;; [[file:~/.emacs.d/README.org::*cosmoem][cosmoem:5]]
(toggles (:color blue) ", t" ("`" nil "cancel"))
;; cosmoem:5 ends here



;; A ~deino~ for all my modal-mode keymaps:


;; [[file:~/.emacs.d/README.org::*cosmoem][cosmoem:6]]
(all-keymaps (:color blue) ", k" ("`" nil "cancel")))
;; cosmoem:6 ends here

;; sorrow
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Finally, [[https://github.com/shadowrylander/sorrow][sorrow]], forked from [[https://github.com/Kungsgeten/ryo-modal][ryo-modal]] by [[https://github.com/Kungsgeten][Erik Sjöstrand]]:


;; [[file:~/.emacs.d/README.org::*sorrow][sorrow:1]]
(meq/up sorrow :demon ((alloy-chord "kk") 'meq/sorrow-execute-with-current-bindings)
    :config (with-eval-after-load 'prime (primer+ "t" "toggles"))
            ;; From: https://github.com/shadowrylander/sorrow#which-key-integration
            (push '((nil . "sorrow:.*:") . (nil . "")) which-key-replacement-alist))
;; sorrow:1 ends here

;; exec-path-from-shell
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Unless I'm on Windows or a DOS-based OS, I'll need to make sure every executable available on my ~$PATH~ can be found by Emacs as well, using
;; [[https://github.com/purcell/exec-path-from-shell][exec-path-from-shell]] by [[https://github.com/purcell][Steve Purcell]]:


;; [[file:~/.emacs.d/README.org::*exec-path-from-shell][exec-path-from-shell:1]]
(unless (meq/windows-p)
    (meq/up exec-path-from-shell
        :gsetq
            (exec-path-from-shell-check-startup-files nil)
            (exec-path-from-shell-variables '("PATH" "MANPATH" "CACHE_HOME" "FPATH" "PYENV_ROOT"))
            (exec-path-from-shell-arguments '("-l"))
        :config
            (exec-path-from-shell-initialize)))
;; exec-path-from-shell:1 ends here

;; undo-fu
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Set up [[https://github.com/emacsmirror/undo-fu][undo-fu]] and [[https://github.com/emacsmirror/undo-fu-session][undo-fu-session]]:


;; [[file:~/.emacs.d/README.org::*undo-fu][undo-fu:1]]
(meq/up undo-fu
    :demon ((alloy-chord "ui") 'deino-undo/body)
    :deino (deino-undo nil "u"
            ("u" undo-fu-only-undo "undo")
            ("r" undo-fu-only-redo "redo")
            ("R" undo-fu-only-redo-all "redo all"))
    :upnsd-postconfig
        (undo-fu-session
            :gsetq
              (undo-fu-session-directory (meq/ued-local "undo-fu-session"))
              (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
            :hook (after-init . global-undo-fu-session-mode)))
;; undo-fu:1 ends here

;; lode
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Set up [[https://github.com/shadowrylander/lode][lode]] by yours truely:


;; [[file:~/.emacs.d/README.org::*lode][lode:1]]
(meq/up lode)
;; lode:1 ends here

;; meta
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Set up [[https://github.com/shadowrylander/lode][meta]] by yours truely:


;; [[file:~/.emacs.d/README.org::*meta][meta:1]]
(meq/up meta)
;; meta:1 ends here

;; prime
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Set up [[https://github.com/shadowrylander/lode][prime]] by yours truely:


;; [[file:~/.emacs.d/README.org::*prime][prime:1]]
(meq/up prime)
;; prime:1 ends here

;; aiern
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; [[https://github.com/shadowrylander/aiern][aiern]] is my version of [[https://github.com/emacs-evil/evil][evil]]; this first bit of config will enable ~aiern~ on initialization and add it to the list of ignored modal-modes,
;; such that it isn't disabled by ~meq/disable-all-modal-modes~:


;; [[file:~/.emacs.d/README.org::*aiern][aiern:1]]
(meq/up aiern
    :gsetq (aiern-undo-system 'undo-fu)
    :hook (after-init . (lambda nil (interactive)
        (setq state (list aiern-default-state))
        (aiern-mode 1)
        (push 'aiern-mode meq/var/ignored-modal-modes)
        (push "aiern" meq/var/ignored-modal-prefixes)))
;; aiern:1 ends here



;; Both ~evil~ and ~aiern~ require [[https://github.com/justbur/emacs-bind-map][emacs-bind-map]], by [[https://github.com/justbur][Justin Burkett]]:


;; [[file:~/.emacs.d/README.org::*aiern][aiern:2]]
:use-package-preconfig (bind-map)
;; aiern:2 ends here



;; More aiern, courtesy of [[https://github.com/shadowrylander/aiern-aiernhide-state][this]], [[https://github.com/mohsenil85/evil-evilified-state][this]], and [[https://github.com/syl20bnr/spacemacs][this]]:


;; [[file:~/.emacs.d/README.org::*aiern][aiern:3]]
:use-package-postconfig (aiern-aiernhide-state)
;; aiern:3 ends here



;; The ~:meta-aiern~ keyword, from my very own [[https://github.com/shadowrylander/meta][meta]] package, creates a hydra out of the keymaps passed to it,
;; in this case being ~aiern-normal-state-map~ and ~aiern-insert-state-map~:


;; [[file:~/.emacs.d/README.org::*aiern][aiern:4]]
:meta-aiern (aiern-normal-state-map) (aiern-insert-state-map)
;; aiern:4 ends here

;; [[file:~/.emacs.d/README.org::*aiern][aiern:5]]
:demon
    ((alloy-chord "jj") 'meq/aiern-execute-with-current-bindings)
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
;; aiern:5 ends here



;; And here are the ~aiern~ bindings set in the ~sorrow~ modal-mode:


;; [[file:~/.emacs.d/README.org::*aiern][aiern:6]]
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
;; aiern:6 ends here

;; all-the-icons
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; I use [[https://connary.com/cartograph.html][Cartograph]] by [[https://connary.com/index.html][Connary Fagen, Inc.]], but I got it cheaper [[https://www.fontspring.com/fonts/connary-fagen-type-design/cartograph-cf][here]]; the second site often has sales on fonts.


;; [[file:~/.emacs.d/README.org::*all-the-icons][all-the-icons:1]]
(meq/up all-the-icons :config
    (ignore-errors
        (set-face-attribute 'default nil :font "Cartograph CF Extra Bold-12")
        (set-face-attribute 'mode-line nil :font "Cartograph CF Extra Bold-12")
        (set-face-attribute 'mode-line-inactive nil :font "Cartograph CF Extra Bold-12")))
;; all-the-icons:1 ends here

;; buffer
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; This binds ~ctrl-tab~ and ~ctrl-shift-tab~ to buffer-cycling motions:


;; [[file:~/.emacs.d/README.org::*buffer][buffer:1]]
(alloy-def :keymaps demon-run "C-tab" 'next-buffer "C-<iso-lefttab>" 'previous-buffer)
;; buffer:1 ends here



;; And this is mostly adapted from [[https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/][here]]:


;; [[file:~/.emacs.d/README.org::*buffer][buffer:2]]
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
;; buffer:2 ends here

;; ivy
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; From the [[https://github.com/abo-abo/swiper][swiper]] package by [[https://github.com/abo-abo][Oleh Krehel]]:


;; [[file:~/.emacs.d/README.org::*ivy][ivy:1]]
(meq/up ivy :sorrow ("x" :deino '(deino-execute (:color blue) "x" "A deino for launching stuff!"
        ("`" nil "cancel")
        ("e" execute-extended-command "M-x")) :name "execute order 65")
    :use-package-preconfig (smex)
    :gsetq (ivy-use-virtual-buffers t))
;; ivy:1 ends here

;; counsel
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; From the [[https://github.com/abo-abo/swiper][swiper]] package by [[https://github.com/abo-abo][Oleh Krehel]]:


;; [[file:~/.emacs.d/README.org::*counsel][counsel:1]]
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
;; counsel:1 ends here

;; damascus
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Set up the base of this config, including the [[https://github.com/emacsmirror/rainbow-mode][rainbow-mode]] package:


;; [[file:~/.emacs.d/README.org::*damascus][damascus:1]]
(meq/upnsd damascus :use-package-postconfig (rainbow-mode :config (rainbow-mode 1)) (help-fns+)
;; damascus:1 ends here



;; These are keys primarily accessible for me on Android:


;; [[file:~/.emacs.d/README.org::*damascus][damascus:2]]
:alloy (:keymaps demon-run
          "¡" 'ignore "¹" 'ignore "½" 'ignore "⅓" 'ignore "¼" 'ignore "⅛" 'ignore "²" 'ignore "⅜" 'ignore
          "¾" 'ignore "³" 'ignore "⁴" 'ignore "⅚" 'ignore "⁵" 'ignore "⅝" 'ignore "⁶" 'ignore "⅞" 'ignore
          "⁷" 'ignore "⁸" 'ignore "⁹" 'ignore "∅" 'ignore "ⁿ" 'ignore "⁰" 'ignore "·" 'ignore "—" 'ignore
          "∞" 'ignore "≠" 'ignore "≈" 'ignore "ê" 'ignore "é" 'ignore "è" 'ignore "ë" 'ignore "ē" 'ignore
          "ū" 'ignore "ü" 'ignore "ú" 'ignore "û" 'ignore "ù" 'ignore "ì" 'evil-ex "ï" 'ignore "í" 'aiern-ex
          "î" 'ignore "ī" 'ignore "ō" 'ignore "œ" 'ignore "ø" 'ignore "õ" 'ignore "ö" 'ignore "ó" 'ignore
          "ô" 'ignore "ò" 'ignore "à" 'ignore "á" 'ignore "â" 'ignore "ä" 'ignore "æ" 'ignore "ã" 'ignore
          "å" 'ignore "ā" 'ignore "ß" 'ignore "ç" 'ignore "ñ" 'ignore "¿" 'ignore
      :keymaps 'universal-argument-map (alloy-chord "  ") 'universal-argument-more
      :keymaps '(override aiern-insert-state-map evil-insert-state-map)
        (naked "RET") 'newline-and-indent
        (alloy-chord ";'") 'meq/end-of-line-and-indented-new-line)
;; damascus:2 ends here



;; Load the latest help package, and set a few self-describing variables:


;; [[file:~/.emacs.d/README.org::*damascus][damascus:4]]
:load-siluam-file-postconfig ("help+20")
:gsetq
    (indent-tabs-mode nil
        confirm-kill-emacs nil
        echo-keystrokes .1
        column-number-mode t
        size-indicator-mode t
        user-full-name "Jeet Ray"
        user-mail-address "aiern@protonmail.com"
        vc-follow-symlinks t)
;; damascus:4 ends here



;; Do not show byte-compiler warnings, from [[https://emacs.stackexchange.com/a/19507][this answer]] by [[https://emacs.stackexchange.com/users/50/malabarba][Malabarba]]:


;; [[file:~/.emacs.d/README.org::*damascus][damascus:5]]
(byte-compile-warnings nil)
;; damascus:5 ends here

;; [[file:~/.emacs.d/README.org::*damascus][damascus:7]]
:init
    ;; This determines the style of line numbers in effect. If set to `nil', line
    ;; numbers are disabled. For relative line numbers, set this to `relative'.
    ;; Adapted From: https://www.reddit.com/r/emacs/comments/8fz6x2/relative_number_with_line_folding/dy7lmh7?utm_source=share&utm_medium=web2x&context=3
    ;; (display-line-numbers-mode t)
    (setq display-line-numbers-type 'relative)
;; damascus:7 ends here

;; [[file:~/.emacs.d/README.org::*damascus][damascus:8]]
;; Adapted From:
;; Answer: https://stackoverflow.com/a/50716229/10827766
;; User: https://stackoverflow.com/users/1482346/muro
(global-display-line-numbers-mode t)
;; damascus:8 ends here



;; The foloowing few pieces are adapted from [[https://unix.stackexchange.com/users/72170/ole][Ole's]] answer [[https://unix.stackexchange.com/a/152151][here]], with his comments quoted as well:

;; #+begin_quote
;; Makes *scratch* empty.
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*damascus][damascus:9]]
(setq initial-scratch-message "")
;; damascus:9 ends here



;; #+begin_quote
;; Removes *scratch* from buffer after the mode has been set.
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*damascus][damascus:10]]
(defun meq/remove-scratch-buffer nil (interactive)
    (when (get-buffer "*scratch*") (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'meq/remove-scratch-buffer)
;; damascus:10 ends here



;; #+begin_quote
;; Removes *Completions* from buffer after you've opened a file.
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*damascus][damascus:12]]
(add-hook 'minibuffer-exit-hook
    '(lambda nil
        (let ((buffer "*Completions*"))
        (and (get-buffer buffer)
                (kill-buffer buffer)))))
;; damascus:12 ends here



;; #+begin_quote
;; Don't show *Buffer list* when opening multiple files at the same time.
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*damascus][damascus:13]]
(setq inhibit-startup-buffer-menu t)
;; damascus:13 ends here



;; #+begin_quote
;; Show only one active window when opening multiple files at the same time.
;; #+end_quote


;; [[file:~/.emacs.d/README.org::*damascus][damascus:14]]
(add-hook 'window-setup-hook 'delete-other-windows)
;; damascus:14 ends here



;; And finally, make emacs fullscreen, from [[https://emacs.stackexchange.com/users/253/dan][Dan's]] answer [[https://emacs.stackexchange.com/a/3017/31428][here]]:


;; [[file:~/.emacs.d/README.org::*damascus][damascus:16]]
(add-to-list 'default-frame-alist '(fullscreen . fullboth)))
;; damascus:16 ends here

;; dired-sidebar
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Set up [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html][dired]] and its [[https://github.com/jojojames/dired-sidebar][sidebar]], by [[https://github.com/jojojames][James]]:


;; [[file:~/.emacs.d/README.org::*dired-sidebar][dired-sidebar:1]]
(meq/up dired-sidebar :demon ((alloy-chord "\\\\") 'meq/backslash-toggle)
;; dired-sidebar:1 ends here



;; For some reason, on terminal interfaces, the arrow keys in ~dired~ tried to change ownership of file;
;; this was resolved using the following bit, adapted from [[https://www.reddit.com/r/emacs/comments/pce8f4/dired_ellipses_in_square_brackets_before_file/hakuehb/?utm_medium=android_app&utm_source=share&context=3][here]]:


;; [[file:~/.emacs.d/README.org::*dired-sidebar][dired-sidebar:2]]
:upnsd-preconfig (dired+ :gsetq (diredp-bind-problematic-terminal-keys (display-graphic-p)))
;; dired-sidebar:2 ends here



;; Since ~dired-details~ is already implemented in ~dired~ from Emacs version 24.4, we'll enable it only for prior versions:


;; [[file:~/.emacs.d/README.org::*dired-sidebar][dired-sidebar:3]]
(dired-details :if (version< emacs-version "24.4"))
(dired-details+ :if (version< emacs-version "24.4"))
;; dired-sidebar:3 ends here



;; I don't quite like the ~dired-sidebar~ open all the time, so I close it on opening a file from it;
;; also, no hidden details hint, courtesy of [[https://www.emacswiki.org/emacs/DiredDetails#h5o-1][the Emacs wiki]]:


;; [[file:~/.emacs.d/README.org::*dired-sidebar][dired-sidebar:4]]
:gsetq (dired-sidebar-close-sidebar-on-file-open t)
    (dired-details-hidden-string "")
;; dired-sidebar:4 ends here



;; When in the ~dired~ major mode or derived major modes, [[https://github.com/shadowrylander/uru][uru]] will allow me to quickly create, and optionally open,
;; Zettelkasten files for my novel and documentation:


;; [[file:~/.emacs.d/README.org::*dired-sidebar][dired-sidebar:5]]
:uru (dired-mode t deino-dired-mode (:color blue) "d d"
        ("`" nil "cancel")
        ("f" (meq/dired-create-and-open-fell-markdown) "create & edit fell file")
        ("d" (meq/dired-create-and-open-doc-markdown) "create & edit doc file")
        ("F" (meq/dired-create-fell-markdown) "create fell file" :color red)
        ("D" (meq/dired-create-doc-markdown) "create doc file" :color red)))
;; dired-sidebar:5 ends here

;; doom-aiern-modeline
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; This sets up [[https://github.com/shadowrylander/doom-aiern-modeline][my fork]] of [[https://github.com/seagle0128/doom-modeline][doom-modeline]] by [[https://github.com/seagle0128][Vincent Zhang]] upon initialization:


;; [[file:~/.emacs.d/README.org::*doom-aiern-modeline][doom-aiern-modeline:1]]
(use-package doom-aiern-modeline
    :hook (after-init . doom-aiern-modeline-mode)
;; doom-aiern-modeline:1 ends here



;; Then this sets up [[https://github.com/emacsorphanage/god-mode][god-mode]], [[https://github.com/gridaphobe/evil-god-state][evil-god-state]] by [[https://github.com/gridaphobe][Eric Seidel]], and my fork of it [[https://github.com/shadowrylander/aiern-god-state][aiern-god-state]]


;; [[file:~/.emacs.d/README.org::*doom-aiern-modeline][doom-aiern-modeline:2]]
:use-package-preconfig (shrink-path)
        (god-mode :demon ((alloy-chord "hh") 'meq/god-execute-with-current-bindings)
            :upnsd-postconfig (aiern-god-state) (evil-god-state)
            :config (which-key-enable-god-mode-support))
;; doom-aiern-modeline:2 ends here



;; Most of the following is adapted from [[https://github.com/seagle0128/doom-aiern-modeline#customize][here]]:


;; [[file:~/.emacs.d/README.org::*doom-aiern-modeline][doom-aiern-modeline:3]]
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
;; doom-aiern-modeline:3 ends here

;; evil
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; As mentioned before, both [[https://github.com/emacs-evil/evil][~evil~]] and [[https://github.com/shadowrylander/aiern][~aiern~]] require [[https://github.com/justbur/emacs-bind-map][emacs-bind-map]], by [[https://github.com/justbur][Justin Burkett]]:


;; [[file:~/.emacs.d/README.org::*evil][evil:1]]
(meq/up evil :use-package-preconfig (bind-map)
;; evil:1 ends here



;; More evil, courtesy of [[https://github.com/mohsenil85/evil-evilified-state][this]] and [[https://github.com/syl20bnr/spacemacs][this]]:


;; [[file:~/.emacs.d/README.org::*evil][evil:2]]
:use-package-postconfig (evil-evilified-state)
;; evil:2 ends here



;; Disable the ~evil-escape-key-sequence~, and set the ~evil-undo-system~ to [[https://github.com/emacsmirror/undo-fu][~undo-fu~]]


;; [[file:~/.emacs.d/README.org::*evil][evil:3]]
:gsetq (evil-escape-key-sequence) (evil-undo-system 'undo-fu)
;; evil:3 ends here



;; This allows me to disable ~evil-insert-state~:


;; [[file:~/.emacs.d/README.org::*evil][evil:4]]
:leaf (evil :advice
    (:override evil-insert-state (lambda (&rest args) (interactive)
        (meq/disable-all-modal-modes))))
;; evil:4 ends here

;; [[file:~/.emacs.d/README.org::*evil][evil:5]]
;; :demon
    ;; TODO
    ;; ((alloy-chord "\"\"") 'evil-ex)
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
;; evil:5 ends here

;; olivetti
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Zen-mode with [[https://github.com/rnkn/olivetti][olivetti]], by [[https://github.com/rnkn][Paul W. Rankin]]:


;; [[file:~/.emacs.d/README.org::*olivetti][olivetti:1]]
(meq/up olivetti :gsetq (olivetti-body-width 0.60))
;; olivetti:1 ends here

;; rainbow-identifiers
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Colorful words with [[https://github.com/Fanael/rainbow-identifiers][rainbow-identifiers]], by [[https://github.com/Fanael][Fanael Linithien]]:


;; [[file:~/.emacs.d/README.org::*rainbow-identifiers][rainbow-identifiers:1]]
(meq/up rainbow-identifiers
    ;; Adapted From:
    ;; Answer: https://stackoverflow.com/a/31253253/10827766
    ;; User: https://stackoverflow.com/users/2698552/chillaranand
    ;; :hook ((buffer-list-update window-configuration-change) . (lambda nil (interactive)
    ;;                                                             (rainbow-identifiers-mode 1)))
    ;; :upnsd-preconfig (xxh)
   )
;; rainbow-identifiers:1 ends here

;; vlf
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; View Large Files with [[https://github.com/m00natic/vlfi][vlfi]], by [[https://github.com/m00natic][Andrey Kotlarski]]


;; [[file:~/.emacs.d/README.org::*vlf][vlf:1]]
(meq/up vlf :gsetq (vlf-application 'always))
;; vlf:1 ends here

;; doom-themes
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*doom-themes][doom-themes:1]]
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
    :use-package-postconfig
        (doom-themes-ext-neotree :config (doom-themes-neotree-config))
        (doom-themes-ext-org :config (doom-themes-org-config))
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
;; doom-themes:1 ends here

;; windmove
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*windmove][windmove:1]]
(meq/up windmove
    :config (winner-mode)
    :demon ((alloy-chord "ww") 'deino-wb/body)
    :deino (deino-wb nil nil ("b" deino-buffer/body "buffer") ("w" deino-window/body "window"))
;; windmove:1 ends here



;; The ~sorrow~ config below is adapted from
;; [[https://github.com/abo-abo/hydra/wiki/Window-Management#deluxe-window-moving][here]]:


;; [[file:~/.emacs.d/README.org::*windmove][windmove:2]]
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
;; windmove:2 ends here

;; ace-window
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*ace-window][ace-window:1]]
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
;; ace-window:1 ends here

;; alamode
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*alamode][alamode:1]]
(meq/upnsd alamode)
;; alamode:1 ends here

;; cosmog
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*cosmog][cosmog:1]]
(meq/upnsd cosmog :prime ("c" deino-cosmog/body "cosmog"))
;; cosmog:1 ends here

;; helm
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*helm][helm:1]]
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
;; helm:1 ends here

;; magit
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Adapted From: https://github.com/asok/.emacs.d/blob/master/inits/init-hydra.el#L62


;; [[file:~/.emacs.d/README.org::*magit][magit:1]]
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
;; magit:1 ends here

;; modalka
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*modalka][modalka:1]]
(meq/up modalka)
;; modalka:1 ends here

;; objed
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*objed][objed:1]]
(meq/up objed)
;; objed:1 ends here

;; projectile
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Adapted From: https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/


;; [[file:~/.emacs.d/README.org::*projectile][projectile:1]]
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
;; projectile:1 ends here

;; restart-emacs
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:

;; Cool [[https://github.com/iqbalansari/restart-emacs][package]] by
;; [[https://github.com/iqbalansari][Iqbal Ansari]]!


;; [[file:~/.emacs.d/README.org::*restart-emacs][restart-emacs:1]]
(meq/up restart-emacs
    :demon ((alloy-chord "aa") 'deino-restart/body)
    :deino (deino-restart (:color blue) "r"
            ("`" nil "cancel")
            ("l" meq/reload-emacs "reload")
            ("s" restart-emacs "restart")))
;; restart-emacs:1 ends here

;; ryo modal
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*ryo modal][ryo modal:1]]
(meq/up ryo-modal :demon ((alloy-chord "KK") 'meq/ryo-execute-with-current-bindings)
    :config ;; From: https://github.com/Kungsgeten/ryo-modal#which-key-integration
        (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))
;; ryo modal:1 ends here

;; xah-fly-keys
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*xah-fly-keys][xah-fly-keys:1]]
(meq/up xah-fly-keys
    :commands xah-fly-keys
    :sorrow ("m" :deino
                '(modal-modes (:color blue) "m"
                    "A modal deino!"
                    ("`" nil "cancel")
                    ("x" meq/toggle-xah "xah-fly-keys")) :name "modal modes"))
;; xah-fly-keys:1 ends here

;; show-paren-mode
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle yes
;; :END:


;; [[file:~/.emacs.d/README.org::*show-paren-mode][show-paren-mode:1]]
(meq/up show-paren-mode
    :commands show-paren-mode
    :gsetq (show-paren-delay 0)
    :hook (after-init . show-paren-mode))
;; show-paren-mode:1 ends here

;; titan

;; Set up my super major-mode [[https://github.com/shadowrylander/titan][titan]]:


;; [[file:~/.emacs.d/README.org::*titan][titan:1]]
(meq/up titan :gsetq (meq/var/titan-snippets-dir (meq/ued-lib "titan" "snippets")))
;; titan:1 ends here

;; caddyfile-mode

;; For [[https://github.com/Schnouki/caddyfile-mode][caddyfiles]]:


;; [[file:~/.emacs.d/README.org::*caddyfile-mode][caddyfile-mode:1]]
(use-package caddyfile-mode :mode ("\\caddyfile\\'"))
;; caddyfile-mode:1 ends here

;; dockerfile-mode

;; For [[https://github.com/spotify/dockerfile-mode][dockerfiles]]:


;; [[file:~/.emacs.d/README.org::*dockerfile-mode][dockerfile-mode:1]]
(use-package dockerfile-mode :mode ("\\Dockerfile\\'"))
;; dockerfile-mode:1 ends here

;; hy-mode

;; For [[https://github.com/hylang/hy-mode][hy]], plus [[https://github.com/brantou/ob-hy][ob-hy]]:


;; [[file:~/.emacs.d/README.org::*hy-mode][hy-mode:1]]
(use-package hy-mode
    :mode ("\\.hy\\'")
    :use-package-preconfig (lispy) (sly)
        (ob-hy :commands (org-babel-execute:hy)))
;; hy-mode:1 ends here

;; markdown-mode

;; For [[https://jblevins.org/projects/markdown-mode/][markdown]]:


;; [[file:~/.emacs.d/README.org::*markdown-mode][markdown-mode:1]]
(eval `(use-package markdown-mode :mode ,(meq/titan-append-modes "markdown" "\\.md\\'")))
;; markdown-mode:1 ends here

;; nix-mode

;; For [[https://github.com/NixOS/nix-mode][nix]], with ~org-babel-execute:nix~ coming from [[https://emacs.stackexchange.com/users/20061/zeta][Zeta's]] answer [[https://emacs.stackexchange.com/a/61442][here]]:


;; [[file:~/.emacs.d/README.org::*nix-mode][nix-mode:1]]
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
;; nix-mode:1 ends here

;; python


;; [[file:~/.emacs.d/README.org::*python][python:1]]
(use-package ob-python :commands (org-babel-execute:python))
;; python:1 ends here

;; shell


;; [[file:~/.emacs.d/README.org::*shell][shell:1]]
(use-package ob-shell :commands (org-babel-execute:shell))
;; shell:1 ends here

;; vimrc-mode

;; For [[https://github.com/mcandre/vimrc-mode][vimrc]]:


;; [[file:~/.emacs.d/README.org::*vimrc-mode][vimrc-mode:1]]
(use-package vimrc-mode
    :commands (org-babel-execute:vimrc)
    :mode "\\.vim\\(rc\\)?\\'")
;; vimrc-mode:1 ends here

;; xonsh-mode

;; For [[https://github.com/seanfarley/xonsh-mode][xonsh]], again with ~org-babel-execute:xonsh~ coming from [[https://emacs.stackexchange.com/users/20061/zeta][Zeta's]] answer [[https://emacs.stackexchange.com/a/61442][here]]:


;; [[file:~/.emacs.d/README.org::*xonsh-mode][xonsh-mode:1]]
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
;; xonsh-mode:1 ends here

;; doc

;; Set up documentation super major-mode [[https://github.com/shadowrylander/doc][doc]]:


;; [[file:~/.emacs.d/README.org::*doc][doc:1]]
(use-package doc
    :commands (meq/dired-create-doc-markdown meq/dired-create-and-open-doc-markdown)
    :gsetq (meq/var/doc-snippets-dir (meq/ued-lib "doc" "snippets"))
    :uru (doc-org-mode nil deino-doc-org (:color blue :inherit (deino-org-usually/heads)) "t d o"
            ("d" (meq/insert-snippet "org titan template") "template")))
;; doc:1 ends here

;; fell

;; Set up novel's super major-mode [[https://github.com/shadowrylander/fell][fell]]:


;; [[file:~/.emacs.d/README.org::*fell][fell:1]]
(use-package fell
    :commands (meq/dired-create-fell-markdown meq/dired-create-and-open-fell-markdown)
    :gsetq (meq/var/fell-snippets-dir (meq/ued-lib "fell" "snippets"))
    :uru (fell-org-mode nil deino-fell-org (:color blue :inherit (deino-org-usually/heads)) "t f o"
            ("f" (meq/insert-snippet "org titan template") "template")))
;; fell:1 ends here

;; org-mode

;; The one and only [[https://orgmode.org/][org-mode]], with [[https://orgmode.org/worg/org-contrib/][org-contrib]]:
;; - Here's the [[https://git.savannah.gnu.org/cgit/emacs/org-mode.git/][git repo]]
;; - Here's the [[https://git.sr.ht/~bzg/org-contrib][contrib repo]]


;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:1]]
(eval `(use-package org
;; org-mode:1 ends here



;; ~org-mode~ will activate when any files using the [[https://github.com/shadowrylander/titan][~titan-org~]] super major-mode,
;; or with the extension ~.org~, are opened:


;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:2]]
:mode ,(meq/titan-append-modes "org" '("\\.org\\'" . org-mode))
;; org-mode:2 ends here



;; I use [[https://www.reddit.com/r/emacs/comments/6klewl/comment/djniygy/?utm_source=share&utm_medium=web2x&context=3][this]] to skip over expanding any sub-headers:


;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:3]]
:hook (org-cycle . (lambda (state) (interactive) (when (eq state 'children) (setq org-cycle-subtree-status 'subtree))))
;; org-mode:3 ends here

;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:4]]
:use-package-postconfig (org-contrib)
;; org-mode:4 ends here



;; This sets up [[https://github.com/kawabata/ox-pandoc][ox-pandoc]] by [[https://github.com/kawabata][kawabata]], for exporting files using [[https://pandoc.org/][pandoc]], and my [[https://github.com/shadowrylander/riot][riot]] package, inspired by [[https://github.com/tecosaur][tecosaur's]] package [[https://github.com/tecosaur/org-pandoc-import][org-pandoc-import]];
;; it essentially converts any files convertable by ~pandoc~ to an ~org~ file upon opening it, and then converts it back to the original format
;; on saving the file.


;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:5]]
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
;; org-mode:5 ends here



;; I have advised the ~org-pandoc-export~ function to allow derived modes of ~org-mode~ as well, to account for my super major-modes, such as [[https://github.com/shadowrylander/titan][~titan~]], [[https://github.com/shadowrylander/fell][~fell~]], [[https://github.com/shadowrylander/doc][~doc~]], etc.


;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:6]]
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
;; org-mode:6 ends here



;; Set up [[https://github.com/joaotavora/yasnippet][yasnippet]] by [[https://github.com/joaotavora][João Távora]], with the ~deino~ coming from [[https://github.com/abo-abo/hydra/wiki/YASnippet][here]]:


;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:7]]
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
;; org-mode:7 ends here

;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:8]]
:config (load (meq/ued-settings "org-tangle-functions"))
    ;; (setq auto-mode-alist (append auto-mode-alist (meq/titan-append-modes org ("\\.org\\'" . org-mode))))
:demon (
    ;; (naked "backtab") 'evil-close-fold
    (alloy-chord "bb") 'org-toggle-link-display)
:meta (org-mode-map)
:meta-rename (org-mode-map "ESC" "org-metadir")
:minoru (org-src-mode deino-edit-spc (:color blue) "o s"
        ("`" nil "cancel")
        ("i" meq/narrow-or-widen-dwim "narrow")
        ("x" org-edit-special "org edit special")
;; org-mode:8 ends here



;; The following commands are from [[https://github.com/bzg/org-mode/blob/main/lisp/org-src.el][this file]]:
;; - ~org-edit-src-save~ on [[https://github.com/bzg/org-mode/blob/main/lisp/org-src.el#L1222][Line 1222]]
;; - ~org-edit-src-exit~ on [[https://github.com/bzg/org-mode/blob/main/lisp/org-src.el#L1254][Line 1254]]
;; - ~org-edit-src-abort~ on [[https://github.com/bzg/org-mode/blob/main/lisp/org-src.el#L1207][Line 1207]]


;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:9]]
("s" org-edit-src-save "save")
("e" org-edit-src-exit "exit")
("a" org-edit-src-abort "abort"))
;; org-mode:9 ends here

;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:10]]
:uru (org-mode nil deino-org (:color blue) "o o"
        "A deino for org-mode!"
        ("`" nil "cancel")
        ("t" org-babel-tangle "tangle")
        ("a" meq/org-babel-tangle-append "tangle append")
        ("F" org-babel-tangle-file "tangle file")
        ("n" meq/narrow-or-widen-dwim "narrow")
        ("s" org-edit-special "org edit special")
        ("e" deino-ob-export/body "export"))
:gsetq
    ;; I'm using ox-pandoc
    ;; (org-export-backends '(md gfm latex odt org))
    (org-directory "/tmp")
    (org-roam-directory org-directory)
    (org-descriptive-links nil)
    (org-startup-folded t)
    (org-src-fontify-natively t)
    ;; (org-src-window-setup 'current-window)
    (org-cycle-emulate-tab 'whitestart)
;; org-mode:10 ends here



;; Upon exiting ~org-src-mode~ I don't want any indentation added to my code blocks, so I use [[https://emacs.stackexchange.com/users/29861/doltes][doltes's]] answer [[https://emacs.stackexchange.com/a/60638/31428][here]]:


;; [[file:~/.emacs.d/README.org::*org-mode][org-mode:11]]
(org-edit-src-content-indentation 0)))
;; org-mode:11 ends here
