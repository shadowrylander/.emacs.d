{
    nixConfig = {
        # Adapted From: https://github.com/divnix/digga/blob/main/examples/devos/flake.nix#L4
        # extra-substituters = "https://cache.nixos.org/ https://nix-community.cachix.org/";
        trusted-substituters = "https://cache.nixos.org/";
        # extra-trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
        trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
        # keep-derivations = true;
        # keep-outputs = true;
        extra-experimental-features = "nix-command flakes";
        # accept-flake-config = true;
        # show-trace = true;
        # fallback = true;
        # auto-optimise-store = true;
        # builders-use-substitutes = true;
        # cores = 0;
        # flake-registry = https://raw.githubusercontent.com/sylvorg/settings/main/flake-registry.json;
        # allow-unsafe-native-code-during-evaluation = true;
        # min-free = 262144000;
        # max-free = 1073741824;
    };
    description = "My emacs config!";
    inputs = rec {
        settings.url = github:sylvorg/settings;
        titan.follows = "settings/titan";
        nixpkgs.follows = "settings/nixpkgs";

        epkg-aiern.url = github:syvlorg/aiern;
        epkg-aiern-aiernhide-state.url = github:syvlorg/aiern-aiernhide-state;
        epkg-aiern-god-state.url = github:syvlorg/aiern-god-state;
        epkg-cosmoem.url = github:syvlorg/cosmoem;
        epkg-cosmog.url = github:syvlorg/cosmog;
        epkg-doc-templates.url = github:syvlorg/doc-templates;
        epkg-doom-aiern-modeline.url = github:syvlorg/doom-aiern-modeline;
        epkg-evil-evilified-state.url = github:syvlorg/evil-evilified-state;
        epkg-fell-templates.url = github:syvlorg/fell-templates;
        epkg-helm-ido-like.url = github:syvlorg/helm-ido-like;
        epkg-leaf-keywords.url = github:syvlorg/leaf-keywords;
        epkg-lode.url = github:syvlorg/lode;
        epkg-meta.url = github:syvlorg/meta;
        epkg-prime.url = github:syvlorg/prime;
        epkg-riot.url = github:syvlorg/riot;
        epkg-sorrow.url = github:syvlorg/sorrow;
        epkg-uru.url = github:syvlorg/uru;
        epkg-use-package-deino.url = github:syvlorg/use-package-deino;
        epkg-use-package-extras.url = github:syvlorg/use-package-extras;

        flake-utils.url = github:numtide/flake-utils;
        flake-compat = {
            url = "github:edolstra/flake-compat";
            flake = false;
        };
    };
    outputs = inputs@{ self, flake-utils, settings, ... }: with builtins; with settings.lib; with flake-utils.lib; settings.mkOutputs {
        inherit inputs;
        type = "general";
        pname = "damascus";
        overlays = let
            damascan = j.foldToSet (map (pkg: let
                pname = "damascus" + (removePrefix "emacs" pkg);
            in genAttrs [
                pname
                pkg
            ] (name: final: prev: { ${pname} = final.${pkg}.withPackages (epkgs: with epkgs; flatten [
                auto-compile
                command-log-mode
                company-prescient
                counsel-projectile
                dired-plus
                dired-sidebar
                dmenu
                doom-themes
                evil-god-state
                exec-path-from-shell
                exwm
                helm-projectile
                helm-smex
                helm-swoop
                help-fns-plus
                hy-mode
                gcmh
                general
                helm
                helm-flx
                ivy
                ivy-prescient
                lispy
                magit
                modalka
                no-littering
                ob-hy
                objed
                olivetti
                org-contrib
                ox-pandoc
                pyvenv
                rainbow-identifiers
                rainbow-mode
                restart-emacs
                ryo-modal
                selectrum-prescient
                sly
                use-package-chords
                undo-fu
                undo-fu-session
                vlfi
                xah-fly-keys
                (map (epkg: let epkg' = epkgs.${removePrefix "epkg-" epkg}; in [ epkg' epkg'.propagatedUserEnvPkgs ]) (filter (hasPrefix "epkg-") (attrNames inputs)))
            ]); })) (let
                prev = inputs.nixpkgs.legacyPackages.x86_64-linux;
            in j.emacsenGen prev (settings.inputs.emacs.overlay prev prev)));
        in j.foldToSet [
            damascan
            { default = damascan.emacs-nox; }
        ];
    };
}
