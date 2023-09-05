{
  nixConfig = {
    # Adapted From: https://github.com/divnix/digga/blob/main/examples/devos/flake.nix#L4
    accept-flake-config = true;
    auto-optimise-store = true;
    builders-use-substitutes = true;
    cores = 0;
    extra-experimental-features =
      "nix-command flakes impure-derivations recursive-nix";
    fallback = true;
    flake-registry =
      "https://raw.githubusercontent.com/syvlorg/flake-registry/master/flake-registry.json";
    keep-derivations = true;
    keep-outputs = true;
    max-free = 1073741824;
    min-free = 262144000;
    show-trace = true;
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nickel.cachix.org-1:ABoCOGpTJbAum7U6c+04VbjvLxG9f0gJP5kYihRRdQs="
      "sylvorg.cachix.org-1:xd1jb7cDkzX+D+Wqt6TemzkJH9u9esXEFu1yaR9p8H8="
    ];
    trusted-substituters = [
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
      "https://nickel.cachix.org"
      "https://sylvorg.cachix.org"
    ];
    warn-dirty = false;
  };
  description = "My emacs config!";
  inputs = rec {
    bundle = {
      url = "https://github.com/sylvorg/bundle.git";
      type = "git";
      submodules = true;
    };
    valiant.follows = "bundle/valiant";
    nixpkgs.follows = "bundle/nixpkgs";

    ePkg-aiern-aiernhide-state.url =
      "git+https://github.com/syvlorg/aiern-aiernhide-state.git";
    ePkg-aiern-god-state.url =
      "git+https://github.com/syvlorg/aiern-god-state.git";
    ePkg-cosmoem.url =
      "git+https://github.com/syvlorg/cosmoem.git";
    ePkg-cosmog.url =
      "git+https://github.com/syvlorg/cosmog.git";
    ePkg-doc-templates.url =
      "git+https://github.com/syvlorg/doc-templates.git";
    ePkg-doom-aiern-modeline.url =
      "git+https://github.com/syvlorg/doom-aiern-modeline.git";
    ePkg-evil-evilified-state.url =
      "git+https://github.com/syvlorg/evil-evilified-state.git";
    ePkg-fell-templates.url =
      "git+https://github.com/syvlorg/fell-templates.git";
    ePkg-helm-ido-like.url =
      "git+https://github.com/syvlorg/helm-ido-like-guide.git";
    ePkg-leaf-keywords.url =
      "git+https://github.com/syvlorg/leaf-keywords.git";
    ePkg-lode.url =
      "git+https://github.com/syvlorg/lode.git";
    ePkg-meta.url =
      "git+https://github.com/syvlorg/meta.git";
    ePkg-prime.url =
      "git+https://github.com/syvlorg/prime.git";
    ePkg-riot.url =
      "git+https://github.com/syvlorg/riot.git";
    ePkg-sorrow.url =
      "git+https://github.com/syvlorg/sorrow.git";
    ePkg-uru.url =
      "git+https://github.com/syvlorg/uru.git";
    ePkg-use-package-deino.url =
      "git+https://github.com/syvlorg/use-package-deino.git";
    ePkg-use-package-extras.url =
      "git+https://github.com/syvlorg/use-package-extras.git";

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, flake-utils, bundle, ... }:
    with builtins;
    with bundle.lib;
    with flake-utils.lib;
    bundle.mkOutputs.general {
      inherit inputs self;
      pname = "damascus";
      overlays = let
        damascan = iron.fold.set (map (pkg:
          let pname = "damascus" + (removePrefix "emacs" pkg);
          in genAttrs [ pname pkg ] (name: final: prev: {
            ${pname} = final.${pkg}.withPackages (epkgs:
              with epkgs;
              flatten [
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
                (map (epkg:
                  let epkg' = epkgs.${removePrefix "ePkg-" epkg};
                  in [ epkg' epkg'.propagatedUserEnvPkgs ])
                  (filter (hasPrefix "ePkg-") (attrNames inputs)))
              ]);
          })) (let prev = inputs.nixpkgs.legacyPackages.x86_64-linux;
          in iron.emacsenGen prev (bundle.inputs.emacs.overlay prev prev)));
      in iron.fold.set [ damascan { default = damascan.emacs-nox; } ];
    } { };
}
