{
  inputs = {
    nixpkgs = {
      url = "nixpkgs";
    };

    haskell-nixpkgs-improvements = {
      url = "github:sergv/haskell-nixpkgs-improvements";

      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-unstable.follows = "nixpkgs";
      # inputs.haskellNix.follows = "haskellNix";
    };
  };

  outputs =
    { self, nixpkgs, haskell-nixpkgs-improvements }:
    let
      systems = [
        "x86_64-linux"
        "i686-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      lib = nixpkgs.lib;
      forEachSystem = lib.genAttrs systems;
    in {

      packages = forEachSystem (
        system:
        let
          pkgs         = nixpkgs.legacyPackages."${system}";
          hlib         = pkgs.haskell.lib.compose;
          hutils       =
            haskell-nixpkgs-improvements.lib.make-haskell-utils pkgs;

          haskell-pkgs =
            if pkgs.stdenv.isDarwin
            then haskell-nixpkgs-improvements.haskell-package-sets."${system}".host.default
            else haskell-nixpkgs-improvements.haskell-package-sets."${system}".host.ghc914-pie;

          haskell-pkgs-with-emacs-native = hutils.fixedExtend haskell-pkgs (
            new:
            old:
            builtins.mapAttrs
              (name: x:
                hutils.onlyApplyToHaskellPackages
                  (hlib.appendConfigureFlag "--ghc-option=-fPIC")
                  name
                  x)
              (old // {
                emacs-native =
                  (x: hlib.enableCabalFlag "standalone-foreign-lib" x)
                    (old.callCabal2nix "emacs-native" ./native/emacs-native {});

                rure-ffi = old.callCabal2nix "rure-ffi" ./native/rure-ffi {};

                emacs-module =
                  (old.callHackageDirect
                    {
                      pkg    = "emacs-module";
                      ver    = "0.3";
                      sha256 = "sha256-kBDM3guLbfllhUBo4v/vqaM8MYS8Z5e1pPbkdXoO8kU="; #pkgs.lib.fakeSha256;
                    }
                    {});
              })
          );

          libemacs-native-so =
            haskell-pkgs-with-emacs-native.emacs-native + "/lib/ghc-${haskell-pkgs-with-emacs-native.ghc.version}/lib/libemacs-native.so";

        in {
          default = haskell-pkgs-with-emacs-native.emacs-native;
        }
      );

      # devShells = forEachSystem (system:
      #   let pkgs   = nixpkgs.legacyPackages."${system}";
      #       stdenv = pkgs.stdenv;
      #       cc     = stdenv.cc.cc;
      #       # hpkgs = pkgs.haskell.packages.ghc961;
      #   in {
      #     default = pkgs.mkShell {
      #       nativeBuildInputs = [
      #         # pkgs.tree-sitter
      #         cc
      #         pkgs.libgccjit
      #         # pkgs.nodejs
      #         #hpkgs.ghc
      #         # pkgs.emacs
      #         # pkgs.emacsNativeComp
      #
      #         ## For running tests
      #         #hpkgs.cabal-install
      #         #hpkgs.fast-tags
      #         #pkgs.universal-ctags
      #       ];
      #
      #       # For native compilation
      #       LIBRARY_PATH=
      #         "${pkgs.lib.makeLibraryPath [cc pkgs.glibc]}:${pkgs.lib.getLib pkgs.libgccjit}/lib/gcc/${stdenv.hostPlatform.config}/${pkgs.lib.getVersion cc}";
      #
      #
      #       # ${pkgs.lib.getVersion pkgs.stdenv.cc.cc}
      #       # pkgs.lib.getLib pkgs.stdenv.cc.cc + /lib
      #       # pkgs.lib.getLib pkgs.stdenv.glibc + /lib
      #       # pkgs.lib.getLib pkgs.libgccjit + /lib/gcc/x86_64-unknown-linux-gnu/9.3.0
      #
      #       # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeDeps;
      #
      #       # # Add executable packages to the nix-shell environment.
      #       # packages = [
      #       #   # hpkgs.ghc
      #       #   # hpkgs.cabal-install
      #       #   pkgs.zlib
      #       # ];
      #
      #       # Add build dependencies of the listed derivations to the nix-shell environment.
      #       # inputsFrom = [ pkgs.hello pkgs.gnutar ];
      #
      #       # ... - everything mkDerivation has
      #     };
      #   }
      # );
    };
}
