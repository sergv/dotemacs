{
  description = "Rust module to be called by Emacs";

  inputs = {
    nixpkgs = {
      # # unstable
      # url = "nixpkgs/nixos-unstable";
      #url = "nixpkgs/nixos-22.05";
      url = "/home/sergey/nix/nixpkgs";
    };

    # nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };

      libclang = pkgs.libclang;

      devTools = [pkgs.rustfmt pkgs.clippy];

      emacs-native-rs-package = pkgs.rustPlatform.buildRustPackage {
        pname = "emacs-native-rs";
        version = "0.1.0";

        src = ./.;
        cargoLock = {
          lockFile = ./Cargo.lock;
        };
        nativeBuildInputs = [
          libclang
        ];

        LIBCLANG_PATH = pkgs.lib.makeLibraryPath [ libclang.lib ];

        # RUSTFLAGS="-C target-cpu=native"
        extraRustcOpts = "-C target-cpu=native";

        # Mostly for bindgen
        preBuild = ''
          # From: https://github.com/NixOS/nixpkgs/blob/1fab95f5190d087e66a3502481e34e15d62090aa/pkgs/applications/networking/browsers/firefox/common.nix#L247-L253
          # Set C flags for Rust's bindgen program. Unlike ordinary C
          # compilation, bindgen does not invoke $CC directly. Instead it
          # uses LLVM's libclang. To make sure all necessary flags are
          # included we need to look in a few places.
          export BINDGEN_EXTRA_CLANG_ARGS="$(< ${pkgs.stdenv.cc}/nix-support/libc-crt1-cflags) \
            $(< ${pkgs.stdenv.cc}/nix-support/libc-cflags) \
            $(< ${pkgs.stdenv.cc}/nix-support/cc-cflags) \
            $(< ${pkgs.stdenv.cc}/nix-support/libcxx-cxxflags) \
            ${pkgs.lib.optionalString pkgs.stdenv.cc.isClang "-idirafter ${pkgs.stdenv.cc.cc.lib}/lib/clang/${pkgs.lib.getVersion pkgs.stdenv.cc.cc}/include"} \
            ${pkgs.lib.optionalString pkgs.stdenv.cc.isGNU "-isystem ${pkgs.lib.getDev pkgs.stdenv.cc.cc}/include/c++/${pkgs.lib.getVersion pkgs.stdenv.cc.cc} -isystem ${pkgs.stdenv.cc.cc}/include/c++/${pkgs.lib.getVersion pkgs.stdenv.cc.cc}/${pkgs.stdenv.hostPlatform.config} -isystem ${pkgs.stdenv.cc.cc}/lib/gcc/${pkgs.stdenv.hostPlatform.config}/${pkgs.lib.getVersion pkgs.stdenv.cc.cc}/include"}"
        '';
      };

    in {

      # legacyPackages."<system>"."<name>"
      legacyPackages.emacs_module_rs = emacs-native-rs-package;

      packages = {
        emacs_module_rs = emacs-native-rs-package;
        default         = emacs-native-rs-package;
      };
      devShells = {
        default = emacs-native-rs-package.overrideAttrs (old: {
          nativeBuildInputs  = old.nativeBuildInputs or [] ++ devTools;
          NIX_DEVELOP_PROMPT = "[nix]";
        });
      };
    });
}
