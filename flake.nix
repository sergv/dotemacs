{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          t    = pkgs.lib.trivial;
          hl   = pkgs.haskell.lib;

          stdenv = pkgs.stdenv;

          hpkgs = pkgs.haskell.packages.ghc943;
      in {
        devShell = pkgs.mkShell {

          nativeBuildInputs = [
            hpkgs.ghc
            # pkgs.emacs
            # pkgs.emacsNativeComp

            # For running tests
            hpkgs.cabal-install
            hpkgs.fast-tags
            pkgs.universal-ctags
          ];

          LIBRARY_PATH=
            "${pkgs.lib.makeLibraryPath [stdenv.cc.cc pkgs.glibc]}:${pkgs.lib.getLib pkgs.libgccjit}/lib/gcc/${stdenv.hostPlatform.config}/${pkgs.lib.getVersion stdenv.cc.cc}";


            # ${pkgs.lib.getVersion pkgs.stdenv.cc.cc}
         # pkgs.lib.getLib pkgs.stdenv.cc.cc + /lib
# pkgs.lib.getLib pkgs.stdenv.glibc + /lib
# pkgs.lib.getLib pkgs.libgccjit + /lib/gcc/x86_64-unknown-linux-gnu/9.3.0

          # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeDeps;

          # # Add executable packages to the nix-shell environment.
          # packages = [
          #   # hpkgs.ghc
          #   # hpkgs.cabal-install
          #   pkgs.zlib
          # ];

          # Add build dependencies of the listed derivations to the nix-shell environment.
          # inputsFrom = [ pkgs.hello pkgs.gnutar ];

          # ... - everything mkDerivation has
        };
      });
}
