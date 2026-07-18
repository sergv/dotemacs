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

      mkEmacsWithConfig = system: pkgs: hlib: hutils:
        let
          # cc = pkgs.clang;
          # cc = pkgs.gcc;
          cc = pkgs.stdenv.cc.cc;

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
            haskell-pkgs-with-emacs-native.emacs-native + "/lib/ghc-${haskell-pkgs-with-emacs-native.ghc.version}/lib/libemacs-native${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";

          emacs-pkg = import ./nix/emacs.nix {
            inherit pkgs;
            arch = null;
            # arch = arch.gccArch;
          };

          emacs = emacs-pkg.raw.emacs-bytecode;
          # emacs = emacs-pkg.raw.emacs-native;

          buildTreesitterModule = { dir, subdir, name }:
            # todo: use subdir within dir
            pkgs.stdenv.mkDerivation {
              pname = name;
              version = "0.1";
              src     = dir;
              # buildInputs = [ ];
              nativeBuildInputs = [
                # stdenv already contains these
                # cc
                # pkgs.xz.bin

                # When we’ll want to regenerate treesitter.
                # # nodejs needed to run tree-sitter exe to regenerate grammars
                # pkgs.nodejs
                # pkgs.tree-sitter
              ];
              buildCommand = ''
                mkdir -p "$out/lib"

                parser="parser.c"
                declare -a scanner

                if [[ -f "$src/${subdir}/parser.c.xz" ]]; then
                    xz --decompress --stdout "$src/${subdir}/parser.c.xz" >"$parser"
                else
                    echo "Invalid treesitter library, compressed parser file does not exist: ‘$src/${subdir}/parser.c.xz’" >&2
                    exit 1
                fi

                if [[ -f "$src/${subdir}/scanner.c" ]]; then
                    scanner+=("$src/${subdir}/scanner.c")
                fi
                "''${CC:-cc}" -O2 -fPIC "-I$src/${subdir}" "$parser" "''${scanner[@]}" -shared -o "$out/lib/libtree-sitter-${name}${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}"
              '';
            };

          treesitter-dirs = root:
            builtins.map (x: lib.path.append root x)
              (builtins.attrNames
                (lib.attrsets.filterAttrs
                  (filename: typ: lib.strings.hasPrefix "tree-sitter-" filename && typ == "directory")
                  (builtins.readDir root)));

          treesitter-derivs =
            builtins.map
              buildTreesitterModule
              (builtins.map
                (dir: {
                  inherit dir;
                  subdir = "src";
                  name   = lib.strings.removePrefix "tree-sitter-" (builtins.baseNameOf dir);
                })
                (treesitter-dirs ./native)
              ++
              [
                {
                  # Must use dir for full grammar because we’re reusing lexer.
                  dir    = ./native/tree-sitter-haskell;
                  subdir = "hsc/src";
                  name   = "hsc";
                }
              ]);

          get-cabal-configuration = pkgs.stdenv.mkDerivation {
            pname             = "get-cabal-configuration";
            version           = "0.1";
            src               = ./third-party/flycheck-haskell/get-cabal-configuration.hs;
            nativeBuildInputs = [ haskell-pkgs.ghc ];
            buildCommand      = ''
              ghc -Wall -Werror -O2 -o "$out" "$src"
              strip "$out"
            '';
          };

          buildEmacsConfig = pkgs:
            pkgs.stdenv.mkDerivation {
              pname   = "emacs-config";
              version = "0.9";
              src     = ./.;
              buildInputs = [
                emacs
                haskell-pkgs-with-emacs-native.emacs-native
                # faster-richer-tags
              ] ++
              treesitter-derivs;
              nativeBuildInputs = [
                emacs
                # pkgs.ghc
                pkgs.gzip
                pkgs.xz
              ] ++
              treesitter-derivs;
              buildCommand = ''
                mkdir "$out"
                mkdir "$out/compiled"
                mkdir "$out/lib"
                mkdir "$out/resources"
                mkdir "$out/tree-sitter"
                rm -f "$out/compiled"/*
                rm -f "$out/lib"/*
                rm -f "$out/resources"/*
                rm -f "$out/tree-sitter"/*

                ln -s "${libemacs-native-so}" "$out/lib/"
                ${builtins.concatStringsSep "\n"
                  (builtins.map (x: ''ln -s "${x}/lib"/*.so "$out/tree-sitter/"'') treesitter-derivs)}

                ln -s "${get-cabal-configuration}" "$out/compiled/get-cabal-configuration"
                export EMACS="${emacs}/bin/emacs"

                export EMACS_ROOT="$src"
                export EMACS_COMPILED_ROOT="$out"
                # Prevent config from searching for ~/.bash_env
                export BASHRC_ENV_LOADED="1"
                bash "$src/scripts/recompile.sh" "$src" "$out"

                cp -r "$src/resources/auto-insert" "$out/resources/"
                cp -r "$src/resources/snippets" "$out/resources/"

                cp "$src/resources/good-fortunes.txt" "$out/resources/"

              '';
            };

        in {
          default         = buildEmacsConfig pkgs;
          # default         = builtins.head treesitter-derivs;
          emacs-native-so = haskell-pkgs-with-emacs-native.emacs-native;
          emacs           = emacs;
        };
    in {

      packages = forEachSystem (
        system:
        let
          pkgs   = nixpkgs.legacyPackages."${system}";
          hlib   = pkgs.haskell.lib.compose;
          hutils = haskell-nixpkgs-improvements.lib.make-haskell-utils pkgs;

          emacs-with-config = mkEmacsWithConfig system pkgs hlib hutils;
        in
        emacs-with-config
        # {
        #   default = haskell-pkgs-with-emacs-native.emacs-native;
        # }
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
