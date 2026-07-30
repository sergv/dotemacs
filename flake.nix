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

    trix = {
      url = "github:aanderse/trix";
      flake = true;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, nixpkgs, haskell-nixpkgs-improvements, trix }:
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

          emacs-raw = emacs-pkg.raw.emacs-bytecode;
          emacs = emacs-pkg.wrapped.emacs-bytecode;
          # emacs = emacs-pkg.raw.emacs-native;

          buildTreesitterModule = { dir, subdir, name }:
            pkgs.stdenv.mkDerivation {
              pname   = "tree-sitter-grammar-" + name;
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
            pkgs.stdenvNoCC.mkDerivation {
              pname   = "emacs-config";
              version = "0.9";
              src     = ./.;
              buildInputs = [
                emacs.deriv
                haskell-pkgs-with-emacs-native.emacs-native
                # faster-richer-tags
              ] ++
              treesitter-derivs;
              nativeBuildInputs = [
                emacs-raw
                # emacs.deriv
                # pkgs.ghc
                pkgs.gzip
                pkgs.xz
              ] ++
              treesitter-derivs;
              buildPhase = ''
                runHook preBuild

                dest="$out"
                mkdir "$dest"
                dest_abs="$(realpath $dest)"

                mkdir "$dest/bin"
                mkdir "$dest/compiled"
                mkdir "$dest/lib"
                mkdir "$dest/resources"
                mkdir "$dest/tree-sitter"

                ln -s "${emacs.deriv}/bin/${emacs.exe-name}" "$dest/bin/emacs"

                ln -s "${libemacs-native-so}" "$dest/lib/"
                ${builtins.concatStringsSep "\n"
                  (builtins.map (x: ''ln -s "${x}/lib"/*.so "$dest/lib/"'') treesitter-derivs)}

                ln -s "${get-cabal-configuration}" "$dest/compiled/get-cabal-configuration"

                # Prevent config from searching for ~/.bash_env
                export BASHRC_ENV_LOADED="1"

                echo "[Build]"
                EMACS="${emacs-raw}/bin/emacs" bash "$src/scripts/recompile.sh" "$src" "$dest_abs"

                cp -r "$src/resources/auto-insert" "$dest/resources/"
                cp -r "$src/resources/snippets" "$dest/resources/"

                cp "$src/resources/good-fortunes.txt" "$dest/resources/"

                echo "[Dump]"

                EMACS="${emacs-raw}/bin/emacs" bash "$src/scripts/dump.sh" "$dest_abs"


                runHook postBuild
              '';

              doCheck = true;
              nativeCheckInputs = [
                pkgs.universal-ctags
                pkgs.unzip
                haskell-nixpkgs-improvements.packages."${system}".faster-richer-tags

                # Dependencies for dante tests which won’t work anyway because
                # there’s no internet in nix sandboxes and cabal won’t work without it.
                # trix.packages."${system}".trix
                # haskell-nixpkgs-improvements.packages."${system}".cabal
                # haskell-pkgs.ghc
              ];
              checkPhase = ''
                runHook preCheck

                dest="$out"
                dest_abs="$(realpath $dest)"

                # Work around logic in ‘ert-x.el’ than initializes
                # ‘ert-remote-temporary-file-directory’ and does
                # ‘setenv HOME’ which breaks expansion in ~ in filepaths
                # because Emacs cached HOME value before setenv.
                export REMOTE_TEMPORARY_FILE_DIRECTORY=1

                echo "[Test dumped snapshot]"
                EMACS="$dest_abs/bin/emacs" TMPDIR="/tmp" EMACS_TEST_ROOT="$dest_abs" bash "$src/tests/run-tests.sh"

                echo "[Test with asserts]"
                EMACS="$dest_abs/bin/emacs" TMPDIR="/tmp" EMACS_TEST_ROOT="$dest_abs" EMACS_SKIP_ELC=1 EMACS_FORCE_PRISTINE=1 bash "$src/tests/run-tests.sh" '"t"'

                runHook postCheck
              '';

              dontPatchShebangs = true;
              # installPhase = ''
              #   mkdir "$out"
              #   cp -r "_build"/* "$out/"
              # '';

              # doInstallCheck = true;
              # installCheckPhase = ''
              #   runHook preCheck
              #   echo "Performing install check 1"
              #
              #   echo "Performing install check 2" >&2
              #   exit 1
              #   runHook postCheck
              # '';
            };

        in {
          default         = buildEmacsConfig pkgs;
          # default         = builtins.head treesitter-derivs;
          emacs-native-so = haskell-pkgs-with-emacs-native.emacs-native;
          emacs-raw       = emacs-raw;
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
