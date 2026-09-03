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

    emacs-src = {
      url = "github:sergv/emacs?ref=dev-31";
      flake = false;
    };
  };

  outputs =
    inputs@{ self, nixpkgs, haskell-nixpkgs-improvements, trix, emacs-src }:
    let
      systems = [
        "x86_64-linux"
        "i686-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      lib = nixpkgs.lib;
      forEachSystem = lib.genAttrs systems;

      mk-emacs-with-config =
        { system,
          arch ? null,
          emacs-src ? null,
          pkgs,
          haskell-tools,
          debug,
          native
        }:
        let
          # cc = pkgs.clang;
          # cc = pkgs.gcc;
          cc = pkgs.stdenv.cc.cc;

          hlib = pkgs.haskell.lib.compose;

          hutils = haskell-nixpkgs-improvements.lib.mk-haskell-utils pkgs;

          haskell-pkgs-for-tools = haskell-tools.haskell-package-sets.host.default;

          haskell-pkgs-base-for-emacs-native =
            if pkgs.stdenv.isDarwin
            then haskell-pkgs-for-tools
            else
              (if debug
               then haskell-tools.haskell-package-sets.host.default-pie-debug
               else haskell-tools.haskell-package-sets.host.default-pie);

          haskell-pkgs-for-so =
            if debug
            then hutils.enable-hpkgs-debugging haskell-pkgs-base-for-emacs-native
            else haskell-pkgs-base-for-emacs-native;

          haskell-pkgs-with-emacs-native =
            hutils.fixedExtend
              (hutils.enable-hpkgs-PIC haskell-pkgs-for-so)
              (
                new:
                old:
                {
                  emacs-native =
                    (x: hlib.enableCabalFlag "standalone-foreign-lib" x)
                      ((x: if debug then hlib.enableCabalFlag "runtime-checks" x else x)
                        (old.callCabal2nix "emacs-native" ./native/emacs-native {}));

                  rure-ffi = old.callCabal2nix "rure-ffi" ./native/rure-ffi {};

                  emacs-module =
                    (x: if debug then hlib.enableCabalFlag "assertions" (hlib.enableCabalFlag "call-stacks" x) else x)
                      (old.callHackageDirect
                        {
                          pkg    = "emacs-module";
                          ver    = "0.3";
                          sha256 = "sha256-kBDM3guLbfllhUBo4v/vqaM8MYS8Z5e1pPbkdXoO8kU="; #pkgs.lib.fakeSha256;
                        }
                        {});
                }
              );

          libemacs-native-so =
            haskell-pkgs-with-emacs-native.emacs-native + "/lib/ghc-${haskell-pkgs-with-emacs-native.ghc.version}/lib/libemacs-native${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";

          emacs-pkg = import ./nix/emacs.nix {
            inherit pkgs arch debug native;
            emacs-src = if emacs-src == null then inputs.emacs-src else emacs-src;
          };

          emacs-raw = emacs-pkg.raw;
          emacs = emacs-pkg.wrapped;

          buildTreesitterModule = { dir, subdir, name }:
            let
              output-so = "libtree-sitter-${name}${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";
            in
            {
              inherit output-so;
              deriv =
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
                    "''${CC:-cc}" -O2 -fPIC "-I$src/${subdir}" "$parser" "''${scanner[@]}" -shared -o "$out/lib/${output-so}"
                  '';
                };
            };

          treesitter-dirs = root:
            builtins.map (x: pkgs.lib.path.append root x)
              (builtins.attrNames
                (pkgs.lib.attrsets.filterAttrs
                  (filename: typ: pkgs.lib.strings.hasPrefix "tree-sitter-" filename && typ == "directory")
                  (builtins.readDir root)));

          treesitter-derivs =
            builtins.map
              buildTreesitterModule
              (builtins.map
                (dir: {
                  inherit dir;
                  subdir = "src";
                  name   = pkgs.lib.strings.removePrefix "tree-sitter-" (builtins.baseNameOf dir);
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
            nativeBuildInputs = [ haskell-pkgs-for-tools.ghc ];
            buildCommand      = ''
              ghc -Wall -Werror -O2 -o "$out" "$src"
              strip "$out"
            '';
          };

          emacs-config-source = ./.;

          emacs-config =
            pkgs.stdenvNoCC.mkDerivation {
              pname   = emacs.deriv.name + "-config";
              version = emacs.version + ".1";
              src     = emacs-config-source;
              buildInputs = [
                emacs.deriv
                haskell-pkgs-with-emacs-native.emacs-native
                # faster-richer-tags
              ] ++
              (builtins.map (x: x.deriv) treesitter-derivs);
              nativeBuildInputs =
                [
                  #emacs-raw
                  emacs.deriv
                  pkgs.gdb

                  # pkgs.ghc
                  pkgs.bash
                  pkgs.gzip
                  pkgs.xz
                ] ++
                (builtins.map (x: x.deriv) treesitter-derivs);
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
                  (builtins.map (x: ''ln -s "${x.deriv}/lib/${x.output-so}" "$dest/lib/"'') treesitter-derivs)}

                ln -s "${get-cabal-configuration}" "$dest/compiled/get-cabal-configuration"

                # Prevent config from searching for ~/.bash_env
                export BASHRC_ENV_LOADED="1"

                while IFS= read -d $'\0' -r path ; do
                    # Gzip all *.el files to *.el.gz
                    file_dest="$dest/$(realpath --relative-to="$src" "$path").gz"
                    mkdir -p "$(dirname "$file_dest")"
                    gzip --best --stdout <"$path" >"$file_dest"

                    # Keep vanilla *.el files.
                    # file_dest="$dest/$(realpath --relative-to="$src" "$path")"
                    # mkdir -p "$(dirname "$file_dest")"
                    # cp "$path" "$file_dest"
                done < <(find "$src" \( -path '*/native' -o -path '*/tests' -o -path '*/testing' -o -path '*/test' -o -name 'scripts' -o -name 'resources' -o -name '.cask' -o -name '.git' \) -prune -o -type f \( -name '*.el' -a -not \( -name '*test.el' -o -name '*tests.el' -o -name '*test-utils*' -o -name '.dir-locals.el' \) \) -print0)

                cp -r "$src/resources/auto-insert" "$dest/resources/"
                cp -r "$src/resources/snippets" "$dest/resources/"
                cp "$src/resources/good-fortunes.txt" "$dest/resources/"

                echo "[Build]"
                # NB must have *.el.gz files at their final destination when producing *.elc/*.eln files
                # so that when we run emacs without dump snapthot the elc files will have
                # correct location of their sources.
                EMACS="${emacs.deriv}/bin/${emacs.exe-name}" bash "$src/scripts/recompile.sh" "$dest_abs" "$dest_abs"

                echo "[Dump]"
                # Run in empty environment to not capture build environment variables like ‘buildPhase’
                # that retain references to source directory.
                #
                # Run in destination directory so that references to current directory won’t
                # capture source.
                (cd "$dest_abs"; env -i PATH="${pkgs.lib.makeBinPath [pkgs.bash pkgs.gzip]}" bash "$src/scripts/dump.sh" "$dest_abs" "${emacs-raw}/bin/emacs" "${emacs.exe-name}.dmp")

                # Very important to make config folder read-only after we built everything.
                # Loading of .eln files likes to rename .eln file with tmp suffix before
                # performing actual load (presumably to get fresh handle each time) which
                # breaks concurrent test execution.
                find "$dest_abs" \( -type f -o -type d \) -exec chmod ugo-w {} \;

                runHook postBuild
              '';

              doCheck = true;
              nativeCheckInputs = [
                pkgs.findutils
                pkgs.universal-ctags
                pkgs.unzip

                haskell-tools.tools.faster-richer-tags

                # For rughc for flycheck-haskell and for ghci for dante-tests.
                haskell-pkgs-for-tools.ghc

                # Dependencies for dante tests which won’t work anyway because
                # there’s no internet in nix sandboxes and cabal doesn’t work without it yet.
                # trix.packages."${system}".trix
                # haskell-tools.tools.cabal
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

                echo "[Test dumped snapshot with compiled files]"
                EMACS="$dest_abs/bin/emacs" TMPDIR="/tmp" EMACS_TEST_ROOT="$dest_abs" bash "$src/tests/run-tests.sh" '"t"'

                echo "[Test vanilla .el with asserts]"
                EMACS="$dest_abs/bin/emacs" TMPDIR="/tmp" EMACS_DEBUG=1 EMACS_TEST_ROOT="$src" EMACS_SKIP_ELC=1 EMACS_FORCE_PRISTINE=1 EMACS_TEST_EXTRA_EL_DIR="$dest_abs/compiled" EMACS_TEST_EXTRA_SO_DIR="$dest_abs/lib" bash "$src/tests/run-tests.sh" '"t"'

                echo "[Misc sanity tests]"

                sanity_check_command=$(cat <<EOF
                (progn
                  (defun test--function-source (func-symbol)
                    (let ((def (symbol-function func-symbol)))
                      (find-lisp-object-file-name func-symbol def)))

                  (let ((src (test--function-source #'magit-status)))
                    (when (not (member src
                                       '("$dest_abs/third-party/magit/lisp/magit-status.el.gz"
                                         "$dest_abs/third-party/magit/lisp/magit-status.el"
                                         "$dest_abs/third-party/magit/lisp/magit-status.elc"
                                         "$dest_abs/compiled/elc/magit-status.elc"
                                         "$dest_abs/compiled/eln/magit-status.eln")))
                      (error "Function sources must resolve to a file under '$dest_abs' but it resolved to: '%s'" src))))
                EOF
                )

                echo "[Function source test for dumped snapshot]"
                "$dest_abs/bin/emacs" --batch --eval "$sanity_check_command"

                if [[ -f "$dest_abs/init.elc" ]]; then
                  echo "[Function source test for vanilla .elc]"
                  EMACS_FORCE_PRISTINE=1 "$dest_abs/bin/emacs" --batch --load "$dest_abs/init.elc" --eval "$sanity_check_command"
                fi

                if [[ -f "$dest_abs/init.eln" ]]; then
                  echo "[Function source test for vanilla .eln]"
                  EMACS_FORCE_PRISTINE=1 "$dest_abs/bin/emacs" --batch --load "$dest_abs/init.eln" --eval "$sanity_check_command"
                fi

                runHook postCheck
              '';

              dontPatchShebangs = true;
              # installPhase = ''
              #   mkdir "$out"
              #   cp -r "_build"/* "$out/"
              # '';

              disallowedReferences = [ emacs-config-source ];

              doInstallCheck = true;
              installCheckPhase = ''
                runHook preCheck

                while IFS= read -d $'\0' -r path ; do
                    if zgrep -q -F '${pkgs.lib.lists.last (builtins.split "/" (builtins.baseNameOf emacs-config-source))}' "$path"; then
                        echo "Custom install check: file $path retains reference to ${emacs-config-source}"
                        exit 1
                    fi
                done < <(find "$out" -name '*.gz' -o -name '*.eln' -o -name '*.dmp' -print0)

                runHook postCheck
              '';
            };

        in {
          built-config = emacs-config;
          inherit (emacs-pkg) desktop-entry raw wrapped;
          # sample-treesitter = builtins.head treesitter-derivs;
          # emacs-native-so   = haskell-pkgs-with-emacs-native.emacs-native;
          # emacs-raw         = emacs-raw;
        };
    in {

      lib = {
        mk-emacs-config =
          { system,
            arch ? null,
            emacs-src ? null,
            pkgs,
            haskell-tools
          }@args:
          let
            bytecode     = mk-emacs-with-config (args // { debug = false; native = false; });
            debug        = mk-emacs-with-config (args // { debug = true;  native = false; });
            native       = mk-emacs-with-config (args // { debug = false; native = true; });
            native-debug = mk-emacs-with-config (args // { debug = true;  native = true; });
          in
          {
            default = bytecode;
            inherit bytecode debug native native-debug;
          };
      };

      checks = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages."${system}";
        in
        {
          default = pkgs.symlinkJoin {
            name  = "check-all";
            paths = builtins.attrValues self.packages."${system}";
          };
        }
      );

      packages = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages."${system}";
          haskell-tools = haskell-nixpkgs-improvements.lib.mk-haskell-tools {
            inherit system;
            vanilla-pkgs = pkgs;
          };

        in
        builtins.mapAttrs
          (_name: x: x.built-config)
          (self.lib.mk-emacs-config {
            inherit system pkgs haskell-tools;
          })
      );
    };
}
