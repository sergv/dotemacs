{ pkgs,
  arch
}:
let
  march-mtrune-args =
    (if arch == null then [] else ["-mtune=${arch}"]) ++
    (if arch == null then [] else ["-march=${arch}"]);

  fetchgit-improved =
    pkgs.fetchgit // {
      __functor = self : args :
        (pkgs.fetchgit.__functor self args).overrideAttrs (old: {
          # GIT_SSL_NO_VERIFY         = true;
          # GIT_HTTP_PROXY_AUTHMETHOD = "basic";
          # Somehow only file is picked up. Individual environment variables aren’t!
          gitConfigFile =
            pkgs.writeText
              "git-proxy-config"
              ''
                [http]
                proxyAuthMethod = "basic"
                sslverify = false
              '';
        });
    };

  emacs-src = pkgs.fetchgit {
    url    = "https://github.com/sergv/emacs.git";
    rev    = "424126816a70e492b2472636f83d765b5f1ff506";
    sha256 = "sha256-eAl55ul9tOFlYqOnQx+5c+rywvMtQqX/WoVDMhfUumM="; #pkgs.lib.fakeSha256;
  };

  mk-emacs-base =
    { cflags, ldflags, extraConfigureFlags, ... }:
    (pkgs.emacs30.override (_: {
      withNativeCompilation = false;
      noGui                 = false;
      srcRepo               = true;
      withTreeSitter        = true;
      withSQLite3           = false;
      withPgtk              = false;
      withJansson           = false; # Use native JSON in Emacs instead, aviailable since version 30.

      withX                 = true;
      withGTK3              = true;
      withToolkitScrollBars = false;
      withCairo             = true;
      withXinput2           = true;

      withAcl               = false;
      withAlsaLib           = false;
      withMailutils         = false;
      withGcMarkTrace       = false;
      withImageMagick       = false;
      withXwidgets          = false;
      withDbus              = false;
      withSelinux           = false;

      # Disable General Purpose Mouse (GPM), a background service that
      # provides mouse support for the Linux console (the text-only
      # TTY you see before logging into a graphical desktop). Unless
      # you plan to use Emacs in a bare-metal Linux console (outside
      # of a terminal emulator like Alacritty, Foot, or GNOME
      # Terminal), GPM is unnecessary. Modern terminal emulators use
      # their own internal protocols for mouse interaction that do not
      # rely on the GPM daemon.
      withGpm               = false;

    })).overrideAttrs
      (old: {
        src = emacs-src;

        # NixOS 25.05 patches do not apply to 30.2 any more. Remove throwing away of
        # nixpkgs patches here when moving to a later NixOS release.
        patches = [ ];
        # version        = "30.2";

        configureFlags = old.configureFlags ++ extraConfigureFlags ++ [
          # https://www.jamescherti.com/compiling-emacs/
          "--enable-largefile"
          "--disable-xattr"

          (pkgs.lib.withFeature true "harfbuzz")
          (pkgs.lib.withFeature true "gnutls")

          (pkgs.lib.withFeature true "gsettings")
          (pkgs.lib.withFeature true "threads")
          (pkgs.lib.withFeature true "libgmp")
          (pkgs.lib.withFeature true "xml2")
          (pkgs.lib.withFeature true "zlib")
          (pkgs.lib.withFeatureAs true "file-notification" "inotify")

          (pkgs.lib.withFeature true "wide-int")

          (pkgs.lib.withFeature true "xpm")
          (pkgs.lib.withFeature true "png")
          (pkgs.lib.withFeature true "rsvg")
          (pkgs.lib.withFeature false "tiff")
          (pkgs.lib.withFeature true "jpeg")
          (pkgs.lib.withFeature false "gif")

          (pkgs.lib.withFeatureAs true "pdumper" "yes")
          (pkgs.lib.withFeatureAs true "unexec" "no")
          (pkgs.lib.withFeatureAs true "dumping" "pdumper")

          (pkgs.lib.withFeature false "xft")
          (pkgs.lib.withFeature false "libotf")
          (pkgs.lib.withFeature false "xim")
          (pkgs.lib.withFeature false "gconf")
          (pkgs.lib.withFeature false "sound")
          (pkgs.lib.withFeature false "libsystemd")
          (pkgs.lib.withFeature false "libsmack")
          (pkgs.lib.withFeature false "kerberos")
          (pkgs.lib.withFeature false "pop")
          (pkgs.lib.withFeature false "kerberos5")
          (pkgs.lib.withFeature false "hesiod")
          (pkgs.lib.withFeature false "mail-unlink")
          (pkgs.lib.withFeature false "lcms2")

          # Disables the X11 Double Buffer Extension. This protocol is
          # redundant for modern builds because both the PGTK (Wayland)
          # and GTK3 (X11) layers handle window buffering internally.
          # Disabling it simplifies the binary and ensures Emacs uses
          # modern rendering paths.
          (pkgs.lib.withFeature false "xdbe")
        ];

        CFLAGS  = mk-shell-flags cflags;
        LDFLAGS = mk-shell-flags ldflags;
      });

  emacs-release-cfg = {
    cflags              =
      [
        "-O2"
        "-fno-omit-frame-pointer"
        "-fno-plt"
        "-flto=auto"
      ] ++
      march-mtrune-args;
    ldflags             =
      [
        "-Wl,-O2"
        "-Wl,-z,now"
        "-Wl,-z,relro"
        "-Wl,--sort-common"
        "-Wl,--as-needed"
        "-Wl,-z,pack-relative-relocs"
        "-flto=auto"
      ];
    extraConfigureFlags = ["--enable-link-time-optimization"];
    elispCompilerFlags  =
      [
        # The most meaningful optimizations:
        "-O2"
        # Reduce .eln size and compilation overhead.
        "-g0"
        # Good defensive choice for Emacs stability.
        "-fno-omit-frame-pointer"
        "-fno-finite-math-only"
      ] ++
      march-mtrune-args;
    elispLinkFlags =
      [
        # -Wl,-z,pack-relative-relocs compresses
        # relocation tables to reduce file size and
        # slightly improve load times.
        "-Wl,-z,pack-relative-relocs"

        # -Wl,-O2 applies standard linker-level
        # optimizations (like string merging) to the
        # generated shared object.
        "-Wl,-O2"

        # -Wl,--as-needed prevents the linker from
        # recording dependencies on libraries that
        # are not actually used by the code.
        "-Wl,--as-needed"
      ];
  };

  emacs-debug-cfg = {
    cflags              =
      [
        "-O0"
        "-g3"
        "-fno-omit-frame-pointer"
      ];
    ldflags             = [];
    extraConfigureFlags = [];
    # Really slow checks for serious problems.
    # extraConfigureFlags = ["--enable-checking=yes" "--enable-check-lisp-object-type"];
    elispCompilerFlags  =
      [
        # The most meaningful optimizations:
        "-O0"
        # Reduce .eln size and compilation overhead.
        "-g3"
        # Good defensive choice for Emacs stability.
        "-fno-omit-frame-pointer"
        "-fno-finite-math-only"
      ];
    elispLinkFlags = [];
  };

  wrap-dquotes-concat-with-space =
    xs:
    pkgs.lib.concatStringsSep " " (builtins.map (x: ''"${x}"'') xs);
  mk-shell-flags = xs: pkgs.lib.concatStringsSep " " xs;
  mk-elisp-flags = wrap-dquotes-concat-with-space;

  mk-emacs-native-pkg =
    emacs-cfg:
    ((mk-emacs-base emacs-cfg).override (_: {
      withNativeCompilation = true;
    })).overrideAttrs
      (old: {
        withNativeCompilation = true;
        # NixOS 25.05 patches do not apply to 30.2 any more. Remove throwing away of
        # nixpkgs patches here when moving to a later NixOS release.
        # patches = (old.patches or []) ++ [
        patches = [
          (pkgs.replaceVars ./patches/native-comp-driver-options-30.patch {

            compilerOptionsFlags = mk-elisp-flags emacs-cfg.elispCompilerFlags;

            driverOptionsFlags = mk-elisp-flags emacs-cfg.elispLinkFlags;

            backendPath =
              let
                libGccJitLibraryPaths = [
                  "${pkgs.lib.getLib pkgs.libgccjit}/lib/gcc"
                  "${pkgs.lib.getLib pkgs.stdenv.cc.libc}/lib"
                ]
                ++ pkgs.lib.optionals (pkgs.stdenv.cc ? cc.lib.libgcc) [
                  "${pkgs.lib.getLib pkgs.stdenv.cc.cc.lib.libgcc}/lib"
                ];
              in
              pkgs.lib.concatStringsSep " " (
                builtins.map (x: ''"-B${x}"'') (
                  [
                    # Paths necessary so the JIT compiler finds its libraries:
                    "${pkgs.lib.getLib pkgs.libgccjit}/lib"
                  ]
                  ++ libGccJitLibraryPaths
                  ++ [
                    # Executable paths necessary for compilation (ld, as):
                    "${pkgs.lib.getBin pkgs.stdenv.cc.cc}/bin"
                    "${pkgs.lib.getBin pkgs.stdenv.cc.bintools}/bin"
                    "${pkgs.lib.getBin pkgs.stdenv.cc.bintools.bintools}/bin"
                  ]
                )
              );
          })
        ];
      });

  emacs-native-pkg = mk-emacs-native-pkg emacs-release-cfg;

  emacs-bytecode-pkg =
    ((mk-emacs-base emacs-release-cfg).override (_: {
      withNativeCompilation = false;
    })).overrideAttrs
      (_: {
        withNativeCompilation = false;
      });

  emacs-native-debug-pkg = pkgs.enableDebugging (mk-emacs-native-pkg emacs-debug-cfg);

  emacs-debug-pkg = pkgs.enableDebugging emacs-bytecode-pkg;

  mk-emacs-pkg =
    exe-name: pkg: debug-wrapper:
      {
        inherit exe-name;
        deriv =
          pkgs.writeScriptBin exe-name ''
            #!${pkgs.bash}/bin/bash

            declare -a wrap_cmd
            ${if builtins.length debug-wrapper == 0
              then ""
              else ''
                if [[ "''${EMACS_DEBUG:-1}" == 1 ]]; then
                  wrap_cmd+=(${wrap-dquotes-concat-with-space debug-wrapper})
                fi
              ''}

            default_root="$(realpath "$(dirname "$(dirname "$(realpath --no-symlinks "''${BASH_SOURCE[0]}")")")")"

            if [[ -v EMACS_ROOT && -d "$EMACS_ROOT" ]]; then
                root="$EMACS_ROOT"
            else
                root="$default_root"
            fi

            if [[ "''${EMACS_FORCE_PRISTINE:-0}" != 0 ]]; then
                "''${wrap_cmd[@]}" ${pkg}/bin/emacs --init-directory="$root" "''${@}"
            else
                if [[ -v EMACS_ROOT ]]; then
                    dump_file="$root/compiled/${exe-name}.dmp"
                fi
                if [[ ! -v dump_file || ! -f "$dump_file" ]]; then
                    dump_file="$default_root/compiled/emacs.dmp"

                    if [[ ! -f "$dump_file" ]]; then
                        echo "Default dump file does not exist: $dump_file" >&2
                        exit 1
                    fi
                fi

                "''${wrap_cmd[@]}" ${pkg}/bin/emacs --init-directory="$root" --dump-file "$dump_file" "''${@}"
            fi
          '';
      };

  emacs-native-wrapped = mk-emacs-pkg "emacs-native" emacs-native-pkg [];

  emacs-bytecode-wrapped = mk-emacs-pkg "emacs-bytecode" emacs-bytecode-pkg [];

  # "--command=${emacs-src}/src/.gdbinit" "--directory=${emacs-src}/src/"

  debug_wrapper =
    [
      "gdb"
      "--quiet"
      "--init-eval-command=set auto-load safe-path /"
      "--eval-command=set confirm on"
      "--eval-command=run"
      "--eval-command=quit"
      "--args"
    ];

  emacs-native-debug-wrapped =
    mk-emacs-pkg
      "emacs-native-debug"
      emacs-native-debug-pkg
      debug_wrapper;

  emacs-debug-wrapped =
    mk-emacs-pkg
      "emacs-debug"
      emacs-debug-pkg
      debug_wrapper;

  desktop-entry = {
    type        = "Application";
    exec        = "emacs %u";
    terminal    = false;
    name        = "Emacs";
    icon        = ./icons/emacs.png;
    comment     = "Edit text";
    genericName = "Text Editor";
    categories  = [
      "Utility"
      "TextEditor"
    ];
    mimeType    = [
      "application/x-shellscript"
      "text/english"
      "text/plain"
      "text/x-c"
      "text/x-c++"
      "text/x-c++hdr"
      "text/x-c++src"
      "text/x-chdr"
      "text/x-csrc"
      "text/x-java"
      "text/x-haskell"
      "text/x-makefile"
      "text/x-moc"
      "text/x-pascal"
      "text/x-tcl"
      "text/x-tex"
      "x-scheme-handler/org-protocol"
    ];
    # startupWMClass = "Emacs";
  };

in {
  inherit desktop-entry;

  raw = {
    emacs-bytecode     = emacs-bytecode-pkg;
    emacs-debug        = emacs-debug-pkg;
    emacs-native       = emacs-native-pkg;
    emacs-native-debug = emacs-native-debug-pkg;
  };

  wrapped = {
    emacs-bytecode     = emacs-bytecode-wrapped;
    emacs-debug        = emacs-debug-wrapped;
    emacs-native       = emacs-native-wrapped;
    emacs-native-debug = emacs-native-debug-wrapped;
  };
}
