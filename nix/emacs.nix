{ pkgs,
  arch
}:
let

  mtune = if arch == null then "" else "-mtune=${arch}";
  march = if arch == null then "" else "-march=${arch}";

  emacs-base =
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
        src = pkgs.fetchgit {
          url    = "https://github.com/sergv/emacs.git";
          rev    = "3b9730ce5522861b30e66d1f925baba1ca1fe34b";
          sha256 = "sha256-56c26FA/RQhy9pnHz9/BJFB2DFyM4Q1wUWzrIKeSiko="; # pkgs.lib.fakeSha256;
        };

        # NixOS 25.05 patches do not apply to 30.2 any more. Remove throwing away of
        # nixpkgs patches here when moving to a later NixOS release.
        patches = [ ];
        # version        = "30.2";

        configureFlags = old.configureFlags ++ [
          # https://www.jamescherti.com/compiling-emacs/
          "--enable-link-time-optimization"
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

        CFLAGS  = "-O2 -pipe ${march} ${mtune} -fno-omit-frame-pointer -fno-plt -flto=auto";
        LDFLAGS = "-Wl,-O2 -Wl,-z,now -Wl,-z,relro -Wl,--sort-common -Wl,--as-needed -Wl,-z,pack-relative-relocs -flto=auto";
      });

  emacs-native-pkg =
    (emacs-base.override (_: {
      withNativeCompilation = true;
    })).overrideAttrs
      (old: {
        withNativeCompilation = true;
        # NixOS 25.05 patches do not apply to 30.2 any more. Remove throwing away of
        # nixpkgs patches here when moving to a later NixOS release.
        # patches = (old.patches or []) ++ [
        patches = [
          (pkgs.replaceVars ./patches/native-comp-driver-options-30.patch {
            mtuneFlag = if builtins.stringLength arch == 0 then "" else ''"${mtune}"'';
            marchFlag = if builtins.stringLength arch == 0 then "" else ''"${march}"'';

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

  emacs-bytecode-pkg =
    (emacs-base.override (_: {
      withNativeCompilation = false;
    })).overrideAttrs
      (_: {
        withNativeCompilation = false;
      });

  emacs-debug-pkg = pkgs.enableDebugging emacs-bytecode-pkg;


  mk-emacs-pkg =
    exe-name: pkg: wrapper:
      pkgs.writeScriptBin exe-name ''
        #!${pkgs.bash}/bin/bash

        if [[ "''${EMACS_FORCE_PRISTINE:-0}" != 0 ]]; then
            ${wrapper}${pkg}/bin/emacs "''${@}"
        else
            if [[ ! -z "''${EMACS_ROOT+x}" ]]; then
                dump_file="$EMACS_ROOT/compiled/${exe-name}.dmp"
            else
                dump_file="$HOME/.emacs.d/compiled/${exe-name}.dmp"
            fi

            if [[ ! -f "$dump_file" ]]; then
              ${wrapper}${pkg}/bin/emacs "''${@}"
            else
              ${wrapper}${pkg}/bin/emacs --dump-file "$dump_file" "''${@}"
            fi
        fi

      '';

  emacs-native-wrapped = mk-emacs-pkg "emacs-native" emacs-native-pkg "";

  emacs-bytecode-wrapped = mk-emacs-pkg "emacs" emacs-bytecode-pkg "";

  emacs-debug-wrapped =
    mk-emacs-pkg
      "emacs-debug"
      emacs-debug-pkg
      "gdb -ex='set confirm on' -ex=run -ex=quit --args ";


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
    emacs-bytecode = emacs-bytecode-pkg;
    emacs-debug    = emacs-debug-pkg;
    emacs-native   = emacs-native-pkg;
  };

  wrapped = {
    emacs-bytecode = emacs-bytecode-wrapped;
    emacs-debug    = emacs-debug-wrapped;
    emacs-native   = emacs-native-wrapped;
  };
}
