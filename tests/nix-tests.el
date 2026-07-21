;; nix-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 11 July 2026
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-lib))

(require 'nix-ts-mode)

(require 'tests-utils)

(cl-defmacro nix-tests--test-results (&key name actions-and-values contents modes)
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for mode in (or modes '(nix-ts-mode))
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
           (tests-utils--with-temp-buffer
            :action
            (progn
              ,@(cl-loop
                 for entry in actions-and-values
                 collect
                 `(should (equal ,(car entry) ,(cadr entry)))))
            :contents ,contents
            :initialisation (,mode)
            :buffer-id ,(string->symbol (format "nix-tests-%s" mode)))))))

(cl-defmacro nix-tests--test-result (&key name action expected-value contents modes)
  (declare (indent nil))
  `(nix-tests--test-results :name ,name
                            :actions-and-values ,(list (list action expected-value))
                            :contents ,contents
                            :modes ,modes))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1a
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "pkgs._|_pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1b
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper_|_\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1c
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  _|_''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1d
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''_|_"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1e
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!_|_${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1f
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${_|_pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1g
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/_|_bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1h
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS_|_=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1i
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''_|_${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1j
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${I_|_SERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1k
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configureP_|_hase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1l
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_IS_|_ERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1m
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s _|_${win-iserv-proxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1n
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-p_|_roxy-interpreter}/bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1o
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/_|_bin/*.dll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-1p
 :action
 (cons (point-inside-string? (point)) (point-inside-comment? (point)))
 :expected-value
 (cons t nil)
 :contents
 (tests-utils--multiline
  "pkgs.pkgsBuildBuild.writeScriptBin \"iserv-wrapper\""
  "  ''"
  "    #!${pkgs-cross-win.pkgsBuildBuild.bash}/bin/bash"
  ""
  "    set -euo pipefail"
  ""
  "    ISERV_ARGS=''${ISERV_ARGS:-}"
  "    PROXY_ARGS=''${PROXY_ARGS:-}"
  ""
  "    # May lead to a too large environment so best to unset it."
  "    unset configureFlags"
  "    unset configurePhase"
  "    # Not really needed"
  "    unset pkgsHostTargetAsString"
  ""
  "    REMOTE_ISERV=/tmp/iserv-tmpdir"
  "    if [[ ! -d \"$REMOTE_ISERV\" ]]; then"
  "        mkdir -p \"$REMOTE_ISERV/tmp\""
  "        ln -s ${win-iserv-proxy-interpreter}/bin/*.d_|_ll \"$REMOTE_ISERV\""
  ""
  "        for p in ${pkgs.lib.concatStringsSep \" \" haskell-win-runner-dll-pkgs}; do"
  "            find \"$p\" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \\;"
  "            find \"$p\" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \\;"
  "        done"
  ""
  "        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix."
  "        # This was a problem for `double-conversion` package when used in TH code."
  "        # Creating links from the `X.dll` to `libX.dll` works around this issue."
  "        for dll in \"$REMOTE_ISERV\"/*.dll; do"
  "            small=$(basename \"$dll\")"
  "            ln -s \"$dll\" \"$REMOTE_ISERV/''${small#lib}\""
  "        done"
  "    fi"
  ""
  "    WINEDLLOVERRIDES=\"winemac.drv=d\" \\"
  "        WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag \\"
  "        WINEPREFIX=\"$REMOTE_ISERV/prefix\" \\"
  ""
  "    ${iserv-proxy}/bin/iserv-proxy \"''${@}\" --pipe ${lib.getExe wine} ${win-iserv-proxy-interpreter}/bin/${exe-name} \"$REMOTE_ISERV/tmp\" --stdio ${no-load-call} $ISERV_ARGS"
  "  ''"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-2a
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "let suffix _|_= if builtins.isNull alias-version then \"\" else \"-${alias-version}\";"
  "in ''ln -s \"$out/bin/${old-prefix}$x-${old-version}\" \"$out/bin/${new-prefix}$x${suffix}\"'';"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-2b
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "let suffix = if builtins.isNull alias-version then_|_ \"\" else \"-${alias-version}\";"
  "in ''ln -s \"$out/bin/${old-prefix}$x-${old-version}\" \"$out/bin/${new-prefix}$x${suffix}\"'';"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-2c
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "let suffix = if builtins.isNull alias-version then \"_|_\" else \"-${alias-version}\";"
  "in ''ln -s \"$out/bin/${old-prefix}$x-${old-version}\" \"$out/bin/${new-prefix}$x${suffix}\"'';"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-2d
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "let suffix = if builtins.isNull alias-version then \"\" else _|_\"-${alias-version}\";"
  "in ''ln -s \"$out/bin/${old-prefix}$x-${old-version}\" \"$out/bin/${new-prefix}$x${suffix}\"'';"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-2e
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "let suffix = if builtins.isNull alias-version then \"\" else \"_|_-${alias-version}\";"
  "in ''ln -s \"$out/bin/${old-prefix}$x-${old-version}\" \"$out/bin/${new-prefix}$x${suffix}\"'';"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-2f
 :action
 (point-inside-string? (point))
 :expected-value
 t
 :contents
 (tests-utils--multiline
  "let suffix = if builtins.isNull alias-version then \"\" else \"-_|_${alias-version}\";"
  "in ''ln -s \"$out/bin/${old-prefix}$x-${old-version}\" \"$out/bin/${new-prefix}$x${suffix}\"'';"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-2g
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "let suffix = if builtins.isNull alias-version then \"\" else \"-${alias-version}_|_\";"
  "in ''ln -s \"$out/bin/${old-prefix}$x-${old-version}\" \"$out/bin/${new-prefix}$x${suffix}\"'';"))

(nix-tests--test-result
 :name
 nix-tests/point-inside-string-2h
 :action
 (point-inside-string? (point))
 :expected-value
 nil
 :contents
 (tests-utils--multiline
  "let suffix = if builtins.isNull alias-version then \"\" else \"-${alias-version}\"_|_;"
  "in ''ln -s \"$out/bin/${old-prefix}$x-${old-version}\" \"$out/bin/${new-prefix}$x${suffix}\"'';"))

(provide 'nix-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; nix-tests.el ends here
