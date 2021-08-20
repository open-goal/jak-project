{
  description = "Jak PC ports via OpenGOAL PC port of Naughty Dog's GOAL";

  outputs = { self, nixpkgs }: let
    getVersionPre = output:
      "${nixpkgs.lib.substring 0 8 output.lastModifiedDate}.${output.shortRev or "dirty"}";
    supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
    forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

    supportedPackages = [ "jak" "jak-dev" "jak-asan" "jak-asan-dev" ];
    forAllPackages = f: nixpkgs.lib.genAttrs supportedPackages (pname: f pname);

    release = false;
    version = "0.5.0" + nixpkgs.lib.optionalString (!release) "-${getVersionPre self}";
  in {

    overlay = pkgs: pkgsSuper: {
      jak = pkgs.callPackage (
        { lib, stdenv, buildPackages, fetchFromGitHub, runCommand, writeText
        , jak-googletest-src, jak-zydis-src
        , llvm ? null
        , python3Packages ? null
        , enableDevInputs ? false
        , enableAsan ? false
        , enableFramePointer ? enableAsan
        , enablePIE ? false
        , enableSourceLevelDebug ? enableAsan
        }:

        let
          sh = lib.escapeShellArg;
          optionalFun = b: f: if b then f else x: x;
          inherit (stdenv.cc) isClang;

          llvm-symbolizer = if isClang then runCommand "llvm-symbolizer" {
            allowSubstitutes = false;
            preferLocalBuild = true;
          } ''
            mkdir -p "$out/bin"
            cp ${sh (lib.getBin llvm)}/bin/llvm-symbolizer "$out/bin"/llvm-symbolizer
          '' else null;

          makeFlagArrayBody = prefix: flags:
            lib.concatStrings
              (lib.mapAttrsToList (n: v: " " + sh "${prefix}${n}=${v}" + " \\\n") flags);
          makeFlagArray = name: prefix: flags:
            "${name}Array+=( \\\n${makeFlagArrayBody prefix flags})";

        in stdenv.mkDerivation {
          pname = "jak";
          inherit version;

          # Workaround until `src = self;` works with Git submodules.
          src = runCommand "source" {
            allowSubstitutes = false;
            preferLocalBuild = true;
            src = self;
          } ''
            cp -R "$src" "$out"
            chmod -R u+w "$out"
            shopt -s nullglob dotglob
            files=("$out/third-party/googletest"/*); if (( ''${#files[*]} == 0 )); then
              echo "providing third-party/googletest submodule via Nix"
              rm -df "$out/third-party/googletest"
              cp -R ${sh jak-googletest-src} "$out/third-party/googletest"
            fi
            files=("$out/third-party/zydis"/*); if (( ''${#files[*]} == 0 )); then
              echo "providing third-party/zydis submodule via Nix"
              rm -df "$out/third-party/zydis"
              cp -R ${sh jak-zydis-src} "$out/third-party/zydis"
            fi
          '';

          nativeBuildInputs = [
            buildPackages.cmake
            buildPackages.nasm
          ] ++ lib.optionals enableDevInputs [
            buildPackages.clang-tools # clang-format
            python3Packages.pyqt5
            python3Packages.python
          ];

          preConfigure = let
            cmakeFlags = lib.pipe {
              # https://github.com/NixOS/nixpkgs/pull/108496
              CMAKE_SKIP_BUILD_RPATH = "OFF";
            } [
              (optionalFun enableAsan (o: o // {
                ASAN_BUILD = "TRUE";
              }))
              (optionalFun enableFramePointer (o: o // {
                CMAKE_C_FLAGS = o.CMAKE_C_FLAGS or "" + " -fno-omit-frame-pointer";
                CMAKE_CXX_FLAGS = o.CMAKE_CXX_FLAGS or "" + " -fno-omit-frame-pointer";
              }))
              (optionalFun enablePIE (o: o // {
                POSITION_INDEPENDENT_CODE = "TRUE";
              }))
              (optionalFun enableSourceLevelDebug (o: o // {
                CMAKE_C_FLAGS = o.CMAKE_C_FLAGS or "" + " -g";
                CMAKE_CXX_FLAGS = o.CMAKE_CXX_FLAGS or "" + " -g";
              }))
            ];
          in ''
            ${makeFlagArray "cmakeFlags" "-D" cmakeFlags}
          '';

          doCheck = true;

          dontStrip = enableAsan;

          preFixup = ''
          '' + lib.optionalString (enableAsan && llvm-symbolizer != null) ''
            for f in "$out/bin"/*; do
              wrapProgram "$f" --set LLVM_SYMBOLIZER ${sh llvm-symbolizer}/bin/llvm-symbolizer
            done
          '';

          meta = with lib; {
            description = "OpenGOAL port of Naughty Dog's GOAL";
            homepage = "https://github.com/water111/jak-project";
            license = lib.licenses.isc;
            maintainers = with maintainers; [ bb010g ];
            platforms = platforms.all;
          };
        }
      ) { };
      jak-dev = pkgs.jak.override { enableDevInputs = true; };

      jak-asan = pkgs.jak.override {
        inherit (pkgs.llvmPackages) llvm;
        enableAsan = true;
        stdenv = pkgs.clangStdenv;
      };
      jak-asan-dev = pkgs.jak-asan.override { enableDevInputs = true; };

      jak-googletest-src = pkgs.callPackage ({ fetchFromGitHub }: fetchFromGitHub {
        owner = "google";
        repo = "googletest";
        rev = "adeef192947fbc0f68fa14a6c494c8df32177508";
        sha256 = "1nsl1c5il6mzwggs5fqcp8gyddk9rs6257vlz0zgpik32miq3cgw";
      }) { };

      jak-zydis-src = pkgs.callPackage ({ fetchFromGitHub }: fetchFromGitHub {
        owner = "zyantific";
        repo = "zydis";
        rev = "c88a4b0cc1271b516b9697af8d088c851745dd60";
        sha256 = "1j28vxr3r6w8xawlagcicbd95dcs85shgsm11b98g3qj45bv3haq";
        fetchSubmodules = true;
      }) { };
    };

    packages = forAllSystems (system:
      let systemPackages = (import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      }); in forAllPackages (pname: systemPackages.${pname})
    );

    defaultPackage = forAllSystems (system:
      self.packages.${system}.jak
    );

    devShell = forAllSystems (system:
      self.packages.${system}.jak-dev
    );

  };
}
