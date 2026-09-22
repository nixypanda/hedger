{
  description = "Hedger development workspace";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      backendFor =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        # Ship only the statically linked server binary. Dropping the library and
        # Haddock outputs removes the recorded GHC/toolchain and documentation
        # references that otherwise bloat the deployed closure by gigabytes.
        pkgs.haskell.lib.justStaticExecutables (
          pkgs.haskellPackages.callCabal2nix "hedger-backend" ./backend { }
        );
      frontendFor =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          registry =
            pkgs.runCommand "hedger-elm-registry.dat"
              {
                nativeBuildInputs = [
                  pkgs.coreutils
                  pkgs.gzip
                ];
              }
              ''
                base64 --decode ${./frontend-web/registry.dat.gz.b64} | gzip --decompress > $out
              '';
        in
        pkgs.buildNpmPackage {
          pname = "hedger-frontend";
          version = "0.1.0";
          src = ./frontend-web;
          npmDepsHash = "sha256-MGpNbE7moQI4mAk5RGYwUXg9YZFv/glU7a3mDdBpZeU=";
          nativeBuildInputs = [ pkgs.elmPackages.elm ];
          postConfigure = pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./frontend-web/elm-srcs.nix;
            elmVersion = pkgs.elmPackages.elm.version;
            registryDat = registry;
          };
          npmBuildScript = "build";
          installPhase = ''
            runHook preInstall
            mkdir -p $out/share/hedger
            cp -R dist/. $out/share/hedger/
            runHook postInstall
          '';
        };
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              cabal-install
              elmPackages.elm
              elmPackages.elm-format
              elmPackages.elm-test
              haskellPackages.fourmolu
              haskell-language-server
              (haskellPackages.ghcWithPackages (
                haskell: with haskell; [
                  aeson
                  async
                  fsnotify
                  hedgehog
                  tasty
                  tasty-hedgehog
                  tasty-hunit
                  hledger-lib
                  http-types
                  mtl
                  math-functions
                  network
                  optparse-applicative
                  servant
                  servant-server
                  tasty-bench
                  uuid
                  wai
                  wai-extra
                  warp
                  yaml
                ]
              ))
              hledger
              just
              nodejs_22
              python3
            ];
          };
        }
      );

      formatter = forAllSystems (system: (import nixpkgs { inherit system; }).nixfmt-tree);

      packages = forAllSystems (system: {
        default = backendFor system;
        hedger-backend = backendFor system;
        hedger-frontend = frontendFor system;
      });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${backendFor system}/bin/hedger-server";
          meta.description = "Run the Hedger read-only ledger backend";
        };
      });

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          backend = backendFor system;
          backendGhc = pkgs.haskellPackages.ghcWithPackages (
            haskell: with haskell; [
              aeson
              async
              fsnotify
              hedgehog
              tasty
              tasty-hedgehog
              tasty-hunit
              hledger-lib
              http-types
              math-functions
              network
              mtl
              optparse-applicative
              servant
              servant-server
              tasty-bench
              wai
              wai-extra
              warp
              yaml
            ]
          );
          elmRegistry =
            pkgs.runCommand "hedger-elm-registry.dat"
              {
                nativeBuildInputs = [
                  pkgs.coreutils
                  pkgs.gzip
                ];
              }
              ''
                base64 --decode ${./frontend-web/registry.dat.gz.b64} | gzip --decompress > $out
              '';
          prepareElmDependencies = pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./frontend-web/elm-srcs.nix;
            elmVersion = pkgs.elmPackages.elm.version;
            registryDat = elmRegistry;
          };
        in
        {
          backend = backend;
          frontend = frontendFor system;
          workspace =
            pkgs.runCommand "hedger-workspace-check"
              {
                nativeBuildInputs = with pkgs; [
                  cabal-install
                  elmPackages.elm
                  elmPackages.elm-format
                  elmPackages.elm-test
                  haskellPackages.fourmolu
                  backendGhc
                  hledger
                  just
                  python3
                  ripgrep
                ];
              }
              ''
                export HOME=$TMPDIR
                cp -R ${self} source
                chmod -R u+w source
                cd source
                ${prepareElmDependencies}
                just check
                touch $out
              '';
        }
      );

      nixosModules.default = import ./deploy/module.nix { inherit self; };
    };
}
