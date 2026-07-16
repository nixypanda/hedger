{
  description = "Hedger development workspace";

  # 26.05 is the last supported nixpkgs line for Intel macOS and also keeps
  # Linux CI on the same pinned package set.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";

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
        pkgs.haskellPackages.callCabal2nix "hedger-backend" ./backend { };
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
              (haskellPackages.ghcWithPackages (
                haskell: with haskell; [
                  aeson
                  hspec
                  http-types
                  mtl
                  optparse-applicative
                  servant
                  servant-server
                  wai
                  wai-extra
                  warp
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
              hspec
              http-types
              mtl
              optparse-applicative
              servant
              servant-server
              wai
              wai-extra
              warp
            ]
          );
        in
        {
          backend = backend;
          workspace =
            pkgs.runCommand "hedger-workspace-check"
              {
                nativeBuildInputs = with pkgs; [
                  cabal-install
                  elmPackages.elm-format
                  haskellPackages.fourmolu
                  backendGhc
                  hledger
                  just
                  python3
                ];
              }
              ''
                export HOME=$TMPDIR
                cp -R ${self} source
                chmod -R u+w source
                cd source
                just check
                touch $out
              '';
        }
      );
    };
}
