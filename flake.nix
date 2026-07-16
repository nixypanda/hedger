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
              ghc
              hledger
              just
              nodejs_22
              python3
            ];
          };
        }
      );

      formatter = forAllSystems (system: (import nixpkgs { inherit system; }).nixfmt-tree);

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          workspace =
            pkgs.runCommand "hedger-workspace-check"
              {
                nativeBuildInputs = with pkgs; [
                  cabal-install
                  elmPackages.elm-format
                  ghc
                  hledger
                  just
                  python3
                ];
              }
              ''
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
