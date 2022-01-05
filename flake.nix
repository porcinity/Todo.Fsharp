{
  description = "Initial Flake file";

  inputs = {

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    nixpkgs = {
      url = "nixpkgs/nixos-21.11";
    };

  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs
            (with dotnetCorePackages; combinePackages [
              dotnet-sdk_6
              dotnet-sdk_5
              dotnetPackages.Nuget
            ])
          ];
        };
      });
}
