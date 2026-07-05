{
  inputs = {

    nixpkgs = {
      url = "github:nixos/nixpkgs";
    };

  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "i686-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forEachSystem = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages."${system}";
        in
        {
          default = pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.rure
              pkgs.rure.dev
              pkgs.pkg-config
            ];
            LD_LIBRARY_PATH = "${pkgs.rure}/lib";
            # shellHook = ''
            #   echo "Updated rure-dev is at ${pkgs.rure.dev}"
            # '';
          };
        }
      );
    };
}
