{
  description = "Development environment for Protein Design Archive (PDA)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Backend dependencies & tools
            python311
            uv

            # Frontend dependencies & tools
            nodejs_22
            elmPackages.elm
            elmPackages.elm-format

            # Database & container tools
            docker-compose
            mongodb-tools
          ];

          shellHook = ''
            echo "Protein Design Archive (PDA) dev environment loaded!"
            echo "Python: $(python3 --version)"
            echo "uv:     $(uv --version)"
            echo "Node:   $(node --version)"
            echo "Elm:    $(elm --version)"
          '';
        };
      }
    );
}
