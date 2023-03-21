{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    fortran-src = {
      url = "github:camfort/fortran-src";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          # packages.example.root = ./.;  # This value is detected based on .cabal files
          overrides = self: super: {
            fortran-src = self.callCabal2nix "fortran-src" inputs.fortran-src {};
          };
          # devShell = {
          #  enable = true;  # Enabled by default
          #  tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          #  hlsCheck.enable = true;
          # };
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.fortran-src-extras;
      };
    };
}
