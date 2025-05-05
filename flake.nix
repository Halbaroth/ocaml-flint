{
  description = "OCaml flint";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    rec {
      packages = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ocamlPackages = pkgs.ocamlPackages;
        in
        {
          default = self.packages.${system}.ocaml-flint;

          ocaml-flint = ocamlPackages.buildDunePackage {
            pname = "ocaml-flint";
            version = "0.0.1";
            duneVersion = "3";
            src = ./.;

            buildInputs = (with pkgs; [
              bash
              gcc
              gnumake
              pkg-config
              m4

              gmp
              flint
              mpfr
            ]) ++ (with ocamlPackages; [
              ocaml
              dune_3
              dune-configurator
              dune-site

              ctypes
              zarith
            ]);

          };
        });

      devShells = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ocamlPackages = pkgs.ocamlPackages;
        in
        {
          default = pkgs.mkShell {
            packages = (with pkgs; [
              nixpkgs-fmt
            ]) ++ (with ocamlPackages; [
              odoc
              ocaml-lsp
              utop
            ]);

            inputsFrom = [
              self.packages.${system}.ocaml-flint
            ];
          };
        });
    };
}
