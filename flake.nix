{
  description = "OCaml dev and build environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              dune_3
              ocaml
              ocamlPackages.findlib
              ocamlPackages.ocaml-lsp
              ocamlPackages.alcotest
              pre-commit
            ];
            shellHook = ''
              echo "OCaml dev shell"
            '';
          };
        }
      );

      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          ocamlPackages = pkgs.ocamlPackages;
          staticPkgs = pkgs.pkgsStatic;
          mkPkg = ocamlPkgs: ocamlPkgs.buildDunePackage {
            pname = "xoml";
            version = "0.1.9";
            src = ./.;
          };
        in {
          default = mkPkg ocamlPackages;
          static  = mkPkg staticPkgs.ocamlPackages;
        }
      );

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/xoml";
        };
      });

    };
}

